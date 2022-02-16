#' Extract risk factors interaction from decision tree
#'
#' \code{interaction.transformer} extracts the interaction between supplied risk factors from decision tree.
#' It implements customized decision tree algorithm that takes into account different conditions such as minimum 
#' percentage of observations and defaults in each node, maximum tree depth and monotonicity condition 
#' at each splitting node. Gini index is used as metric for node splitting .
#'@param db Data frame of risk factors and target variable supplied for interaction extraction.
#'@param rf Character vector of risk factor names on which decision tree is run.
#'@param target Name of target variable (default indicator 0/1) within db argument.
#'@param min.pct.obs Minimum percentage of observation in each leaf.
#'@param min.avg.rate Minimum percentage of defaults in each leaf.
#'@param max.depth Maximum number of splits.
#'@param monotonicity Logical indicator. If \code{TRUE}, observed trend between risk factor and target will be preserved
#'			    in splitting node.
#'@param create.interaction.rf Logical indicator. If \code{TRUE}, second element of the output will be data frame with
#'					 interaction modalities.
#'@return The command \code{interaction.transformer} returns a list of two data frames. The first data frame provides
#' the tree summary. The second data frame is a new risk factor extracted from decision tree.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#modify risk factors in order to show how the function works with missing values
#'loans$"Account Balance"[1:10] <- NA
#'loans$"Duration of Credit (month)"[c(13, 15)] <- NA
#'it <- interaction.transformer(db = loans, #
#'					 rf = c("Account Balance", "Duration of Credit (month)"), 
#'					 target = "Creditability",
#'					 min.pct.obs = 0.05,
#'					 min.avg.rate = 0.01,
#'					 max.depth = 2,
#'				 	 monotonicity = TRUE,
#'					 create.interaction.rf = TRUE)
#'names(it)
#'it[["tree.info"]]
#'tail(it[["interaction"]])
#'table(it[["interaction"]][, "rf.inter"], useNA = "always")
#'@import dplyr
#'@importFrom stats ave
#'@export
interaction.transformer <- function(db, rf, target, min.pct.obs, min.avg.rate, max.depth, monotonicity, create.interaction.rf) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	if	(any(!c(rf, target)%in%names(db))) {
		stop("rf or target variable do not exist in supplied db.")
		}
	if	(!is.numeric(min.pct.obs) | !is.numeric(min.avg.rate) |
		 min.pct.obs <= 0 | min.pct.obs >= 1 |
		 min.avg.rate <= 0 | min.avg.rate >= 1) {
		stop(" min.pct.obs and min.avg.rate has to be numeric values greater than 0 and less than 1")
		}
	if	(!is.na(max.depth)) {
		if	(!is.numeric(max.depth) | max.depth < 2) {
			stop("max.depth has to be numeric value greater than 2.")
			}
		}
	if	(is.na(monotonicity) | !is.logical(monotonicity)) {
		stop("monotonicity has to be logical value - TRUE or FALSE")
		}
	if	(is.na(create.interaction.rf) | !is.logical(create.interaction.rf)) {
		stop("create.interaction.rf has to be logical value - TRUE or FALSE")
		}	
	d <- db[, c(target, rf)]
	d <- d[!is.na(d[, target]), ]
	nr <- nrow(d)
	if	(nr == 0) {
		stop("target contains only missing values.")
		}
	if	(length(unique(d[, target])) == 1) {
		stop("target has only 1 class.")
		}
	if	(!sum(d[, target]%in%c(0, 1)) == nr) {
		stop("target contains value(s) different than 0 and 1.")
		}
	nd <- sum(d[, target])
	rf.check <- sapply(d[, rf, drop = FALSE], function(x) sum(is.na(x)) == length(x))
	if	(any(rf.check)) {
		msg <- paste0("The following risk factor(s) contain(s) only missing values: ", 
				  paste0(names(rf.check)[rf.check], collapse = ", "), ".")
		stop(msg)
		}		
	tree.res <- dt.algo(d = d, 
			    rf = rf, 
			    target = target, 
			    min.obs = ceiling(ifelse(nr * min.pct.obs < 30, 30, nr * min.pct.obs)), 
			    min.rate = ceiling(ifelse(nd * min.avg.rate < 1, 1, nd * min.avg.rate)), 
			    max.depth = max.depth, 
			    monotonicity = monotonicity)
	if	(create.interaction.rf) {
		interaction <- extract.interactions(db = db, tree.info = tree.res)
		} else {
		interaction <- data.frame()
		}
return(list(tree.info = tree.res, interaction = interaction))
}

#decision tree algo
dt.algo <- function(d, rf, target, min.obs, min.rate, max.depth, monotonicity) {
	rf.num <- sapply(d[, rf, drop = FALSE], is.numeric)
	rf.num <- names(rf.num)[rf.num]
	rf.cat <- rf[!rf%in%rf.num]	
	if	(monotonicity & length(rf.num) > 0) {
		corr.dir <- sapply(rf.num, function(x) sign(cor(d[, x], d[, target], 
						    			  method = "spearman", 
									  use = "complete.obs")))
		} else {
		corr.dir <- c()
		}
	#initialize the process
	do.splits <- TRUE
	tree.info <- data.frame(node = 1, 
				rf = NA, 
				nobs = nrow(d), 
				filter = NA,
				terminal = "split",
				stringsAsFactors = FALSE)
	#start splitting
	while	(do.splits) {
		to.calculate <- which(tree.info$terminal == "split")
		for	(i in to.calculate) {
      		if	(!is.na(tree.info$filter[i])) {
        			tbl.l <- subset(d, eval(parse(text = tree.info$filter[i])))
      			} else {
        			tbl.l <- d
      			}	
			splitting <- lapply(rf, function(x) {
							best.split(tbl = tbl.l, 
								     rf.l = x, 
								     target = target, 
								     min.obs = min.obs, 
								     min.rate = min.rate,   
								     monodir = corr.dir[x])})
			bs.rf.indx <- which.max(sapply(splitting, "[[", "ssv"))
			if	(length(bs.rf.indx) == 0) {
				tree.info$terminal[i] <- "leaf"
				next
				}
			tmp.sp <- splitting[[bs.rf.indx]]
			mn <- max(tree.info$node) 
			if	(tmp.sp$rf.t%in%"numeric") {
				tmp.filter <- c(paste(tmp.sp$rf, "<", tmp.sp$split), paste(tmp.sp$rf, ">=", tmp.sp$split))
				} else {
				tmp.filter <- c(paste0(tmp.sp$rf, "%in%", tmp.sp$split), paste0("!", tmp.sp$rf, "%in%", tmp.sp$split))
				}
			if	(!is.na(tree.info$filter[i])) {
				tmp.filter <- paste(tree.info$filter[i], tmp.filter, sep = " & ")
			 	} 
			tmp.filter.nobs <- paste(paste0("!is.na(", tmp.sp$rf, ")"), tmp.filter, sep = " & ")
			tmp.nobs <- sapply(tmp.filter.nobs, FUN = function(i, x) {		
									 nrow(subset(x = x, subset = eval(parse(text = i))))
									 }, x = tbl.l) 
			children <- data.frame(node = c(mn + 1, mn + 2),
					       rf = tmp.sp$rf, 
					       nobs = unname(tmp.nobs),
				               filter = tmp.filter,
					       terminal = rep("split", 2),
					       row.names = NULL)    
			tree.info$terminal[i] <- "parent"
			tree.info <- rbind(tree.info, children)
			if	(!is.na(max.depth) & sum(tree.info$terminal%in%c("leaf", "split")) > max.depth) {
				tree.info$terminal[tree.info$terminal%in%"split"] <- "leaf"
				do.splits <- FALSE
				break
				} 
			}
		do.splits <- !all(tree.info$terminal != "split")
		}
	tree.info$ind <- as.numeric(ave(tree.info$terminal, tree.info$terminal, 
				   		  FUN = function(x) if	(all(x%in%"leaf")) {1:length(x)} else {NA}))
return(tree.info)
}
#node split functions
best.split <- function(tbl, rf.l, target, min.obs, min.rate, monodir) {
	rf.c <- is.numeric(tbl[, rf.l])
	if	(rf.c) {
		 bs <- best.split.num(tbl = tbl, 
				      rf.l = rf.l, 
			  	      target = target, 
				      min.obs = min.obs, 
				      min.rate = min.rate,   
				      monodir = monodir)
		bs <- cbind.data.frame(rf.t = "numeric", bs)	
		} else {
		bs <- best.split.cat(tbl = tbl, 
				     rf.l = rf.l, 
				     target = target, 
				     min.obs = min.obs, 
				     min.rate = min.rate)
		bs <- cbind.data.frame(rf.t = "categorical", bs)
		}
return(bs)
}
best.split.num <- function(tbl, rf.l, target, min.obs, min.rate, monodir) {
	g <- unname(quantile(tbl[, rf.l],seq(1 / 50, 1, length.out = 50), type = 3, na.rm = TRUE))
	g <- unique(c(min(tbl[, rf.l], na.rm = TRUE), g))
	if	(length(g) == 1 | all(is.na(tbl[, rf.l]))) {
		res <- data.frame(rf = paste0("`", rf.l, "`"), ssv = NA, split = NA, y.l = NA, y.r = NA)
		return(res)
		}
    	tbl$bin <- cut(tbl[, rf.l], breaks = g, include.lowest = TRUE)
	tbl <- tbl[complete.cases(tbl), ]
	tbl.s <- tbl %>%
		   group_by(bin) %>%
		   summarise(n = n(),
		   y.sum = sum(!!sym(target)),
		   y.avg = mean(!!sym(target)), 
		   x.min = min(!!sym(rf.l)),
		   x.max = max(!!sym(rf.l))) %>%
		   ungroup() %>%
		   mutate(n.cs = cumsum(n),
			    y.cs = cumsum(y.sum),
			    y.cs.a = y.cs / n.cs,
			    n.cs.rev = cumsum(rev(n)),
			    y.cs.rev = cumsum(rev(y.sum)),
			    y.cs.a.rev = y.cs.rev / n.cs.rev,
			    dr.c = y.cs / n.cs,
			    dr.r = (sum(y.sum) - y.cs) / (sum(n) - n.cs))
	lt <- min(which(tbl.s$n.cs >= min.obs & tbl.s$y.cs >= min.rate)) + 1
	ut <- nrow(tbl.s) - min(which(tbl.s$n.cs.rev >= min.obs & tbl.s$y.cs.rev >= min.rate)) 
	if	(lt > ut) {
		res <- data.frame(rf = paste0("`", rf.l, "`"), ssv = NA, split = NA, y.l = NA, y.r = NA)
		return(res)
		}
	if	(length(monodir) > 0) {
		if	(monodir == 1) {
			ms <- which(tbl.s$dr.c < tbl.s$dr.r)
			} else {
			ms <- which(tbl.s$dr.c > tbl.s$dr.r)
			}
		} else {
		ms <- lt:ut
		}
	sp <- intersect(lt:ut, ms)	
	if	(length(sp) == 0) {
		res <- data.frame(rf = paste0("`", rf.l, "`"), ssv = NA, split = NA, y.l = NA, y.r = NA)
		return(res)
		}
	splits <- tbl.s$x.min[sp]
	sl <- length(splits)
	ssv <- rep(NA, sl)
	for	(i in 1:sl) {
		split.l <- splits[i]
		ssv[i] <- gini.num(y = tbl[, target], x = tbl[, rf.l], sp = split.l)
		}
	split.at <- splits[which.max(ssv)]
	y.l <- mean(tbl[tbl[, rf.l] < split.at, target])
	y.r <- mean(tbl[tbl[, rf.l] >= split.at, target])
	res <- data.frame(rf = paste0("`", rf.l, "`"), ssv = min(ssv), split = split.at, y.l = y.l, y.r = y.r)
return(res)
}
best.split.cat <- function(tbl, rf.l, target, min.obs, min.rate) {
	if	(all(is.na(tbl[, rf.l]))) {
		res <- data.frame(rf = paste0("`", rf.l, "`"), ssv = NA, split = NA, y.l = NA, y.r = NA)
		return(res)
		}
    	tbl$bin <- tbl[, rf.l]
	tbl <- tbl[complete.cases(tbl), ]
	tbl.s <- tbl %>%
		   group_by(bin) %>%
		   summarise(n = n(),
		   y.sum = sum(!!sym(target)),
		   y.avg = mean(!!sym(target)), 
		   x.min = min(!!sym(rf.l)),
		   x.max = max(!!sym(rf.l))) %>%
		   ungroup() %>%
		   mutate(n.cs = cumsum(n),
			    y.cs = cumsum(y.sum),
			    y.cs.a = y.cs / n.cs,
			    n.cs.rev = cumsum(rev(n)),
			    y.cs.rev = cumsum(rev(y.sum)),
			    y.cs.a.rev = y.cs.rev / n.cs.rev,
			    dr.c = y.cs / n.cs,
			    dr.r = (sum(y.sum) - y.cs) / (sum(n) - n.cs))
	lt <- min(which(tbl.s$n.cs >= min.obs & tbl.s$y.cs >= min.rate))
	ut <- nrow(tbl.s) - min(which(tbl.s$n.cs.rev >= min.obs & tbl.s$y.cs.rev >= min.rate)) 
	if	(lt > ut) {
		res <- data.frame(rf = paste0("`", rf.l, "`"), ssv = NA, split = NA, y.l = NA, y.r = NA)
		return(res)
		}
	splits <- lt:ut	
	sl <- length(splits)
	ssv <- rep(NA, sl)
	for	(i in 1:sl) {
		split.l <- tbl.s$bin[1:splits[i]]
		ssv[i] <- gini.cat(y = tbl[, target], x = tbl[, rf.l], sp = split.l)
		}
	split.at <- splits[which.max(ssv)]
	y.l <- mean(tbl[tbl[, rf.l]%in%tbl.s$bin[1:split.at], target])
	y.r <- mean(tbl[!tbl[, rf.l]%in%tbl.s$bin[1:split.at], target])
	split.at <- paste0("c(", paste(paste0("'", tbl.s$bin[1:split.at], "'"), collapse = ", "), ")")
	res <- data.frame(rf = paste0("`", rf.l, "`"), ssv = min(ssv), split = split.at, y.l = y.l, y.r = y.r)
return(res)
}
#gini
gini.num <- function(y, x, sp) {
	ct <- table(y, x < sp)
	nx <-  apply(ct, 2, sum)
	n <- sum(ct)
	pxy <- ct / matrix(rep(nx, each = 2), nrow = 2)
	omega <- matrix(rep(nx, each = 2), nrow = 2) / n
	res <- -sum(omega * pxy * (1 - pxy))
return(res)
}
gini.cat <- function(y, x, sp) {
	ct <- table(y, x%in%sp)
	nx <-  apply(ct, 2, sum)
	n <- sum(ct)
	pxy <- ct / matrix(rep(nx, each = 2), nrow = 2)
	omega <- matrix(rep(nx, each = 2), nrow = 2) / n
	res <- -sum(omega * pxy * (1 - pxy))
return(res)
}
#interactions
extract.interactions <- function(db, tree.info) {
	if	(nrow(tree.info) == 1) {
		return(data.frame())
		}
	rf.tree <- unique(tree.info$rf[!is.na(tree.info$rf)])
	tree.leaves <- tree.info[tree.info$terminal%in%"leaf", ]
	inter.c <- rep(NA, nrow(db))
	for	(i in 1:nrow(tree.leaves)) {
		filter <- tree.leaves$filter[i]
		rf.s <- tree.leaves$rf[i]
		filter.eval <- paste0(c(paste0("!is.na(", rf.s, ")"), filter), collapse = " & ")
		indx <- with(db, {eval(parse(text = filter.eval))})
		inter.c[indx] <- i
		}
	interaction <- data.frame(rf.inter = inter.c)
return(interaction)
}


