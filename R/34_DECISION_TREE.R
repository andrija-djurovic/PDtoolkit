#' Custom decision tree algorithm
#'
#' \code{decision.tree} runs customized decision tree algorithm. Customization refers to minimum 
#' percentage of observations and defaults in each node, maximum tree depth, monotonicity condition 
#' at each splitting node and statistical test (test of two proportions) used for node splitting.
#'@param db Data frame of risk factors and target variable supplied for interaction extraction.
#'@param rf Character vector of risk factor names on which decision tree is run.
#'@param target Name of target variable (default indicator 0/1) within db argument.
#'@param min.pct.obs Minimum percentage of observation in each leaf. Default is 0.05.
#'@param min.avg.rate Minimum percentage of defaults in each leaf. Default is 0.01.
#'@param p.value Significance level of test of two proportions for splitting criteria. Default is 0.05.
#'@param max.depth Maximum tree depth.
#'@param monotonicity Logical indicator. If \code{TRUE}, observed trend between risk factor and target will be preserved
#'			    in splitting node.
#'@return The command \code{decision.tree} returns a object of class cdt. For details on output elements see the Examples.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#modify risk factors in order to show how the function works with missing values
#'loans$"Account Balance"[1:10] <- NA
#'loans$"Duration of Credit (month)"[c(13, 15)] <- NA
#'tree.res <- decision.tree(db = loans,
#'		rf = names(loans)[!names(loans)%in%"Creditability"], 
#'		target = "Creditability",
#'		min.pct.obs = 0.05,
#'		min.avg.rate = 0.01,
#'		p.value = 0.05,
#'		max.depth = NA,
#'		monotonicity = TRUE)
#'str(tree.res)
#'@export
decision.tree <- function(db, rf, target, min.pct.obs = 0.05, min.avg.rate = 0.01, p.value = 0.5, max.depth = NA, monotonicity) {
	ops <- options(warn = -1)
	on.exit(options(ops)) 
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
	tree.res <- cdt.algo(d = d, 
			    rf = rf, 
			    target = target, 
			    min.obs = ceiling(ifelse(nr * min.pct.obs < 30, 30, nr * min.pct.obs)), 
			    min.rate = ceiling(ifelse(nd * min.avg.rate < 1, 1, nd * min.avg.rate)), 
			    p.value = p.value, 
			    max.depth = max.depth, 
			    monotonicity = monotonicity)
	where <- extract.interactions(db = db, tree.info = tree.res)
	nodes <- unname(c(where, recursive = TRUE))
	averages <- cbind.data.frame(where = nodes, target = db[, target]) %>%
			group_by(where) %>%
			summarise(average = mean(target, na.rm = TRUE))
	averages <- averages[!is.na(averages$where), ]
			
return(structure(list(inputs = data.frame(target, min.pct.obs, min.avg.rate, p.value, max.depth, monotonicity),
			    tree.info = tree.res, 
			    where = nodes,
			    average = data.frame(averages),
			    data = db), 
		     class = "cdt"))
}

#customized decision tree algo
cdt.algo <- function(d, rf, target, min.obs, min.rate, p.value, max.depth, monotonicity) {
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
							c.best.split(tbl = tbl.l, 
									 rf.l = x, 
									 target = target, 
									 min.obs = min.obs, 
									 min.rate = min.rate,  
									 p.value = p.value,  
									 monodir = corr.dir[x])})
			bs.rf.indx <- which.min(sapply(splitting, "[[", "p.val"))
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
#customized node split functions
c.best.split <- function(tbl, rf.l, target, min.obs, min.rate, p.value, monodir) {
	rf.c <- is.numeric(tbl[, rf.l])
	if	(rf.c) {
		 bs <- c.best.split.num(tbl = tbl, 
						rf.l = rf.l, 
			  			target = target, 
						min.obs = min.obs, 
						min.rate = min.rate,   
						p.value = p.value, 
						monodir = monodir)
		bs <- cbind.data.frame(rf.t = "numeric", bs)	
		} else {
		bs <- c.best.split.cat(tbl = tbl, 
					     rf.l = rf.l, 
					     target = target, 
					     min.obs = min.obs, 
					     min.rate = min.rate,
					     p.value = p.value)
		bs <- cbind.data.frame(rf.t = "categorical", bs)
		}
return(bs)
}
c.best.split.num <- function(tbl, rf.l, target, min.obs, min.rate, p.value, monodir) {
	g <- unname(quantile(tbl[, rf.l],seq(1 / 50, 1, length.out = 50), type = 3, na.rm = TRUE))
	g <- unique(c(min(tbl[, rf.l], na.rm = TRUE), g))
	if	(length(g) == 1 | all(is.na(tbl[, rf.l]))) {
		res <- data.frame(rf = paste0("`", rf.l, "`"), p.val = NA, split = NA)
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
		res <- data.frame(rf = paste0("`", rf.l, "`"), p.val = NA, split = NA)
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
		res <- data.frame(rf = paste0("`", rf.l, "`"), p.val = NA, split = NA)
		return(res)
		}
	splits <- tbl.s$x.min[sp]
	sl <- length(splits)
	p.val <- rep(NA, sl)
	for	(i in 1:sl) {
		split.l <- splits[i]
		p.val[i] <- t2p.num(y = tbl[, target], 
					  x = tbl[, rf.l], 
					  sp = split.l, 
					  monodir = monodir)
		}
	split.at <- splits[which.min(p.val)[1]]
	check <- min(p.val) < p.value
	split.at <- ifelse(check, split.at, NA)
	p.val <- ifelse(check , min(p.val), NA)
	res <- data.frame(rf = paste0("`", rf.l, "`"), p.val = p.val, split = split.at)
return(res)
}
c.best.split.cat <- function(tbl, rf.l, target, min.obs, min.rate, p.value) {
	if	(all(is.na(tbl[, rf.l]))) {
		res <- data.frame(rf = paste0("`", rf.l, "`"), p.val = NA, split = NA)
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
		res <- data.frame(rf = paste0("`", rf.l, "`"), p.val = NA, split = NA)
		return(res)
		}
	splits <- lt:ut	
	sl <- length(splits)
	p.val <- rep(NA, sl)
	monodir <- sign(cor(1:length(tbl.s$bin[!is.infinite(tbl.s$y.avg)]), 
				  tbl.s$y.avg[!is.infinite(tbl.s$y.avg)], 
				  method = "spearman", 
				  use = "complete.obs"))
	for	(i in 1:sl) {
		split.l <- tbl.s$bin[1:splits[i]]
		p.val[i] <- t2p.cat(y = tbl[, target], 
					  x = tbl[, rf.l], 
					  sp = split.l,
					  monodir = monodir)
		}
	split.at <- splits[which.min(p.val)]
	check <- min(p.val) < p.value
	split.at <- ifelse(check, paste0("c(", paste(paste0("'", tbl.s$bin[1:split.at], "'"), collapse = ", "), ")"), NA)
	p.val <- ifelse(check , min(p.val), NA)
	res <- data.frame(rf = paste0("`", rf.l, "`"), p.val = min(p.val), split = split.at)
return(res)
}
#test of two proportions
t2p.num <- function(y, x, sp, monodir) {
	if	(length(monodir) > 0) {
		alternative <- unname(ifelse(monodir == 1, "less", "greater"))
		} else {
		alternative <- "two.sided" 
		}
	res <- prop.test(x = c(sum(y[x < sp]), sum(y[x >= sp])), 
			     n = c(sum(x < sp), sum(x >= sp)), 
			     alternative = alternative, 
			     correct = FALSE)$p.value
return(res)
}
t2p.cat <- function(y, x, sp, monodir) {
	alternative <- ifelse(monodir == 1, "less", "greater")
	res <- prop.test(x = c(sum(y[x%in%sp]), sum(y[!x%in%sp])), 
			     n = c(sum(x%in%sp), sum(!x%in%sp)), 
			     alternative = alternative, 
			     correct = FALSE)$p.value
return(res)
}

#' Predict method for custom decision tree
#'
#'@param object Custom decision tree model (class cdt).
#'@param newdata Optionally, a data frame in which to look for variables with which to predict. 
#'               If omitted, the fitted predictors are used.
#'@param ... further arguments passed to or from other methods. 
#'@return Returns average default rate along with leaf identificator.
#'@examples
#'# S3 method for class "cdt"
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'tree.res <- decision.tree(db = loans,
#'		rf = names(loans)[!names(loans)%in%"Creditability"], 
#'		target = "Creditability",
#'		min.pct.obs = 0.05,
#'		min.avg.rate = 0.01,
#'		p.value = 0.05,
#'		max.depth = NA,
#'		monotonicity = TRUE)
#'str(tree.res)
#'#predict method - development sample
#'pred.1 <- predict(object = tree.res, newdata = NULL)
#'head(pred.1)
#'auc.model(predictions = pred.1$average, observed = loans$Creditability)
#'#predict method - new data
#'set.seed(321)
#'loans.m <- loans[sample(1:nrow(loans), 500, replace = TRUE), ]
#'pred.2 <- predict(object = tree.res, newdata = loans.m)
#'head(pred.2)
#'auc.model(predictions = pred.2$average, observed = loans.m$Creditability)
#'@rdname predict.cdt
#'@export
predict.cdt <- function(object, newdata = NULL, ...) {
	if	(!class(object)%in%"cdt") {
		stop("object has to be of class cdt")
		}
	avg.pred <- object$average$average 
	names(avg.pred) <- object$average$where
	if	(is.null(newdata)) {
		res <- data.frame(where = object$where)
		res$average <- unname(avg.pred[res$where])
		} else {
		rfs <- unique(object$tree.info$rf)
		rfs <- rfs[!is.na(rfs)]
		rfs <- substr(rfs, start = 2, stop = nchar(rfs) - 1)
		cond <- !rfs%in%names(newdata)
		if	(any(cond)) {
			msg <- paste(rfs[cond], collapse = ", ")
			msg <- paste0("Missing risk factor(s): ", msg, ".")
			stop(msg)
			}
		where <- extract.interactions(db = newdata, tree.info = object$tree.info)
		res <- data.frame(where = unname(c(where, recursive = TRUE)))
		res$average <- unname(avg.pred[res$where])
		}
return(res)
}

