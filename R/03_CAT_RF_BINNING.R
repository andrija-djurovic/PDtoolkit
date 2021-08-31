#' Categorical risk factor binning
#'
#' \code{cat.bin} implements three-stage binning procedure for categorical risk factors. 
#' The first stage is possible correction for minimum percentage of observations.  
#' The second stage is possible correction for target rate (default rate), while the third one is 
#' possible correction for maximum number of bins. Last stage implements procedure known as 
#' adjacent pooling algorithm (APA) which aims to minimize information loss while iterative merging of the bins.
#'@param x Categorical risk factor.
#'@param y Numeric target vector (binary).
#'@param sc Special case elements. Default value is \code{NA}.  
#'@param sc.merge Define how special cases will be treated. Available options are: \cr
#'	   \code{"none", "first", "last", "closest"}. If \code{"none"} is selected, then the special cases
#'	   will be kept in separate bin. If \code{"first"} or \code{"last"} is selected, then the special cases
#'	   will be merged with first or last bin. Depending on sorting option \code{force.trend}, first or last
#'	   bin will be determined based on alphabetic order (if \code{force.trend} is selected as \code{"modalities"}) or 
#'	   on minimum or maximum default rate (if \code{force.trend} is selected as \code{"dr"}).
#'	   If \code{"closest"} is selected, then the special case will be merged with the bin 
#'	   that is closest based on default rate. Merging of the special cases with other bins is performed at the 
#'	   beginning i.e. before running any of three-stage procedures. 
#'@param min.pct.obs Minimum percentage of observations per bin. Default is 0.05 or minimum 30 observations.
#'@param min.avg.rate Minimum default rate. Default is 0.01 or minimum 1 bad case for \code{y} 0/1.
#'@param max.groups Maximum number of bins (groups) allowed for analyzed risk factor. If in the first two stages
#'			  number of bins is less or equal to selected \code{max.groups} or if \code{max.groups} is
#'			  default value (\code{NA}), no adjustment is performed. Otherwise, APA algorithm is applied
#'			  which minimize information loss in further iterative process of group merging. 
#'@param force.trend Defines how initial summary table will be ordered. Possible options are:\cr
#'		     \code{"modalities"} and \code{"dr"}. If  \code{"modalities"} is selected, then merging will be 
#'		     performed forward based on alphabetic order of risk factor modalities. On the other hand,
#'		     if \code{"dr"} is selected, then bins merging will be performed forward based on increasing order of
#' 		     default rate per modality. This direction of merging is applied in the all three stages.
#'@return The command \code{cat.bin} generates a list of two objects. The first object, data frame \code{summary.tbl}
#'presents a summary table of final binning, while \code{x.trans} is a vector of new grouping values.
#'@references 
#'Anderson, R. (2007). The credit scoring toolkit : theory and practice for 
#'			     retail credit risk management and decision automation,  
#'			     Oxford University Press 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#prepare risk factor Purpose for the analysis
#'loans$Purpose <- ifelse(nchar(loans$Purpose) == 2, loans$Purpose, paste0("0", loans$Purpose))
#'#artificially add missing values in order to show functions' features
#'loans$Purpose[1:6] <- NA
#'#run binning procedure
#'res <- cat.bin(x = loans$Purpose, 
#'		   y = loans$Creditability, 
#'		   sc = NA,
#'		   sc.merge = "none",
#'		   min.pct.obs = 0.05, 
#'		   min.avg.rate = 0.05,
#'		   max.groups = NA, 
#'		   force.trend = "modalities")
#'res[[1]]
#'#check new risk factor against the original 
#'table(loans$Purpose, res[[2]], useNA = "always")
#'#repeat the same process with setting max.groups to 4 and force.trend to dr
#'res <- cat.bin(x = loans$Purpose, 
#'		   y = loans$Creditability, 
#'		   sc = NA,
#'		   sc.merge = "none",
#'		   min.pct.obs = 0.05, 
#'		   min.avg.rate = 0.05,
#'		   max.groups = 4, 
#'		   force.trend = "dr")
#'res[[1]]
#'#check new risk factor against the original 
#'table(loans$Purpose, res[[2]], useNA = "always")
#'#example of shrinking number of groups for numeric risk factor
#'#copy exisitng numeric risk factor to new called maturity
#'loans$maturity <- loans$"Duration of Credit (month)"
#'#artificially add missing values in order to show functions' features
#'loans$maturity[1:10] <- NA
#'#categorize maturity with MAPA algorithim from monobin package
#'loans$maturity.bin <- cum.bin(x = loans$maturity, 
#'					y = loans$Creditability, g = 50)[[2]]
#'table(loans$maturity.bin)
#'#run binning procedure
#'res <- cat.bin(x = loans$maturity.bin, 
#'		   y = loans$Creditability, 
#'		   sc = "SC",
#'		   sc.merge = "closest",
#'		   min.pct.obs = 0.05, 
#'		   min.avg.rate = 0.01,
#'		   max.groups = 5, 
#'		   force.trend = "modalities")
#'res[[1]]
#'#check new risk factor against the original 
#'table(loans$maturity.bin, res[[2]], useNA = "always")
#'@import monobin
#'@import dplyr
#'@export
cat.bin <- function(x, y, sc, sc.merge = "none", min.pct.obs = 0.05, min.avg.rate = 0.01,
			  max.groups = NA, force.trend = "modalities") {	
	cond.00 <- !sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])
	if	(cond.00 ) {
		stop("Target is not 0/1 variable.")
		}
	cond.01 <- !(is.character(x) | is.factor(x) | is.logical(x))
	if	(cond.01) {
		stop("Inappropriate class. It has to be one of: character, factor or logical.")
		}
	cond.02 <- !(is.numeric(min.pct.obs) | is.numeric(min.avg.rate))
	if	(cond.01) {
		stop("min.pct.obs and min.avg.rate have to be numeric vectors.")
		}
	force.trend.opt <- c("modalities", "dr")
	if	(!force.trend%in%force.trend.opt) {
		msg <- paste0("force.trend argument has to be one of: ", 
				  paste(force.trend.opt, collapse = ", "), ".")
		stop(msg)
		}
	sc.merge.opt <- c("none", "first", "last", "closest")
	if	(!sc.merge%in%sc.merge.opt) {
		msg <- paste0("sc.merge argument has to be one of: ", 
				  paste(sc.merge.opt, collapse = ", "), ".")
		stop(msg)
		}
	d <- data.frame(y, x)
	d <- d[!is.na(y), ]
	checks.res <- checks(d = d, d.cc = d[!d$x%in%sc, ])
	if	(checks.res[[1]] > 0) {
		return(eval(parse(text = checks.res[[2]])))
		} 
	nr <- nrow(d)
	min.obs <- ceiling(ifelse(nr * min.pct.obs < 30, 30, nr * min.pct.obs))
	nd <- sum(d$y)
	min.rate <- ceiling(ifelse(nd * min.avg.rate < 1, 1, nd * min.avg.rate))
	ds <- d %>% 
		group_by(bin = x) %>%
		summarise(no = length(y),
			    nb = sum(y)) %>%
		ungroup() %>%
		mutate(dr = nb / no)
	if	(force.trend%in%"dr") {
		ds <- ds[order(ds$dr), ]	
		}
	if	(sum(d$x%in%sc) > 0) {
		d$x[d$x%in%sc] <- switch(sc.merge, "none" = d$x[d$x%in%sc],
							     "first" = ds$bin[1],
							     "last" = rev(ds$bin[!ds$bin%in%sc])[1],
							     "closest" = find.closest(ds = ds, sc = sc))
		if	(!sc.merge%in%"none") {
			ds <- d %>% 
				group_by(bin = x) %>%
				summarise(no = length(y),
					    nb = sum(y))%>%
				ungroup() %>%
				mutate(dr = nb / no)
			}
		}
	ds$group <- 1:nrow(ds)
	#correction for num of obs
	ds.no <- tbl.correction(tbl = ds[!ds$bin%in%sc, ], mno = min.obs, mrate = min.rate, what = "obs")
	ds.no.1 <- ds.no[[1]]
	#correction for min num of bads
	ds.nb <- tbl.correction(tbl = ds.no[[2]], mno = min.obs, mrate = min.rate, what = "bad")
	ds.nb.1 <- ds.nb[[1]]
	ds.cor <- merge(ds.no.1, ds.nb.1[, c("bin", "group")], 
			    by.x = "label", by.y = "bin", all.x = TRUE)
	ds.cor$label <- format.labels(g = ds.cor$group.y, b = ds.cor$bin)
	d <- left_join(d, ds.cor[, c("bin", "label")], by = c("x" = "bin"))
	d$label[d$x%in%sc] <- d$x[d$x%in%sc]
	#correction for max num of bins
	ds <- woe.tbl(tbl = d, x = "label", y = "y")
	if	(is.na(max.groups) | nrow(ds) <= max.groups) {	
		summary.tbl <- ds
		x.trans <- d$label
		} else {
		ds.sc <- ds[ds$bin%in%sc, ]
		ds.cc <- ds[!ds$bin%in%sc, ]
		tbl.apa <- apa(tbl = ds.cc, no.sc = ds.sc$no, ng.sc = ds.sc$ng, 
				   nb.sc = ds.sc$nb, mg = max.groups)
		d <- left_join(d, tbl.apa[, c("bin", "label")], by = c("label" = "bin"))
		ds <- d %>%
			filter(!x%in%sc) %>%
			group_by(bin = x) %>%
			summarise(g = unique(label.y))
		ds$label <- format.labels(g = ds$g, b = ds$bin)
		d <- left_join(d[, c("x", "y")], ds[,c("bin", "label")], by = c("x" = "bin"))
		d$label[d$x%in%sc] <- d$x[d$x%in%sc]
		summary.tbl <- woe.tbl(tbl = d, x = "label", y = "y")	
		x.trans <- d$label
		}
return(list(summary.tbl = summary.tbl, x.trans = x.trans))
}

find.closest <- function(ds, sc) {
	dr.sc <- sum(ds$nb[ds$bin%in%sc]) / sum(ds$no[ds$bin%in%sc])
	dr.cc <- ds$dr[!ds$bin%in%sc]
	names(dr.cc) <-  ds$bin[!ds$bin%in%sc]
	wm <- names(which.min(abs(dr.sc - dr.cc)))
return(wm)
}

checks <- function(d, d.cc) {
	cond.01 <- length(unique(d.cc$y)) == 1
	cond.02 <- length(unique(d.cc$x)) == 1
	cond.03 <- !sum(d$y[!is.na(d$y)]%in%c(0, 1)) == length(d$y[!is.na(d$y)])
	cond.04 <- nrow(d.cc) == 0
	cond.all <- c(cond.01, cond.02, cond.03, cond.04)
		if	(sum(cond.all) > 0) {
		which.cond <- min(which(cond.all))
		} else {
		which.cond <- 0
		}
	msger <- switch(as.character(which.cond), 
			    "1" = "data.frame(bin = 'y has single unique value for complete cases')",
			    "2" = "data.frame(bin = 'x has single unique value for complete cases')",	
			    "3" = "stop('y is not 0/1 variable')",
			    "4" = "data.frame(bin = 'no complete cases')")
return(list(which.cond, msger))
}

tbl.correction <- function(tbl, mno, mrate, what) {
	if	(what == "obs") {
		cn <- "no"; thr <- mno
		} else {
		cn <- "nb"; thr <- mrate
		}
	tbl.s <- tbl
	repeat {
		 if	(nrow(tbl) == 1) {break}
		 values <- tbl[, cn]
		 if	(all(values >= thr)) {break}
		 gap <- min(which(values < thr))
		 if	(gap == nrow(tbl)) {
			gm <- tbl$group[(gap - 1):gap] 
			gr <- tbl$group[(gap - 1)]
			tbl.s$group[tbl.s$group%in%gm] <- gr
			tbl$group[(gap - 1):gap]  <- tbl$group[(gap - 1)]
			} else {
			gm <- tbl$group[gap:(gap + 1)] 
			gr <- tbl$group[gap + 1]
			tbl.s$group[tbl.s$group%in%gm] <- gr
			tbl$group[gap:(gap + 1)]  <- tbl$group[gap + 1]
			}	
		 tbl <- tbl %>%
			  group_by(group) %>%
			  summarise(no = sum(no),
					nb = sum(nb))
		} 
	tbl.s$label <- format.labels(g = tbl.s$group, b = tbl.s$bin)
	tbl.np <- tbl.s %>%
		    group_by(bin = label) %>%
		    summarise(no = sum(no),
				  nb = sum(nb))
	tbl.np$group <- 1:nrow(tbl.np)	
return(list(tbl.s, tbl.np))
}

#adjacent pooling alogirthm
sum.adjacent <- function(x, n) stats::filter(x, rep(1, n))[-length(x)]
woe.adjacent <- function(tbl, no.sc, ng.sc, nb.sc) {
	tbl.nr <- nrow(tbl)
	so = sum(tbl$no) + no.sc
	sg = sum(tbl$ng) + ng.sc
	sb = sum(tbl$nb) + nb.sc
	res <- c(NA, nrow(tbl.nr))
	for	(i in 2:tbl.nr) {
		dist.g = sum(tbl$ng[c(i-1, i)]) / sg
		dist.b = sum(tbl$nb[c(i-1, i)]) / sb
		woe = log(dist.g / dist.b)
		iv.b = (dist.g - dist.b) * woe
		res[i] <- iv.b
		}
return(res)
}
woe.calc <- function(tbl, no.sc, ng.sc, nb.sc) {
	so = sum(tbl$no) + no.sc
	sg = sum(tbl$ng) + ng.sc
	sb = sum(tbl$nb) + nb.sc
	dist.g = tbl$ng / sg
	dist.b = tbl$nb / sb
	woe = log(dist.g / dist.b)
	iv.b = (dist.g - dist.b) * woe
return(iv.b)
}

format.labels <- function(g, b) {
	d <- data.frame(g, b)
	ds <- d %>%
		group_by(group = g) %>%
		summarise(bin = paste0("[", paste0(b, collapse = ","), "]"))
	ds$gn <- 1:nrow(ds)
	nd <- nchar(max(ds$gn))
	ds$gn <- sprintf(paste0("%0",nd,"d"), ds$gn)	
	label <- paste(ds$gn, ds$bin)
	names(label) <- ds$group
	label.f <- unname(label[as.character(d$g)])
return(label.f)
}

apa <- function(tbl, no.sc, ng.sc, nb.sc, mg) {
	if	(nrow(tbl) <= mg) {
		return(tbl)
		}
	tbl$group <- 1:nrow(tbl)
	tbl.s <- tbl
	no.sc <- sum(no.sc)
	ng.sc <- sum(ng.sc)
	nb.sc <- sum(nb.sc)
	repeat {
		if	(nrow(tbl) == mg) {break}
		tbl$f2.1 <- c(NA, sum.adjacent(x = tbl$iv.b, n = 2))
		tbl$f21 <- woe.adjacent(tbl = tbl, no.sc = no.sc, ng.sc = ng.sc, nb.sc = nb.sc)
		tbl$iv.loss <- tbl$f2.1 - tbl$f21
		mg.loc <- which.min(tbl$iv.loss)
		mg.idx <- c(mg.loc - 1, mg.loc)
		tbl.s$group[tbl.s$group%in%tbl$group[mg.idx]] <- tbl$group[mg.loc]
		tbl$group[mg.idx] <- tbl$group[mg.loc]
		tbl <- tbl %>% 
			 group_by(group) %>%
			 summarise(no = sum(no),
				     ng = sum(ng), 
				     nb = sum(nb))
		tbl$iv.b <- woe.calc(tbl = tbl, no.sc = no.sc, ng.sc = ng.sc, nb.sc = nb.sc)
		}
	tbl.s$label <- format.labels(g = tbl.s$group, b = tbl.s$bin)
return(tbl.s)
}


