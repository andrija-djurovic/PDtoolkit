#' Slice numeric variable
#'
#' \code{num.slice} implements manual discretization of numeric vector for a given boundaries. This procedure is one
#' of the helper functions which are handy for the model monitoring phase (i.e. after model implementation).
#'@param x Numeric vector to be discretized.
#'@param mapping Data frame with compulsory columns: \code{x.min} and \code{x.max} which represent the discretized
#'		     boundaries.
#'@param sc Numeric vector with special case elements. Default values are \code{c(NA, NaN, Inf, -Inf)}.
#'@param sc.r Character vector used for replacement of special cases. If supplied as one 
#'		  element vector, it will be recycled to the length of \code{sc}. Default value is \code{"SC"}.
#'@return The command \code{num.slice} returns vector of discretized values and coded special cases. 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'x <- gcd$maturity
#'#artificially add some special values
#'x[1:5] <- Inf
#'x[6:7] <- NA
#'#perform monotonic grouping in order to get bins' boundaries
#'mbin <- sts.bin(x = x, y = gcd$qual, sc.method = "separately")
#'mbin[[1]]
#'#slice numeric variable
#'sn <- num.slice(x = x, 
#'		      mapping = data.frame(x.min = mbin[[1]]$x.min[-c(1, 2)], 
#'						 x.max = mbin[[1]]$x.max[-c(1, 2)]), 
#'		      sc = c(NA, NaN, Inf, -Inf), 
#'		      sc.r = "SC")
#'#compare automatic and manual binning
#'table(mbin[[2]], useNA = "always")
#'table(sn, useNA = "always")
#'@export
num.slice <- function(x, mapping, sc = c(NA, NaN, Inf, -Inf), sc.r = "SC") {
	if	(!is.numeric(x)) {
		stop("x has to be numeric vector.")
		}
	if	(!is.data.frame(mapping)) {
		stop("mapping is not a data frame.")
		}
	map.cols <- c("x.min", "x.max")
	if	(any(!map.cols%in%names(mapping))) {
		stop(paste0("mapping data frame has to contain columns: ", 
		     paste(map.cols, collapse = ", "), "."))
		}	
	if	(any(is.na(mapping) | sapply(mapping, function(x) x%in%c(Inf, -Inf)))) {
		stop("mapping data frame contains missing or infinite values.")
		}
	scl <- length(sc)
	sc.rl <- length(sc.r)
	if	(sc.rl == 1) {
		sc.r <- rep(sc.r, scl)
		} else {
		if	(sc.rl != scl) {
			stop("sc and sc.r has to be of the same length unless sc.r is of length one and recycled.")	
			}
		}
	x.trans <- x
	for	(i in 1:scl) {
		x.trans[x.trans%in%sc[i]] <- sc.r[i]
		}
	x.lb <- mapping$x.min
	x.ub <- mapping$x.max
	x.lb[1] <- -Inf
	x.lb.lag <- c(x.lb[-1], Inf)
	lg <- nrow(mapping)
	for	(i in 1:lg) {
		x.lb.l <- x.lb[i]
		x.lb.lag.l <- x.lb.lag[i]
		x.ub.l <- x.ub[i]
		bin.n <- sprintf("%02d", i)
		bin.f <- ifelse(abs(x.lb.l - x.ub.l) < 1e-8, 
			    paste0(bin.n, " [", round(x.lb.l, 4), "]"), 
			    ifelse(x.lb.l == -Inf,
				     paste0(bin.n, " (", round(x.lb.l, 4), ",", round(x.lb.lag.l, 4), ")"),
				     paste0(bin.n, " [", round(x.lb.l, 4), ",", round(x.lb.lag.l, 4), ")")))
		rep.indx <- which(!x%in%sc & x >= x.lb.l & x <= x.lb.lag.l)
		x.trans[rep.indx] <- bin.f
		}	
return(x.trans)
}

#' Slice categorical variable
#'
#' \code{cat.slice} implements manual re-coding of character vector values for a given mapping scheme. 
#' This procedure is one of the helper functions which are handy for the model monitoring phase 
#' (i.e. after model implementation).
#'@param x Character vector to be re-coded.
#'@param mapping Data frame with compulsory columns: \code{x.orig} and \code{x.mapp} which represent the mapping
#'		     scheme. Column \code{x.orig} should contain unique values of original vector \code{x}, while \code{x.mapp}
#'		     should contain corresponding mapping values.
#'@param sc Character vector with special case elements. Default value is \code{NA}.
#'@param sc.r Character vector used for replacement of special cases. If supplied as one 
#'		  element vector, it will be recycled to the length of \code{sc}. Default value is \code{"SC"}.
#'@return The command \code{cat.slice} returns vector of re-coded values and special cases. 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'x <- gcd$maturity
#'#artificially add some special values
#'x[1:5] <- Inf
#'x[6:7] <- NA
#'mbin <- cum.bin(x = x, y = gcd$qual, sc.method = "together")
#'mbin[[1]]
#'gcd$x <- mbin[[2]]
#'cb <- cat.bin(x = gcd$x, 
#'		  y = gcd$qual, 
#'		  sc = "SC",
#'		  sc.merge = "none",
#'		  min.pct.obs = 0.05, 
#'		  min.avg.rate = 0.05)
#'x <- gcd$x
#'mapping <- data.frame(x.orig = x, x.mapp = cb[[2]])%>%
#'	       group_by(x.orig, x.mapp) %>%
#'	       summarise(n = n(), .groups = "drop")
#'mapping <- data.frame(mapping[, -3])
#'sc <- cat.slice(x = x, 
#'		     mapping = mapping, 
#'		     sc = NA, 
#'		     sc.r = "SC")
#'#compare automatic and manual re-coding
#'table(cb[[2]], useNA = "always")
#'table(sc, useNA = "always")
#'@export
cat.slice <- function(x, mapping, sc = NA, sc.r = "SC") {
	if	(!(is.character(x) | is.factor(x) | is.logical(x))) {
		stop("x is of inappropriate class. It has to be one of: character, factor or logical.")
		}
	if	(!is.data.frame(mapping)) {
		stop("mapping is not a data frame.")
		}
	map.cols <- c("x.orig", "x.mapp")
	if	(any(!map.cols%in%names(mapping))) {
		stop(paste0("mapping data frame has to contain columns: ", 
		     paste(map.cols, collapse = ", "), "."))
		}	
	ux <- unique(x)	
	if	(!any(ux%in%c(mapping$x.orig, sc))) {
		ux <- ux[!ux%in%c(mapping$x.orig, sc)]
		stop(paste0("x contains that are not reported in mapping (x.orig) data frame: " ,
		     paste(ux, collapse = ", "), "."))
		}
	scl <- length(sc)
	sc.rl <- length(sc.r)
	if	(sc.rl == 1) {
		sc.r <- rep(sc.r, scl)
		} else {
		if	(sc.rl != scl) {
			stop("sc and sc.r has to be of the same length unless sc.r is of length one and recycled.")	
			}
		}
	nv <- mapping$x.mapp
	names(nv) <- mapping$x.orig
	x.trans <- unname(nv[x])
	if	(sum(x.trans%in%sc) > 0) {
		for	(i in 1:scl) {
			x.trans[x%in%sc[i]] <- sc.r[i]
			}
		}
return(x.trans)
}

#' Encode WoE
#'
#' \code{encode.woe} implements replacement of character vector values with WoE values for a given mapping scheme. 
#' This procedure is one of the helper functions which are handy for the model monitoring phase 
#' (i.e. after model implementation).
#'@param x Character vector to be re-coded.
#'@param mapping Data frame with compulsory columns: \code{x.mod} and \code{x.woe} which represents the mapping
#'		     scheme. Column \code{x.mod} should contain unique values of original vector \code{x}, while \code{x.woe}
#'		     should contain corresponding mapping values.
#'@return The command \code{encode.woe} returns vector of re-coded WoE values. 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'mbin <- cum.bin(x = gcd$maturity, y = gcd$qual, sc.method = "together")
#'mbin[[1]]
#'table(mbin[[2]], useNA = "always")
#'gcd$x.mod <- mbin[[2]]
#'woe.rep <- replace.woe(db = gcd[, c("qual", "x.mod")], target = "qual")
#'gcd$x.woe <- woe.rep[[1]]$x
#'mapping <- data.frame(x.mod = gcd$x.mod, x.woe = gcd$x.woe)%>%
#'	     group_by(x.mod, x.woe) %>%
#'	     summarise(n = n(), .groups = "drop")
#'mapping <- data.frame(mapping[, -3])
#'ewoe <- encode.woe(x = gcd$x.mod, mapping = mapping)
#'identical(ewoe, woe.rep[[1]]$x)
#'@export
encode.woe <- function(x, mapping) {
	if	(!(is.character(x) | is.factor(x) | is.logical(x))) {
		stop("x is of inappropriate class. It has to be one of: character, factor or logical.")
		}
	if	(!is.data.frame(mapping)) {
		stop("mapping is not a data frame.")
		}
	map.cols <- c("x.mod", "x.woe")
	if	(any(!map.cols%in%names(mapping))) {
		stop(paste0("mapping data frame has to contain columns: ", 
		     paste(map.cols, collapse = ", "), "."))
		}	
	ux <- unique(x)	
	if	(!any(ux%in%c(mapping$x.mod, NA))) {
		ux <- ux[!ux%in%c(mapping$x.mod, NA)]
		stop(paste0("x contains value(s) not reported in mapping (x.mod) data frame: " ,
		     paste(ux, collapse = ", "), "."))
		}
	nv <- mapping$x.woe
	x.trans <- unname(nv[as.character(x)])
	na.idx <- is.na(x.trans)
	if	(sum(na.idx) > 0) {
		x.trans[na.idx] <- mapping$x.woe[is.na(mapping$x.mod)]
		}
return(x.trans)
}
