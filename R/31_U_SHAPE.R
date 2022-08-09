#' Testing for U-shape relation
#'
#' \code{ush.test} performs U-shape testing between the target and analyzed risk factor.
#' Testing is based on B-splines basis functions and change of the sign of the estimated coefficients.
#'@param x Numeric vector to be tested for U-shape.
#'@param y Numeric target vector (binary).
#'@param p.value Threshold for p-value of statistical significance of the estimated coefficients
#'		     next to basis functions. Default is 0.05.
#'@param min.pct.obs Minimum percentage of observations per bin. Default is 0.05.
#'@param min.pct.def Minimum \code{y} average rate. Default is 0.01 or minimum 1 bad case for y 0/1.
#'@param g Number of knots used for testing the U-shape (integer). It should take values between 2 and 50 with 
#'	     default value of 20.
#'@param sc Numeric vector with special case elements. Default values are \code{c(NA, NaN, Inf, -Inf)}.
#' Recommendation is to keep the default values always and add new ones if needed. Otherwise, if these values exist
#' in \code{x} and are not defined in the \code{sc} vector, function can report the error.
#'@return The command \code{ush.test} returns list of three objects. The first object (\code{candidates}) 
#'	    is the data frame with summary of tested candidate knots. Using the reported results of this data frame
#'	    user can conclude if U-shape exists at all (column where \code{direction} is equal to \code{TRUE}) and 
#'	    check its statistical significance (column \code{significance} - \code{TRUE, FALSE}). 
#'	    The second object (\code{optimal}) reports optimal knot value (if exists). 
#'	    It is selected as the knot with minimum deviance among all candidates for
#'	    which \code{direction} and \code{significance} are equal to \code{TRUE}. 
#'	    The last, third, object (\code{basis.functions}) exports basis functions for optimal knot. Basis functions
#'	    will be exported only in case optimal knot is found. \cr
#'	    If optimal knot is not found, then users are encouraged to inspect closer the results of candidate testing.
#'@examples
#'data(gcd)
#'res <- ush.test(x = gcd$amount, y = gcd$qual)
#'res 
#'#optimal knot is not found so candidate can be defined as follows:
#'direction.t <- res$candidate[res$candidate$direction, ]
#'optimal.k <- direction.t$cp[direction.t$deviance%in%min(direction.t$deviance)]
#'optimal.k
#'@export
ush.test <- function(x, y, p.value = 0.05, min.pct.obs = 0.05, min.pct.def = 0.01, g = 20, 
			   sc = c(NA, Inf, -Inf, NaN)) {
	if	(length(y) != length(x)) {
		stop("y and x has to be of the same lenght.")
		}	
	if	(!sum(y%in%c(0, 1)) == length(y)) {
		stop("y is not 0/1 variable.")
		}	
	if	(p.value[1] <= 0 | p.value[1] >= 1) {
		stop("p.value has to be between 0 and 1")
		}
	if	(min.pct.obs < 0.05 | min.pct.obs > 0.5) {
		stop("min.pct.obs has to be between 0.05 and 0.5.")
		}
	if	(min.pct.def < 0.01 | min.pct.def > 0.99) {
		stop("min.pct.def has to be between 0.01 and 0.99.")
		}
	if	(g <= 2 | g > 50) {
		stop("g has to be between 2 and 50.")
		}
	sc.cond <- ifelse(length(sc) == 1, 
		     ifelse(is.numeric(sc) | is.na(sc), FALSE,  TRUE), 
		     ifelse(is.numeric(sc), FALSE, TRUE))
	if	(sc.cond) {
		stop("sc has to be numeric vectors")
		}
	cc <- !x%in%sc
	y <- y[cc]
	x <- x[cc]
	if	(length(x) < 0.1 * length(cc)) {
		stop("more than 90% of special cases.")
		}
	nd <- sum(y)
	min.rate <- ceiling(ifelse(nd * min.pct.def < 1, 1, nd * min.pct.def))
	bins <- unname(quantile(x, c(min.pct.obs, 1 - min.pct.obs)))
	if	(length(unique(bins)) == 1) {
		stop("Too few unique values, lower and upper bound of obs percentage the same.")
		}
	#check for min.avg.rate
	mr.lt <- sum(y[x <= bins[1]])
	mr.ut <- sum(y[x >= bins[2]])
	change.points <- quantile(x[x > bins[1] & x < bins[2]], 0:(g - 1) / (g - 1))
	change.points <- unique(unname(change.points))
	#possible correction for min.avg.rate 
	cpl <- length(change.points)
	if	(mr.lt < min.rate) {
		for	(i in 1:cpl) {
			cp.l <- change.points[i]
			lt.l <- sum(y[x <= cp.l])
			if	(lt.l >= min.rate) {
				lt.corr <- change.points[i - 1]
				break
				}
			}
		change.points <- change.points[!change.points <= lt.corr]
		}
	if	(length(change.points) == 0) {
		warning("candidate change points cannot be identified based on min.pct.obs and min.pct.def.")
		return(list())
		}
	if	(mr.ut < min.rate) {
		for	(i in 1:cpl) {
			cp.l <- change.points[length(change.points) - (i - 1)]
			ut.l <- sum(y[x >= cp.l])
			if	(ut.l >= min.rate) {
				ut.corr <- change.points[length(change.points) - (i - 1)]
				break
				}
			}
		change.points <- change.points[!change.points >= ut.corr]
		}
	if	(length(change.points) == 0) {
		warning("candidate change points cannot be identified based on min.pct.obs and min.pct.def.")
		return(candidates = NULL, optimal = NULL)
		}
	#u-shape testing
	cpl <- length(change.points)
	candidates <- vector("list", cpl)
	basis.functions <- vector("list", cpl)
	for	(i in 1:cpl) {
		cp.l <- change.points[i]
		bf <- bs.cp(x = x, knot = cp.l)
		lr.l <- lr.s(y = y, b1 = bf$b1, b2 = bf$b2, p.value = p.value[1])
		candidates[[i]] <- cbind.data.frame(cp = cp.l, lr.l)
		basis.functions[[i]] <- bf
		}
	candidates <- bind_rows(candidates)
	indx <- which(candidates$direction & candidates$significane)
	optimal <- candidates[indx, ]
	if	(nrow(optimal) == 0) {
		optimal <- NULL 
		basis.functions <- NULL
		} else {
		optimal <- optimal[which.min(optimal$deviance), ]
		basis.functions <- basis.functions[[indx]]	
		}
	res <- list(candidates = candidates, optimal = optimal, basis.functions = basis.functions)
return(res)
}

#' U-shape binning algorithm
#'
#' \code{ush.bin} performs U-shape binning. All algorithms from \href{https://CRAN.R-project.org/package=monobin}{monobin}
#' package are available. Due to specific nature of 
#' binning algorithms it is possible that for some selected knots algorithm will not be able to find U-shape. Therefore, 
#' users are encourage to inspect the results more into details and to try different binning algorithms. 
#'@param x Numeric vector to be binned.
#'@param y Numeric target vector (binary).
#'@param knot Numeric value of selected knot. Usually the results of \code{\link{ush.test}} function.
#'@param method Binning method. Available options are all from monobin package: 
#'		    \code{"cum.bin", "iso.bin", "ndr.bin", "pct.bin", "sts.bin", "woe.bin", "mdt.bin"}.
#'@param sc Numeric vector with special case elements. Default values are \code{c(NA, NaN, Inf, -Inf)}.
#' Recommendation is to keep the default values always and add new ones if needed. Otherwise, if these values exist
#' in \code{x} and are not defined in the \code{sc} vector, function can report the error.
#'@param sc.method Define how special cases will be treated, all together or separately.
#' Possible values are \code{"together", "separately"}.
#'@param g Number of starting groups. Only needed for \code{"cum.bin"}, \code{"pct.bin"} and \code{mdt.bin} methods. Default is 20.
#'@param min.pct.obs Minimum percentage of observations per bin. Default is 0.05 or 30 observations.
#'@param min.avg.rate Minimum \code{y} average rate. Default is 0.05 or 30 observations.
#'@param p.val Threshold for p-value. Only needed for \code{"sts.bin"} and \code{"ndr.bin"} methods.
#'		   Default is 0.05.
#'@param woe.trend Logical. Only needed for \code{"pct.bin"} method with default \code{TRUE}.
#'@param woe.gap Minimum WoE gap between bins. Only needed for \code{"woe.bin"} method with default of 0.1.
#'@return The command \code{ush.bin} generates a list of two objects. The first object, data frame \code{summary.tbl}
#' presents a summary table of final binning, while \code{x.trans} is a vector of discretized values. 
#'@examples
#'res <- ush.bin(x = gcd$amount, y = gcd$qual, knot = 2992.579, method = "ndr.bin")
#'res[[1]]
#'plot(res[[1]]$dr, type = "l")
#'@export
ush.bin <- function(x, y, knot, method, sc = c(NA, Inf, -Inf, NaN),
			  sc.method = "together", g = 20, min.pct.obs = 0.05, min.avg.rate = 0.01,
			  p.val = 0.05, woe.trend = TRUE, woe.gap = 0.1) {	
	if	(length(y) != length(x)) {
		stop("y and x has to be of the same lenght.")
		}
	if	(!sum(y%in%c(0, 1)) == length(y)) {
		stop("y is not 0/1 variable.")
		}
	if	(!is.numeric(knot[1])) {
		stop("knot has to be numberic vector of lenght 1.")
		}
	method.opts <- c("cum.bin", "iso.bin", "ndr.bin", "pct.bin", "sts.bin", "woe.bin", "mdt.bin")
	if	(!method%in%method.opts) {
		stop(paste0("method argument has to be one of: ", paste0(method.opts, collapse = ', '), "."))
		}
	cc <- !x%in%sc
	y0 <- y[cc]
	x0 <- x[cc]
	bf <- bs.cp(x = x0, knot = knot)
	lr <- lr.s(y = y0, b1 = bf$b1, b2 = bf$b2, p.value = p.val)
	cond <- !(lr$direction | lr$significance)
	if	(cond) {
		stop("change of relationship direction does not exist for selected knot.")
		}
	monobin.algo <- switch(method, "cum.bin" = "cum.bin(y = y1, x = x1, sc = sc, sc.method = sc.method, g = g, y.type = NA,
									    force.trend = NA)",
						 "iso.bin" = "iso.bin(y = y1, x = x1, sc = sc, sc.method = sc.method, y.type = NA, 
									    min.pct.obs = min.pct.obs, min.avg.rate = min.avg.rate, 
									    force.trend = NA)",
						 "ndr.bin" = "ndr.bin(y = y1, x = x1, sc = sc, sc.method = sc.method, y.type = NA, 
									    min.pct.obs = min.pct.obs, min.avg.rate = min.avg.rate, 
									    p.val = p.val, force.trend = NA)",
						 "pct.bin" = "pct.bin(y = y1, x = x1, sc = sc, sc.method = sc.method, g = g, y.type = NA,
									    woe.trend = woe.trend, force.trend = NA)",
						 "sts.bin" = "sts.bin(y = y1, x = x1, sc = sc, sc.method = sc.method, y.type = NA, 
									    min.pct.obs = min.pct.obs, min.avg.rate = min.avg.rate, 
									    p.val = p.val, force.trend = NA)",
						 "woe.bin" = "woe.bin(y = y1, x = x1, sc = sc, sc.method = sc.method, y.type = NA, 
									    min.pct.obs = min.pct.obs, min.avg.rate = min.avg.rate, 
									    woe.gap = woe.gap, force.trend = NA)",
						 "mdt.bin" = "mdt.bin(y = y1, x = x1, sc = sc, sc.method = sc.method, y.type = NA, 
									    min.pct.obs = min.pct.obs, min.avg.rate = min.avg.rate, 
									    g = g, force.trend = NA)")
	#correction of min obs and defs
	yx.l <- length(y)
	nd <- sum(y)
	min.obs <- ceiling(ifelse(yx.l * min.pct.obs < 30, 30, yx.l * min.pct.obs))
	min.def <- ceiling(ifelse(nd * min.avg.rate < 1, 1, nd * min.avg.rate))
	#direction 1
	y1 <- y0[x0 < knot]
	x1 <- x0[x0 < knot]
	min.pct.obs <- min.obs / length(y1)
	min.avg.rate <- min.def / sum(y1)	
	if	(method%in%"woe.bin") {
		y1 <- c(y1, rep(0, sum(y0[x0 >= knot]%in%0)), rep(1, sum(y0[x0 >= knot]%in%1)))
		x1 <- c(x1, rep(sc[1], sum(x0 >= knot)))
		}
	d1 <- eval(parse(text = monobin.algo))[[1]]
	#direction 2
	y1 <- y0[x0 >= knot]
	x1 <- x0[x0 >= knot]
	min.pct.obs <- min.obs / length(y1)
	min.avg.rate <- min.def / sum(y1)	
	if	(method%in%"woe.bin") {
		y1 <- c(y1, rep(0, sum(y0[x0 < knot]%in%0)), rep(1, sum(y0[x0 < knot]%in%1)))
		x1 <- c(x1, rep(sc[1], sum(x0 < knot)))
		}
	d2 <- eval(parse(text = monobin.algo))[[1]]
	#d1 & d2 correction for woe.bin method
	d1 <- d1[!d1$bin%in%sc & !d1$bin%in%"SC", ]
	d2 <- d2[!d2$bin%in%sc & !d2$bin%in%"SC", ]
	#check 
	dir.c <- sign(diff(c(d1$y.avg, d2$y.avg)))
	if	(length(unique(dir.c)) == 1) {
		mapping <- data.frame(x.min = min(x0), x.max = max(x0))
		} else {
		mapping <- data.frame(x.min = c(d1$x.min, d2$x.min), x.max = c(d1$x.max, d2$x.max))
		}
	#slice x
	if	(sc.method%in%"together") {
		sc.r <- "SC"
		} else {
		sc.r <- sc
		}
	x.trans <- num.slice(x = x, 
		     mapping = mapping, 
		     sc = sc, 
		     sc.r = sc.r)
	#summary table
	tbl <- data.frame(y = y, x.trans  = x.trans)
	tbl.s <- woe.tbl(tbl = tbl, x = "x.trans", y = "y", y.check = TRUE)
	res <- list(summary.tbl = tbl.s, x.trans = x.trans)
return(res) 
}

#b-spline basis functions (one knot)
bs.cp <- function(x, knot) {
	k <- sort(c(min(x), knot, max(x)))
	b1 <- (x - k[1]) / (k[2] - k[1]) * (k[1] <= x & x < k[2]) + (k[3] - x) / (k[3] - k[2]) * (k[2] <= x & x < k[3])
	b2 <- (x - k[2]) / (k[3] - k[2]) * (k[2] <= x & x <= k[3])
	bf <- data.frame(b1, b2)
return(bf)
}
#lr model summary
lr.s <- function(y, b1, b2, p.value) {
	lr.m <- glm(y ~ b1 + b2, family = "binomial")
	lr.c <- summary(lr.m)$coefficients
	est.b1 <- lr.c[2, "Estimate"]
	est.b2 <- lr.c[3, "Estimate"]
	dir <- sign(est.b1) != sign(est.b2) 
	sig <- all(lr.c[2:3, "Pr(>|z|)"] < p.value)
	res <- data.frame(estimate.b1 = est.b1,
				estimate.b2 = est.b2,
				direction = dir, 
				p.value = p.value, 
				p.value.b1 = lr.c[2, "Pr(>|z|)"],
				p.value.b2 = lr.c[3, "Pr(>|z|)"],
				significance = sig, 
				deviance = lr.m$deviance)
return(res)
}	







