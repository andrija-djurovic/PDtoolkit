#' Calibration of the rating scale
#'
#' \code{rs.calibration} performs calibration of the observed default rates for a given rating scale. 
#'@param rs Rating scale that contain observed default rate and weights used for optimization.
#'@param dr Observed default rate per rating.
#'@param w Weights, usually number of observations (clients/accounts) per rating.
#'@param ct Value of central tendency to which calibration is performed.
#'@param min.pd Minimum probability of default (PD) per rating, as constrain for calibration process.
#'@param method Calibration method. Available options are \code{"scaling", "log.odds.a", "log.odds.ab"}.
#'@details
#'Method \code{"scaling"} relies on linear rescaling of observed default rate. Rescaling factor is
#'calculated as a ratio between \code{ct} and observed portfolio default rate.
#'Method \code{"log.odds.a"} optimize intercept of logit transformation in a way that makes portfolio default
#'rate equal to selected central tendency (\code{ct}). 
#'Method \code{"log.odds.ab"} optimize intercept and slope of logit transformation in a way that makes
#'portfolio default rate equal to selected central tendency (\code{ct}). 
#'For respecting selected constrain of minimum PD (\code{min.pd}), two-stage iterative procedure is implemented.
#'Additional constrain of maximum PD (100%) is also implemented.
#'@return The command \code{rs.calibration} returns a list of two elements. The first element is 
#'vector of calibrated PDs and the second one is dataframe of optimization parameters.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#estimate some dummy model
#'mod.frm <- `Creditability` ~ `Account Balance` + `Duration of Credit (month)` +
#'				`Age (years)`
#'lr.mod <- glm(mod.frm, family = "binomial", data = loans)
#'summary(lr.mod)$coefficients
#'#model predictions
#'loans$pred <-  unname(predict(lr.mod, type = "response", newdata = loans))
#'#scale probabilities
#'loans$score <- scaled.score(probs = loans$pred, score = 600, odd = 50/1, pdo = 20)
#'#group scores into rating
#'loans$rating <- sts.bin(x = round(loans$score), y = loans$Creditability, y.type = "bina")[[2]]
#'#create rating scale
#'rs <- loans %>%
#'group_by(rating) %>%
#'summarise(no = n(),
#'	    nb = sum(Creditability),
#'	    ng = sum(1 - Creditability)) %>%
#'mutate(dr = nb / no)
#'rs
#'#calcualte portfolio default rate
#'sum(rs$dr * rs$no / sum(rs$no))
#'#calibrate rating scale to central tendency of 27% with minimum PD of 5%
#'ct <- 0.33
#'min.pd <- 0.05
#'#scaling
#'pd.calib.s <- rs.calibration(rs = rs, 
#'				     dr = "dr", 
#'				     w = "no", 
#'				     ct = ct, 
#'				     min.pd = min.pd,
#'				     method = "scaling")
#'rs$pd.scaling <- pd.calib.s[[1]]
#'#log-odds a
#'pd.calib.a <- rs.calibration(rs = rs, 
#'				     dr = "dr", 
#'			 	     w = "no", 
#'				     ct = ct, 
#'				     min.pd = min.pd,
#'				     method = "log.odds.a")
#'rs$pd.log.a <- pd.calib.a[[1]]
#'#log-odds ab
#'pd.calib.ab <- rs.calibration(rs = rs, 
#'				      dr = "dr", 
#'			 	      w = "no", 
#'				      ct = ct, 
#'				      min.pd = min.pd,
#'				      method = "log.odds.ab")
#'rs$pd.log.ab <- pd.calib.ab[[1]]
#'#checks
#'rs
#'sum(rs$pd.scaling * rs$no / sum(rs$no))
#'sum(rs$pd.log.a * rs$no / sum(rs$no))
#'sum(rs$pd.log.ab * rs$no / sum(rs$no))
#'@importFrom stats uniroot optim
#'@export
rs.calibration <- function(rs, dr, w, ct, min.pd, method) {
	if	(!is.data.frame(rs)) {
		stop("rs (rating scale) is not a data frame.")
		}
	if	(any(!(c(dr, w)%in%names(rs)))) {
		stop("arguments dr (default rate) and w (weights) cannot be found in rs (rating scale).")
		}
	method.opt <- c("scaling", "log.odds.a", "log.odds.ab")
	if	(!method[1]%in%method.opt) {
		msg <- paste0("method argument has to be one of: ", paste(method.opt, collapse = ", "), ".")
		stop(msg)
		}
	dr <- unname(c(rs[, dr], recursive = TRUE)) 
	w <- unname(c(rs[, w], recursive = TRUE))
	if	(any(dr > 1 | dr < 0)) {
		stop("Default rate has to be within 0-1 range.")
		}
	pd.calib <- switch(method, "scaling" = calib.scaling(dr = dr, w = w, ct = ct, min.pd = min.pd),
					   "log.odds.a" = calib.log.odds.a(dr = dr, w = w, ct = ct, min.pd = min.pd),
					   "log.odds.ab" = calib.log.odds.ab(dr = dr, w = w, ct = ct, min.pd = min.pd))
return(pd.calib)
}

calib.scaling <- function(dr, w, port.dr, ct, min.pd) {
	params <- data.frame(iter = 1:2, scaling.factor = c(NA, NA))
	port.dr <- sum(dr * w / sum(w))
	sf <- ct / port.dr
	params[1, 2] <- sf
	pd.calib <- dr * sf
	check.min <- pd.calib < min.pd 
	check.max <- pd.calib > 1
	if	(any(check.min | check.max)) {
		min.indx <- which(check.min)
		max.indx <- which(check.max)
		rc.indx <- c(min.indx, max.indx)
		dr.n <- pd.calib
		dr.n[min.indx] <- NA
		dr.n[max.indx] <- NA
		pd.calib[min.indx] <- min.pd
		pd.calib[max.indx] <- 1
		ct.n <- ct - sum((pd.calib[rc.indx ] * w[rc.indx] / sum(w)))
		sf.n <- ct.n / sum((dr.n * w / sum(w)), na.rm = TRUE)
		params[2, 2] <- sf.n
		pd.calib.r <- dr.n * sf.n	
		pd.calib.f <- ifelse(is.na(pd.calib.r), pd.calib, pd.calib.r)
		} else {
		pd.calib.f <- pd.calib
		}
return(list(pd.calib = pd.calib.f, params = params))
}

calib.log.odds.a <- function(dr, w, ct, min.pd) {	
	params <- data.frame(iter = 1:2, a = c(NA, NA))
	dr <- ifelse(dr == 1, 1 - 1 / 1e6, dr)
	log.odds <- log(dr / (1 - dr))	
	opt.f <- function(a, lo, w, ct, corr = 0) {
				cc <- complete.cases(lo, w)
				lo <- lo[cc]
				w <- w[cc]
				pd.inverse <- exp(a + lo) / ( 1 + exp(a + lo))
				opt <- sum(w * pd.inverse /(sum(w) + corr)) - ct
				return(opt)
				}
	lo.a <- uniroot(f = opt.f, lo = log.odds, w = w, ct = ct, interval = c(-10, 10))
	a <- lo.a$root
	params[1, 2] <- a
	pd.calib <- exp(a + log.odds) / (1 + exp(a + log.odds))
	check <- pd.calib < min.pd
	if	(any(check)) {
		rc.indx <- which(check)
		log.odds.n <- log.odds
		log.odds.n[rc.indx] <- NA
		pd.calib[rc.indx] <- min.pd
		ct.n <- ct - sum((pd.calib[rc.indx] * w[rc.indx] / sum(w)))
		lo.a <- uniroot(f = opt.f, lo = log.odds.n, w = w, ct = ct.n, 
				    corr = sum(w[is.na(log.odds.n)]), interval = c(-5, 5))
		a <- lo.a$root	
		params[2, 2] <- a
		pd.calib.r <- exp(a + log.odds.n) / (1 + exp(a + log.odds.n))			
		pd.calib.f <- ifelse(is.na(pd.calib.r), pd.calib, pd.calib.r)
		} else {
		pd.calib.f <- pd.calib
		}
return(list(pd.calib = pd.calib.f, params = params))
}

calib.log.odds.ab <- function(dr, w, ct, min.pd) {
	params <- data.frame(iter = 1:2, a = c(NA, NA), b = c(NA, NA))
	dr <- ifelse(dr == 1, 1 - 1 / 1e6, dr)
	log.odds <- log(dr / (1 - dr))
	opt.f <- function(x, lo, w, ct, corr = 0) {
				a <- x[1]
				b <- x[2]
				cc <- complete.cases(lo, w)
				lo <- lo[cc]
				w <- w[cc]
				pd.inverse <- exp(a + b * lo) / ( 1 + exp(a + b * lo))
				opt <- (sum(w * pd.inverse / (sum(w) + corr)) - ct)^2
				return(opt)
				}
	lo.ab <- optim(par = c(0, 1), 
			   fn = opt.f, 
			   lo = log.odds, 
			   w = w,
			   ct = ct,
			   method = "BFGS")
	ab <- lo.ab$par
	a <- ab[1]
	b <- ab[2]
	params[1, 2:3] <- c(a, b)
	pd.calib <- exp(a + b * log.odds) / (1 + exp(a + b * log.odds))
	check <- pd.calib < min.pd
	if	(any(check)) {
		rc.indx <- which(check)
		log.odds.n <- log.odds
		log.odds.n[rc.indx] <- NA
		pd.calib[rc.indx] <- min.pd
		ct.n <- ct - sum((pd.calib[rc.indx] * w[rc.indx] / sum(w)))
		lo.ab <- optim(par = c(0, 1), 
			   fn = opt.f, 
			   lo = log.odds.n, 
			   w = w,
			   corr = sum(w[is.na(log.odds.n)]),
			   ct = ct.n,
			   method = "BFGS")
		ab <- lo.ab$par
		a <- ab[1]
		b <- ab[2]
		params[2, 2:3] <- c(a, b)
		pd.calib.r <- exp(a + b * log.odds.n) / (1 + exp(a + b * log.odds.n))			
		pd.calib.f <- ifelse(is.na(pd.calib.r), pd.calib, pd.calib.r)
		} else {
		pd.calib.f <- pd.calib
		}	
return(list(pd.calib = pd.calib.f, params = params))
}


