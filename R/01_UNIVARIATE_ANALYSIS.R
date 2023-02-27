#' Univariate analysis
#'
#' \code{univariate} returns the univariate statistics for risk factors supplied in data frame \code{db}. \cr
#' For numeric risk factors univariate report includes:
#' \itemize{
#'   \item rf: Risk factor name.
#'   \item rf.type: Risk factor class. This metric is always equal to \code{numeric}.
#'   \item bin.type: Bin type - special or complete cases.
#'   \item bin: Bin type. If a \code{sc.method} argument is equal to \code{"together"}, then 
#'		    \code{bin} and \code{bin.type} have the same value. If the \code{sc.method} argument
#'		    is equal to \code{"separately"}, then the \code{bin} will contain all special cases that
#'		    exist for analyzed risk factor (e.g. \code{NA}, \code{NaN}, \code{Inf}).
#'   \item pct: Percentage of observations in each \code{bin}.
#'   \item cnt.unique: Number of unique values per \code{bin}.
#'   \item min: Minimum value. 
#'   \item p1, p5, p25, p50, p75, p95, p99: Percentile values.
#'   \item avg: Mean value.  
#'   \item avg.se: Standard error of the mean.
#'   \item max: Maximum value.  
#'   \item neg: Number of negative values.    
#'   \item pos: Number of positive values.   
#'   \item cnt.outliers: Number of outliers. Records above or below 
#'				  \code{Q75}\eqn{\pm}\code{1.5 * IQR}, where \code{IQR = Q75 - Q25}.
#'   \item sc.ind: Special case indicator. It takes value 1 if share of special cases exceeds
#'			 \code{sc.threshold} otherwise 0.	
#'}
#' For categorical risk factors univariate report includes:
#' \itemize{
#'   \item rf: Risk factor name.
#'   \item rf.type: Risk factor class. This metric is equal to one of: \code{character}, 
#'			  \code{factor} or \code{logical}.
#'   \item bin.type: Bin type - special or complete cases.
#'   \item bin: Bin type. If a \code{sc.method} argument is equal to \code{"together"}, then 
#'		    \code{bin} and \code{bin.type} have the same value. If the \code{sc.method} argument
#'		    is equal to \code{"separately"}, then the \code{bin} will contain all special cases that
#'		    exist for analyzed risk factor (e.g. \code{NA}, \code{NaN}, \code{Inf}).
#'   \item pct: Percentage of observations in each \code{bin}.
#'   \item cnt.unique: Number of unique values per \code{bin}.
#'   \item sc.ind: Special case indicator. It takes value 1 if share of special cases exceeds
#'			 \code{sc.threshold} otherwise 0.		
#'}  
#'@param db Data frame of risk factors supplied for univariate analysis.
#'@param sc Vector of special case elements. Default values are \code{c(NA, NaN, Inf)}.			    
#'@param sc.method Define how special cases will be treated, all together or in separate bins. 
#'                 Possible values are \code{"together"}, \code{"separately"}.
#'@param sc.threshold Threshold for special cases expressed as percentage of total number of observations.
#'			    If \code{sc.method} is set to \code{"separately"}, then percentage for each special case
#'			    will be summed up.
#' 
#'@return The command \code{univariate} returns the data frame with explained univariate metrics for numeric,
#'	    character, factor and logical class of risk factors.
#'@examples
#' suppressMessages(library(PDtoolkit))
#' data(gcd)
#' gcd$age[100:120] <- NA
#' gcd$age.bin <- ndr.bin(x = gcd$age, y = gcd$qual, y.type = "bina")[[2]]
#' gcd$age.bin <- as.factor(gcd$age.bin)
#' gcd$maturity.bin <- ndr.bin(x = gcd$maturity, y = gcd$qual, y.type = "bina")[[2]]
#' gcd$amount.bin <- ndr.bin(x = gcd$amount, y = gcd$qual, y.type = "bina")[[2]]
#' gcd$all.miss1 <- NaN
#' gcd$all.miss2 <- NA
#' gcd$tf <- sample(c(TRUE, FALSE), nrow(gcd), rep = TRUE)
#' #create date variable to confirm that it will not be processed by the function
#' gcd$dates <- Sys.Date()
#' str(gcd)
#' univariate(db = gcd)
#'@import monobin
#'@import dplyr
#'@importFrom graphics boxplot
#'@importFrom stats quantile sd
#'@export
univariate <- function(db, sc = c(NA, NaN, Inf, -Inf), sc.method = "together", sc.threshold = 0.2) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	scm.opts <- c("together", "separately")
	cond.01 <- !sc.method[1]%in%scm.opts
	if	(cond.01) {
		stop(paste0("sc.method  argument has to be one of: ", 
				paste0(scm.opts, collapse = ", ")))
		}
	varl <- ncol(db)
	var.n <- names(db)
	res <- vector("list", varl)
	for	(i in 1:varl) {
		var.l <- var.n[i]
		if	(sc.method[1] == "together") {
			db$bin <- ifelse(db[, var.l]%in%sc, "special cases", "complete cases")
			} else {
			db$bin <- ifelse(db[, var.l]%in%sc, db[, var.l], "complete cases")
			}
		db$type <- factor(ifelse(db$bin%in%"complete cases", "complete cases", "special cases"),
					levels = c("special cases", "complete cases"), 
					ordered = TRUE)
		if	(is.numeric(db[, var.l])) {
			res[[i]] <- univariate.num(db = db, x = var.l, sc.threshold = sc.threshold) 
			res[[i]]$bin <- as.character(res[[i]]$bin)
			} 
		if	(is.character(db[, var.l]) | is.factor(db[, var.l]) | is.logical(db[, var.l])) {
			res[[i]] <- univariate.cat(db = db, x = var.l, sc.threshold = sc.threshold) 
			res[[i]]$bin <- as.character(res[[i]]$bin)
			}
		}
	res <- data.frame(bind_rows(res))
return(res)
}

univariate.num <- function(db, x, sc.threshold = sc.threshold) {
	res <- suppressWarnings(
		 db %>% 
		 group_by_at(c("bin.type" = "type", "bin" = as.character("bin"))) %>%
		 summarise_at(all_of(x), 
				  funs(
				  cnt = n(),
				  pct = n() / nrow(db),
				  cnt.unique = length(unique(.)), 
				  min = min(., na.rm = TRUE),
				  p1 = quantile(., prob = 0.01, na.rm = TRUE),
				  p5 = quantile(., prob = 0.05, na.rm = TRUE),				
				  p25 = quantile(., prob = 0.25, na.rm = TRUE),
				  p50 = quantile(., prob = 0.50, na.rm = TRUE),
				  avg = mean(., na.rm = TRUE), 
				  avg.se = sd(., na.rm = TRUE) / sqrt(n()), 
				  p75 = quantile(., prob = 0.75, na.rm = TRUE),				
				  p95 = quantile(., prob = 0.95, na.rm = TRUE),	
				  p99 = quantile(., prob = 0.99, na.rm = TRUE),	
				  max = max(., na.rm = TRUE),
				  neg = sum(. < 0),
				  pos = sum(. > 0),
				  cnt.outliers = length(boxplot(.[!is.na(.)], plot = FALSE)$out)
				  )) %>%
		 ungroup() %>%
		 mutate(sc.ind = ifelse(sum(pct[bin.type%in%"special cases"]) > sc.threshold, 
						1, 0))
		 )
	res <- data.frame(res)
	res <- cbind.data.frame(rf = x, rf.type = "numeric", res)
return(res)
}

univariate.cat <- function(db, x, sc.threshold = sc.threshold) {
	res <- suppressWarnings(
		 db %>% 
		 group_by_at(c("bin.type" = "type", "bin" = as.character("bin"))) %>%
		 summarise_at(all_of(x), 
				  funs(
				  cnt = n(),
				  pct = n() / nrow(db),
				  cnt.unique = length(unique(.))
				  )) %>%
		 ungroup() %>%
		 mutate(sc.ind = ifelse(sum(pct[bin.type%in%"special cases"]) > sc.threshold, 
						1, 0))
		 )
	res <- data.frame(res)
	res <- cbind.data.frame(rf = x, rf.type = class(db[, x]), res)
return(res)
}

#' Imputation methods for special cases
#'
#' \code{imp.sc} imputes value for special cases.
#'@param db Data frame of risk factors supplied for imputation.
#'@param sc.all Vector of all special case elements. Default values are \code{c(NA, NaN, Inf)}.
#'@param sc.replace Vector of special case element to be replaced. Default values are \code{c(NA, NaN, Inf)}.	
#'@param method.num Imputation method for numeric risk factors. Available options are: \cr
#'		    \code{"automatic", "mean", "median", "zero"}.
#'@param p.val Significance level of p-value for Pearson normality test. Applicable only if \code{method.num} is 
#' \code{automatic}.
#'@return This function returns list of two data frames. The first data frame contains analyzed risk factors with
#' imputed values for special cases, while the second data frame presents the imputation report. 
#' Using the imputation report, for each risk factor, user can inspect imputed info (\code{info}), 
#' imputation method (\code{imputation.method}), imputed value (\code{imputed.value}), 
#' number of imputed observations (\code{imputation.num}) and imputed mode
#' (\code{imputed.mode} - applicable only for categorical risk factors) for each risk factor.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'gcd$age[1:20] <- NA
#'gcd$age.bin <- ndr.bin(x = gcd$age, y = gcd$qual, sc.method = "separately", y.type = "bina")[[2]]
#'gcd$dummy1 <- NA
#'#select risk factors for which we want to impute missing values (NA)
#'db.imp <- gcd[, c("age", "age.bin", "dummy1")]
#'colSums(is.na(db.imp))
#'imput.res <- imp.sc(db = db.imp, 
#'		      method.num = "automatic",
#'		      p.val = 0.05)
#'#analyzed risk factors with imputed values
#'head(imput.res[[1]])
#'#imputation report
#'imput.res[[2]]
#'@import monobin
#'@export
imp.sc <- function(db, sc.all = c(NA, NaN, Inf, -Inf), sc.replace = c(NA, NaN, Inf, -Inf), 
			    method.num = "automatic", p.val = 0.05) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	method.opts <- c("automatic", "mean", "median", "zero")
	if	(length(method.num) != 1 | !method.num%in%method.opts) {
		stop(paste0("method.num argument has to be one of: ", 
				paste0(method.opts, collapse = ", ")))
		}
	rf <- names(db)
	rfl <- length(rf)
	report <- vector("list", rfl)
	if	(method.num%in%"automatic") {
		if	(p.val > 1 | p.val < 0) {
			stop("p.val has to be between 0 and 1.")
			}
		eval.exp <- paste0("ifelse(", "eval(parse(text = dist.check))", 
					 " < ", p.val, ", ",
					 "median(rf.imp[!rf.imp%in%sc.all]), ",
					 "mean(rf.imp[!rf.imp%in%sc.all])", ")")
		}
	if	(method.num%in%c("mean", "median")) {
		eval.exp <- paste0(method.num, "(rf.imp[!rf.imp%in%sc.all])")
		}
	if	(method.num%in%"zero") {
		eval.exp <- "0"
		}
	for	(i in 1:rfl) {
		rf.l <- rf[i]
		rf.imp <- db[, rf.l]
		cc <- length(rf.imp[!rf.imp%in%sc.all])
		if	(cc < 0.1 * length(rf.imp)) {
			report[[i]] <- data.frame(rf = rf.l, 
							  info = "Less than 10% of complete cases.", 
							  imputation.method = NA)
			next
			}
		if	(is.numeric(rf.imp)) {
			auto.algo <- NULL
			if	(method.num%in%"automatic") {
				dist.check <- pearson.norm.test(x = rf.imp[!rf.imp%in%sc.all])$p.val
				auto.algo <- ifelse(dist.check < p.val, "median", "mean")
				}
			rf.imp.val <- eval(parse(text = eval.exp))
			if	(rf.imp.val%in%c(NA, Inf, NaN, -Inf)) {
				report[[i]] <- data.frame(rf = rf.l, 
							  	  info = "Imputed value cannot be calculated.", 
							  	  imputation.method = method.num)
				next
				} else {
				imputation.num <- sum(rf.imp%in%sc.replace)
				rf.imp[rf.imp%in%sc.replace] <- ifelse(is.integer(rf.imp[!rf.imp%in%sc.all]), 
										   round(rf.imp.val),
										   rf.imp.val)
				db[, rf.l] <- rf.imp
				report[[i]] <- data.frame(rf = rf.l, 
							  	  info = "Imputation completed.", 
							  	  imputation.method = ifelse(method.num%in%"automatic",
												     paste0("automatic - ", auto.algo),
												     rf.imp.val), 
								  imputed.value = rf.imp.val,
								  imputation.num = imputation.num)

				}
			} else {
			eval.exp <- "names(which.max(table(rf.imp[!rf.imp%in%sc.all])))"
			rf.mode <- eval(parse(text = eval.exp))
			if	(rf.mode%in%c(NA, Inf, NaN, -Inf)) {
				report[[i]] <- data.frame(rf = rf.l, 
							  	  info = "Imputed value (mode) cannot be calculated.", 
							  	  imputation.method = "mode")
				next
				} else {
				imputation.num <- sum(rf.imp%in%sc.replace)
				rf.imp[rf.imp%in%sc.replace] <- rf.mode
				db[, rf.l] <- rf.imp
				report[[i]] <- data.frame(rf = rf.l, 
							  	  info = "Imputation completed.", 
							  	  imputation.method = "mode",
								  imputed.mode = rf.mode,
								  imputation.num = imputation.num)

				}
			}
		}
	report <- data.frame(bind_rows(report))	
return(list(db = db, report = report))			
}

pearson.norm.test <- function(x) {
	n <- length(x)
	n.classes <- ceiling(2 * (n^(2 / 5)))
	num <- floor(1 + n.classes * pnorm(x, mean(x), sd(x)))
	count <- tabulate(num, n.classes)
	prob <- rep(1 / n.classes, n.classes)
	xpec <- n * prob
	test.stat <- sum(((count - xpec)^2) / xpec)
	p.val <- pchisq(test.stat, n.classes - 2 - 1, lower.tail = FALSE)
	res <- data.frame(test.stat = test.stat, p.val = p.val)
return(res)
}

#' Imputation methods for outliers
#'
#' \code{imp.outliers} replaces predefined quantum of the smallest and largest values by the less
#' extreme values. This procedure is applicable only to the numeric risk factors.
#'@param db Data frame of risk factors supplied for imputation.
#'@param sc Vector of all special case elements. Default values are \code{c(NA, NaN, Inf)}. Those values will be 
#'		excluded from calculation of imputed value and replacements.	
#'@param method Imputation method. Available options are: \code{"iqr"} and \code{"percentile"}. Method \code{iqr}
#' 		    performs identification of outliers by the method applied in boxplot 5-figures, while for 
#'		    \code{percentile} method user defines lower and upper limits for replacement. 
#'		    Default value is \code{"iqr"}.
#'@param range Determines how far the plot whiskers extend out from the box. If range is positive, 
#'	       the whiskers extend to the most extreme data point which is no more than range times the 
#'	       interquartile range from the box. A value of zero causes the whiskers to extend to 
#'	       the data extremes. Default \code{range} is set to is 1.5.
#'@param upper.pct Upper limit for percentile method. All values above this limit will be replaced by the value
#'		   identified at this percentile. Default value is set to \eqn{95^{th}} percentile (0.95).
#'		   This parameter is used only if selected \code{method} is \code{percentile}.
#'@param lower.pct Lower limit for percentile method. All values below this limit will be replaced by the value
#'		   identified at this percentile. Default value is set to \eqn{5^{th}} percentile (0.05).
#'		   This parameter is used only if selected \code{method} is \code{percentile}.
#'@return This function returns list of two data frames. The first data frame contains analyzed risk factors with
#' imputed values for outliers, while the second data frame presents the imputation report. Using the imputation report, 
#' for each risk factor, user can inspect imputed info (\code{info}), imputation method (\code{imputation.method}), 
#' imputed value (\code{imputation.val.upper} and \code{imputation.val.lower}), 
#' number of imputed observations (\code{imputation.num.upper} and \code{imputation.num.lower}).
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'gcd$age[1:20] <- NA
#'gcd$age.bin <- ndr.bin(x = gcd$age, y = gcd$qual, sc.method = "separately", y.type = "bina")[[2]]
#'gcd$dummy1 <- NA
#'imput.res.1 <- imp.outliers(db = gcd[, -1], 
#'			      method = "iqr",
#'			      range = 1.5)
#'#analyzed risk factors with imputed values
#'head(imput.res.1[[1]])
#'#imputation report
#'imput.res.1[[2]]
#'#percentile method
#'imput.res.2 <- imp.outliers(db = gcd[, -1], 
#'			      method = "percentile",
#'			      upper.pct = 0.95,
#'			      lower.pct = 0.05)
#'#analyzed risk factors with imputed values
#'head(imput.res.2[[1]])
#'#imputation report
#'imput.res.2[[2]]
#'@import monobin
#'@importFrom graphics boxplot
#'@importFrom stats quantile
#'@export
imp.outliers <- function(db, sc = c(NA, NaN, Inf, -Inf), method = "iqr", range = 1.5, 
			    upper.pct = 0.95, lower.pct = 0.05) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	method.opts <- c("iqr", "percentile")
	if	(length(method) != 1 | !method%in%method.opts) {
		stop(paste0("method argument has to be one of: ", 
				paste0(method.opts, collapse = ", ")))
		}
	if	(length(range) != 1 | !is.numeric(range)) {
		stop("range argument has to be numeric vector of lenght 1.")
		}
	if	(length(upper.pct) != 1 | !is.numeric(upper.pct) | upper.pct > 1 | upper.pct < 0 |
		 length(lower.pct) != 1 | !is.numeric(lower.pct) | lower.pct > 1 | lower.pct < 0 |
		 lower.pct >= upper.pct) {
		stop("upper.pct and lower.pct arguemnts have to be numeric vector between 0 and 1.
			Additonally, lower.pct has to be less than upper.pct.")
		}
	rf <- names(db)
	rfl <- length(rf)
	report <- vector("list", rfl)
	for	(i in 1:rfl) {
		rf.l <- rf[i]
		rf.imp <- db[, rf.l]
		if	(!is.numeric(rf.imp)) {
			report[[i]] <- data.frame(rf = rf.l, 
							  info = "Risk factor is not of numeric type.", 
							  imputation.method = NA)
			next			
			}
		complete.c <- rf.imp[!rf.imp%in%sc]
		cc <- length(complete.c)
		if	(cc < 0.1 * length(rf.imp)) {
			report[[i]] <- data.frame(rf = rf.l, 
							  info = "Less than 10% of complete cases.", 
							  imputation.method = NA)
			next
			}
		if	(method%in%"iqr") {
			bs <- boxplot(complete.c, range = range, plot = FALSE)
			rf.imp.ub <- max(bs$stats)
			rf.imp.lb <- min(bs$stats)
			cond <- rf.imp.ub%in%c(NA, Inf, NaN) | rf.imp.lb%in%c(NA, Inf, NaN)
			} else {
			rf.imp.ub <- try(quantile(complete.c, upper.pct, type = 2), silent = TRUE)
			rf.imp.lb <- try(quantile(complete.c, lower.pct, type = 2), silent = TRUE)
			cond <- class(rf.imp.ub)%in%"try-error" | class(rf.imp.lb)%in%"try-error" |
			  	  rf.imp.ub%in%c(NA, Inf, NaN) | rf.imp.lb%in%c(NA, Inf, NaN)
		
			}
		if	(cond) {
			report[[i]] <- data.frame(rf = rf.l, 
							  info = "Imputed value cannot be calculated.", 
							  imputation.method = NA)			
			next
			} else {
			num.upper <- sum(!rf.imp%in%sc & rf.imp > rf.imp.ub)
			num.lower <- sum(!rf.imp%in%sc & rf.imp < rf.imp.lb)
			rf.imp[!rf.imp%in%sc & rf.imp > rf.imp.ub] <- rf.imp.ub
			rf.imp[!rf.imp%in%sc & rf.imp < rf.imp.lb] <- rf.imp.lb
			db[, rf.l] <- rf.imp
			report[[i]] <- data.frame(rf = rf.l, 
							  info = "Imputation completed.", 
							  imputation.method = method,
							  imputation.val.upper = unname(rf.imp.ub),
							  imputation.val.lower = unname(rf.imp.lb),
							  imputation.num.upper = num.upper, 
							  imputation.num.lower = num.lower)
			}
		}
	report <- data.frame(bind_rows(report))	
return(list(db = db, report = report))
}
