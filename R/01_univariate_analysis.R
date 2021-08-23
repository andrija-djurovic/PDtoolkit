#' Univariate analysis
#'
#' \code{univariate} returns the univariate statistics for risk factors supplied in db argument.
#' \nFor numeric risk factors univariate report includes:
#' \itemize{
#'   \item rf.name: Risk factor name.
#'   \item rf.type: Risk factor type. This metric is always equal to \code{numeric}.
#'   \item bin.type: Bin type - special or complete cases.
#'   \item bin: Bin type. If a \code{sc.method} argument is equl to \code{"together"} then 
		    \code{bin} and \code{bin.type} have the same value. If the \code{sc.method} argument
		    is equal to  \code{"separately"} then \code{bin} bin will contain all special cases that
		    exist for analyized risk factor (e.g. \code{NA}, \code{NaN}, \code{Inf}).
#'   \item pct: Percentage of observations in each \code{bin}.
#'   \item cnt.unique: Number of unique values per \code{bin}.
#'   \item min: Minimum value. 
#'   \item p1, p5, p25, p50, p75, p95, p99: Percentile values.
#'   \item avg: Mean value.  
#'   \item avg.se: Standard error of the mean.
#'   \item max: Maximum value.  
#'   \item neg: Number of negative values.    
#'   \item pos: Number of positive values.   
#'   \item cnt.outliers: Number of outliers. 
#'Records above \code{Q75 + 1.5 * IQR}, where \code{IQR = Q75 - Q25} 	
#'}
#' For categorical risk factors univariate report includes:
#' \itemize{
#'   \item rf.name: Risk factor name.
#'   \item rf.type: Risk factor type. This metric is equal to one of \code{character}, 
			  \code{factor} or \code{logical}.
#'   \item bin.type: Bin type - special or complete cases.
#'   \item bin: Bin type. If a \code{sc.method} argument is equl to \code{"together"} then 
		    \code{bin} and \code{bin.type} have the same value. If the \code{sc.method} argument
		    is equal to  \code{"separately"} then \code{bin} bin will contain all special cases that
		    exist for analyized risk factor (e.g. \code{NA}, \code{NaN}, \code{Inf}).
#'   \item pct: Percentage of observations in each \code{bin}.
#'   \item cnt.unique: Number of unique values per \code{bin}.

#'   \item neg: Number of negative values.    
#'   \item pos: Number of positive values. 	
#'}  
#'@param db Data frame of risk factors supplied for univariate analysis.
#'@param sc Numeric vector with special case elements. Default values are c(NA, NaN, Inf).
#'@param sc.g Define how special cases will be treated, all together or in separate bins. Possible values are "together", "separately".
#' 
#'@return The command \code{univariate} returns the data frame with explained univariate metrics for numeric,
	    character, factor and logical class of risk factors.
#'@example
#' suppressMessages(library(monobin))
#' data(gcd)
#' gcd$age[100:120] <- NA
#' gcd$age.bin <- ndr.bin(x = gcd$age, y = gcd$qual)[[2]]
#' gcd$age.bin <- as.factor(gcd$age.bin)
#' gcd$maturity.bin <- ndr.bin(x = gcd$maturity, y = gcd$qual)[[2]]
#' gcd$amount.bin <- ndr.bin(x = gcd$amount, y = gcd$qual)[[2]]
#' gcd$all.miss1 <- NaN
#' gcd$all.miss2 <- NA
#' gcd$tf <- sample(c(TRUE, FALSE), nrow(gcd), rep = TRUE)
#' gcd$dates <- Sys.Date()
#' str(gcd)
#' univariate(db = gcd)
#'@export
univariate <- function(db, sc = c(NA, NaN, Inf), sc.method = "together") {
	scm.opts <- c("together", "separately")
	cond.01 <- !is.numeric(sc[!sc%in%NA]) 
	cond.02 <- !sc.method[1]%in%scm.opts
	if	(cond.01) {
		stop("sc has to be numeric vector")
		}
	if	(cond.02) {
		stop(paste0("sc.method  argument has to be one of: ", 
				paste0(scm.opts, collapse = ", ")))
		}
	var.l <- ncol(db)
	var.n <- names(db)
	res <- vector("list", var.l)
	for	(i in 1:var.l) {
		varl <- var.n[i]
		if	(sc.method[1] == "together") {
			db$bin <- ifelse(db[, varl]%in%sc, "special cases", "complete cases")
			} else {
			db$bin <- ifelse(d[, varl]%in%sc, d[, varl], "complete cases")
			}
		db$type <- factor(ifelse(db$bin%in%"complete cases", "complete cases", "special cases"),
					levels = c("special cases", "complete cases"), 
					ordered = TRUE)
		if	(is.numeric(db[, varl])) {
			res[[i]] <- univariate.num(db = db, x = varl) 
			} 
		if	(is.character(db[, varl]) | is.factor(db[, varl]) | is.logical(db[, varl])) {
			res[[i]] <- univariate.cat(db = db, x = varl) 
			}
		}
	res <- data.frame(bind_rows(res))
return(res)
}

univariate.num <- function(db, x) {
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
				  ))
		 )
	res <- data.frame(res)
	res <- cbind.data.frame(rf.name = x, rf.type = "numeric", res)
return(res)
}

univariate.cat <- function(db, x) {
	res <- suppressWarnings(
		 db %>% 
		 group_by_at(c("bin.type" = "type", "bin" = as.character("bin"))) %>%
		 summarise_at(all_of(x), 
				  funs(
				  cnt = n(),
				  pct = n() / nrow(db),
				  cnt.unique = length(unique(.)),
				  neg = sum(. < 0),
				  pos = sum(. > 0)
				  ))
		 )
	res <- data.frame(res)
	res <- cbind.data.frame(rf.name = x, rf.type = class(db[, x]), res)
return(res)
}



