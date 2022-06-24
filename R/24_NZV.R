#' Near-zero variance
#'
#' \code{nzv} procedure aims to identify risk factors with low variability (almost constants). Usually these risk factors are 
#' expertly investigated and decision is made if they should be excluded from further modeling process. \cr
#' \code{nzv} output report includes the following metrics:
#' \itemize{
#'   \item rf: Risk factor name.
#'   \item rf.type: Risk factor class. This metric is always one of: \code{numeric} or \code{categorical}.
#'   \item sc.num: Number of special cases.
#'   \item sc.pct: Percentage of special cases in total number of observations.
#'   \item cc.num: Number of complete cases.
#'   \item cc.pct: Percentage of complete cases in total number of observations. Sum of this value and \code{sc.pct} is equal to 1.
#'   \item cc.unv: Number of unique values in complete cases. 
#'   \item cc.unv.pct: Percentage of unique values in total number of complete cases.
#'   \item cc.lbl.1: The most frequent value in complete cases.  
#'   \item cc.frq.1: Number of occurrence of the most frequent value in complete cases.
#'   \item cc.lbl.2: The second most frequent value in complete cases.  
#'   \item cc.frq.2: Number of occurrence of the second most frequent value in complete cases.
#'   \item cc.fqr: Frequency ratio - the ratio between the occurrence of most frequent and the second most frequent value in 
#'			 complete cases.  
#'   \item ind: Indicator which takes value of \code{1} if the percentage of complete cases is less then 10% and frequency ratio 
#'		    (\code{cc.fqr}) greater than 19. This values can be used for filtering risk factors that need further expert
#'		    investigation, but user are also encourage to derive its own indicators based on reported
#'		    metrics.
#'}
#'@param db  Data frame of risk factors supplied for near-zero variance analysis.
#'@param sc Numeric or character vector with special case elements. Default values are \code{c(NA, NaN, Inf, -Inf)}.
#'@return The command \code{nzv} returns the data frame with different matrices needed for identification of near-zero variables.
#'	    For details see Description section. 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#artificially add some special values
#'loans$"Account Balance"[1:10] <- NA
#'rf.s <- nzv(db = loans, sc = c(NA, NaN, Inf, -Inf))
#'rf.s
#'@export
nzv <- function(db, sc = c(NA, NaN, Inf, -Inf)) {
	ops <- options(warn = -1)
	on.exit(options(ops))

	if	(!is.data.frame(db)) {
		stop("mapping is not a data frame.")
		}
	sc.num <- unique(as.numeric(sc))
	sc.cat <- unique(as.character(sc))
	rf <- names(db)
	rfl <- length(rf)
	res <- vector("list", rfl)
	for	(i in 1:rfl) {
		rf.l <- rf[i]
		x <- db[, rf.l]
		x.t <- is.numeric(x) 
		if	(x.t) {
			x.sc <- x[x%in%sc.num]
			x.cc <- x[!x%in%sc.num]
			} else {
			x.sc <- x[x%in%sc.cat]
			x.cc <- x[!x%in%sc.num]
			}
		x.sc.num <- length(x.sc) 
		x.sc.pct <- x.sc.num / length(x)
		x.cc.num <- length(x.cc)
		x.cc.pct <- 1 - x.sc.pct
		x.cc.unv <- length(unique(x.cc))
		x.cc.upc <- x.cc.unv / length(x.cc)
		x.cc.tbl <- sort(table(x.cc), decreasing = TRUE)
		if	(length(x.cc.tbl) > 1) {
			x.cc.lb1 <- names(x.cc.tbl[1])
			x.cc.lb2 <- names(x.cc.tbl[2])			
			x.cc.fq1 <-  unname(x.cc.tbl[1])	
			x.cc.fq2 <-  unname(x.cc.tbl[2])		
			x.cc.fqr <- x.cc.fq1 / x.cc.fq2
			} else {
			x.cc.lb1 <- names(x.cc.tbl[1])
			x.cc.lb2 <- NA	
			x.cc.fq1 <-  unname(x.cc.tbl[1])	
			x.cc.fq2 <-  NA					
			x.cc.fqr <- 1
			}
		res.l <- data.frame(rf = rf.l, 
					  type = ifelse(x.t, "numeric", "categorical"), 
					  sc.num = x.sc.num,
					  sc.pct = x.sc.pct,
					  cc.num = x.cc.num,
					  cc.pct = x.cc.pct,
					  cc.unv = x.cc.unv, 
					  cc.unv.pct = x.cc.upc,
					  cc.lbl.1 = x.cc.lb1,
					  cc.frq.1 = x.cc.fq1, 	
					  cc.lbl.2 = x.cc.lb1,
					  cc.frq.2 = x.cc.fq2, 
					  cc.fqr = x.cc.fqr, 
					  ind = ifelse(x.cc.pct < 0.1 & x.cc.fqr > 19, 1, 0))
		res[[i]] <- res.l
		}
	res <- bind_rows(res)
return(res)
}


