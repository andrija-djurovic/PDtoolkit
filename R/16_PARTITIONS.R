#' Create partitions (aka nested dummy variables)
#'
#' \code{create.partitions} performs creation of partitions (aka nested dummy variables).
#' Using directly into logistic regression, partitions provide insight into difference of log-odds of adjacent risk factor bins (groups). 
#' Adjacent bins are selected based on alphabetic order of analyzed risk factor modalities, therefore it is important 
#' to ensure that modality labels are defined in line with expected monotonicity or any other criterion 
#' that is considered while engineering the risk factors. 
#'@param db Data set of risk factors to be converted into partitions.
#'@return The command \code{create.partitions} returns a list of two objects (data frames).\cr
#'	    The first object (\code{partitions}), returns the data set with newly created nested dummy variables.\cr
#'	    The second object (\code{info}), is the data frame that returns info on partition process.
#'	    Set of quality checks are performed and reported if any of them observed. Two of them are of terminal nature
#'	    i.e. if observed, risk factor is not processed further (less then two non-missing groups and more than 10 modalities) 
#'	    while the one provides only info (warning) as usually deviates from the main principles of risk factor processing 
#'	    (less than 5% of observations per bin).
#'@references 
#'Scallan, G. (2011). Class(ic) Scorecards: Selecting Characteristics and Attributes in Logistic Regression,  
#'			    Edinburgh Credit Scoring Conference, downloaded from 
#'			    \href{https://www.scoreplus.com/papers/papers}{here}.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#identify numeric risk factors
#'num.rf <- sapply(loans, is.numeric)
#'num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#'#discretized numeric risk factors using ndr.bin from monobin package
#'loans[, num.rf] <- sapply(num.rf, function(x) 
#'	cum.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#'str(loans)
#'loans.p <- create.partitions(db = loans[, num.rf])
#'head(loans.p[["partitions"]])
#'loans.p[["info"]]
#'#bring target to partitions
#'db.p <- cbind.data.frame(Creditability = loans$Creditability, loans.p[[1]])
#'#prepare risk factors for stepMIV 
#'db.p[, -1] <- sapply(db.p[, -1], as.character)
#'#run stepMIV
#'res <- stepMIV(start.model = Creditability ~ 1, 
#'	   miv.threshold = 0.02, 
#'	   m.ch.p.val = 0.05,
#'	   coding = "dummy",
#'	   db = db.p)
#'#check output elements
#'names(res)
#'#extract the final model
#'final.model <- res$model
#'#print coefficients
#'summary(final.model)$coefficients
#'@export
create.partitions <- function(db) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	rf <- names(db)
	rfl <- length(rf)
	res <- vector("list", rfl)
	info <- vector("list", rfl)
	for	(i in 1:rfl) {
		rf.l <- rf[i]
		x <- db[, rf.l]
		res.l <- ndv(x = x, rf.n = rf.l)
		if	(res.l[[1]]) {
			res[[i]] <- res.l[[3]]
			info[[i]] <- cbind.data.frame(rf = rf.l, res.l[[2]])
			} else {
			info[[i]] <- cbind.data.frame(rf = rf.l, res.l[[2]])
			}	
		}
	res <- do.call("cbind", res)
	info <- data.frame(bind_rows(info))
return(list(partitions = res, info = info))
}

ndv <- function(x, rf.n) {
	x.t <- table(x, useNA = "always")
	x.n <- names(x.t)
	x.c <- x.n[!is.na(x.n)]	
	info <- NULL
	if	(length(x.c) < 2) {
		return(list(success = FALSE, info = "Less than 2 non-NA groups."), code = "terminal")
		}
	if	(length(x.c) > 10) {
		return(list(success = FALSE, info = "More than 10 groups.", code = "terminal"))
		}
	x.l <- sum(x.t)
	x.s <- x.t / x.l
	x.s.c <- x.s[!is.na(x.n)] < 0.05
	if	(any(x.s.c)) {
		info <- paste0(names(x.s.c)[x.s.c], collapse = ", ")
		info <- paste0("Group(s) with less than 5% of obs.: ", info)
		info <- data.frame(info = info, code = "warning")
		}
	x.c.l <- length(x.c)
	nd.db <- vector("list", x.c.l)
	for	(i in 2:x.c.l) {
		level.l <- x.c[1:(i - 1)]
		nd <- data.frame(dummy = ifelse(is.na(x), NA, ifelse(x%in%level.l, 0, 1)))
		names(nd) <- paste0(rf.n, x.c[i])
		nd.db[[i]] <- nd
		}
	nd.db <- bind_cols(nd.db)
	if	(is.null(info)) {
		info <- data.frame(info = "Partitions created.", code = "success")
		}
return(list(success = TRUE, info = info, db = nd.db))
}
