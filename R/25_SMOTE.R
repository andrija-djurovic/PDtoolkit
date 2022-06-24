#' Synthetic Minority Oversampling Technique (SMOTE)
#'
#' \code{smote} performs type of data augmentation for the selected (usually minority). In order to process continious and 
#' categorical risk factors simultaneously, Heterogeneity Eucledian Overlapping Metric (HEOM) is used in nearest neighbours
#' algorithm.
#'@param db Data set of risk factors and target variable.
#'@param target Name of target variable within \code{db} argument.
#'@param minority.class Value of minority class. It can be numeric or character value, but it has to exist in target variable.
#'@param osr Oversampling rate. It has to be numeric value greater than 0 (for example 0.2 for 20% oversampling).
#'@param ordinal.rf Character vector of ordinal risk factors. Default value is \code{NULL}.
#'@param num.rf.const Data frame with constrains for numeric risk factors. It has to contain the following columns:
#'			    \code{rf}(numeric risk factor names from \code{db}), 
#'			    \code{lower} (lower bound of numeric risk factor), 
#'			    \code{upper} (upper bound of numeric risk factor),
#'			    \code{type} (type of numeric risk factor - \code{"numeric"} or \code{"integer"}).
#'			    Constrains are used for correction of syntetic data for selected numeric risk factors.
#'			    Default valus is \code{NULL} which means that no corrections are assumed.
#'@param k Number of nearest neighbours. Default value is 5.
#'@param seed Random seed needed for ensuring the result reproducibility. Default is 81000.
#'@return The command \code{smote} returns a data frame with added syntetic observations for selected minority class.
#' The data frame contains all variables from \code{db} data frame plus additional variable (\code{smote}) that serves as 
#' indicator for distinguishing between original and synthetic observations.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#check numeric variables (note that one of variables is target not a risk factor)
#'names(loans)[sapply(loans, is.numeric)]
#'#define constains of numeric risk factors
#'num.rf.const <- data.frame(rf = c("Duration of Credit (month)", "Credit Amount", "Age (years)"),
#'				   lower = c(4, 250, 19),
#'				   upper = c(72, 20000, 75),
#'				   type = c("integer", "numeric", "integer"))
#'num.rf.const
#'
#'#loans$"Account Balance"[990:1000] <- NA
#'#loans$"Credit Amount"[900:920] <- NA
#' 
#'loans.s <- smote(db = loans,
#'		     target = "Creditability",
#'		     minority.class = 1,  
#'		     osr = 0.5,
#'		     ordinal.rf = NULL, 
#'		     num.rf.const = num.rf.const, 
#'		     k = 5, 
#'		     seed = 81000)
#'str(loans.s)
#'table(loans.s$Creditability, loans.s$smote)
#'#select minority class
#'loans.mc <- loans.s[loans.s$Creditability%in%1, ]
#'#compare some risk factors of original data and smote simulations
#'nrf <- c("Duration of Credit (month)", "Credit Amount", "Age (years)")
#'lapply(split(loans.mc[, nrf], loans.mc$smote), summary)
#'lapply(split(loans.mc[, "Account Balance", drop = FALSE], loans.mc$smote), 
#'	 function(x) prop.table(table(x)))
#'@export
smote <- function(db, target, minority.class, osr, ordinal.rf = NULL, num.rf.const = NULL, k = 5, seed = 81000) {
	dfl.n <- c("rf", "lower", "upper", "type")
	dfl.t <- c("integer", "numeric")
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	if	(length(names(db)[!names(db)%in%target]) == 0) {
		stop("only target supplied in db.")
		}
	if	(!is.null(num.rf.const)) {
		if	(!is.data.frame(num.rf.const)) {
			stop("num.rf.const is not a data frame.")
			}
		if	(any(!dfl.n%in%names(num.rf.const))) {
			stop(paste0("column names of num.rf.const data frame have to be the following: ",
			     paste(dfl.n, collapse = ", "), "."))
			}
		if	(any(!unique(num.rf.const$type)%in%dfl.t)) {
			stop("column type of num.rf.const has to be one of integer or numeric.")
			}
		if	(any(!num.rf.const$rf%in%names(db))) {
			stop("supplied risk factor(s) in num.rf.const do not exist in db.")
			}
		}		
	var.check <- c(target, ordinal.rf)
	if	(!sum(var.check%in%names(db)) == length(var.check)) {
		stop("target and/or ordinal.rf do not exist in db.")
		}
	target.mod <- unique(db[, target])
	excl.ind <- is.na(target.mod) | is.infinite(target.mod)
	if	(length(target.mod[!excl.ind]) == 0) {
		stop("target contains only unavailable values")
		} 
	if	(!minority.class%in%target.mod[!excl.ind]) {
		stop("monority.class does not exists in target inidicator.")
		}
	if	(!is.numeric(osr) | !is.numeric(k)) {
		stop("osr and k has to be of numeric type.")
		}
	if	(k <= 0 | k > 50) {
		stop("k has to be between 1 and 50.")
		}
	if	(osr <= 0) {
		stop("oversampling rate has to be greater than 0.")
		}
	db.c <- db
	#ordinal to interval risk factors
	ord.l <- length(ordinal.rf)
	if	(ord.l > 0) {
		for	(i in 1:ord.l) {
			db.c[, ordinal.rf[i]] <- ord.to.int(x = db.c[, ordinal.rf[i]])
			}
		}
	#normalization
	rf.num.check <- sapply(db.c[, !names(db.c)%in%target], is.numeric)
	rf.num <- names(rf.num.check)[rf.num.check]
	if	(length(rf.num) > 0) {
		for	(i in 1:length(rf.num)) {
			rf.num.l <- rf.num[i]
			db.c[, rf.num.l] <- db.c[, rf.num.l] / (max(db.c[, rf.num.l], na.rm = TRUE) - 
								    	    min(db.c[, rf.num.l], na.rm = TRUE))
			}
		}
	#index sampling
	db.mc <- db[db[, target]%in%minority.class, ]
	db.c.mc <- db.c[db.c[, target]%in%minority.class, ]
	mc.num <- nrow(db.c.mc)
	os.num <- round(mc.num * osr)
	set.seed(seed)
	indx <- sample(1:mc.num, os.num, replace = ifelse(os.num > mc.num, TRUE, FALSE))
	#k check
	if	(k > mc.num) {k <- mc.num}
	#find k nearest neighbours
	res.knn <- heom(db = db.c.mc[, !names(db.c.mc)%in%target], 
			    indx = indx, 
			    k = k, 
		     	    rfn = unname(which(rf.num.check)), 
		    	    nc = length(rf.num.check),
			    db.nr = nrow(db.c.mc))
	#create synthetic data
	db.s <- create.synthetic.data(db = db.mc[, !names(db.mc)%in%target],
						target = target,
						minority.class = minority.class,
						res.knn = res.knn, 
						seed = seed)
	#correct for given constrains
	if	(!is.null(num.rf.const)) {
		db.s <- synthetic.data.corr(db = db.s, num.rf.const = num.rf.const)
		}
	db.s <- cbind.data.frame(xxxxx = minority.class, db.s, smote = 1)
	names(db.s)[1] <- target
	db.smote <- bind_rows(cbind.data.frame(db, smote = 0), db.s)
return(db.smote)
}

ord.to.int <- function(x) {
	x.u <- sort(unique(x), na.last = TRUE)
	x.r <- 1:length(x.u[!is.na(x.u)])
	names(x.r) <- x.u[!is.na(x.u)]	
	x.n <- unname(x.r[x])
return(x.n)
}
heom <- function(db, indx, k, rfn, nc, db.nr) {
	indx.u <- sort(unique(indx))
	indxl <- length(indx.u)
	res <- data.frame(matrix(ncol = k + 2, nrow = indxl))
	names(res) <- c("indx", "ss", paste0("k", 1:k))
	for	(i in 1:indxl) {
		indx.l <- indx.u[i]
		ss <- sum(indx%in%indx.l)
		db.l <- db[indx.l, ]
		res.l <- rep(NA, db.nr)
		for	(y in 1:db.nr) {
			res.l[y] <- heom.aux(x = db.l, y = db[y, ], rfn = rfn, nc = nc)
			}
		dist.sorted <- sort(res.l, index.return = TRUE)$ix
		find.k <- dist.sorted[!dist.sorted%in%indx.l][1:k]
		res[i, ] <- c(indx.l, ss, find.k)
		}
return(res)
}
heom.aux <- function(x, y, rfn, nc) {
	res <- rep(NA, nc)
	for	(i in 1:nc) {
		x.l <- x[1, i]
		y.l <- y[1, i]
		if	(is.na(x.l) | is.infinite(x.l) | is.na(y.l) | is.infinite(y.l)) {
			res[i] <- 1; next
			}
		if	(i%in%rfn) {
			res[i] <- abs(x.l - y.l)
			} else {
			res[i] <- ifelse(x.l == y.l, 0, 1)
			}
		}
	distance <- sqrt(sum(res ^ 2))
return(distance)
}
create.synthetic.data <- function(db, target = target, minority.class, res.knn, seed) {
	sd.r <- sum(res.knn$ss)
	sd.c <- ncol(db)
	rf.num <- unname(which(sapply(db, is.numeric)))
	res <- data.frame(matrix(ncol = sd.c, nrow = sd.r)) 
	names(res) <- names(db)
	sd.r.cs <- c(0, cumsum(res.knn$ss))
	for	(i in 1:nrow(res.knn)) {
		indx.l <- res.knn$indx[i]
		ss <-  res.knn$ss[i]
		knn <- unname(c(res.knn[i, -c(1, 2)], recursive = TRUE))
		seed <- seed + i
		set.seed(seed)
		nn.indx <- sample(knn, ss, replace = TRUE)
		db.l <- db[indx.l, ]
		db.nn <- db[nn.indx, ]
		synt.d <- synthetic.data.aux(x = db.l, 
						     y = db.nn, 
						     nc = ncol(db.l), 
						     rfn = rf.num, 
						     seed = seed)
		res[(sd.r.cs[i] + 1):sd.r.cs[i + 1], ] <- synt.d
		}
return(res)
}
synthetic.data.aux <- function(x, y, nc, rfn, seed) {
	res.l <- data.frame(matrix(ncol = nc, nrow = 1))
	names(res.l) <- names(x)
	ss <- nrow(y)
	res <- vector("list", ss)	
	if	(length(rfn) > 0) {
		synth.exp <- "res.i[, rfn] <- x[, rfn] +  rand.n * (y.l[, rfn] - x[, rfn]);
				  res.i[, -rfn] <- y.l[, -rfn]"
		} else {
		synth.exp <- "res.i <- y.l"
		}
	for	(i in 1:ss) {
		res.i <- res.l
		y.l <- y[i, ]
		set.seed(seed)
		rand.n <- runif(1)
		eval(parse(text = synth.exp))
		res[[i]] <- res.i
		}
	res <- bind_rows(res)
return(res)
}
synthetic.data.corr <- function(db, num.rf.const) {
	for	(i in 1:nrow(num.rf.const)) {
		rf.n <- num.rf.const$rf[i]
		rf.l <- num.rf.const$lower[i]
		rf.u <- num.rf.const$upper[i]
		rf.t <- num.rf.const$type[i]
		db[, rf.n] <- ifelse(db[, rf.n] < rf.l, rf.l, db[, rf.n])
		db[, rf.n] <- ifelse(db[, rf.n] > rf.u, rf.u, db[, rf.n])
		if	(rf.t%in%"integer") {db[, rf.n] <- round(db[, rf.n])}
		}
return(db)
}


