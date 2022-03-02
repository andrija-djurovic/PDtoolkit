#' Stepwise logistic regression based on marginal information value (MIV)
#'
#' \code{stepMIV} performs stepwise logistic regression based on MIV.
#'@param start.model Formula class that represent starting model. It can include some risk factors, but it can be
#'			   defined only with intercept (\code{y ~ 1} where \code{y} is target variable).
#'@param miv.threshold MIV entrance threshold. Only the risk factors with MIV higher than the threshold are candidate
#'			     for the new model. Additional criteria is that MIV value should significantly separate
#'			     good from bad cases measured by marginal chi-square test. 
#'@param m.ch.p.val Significance level of p-value for marginal chi-square test. This test additionally supports MIV value of 
#'		     candidate risk factor for final decision.
#'@param coding Type of risk factor coding within the model. Available options are: \code{"WoE"} and
#'		    \code{"dummy"}. If \code{"WoE"} is selected, then modalities of the risk factors are replaced
#'		    by WoE values, while for \code{"dummy"} option dummies (0/1) will be created for \code{n-1} 
#'		    modalities where \code{n} is total number of modalities of analyzed risk factor.
#'@param coding.start.model Logical (\code{TRUE} or \code{FALSE}), if risk factors from the starting model should be WoE coded. 
#'				    It will have an impact only for WoE coding option. Default value is \code{FALSE}.
#'@param db Modeling data with risk factors and target variable. All risk factors should be categorized and as of
#'		character type.
#'@param offset.vals This can be used to specify an a priori known component to be included in the linear predictor during fitting. 
#'		    	   This should be \code{NULL} or a numeric vector of length equal to the number of cases. 
#'		    	   One or more offset terms can be included in the formula instead or as well, and if more than one is specified 
#'		   	   their sum is used. Default is \code{NULL}.
#'@return The command \code{stepMIV} returns a list of five objects.\cr
#'	    The first object (\code{model}), is the final model, an object of class inheriting from \code{"glm"}.\cr
#'	    The second object (\code{steps}), is the data frame with risk factors selected at each iteration.\cr
#'	    The third object (\code{miv.iter}), is the data frame with iteration details.\cr
#'	    The fourth object (\code{warnings}), is the data frame with warnings if any observed.
#'	    The warnings refer to the following checks: if risk factor has more than 10 modalities,
#'	    if any of the bins (groups) has less than 5% of observations and 
#'	    if there are problems with WoE calculations.\cr
#'	    The final, fifth, object \code{dev.db} object \code{dev.db} returns the model development database.
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
#'	ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#'str(loans)
#'#run stepMIV
#'res <- stepMIV(start.model = Creditability ~ 1, 
#'		   miv.threshold = 0.02, 
#'		   m.ch.p.val = 0.05,
#'		   coding = "WoE",
#'		   coding.start.model = FALSE,
#'		   db = loans)
#'#check output elements
#'names(res)
#'#extract the final model
#'final.model <- res$model
#'#print coefficients
#'summary(final.model)$coefficients
#'#print steps of stepwise
#'res$steps
#'#print head of all iteration details
#'head(res$miv.iter)
#'#print warnings
#'res$warnings
#'#print head of coded development data
#'head(res$dev.db)
#'#calculate AUC
#'auc.model(predictions = predict(final.model, type = "response", newdata = res$dev.db),
#'	    observed = res$dev.db$Creditability)
#'@import monobin
#'@importFrom stats formula
#'@export
stepMIV <- function(start.model, miv.threshold, m.ch.p.val, coding, coding.start.model = FALSE, db, offset.vals = NULL) {
	#check arguments
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	coding.opt <- c("WoE", "dummy")
	if	(!coding%in%coding.opt) {
		stop(paste0("coding argument has to be one of: ", paste0(coding.opt, collapse = ', '), "."))
		}
	if	(!(is.numeric(miv.threshold) & length(miv.threshold) == 1 &
		 is.numeric(m.ch.p.val) & length(m.ch.p.val) == 1)) {
		stop("miv.threshold and m.ch.p.val has to be of numeric type.")
		}
	if	(!is.logical(coding.start.model)) {
		stop("coding.start.model has to be logical (TRUE or FALSE).")
		}
	#extract model variables
	start.vars <- all.vars(start.model)
	target <- start.vars[1]
	if	(length(start.vars) > 1) {
		rf.start <- start.vars[-1]
		} else {
		rf.start <- NULL
		}
	#check starting model formula
	check <- any(!c(target, rf.start)%in%names(db))
	if	(check | is.na(target)) {
		stop("Formula for start.model not specified correctly. 
			Check column names and if formula class is passed to start.model.")
		}
	#check target against 0/1
	y <- db[, target]
	if	(!sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])) {
		stop("Target is not 0/1 variable.")
		}
	rf.rest <- names(db)[!names(db)%in%c(c(target, rf.start))]
	#check supplied risk factors
	rf.restl <- length(rf.rest)
	if	(rf.restl == 0) {
		stop("Risk factors are missing. Check db argument.")
		}
	#define warning table
	warn.tbl <- data.frame()
	#check num of modalities per risk factor
	unique.mod <- sapply(db[, rf.rest, drop = FALSE], function(x) length(unique(x)))
	check.mod <- names(unique.mod)[unique.mod > 10]
	if	(length(check.mod) > 0) {
		warn.rep <- data.frame(rf = check.mod, comment = "More than 10 modalities.")
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		}
	#check for numeric risk factors (change the order of numeric check and num of modalities)
	num.type <- sapply(db[, rf.rest, drop = FALSE], is.numeric)
	check.num <- names(num.type)[num.type ]
	if	(length(check.num) > 0) {
		msg <- "Numeric type. Risk factor is excluded from further process."
		warn.rep <- data.frame(rf = check.num, comment = msg)
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		rf.rest <- rf.rest[!rf.rest%in%check.num]
		rf.restl <- length(rf.rest)
		}
	#generate woe table
	rf.woe.o <- vector("list", rf.restl) 
	for	(i in 1:rf.restl) {
		rf.rest.l <- rf.rest[i]
	 	woe.o <- woe.tbl(tbl = db, x = rf.rest[i], y = target, y.check = FALSE)
		woe.o <- cbind.data.frame(rf = rf.rest.l, woe.o)
		pct.check <- any(woe.o$pct.o < 0.05)
		woe.o$pct.check <- pct.check
		woe.o$woe.check <- any(woe.o$woe%in%c(NA, NaN, Inf, -Inf)) 
		rf.woe.o[[i]] <- woe.o
		}
	rf.woe.o <- bind_rows(rf.woe.o)
	#check pct of obs per bin
	check.pct <- unique(rf.woe.o$rf[rf.woe.o$pct.check]) 
	if	(length(check.pct) > 0) {
		warn.rep <- data.frame(rf = check.pct, comment = "At least one pct per bin less than 5%.")
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		}
	#check WoE calc
	check.woe <- unique(rf.woe.o$rf[rf.woe.o$woe.check]) 
	if	(length(check.woe) > 0) {
		msg <- "Problem with WoE calculation (NA, NaN, Inf, -Inf)."
		msg <- paste0(msg, " Risk factor is excluded from further process.")
		warn.rep <- data.frame(rf = check.woe, comment = msg)
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		rf.rest <- rf.rest[!rf.rest%in%check.woe]
		}
	#check coding
	if	(coding.start.model) {
		if	(coding%in%"WoE" & length(rf.start) > 0) {
			woe.rep <- replace.woe(db = db[, c(target, rf.start)], target = target)
			woe.rep.check <- woe.rep[[2]]
			if	(nrow(woe.rep.check) > 0) {
				msg <- "Problem with the WoE calculations for the starting model. 
				  	Check the variable class and the following risk factors for NA or Inf values: "
				msg <- paste0(msg, paste(woe.rep.check$rf, collapse = ", "), ".")
				stop(msg)
				}
			db[, rf.start] <- woe.rep[[1]][, rf.start]
			}
		}
	#miv calculation
	if	(!is.null(offset.vals)) {
		db <- cbind.data.frame(db, offset.vals = offset.vals)
		}
	steps <- data.frame()
	miv.iter.tbl <- data.frame()
	mod.frm <- start.model
	iter <- 1
	repeat {
		print(paste0("Running iteration: ", iter))
		rf.restl <- length(rf.rest)
		if	(rf.restl == 0) {break}
		miv.iter <- vector("list", rf.restl)
		miv.tbl <- vector("list", rf.restl)
		for	(i in 1:rf.restl) {
			rf.l <- rf.rest[i]
			woe.o.l <- rf.woe.o[rf.woe.o$rf%in%rf.l, ]
			miv.res <- miv(model.formula = as.formula(mod.frm), rf.new = rf.l, db = db, 
					   woe.o = woe.o.l, offset.vals = offset.vals)
			miv.iter[[i]] <- miv.res[[1]]
			miv.tbl[[i]] <- miv.res[[2]]
			}
		miv.iter <- bind_rows(miv.iter)
		miv.iter <- miv.iter[order(-miv.iter$miv), ]
		rf.cand <- miv.iter[miv.iter$miv > miv.threshold & 
				  miv.iter$p.val < m.ch.p.val,    ]	
		miv.tbl <- bind_rows(miv.tbl)
		miv.tbl <- miv.tbl[order(-miv.tbl$miv), ]
		miv.tbl <- cbind.data.frame(iter = iter, miv.tbl)
		miv.iter.tbl <- bind_rows(miv.iter.tbl, miv.tbl)
		if	(nrow(rf.cand) > 0) {
			rf.cand <- rf.cand[1, ]
			rf.cand.name <- rf.cand$rf.miv 
			rf.rest <- rf.rest[!rf.rest%in%rf.cand.name]
			steps <- bind_rows(steps, rf.cand)
			rf.start <- c(rf.start, rf.cand.name)
			mod.frm <- paste0("`",target, "`", " ~ ", 
     						paste(paste0("`", rf.start, "`"), collapse = " + "))
			if	(coding%in%"WoE") {	
				woe.o <- rf.woe.o[rf.woe.o$rf%in%rf.cand.name, ]
				db[, rf.cand.name] <- replace.woe.aux(x = db[, rf.cand.name], woe.tbl = woe.o)
				}
			} else {
			break
			}
		iter <- iter + 1	
		}
	if	(is.null(offset.vals)) {
		lr.mod <- glm(formula = as.formula(mod.frm), family = "binomial", data = db)
		} else {
		lr.mod <- glm(formula = as.formula(mod.frm), family = "binomial", data = db, offset = offset.vals)
		}
	if	(nrow(steps) > 0) {steps <- cbind.data.frame(target = target, steps)}
	res <- list(model = lr.mod, 
			steps = steps, 
			miv.iter = miv.iter.tbl, 
			warnings = if (nrow(warn.tbl) > 0) {warn.tbl} else {data.frame(comment = "There are no warnings.")}, 
			dev.db = db)
return(res)	
}
miv <- function(model.formula, rf.new, db, woe.o = NULL, offset.vals) {	
	if	(is.null(offset.vals)) {	
		model.c <- glm(formula = model.formula, family = "binomial", data = db) 
		} else {
		model.c <- glm(formula = model.formula, family = "binomial", data = db, offset = offset.vals)
		} 
	model.p <- unname(predict(model.c, newdata = db, type = "response"))
	db$pred <- model.p
	db <- db[!is.na(db$pred), ]
	if	(is.null(woe.o)) {
		observed <- woe.tbl(tbl = db, x = rf.new, y = all.vars(formula(model.formula))[1])
		} else { 
		observed <- woe.o[, c("bin", "no", "ng", "nb", "woe")]
		}	
	expected <- woe.tbl(tbl = db, x = rf.new, y = "pred", y.check = FALSE)
	comm.cols <- c("bin", "no", "ng", "nb", "woe")
	miv.tbl <- merge(observed[, comm.cols], 
			    expected[, comm.cols], 
			    by = "bin", 
			    all = TRUE, 
			    suffixes = c(".o",".e"))
	#miv
	miv.tbl <- cbind.data.frame(rf = rf.new, miv.tbl)
	miv.tbl$delta <- miv.tbl$woe.o - miv.tbl$woe.e
	miv.val.g <- sum(miv.tbl$ng.o * miv.tbl$delta) / sum(miv.tbl$ng.o)	
	miv.val.b <- sum(miv.tbl$nb.o * miv.tbl$delta) / sum(miv.tbl$nb.o)
	miv.val <- miv.val.g - miv.val.b
	#chi-square test
	m.chiq.g <- miv.tbl$ng.o * log(miv.tbl$ng.o / miv.tbl$ng.e)
	m.chiq.b <- miv.tbl$nb.o * log(miv.tbl$nb.o / miv.tbl$nb.e)
	m.chiq.gb <- m.chiq.g + m.chiq.b
	m.chiq.stat <- 2 * sum(m.chiq.gb)
	p.val <- pchisq(m.chiq.stat, nrow(miv.tbl) - 1, lower.tail = FALSE)
	res.tbl <- cbind.data.frame(miv.tbl, miv.val.g = miv.val.g, miv.val.b = miv.val.b, miv = miv.val, 
					    m.chiq.gb = m.chiq.gb, m.chiq.stat = m.chiq.stat, p.val = p.val)
	res <- data.frame(rf.miv = rf.new, 
				miv = miv.val, 
				m.chiq.stat = m.chiq.stat, 
				p.val = p.val)
	res$miv <- ifelse(is.nan(res$miv) | is.infinite(res$miv), 0, res$miv)
	res$m.chiq.stat <- ifelse(is.nan(res$m.chiq.stat) | is.infinite(res$m.chiq.stat), 0, res$m.chiq.stat)
	res$p.val <- ifelse(is.nan(res$miv) | is.infinite(res$miv), 1, res$p.val)
return(list(res, res.tbl))
}
replace.woe.aux <- function(x, woe.tbl) {
	woe.val <- woe.tbl$woe
	names(woe.val) <- woe.tbl$bin	
	woe.rep <- unname(woe.val[x])
return(woe.rep)
}



