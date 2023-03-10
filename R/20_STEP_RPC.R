#' Stepwise logistic regression based on risk profile concept
#'
#' \code{stepRPC} customized stepwise regression with p-value and trend check which additionally takes into account 
#' the order of supplied risk factors per group when selects a candidate for the final regression model. Trend check is performed
#' comparing observed trend between target and analyzed risk factor and trend of the estimated coefficients within the 
#' logistic regression. Note that procedure checks the column names of supplied \code{db} data frame therefore some 
#' renaming (replacement of special characters) is possible to happen. For details, please, check the help example.
#'@param start.model Formula class that represents the starting model. It can include some risk factors, but it can be
#'			   defined only with intercept (\code{y ~ 1} where \code{y} is target variable).
#'@param risk.profile Data frame with defined risk profile. It has to contain the following columns: \code{rf} and 
#'			    \code{group}. Column \code{group} defines order of groups that will be tested first as a candidate
#'			    for the regression model. Risk factors selected in each group are kept as a starting variables
#'			    for the next group testing. Column \code{rf} contains all candidate risk factors supplied for testing.
#'@param p.value Significance level of p-value of the estimated coefficients. For \code{WoE} coding this value is
#'		     is directly compared to the p-value of the estimated coefficients, while for \code{dummy} coding
#'		     multiple Wald test is employed and its value is used for comparison with selected threshold (\code{p.value}).
#'@param coding Type of risk factor coding within the model. Available options are: \code{"WoE"} and
#'		    \code{"dummy"}. If \code{"WoE"} is selected, then modalities of the risk factors are replaced
#'		    by WoE values, while for \code{"dummy"} option dummies (0/1) will be created for \code{n-1} 
#'		    modalities where \code{n} is total number of modalities of analyzed risk factor.
#'@param coding.start.model Logical (\code{TRUE} or \code{FALSE}), if the risk factors from the starting model should be WoE coded. 
#'				    It will have an impact only for WoE coding option.
#'@param check.start.model Logical (\code{TRUE} or \code{FALSE}), if risk factors from the starting model should 
#'				   checked for p-value and trend in stepwise process.
#'@param db Modeling data with risk factors and target variable. All risk factors (apart from the risk factors from the starting model) 
#'	    should be categorized and as of character type.
#'@param offset.vals This can be used to specify an a priori known component to be included in the linear predictor during fitting. 
#'		    	   This should be \code{NULL} or a numeric vector of length equal to the number of cases. Default is \code{NULL}.
#'@return The command \code{stepRPC} returns a list of four objects.\cr
#'	    The first object (\code{model}), is the final model, an object of class inheriting from \code{"glm"}.\cr
#'	    The second object (\code{steps}), is the data frame with risk factors selected at each iteration.\cr
#'	    The third object (\code{warnings}), is the data frame with warnings if any observed.
#'	    The warnings refer to the following checks: if risk factor has more than 10 modalities,
#'	    if any of the bins (groups) has less than 5% of observations and 
#'	    if there are problems with WoE calculations.\cr
#'	    The final, fourth, object \code{dev.db} returns the model development database.
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
#'#create risk factor priority groups
#'rf.all <- names(loans)[-1]
#'set.seed(591)
#'rf.pg <- data.frame(rf = rf.all, group = sample(1:3, length(rf.all), rep = TRUE))
#'head(rf.pg)
#'#bring AUC for each risk factor in order to sort them within groups
#'bva <- bivariate(db = loans, target = "Creditability")[[1]]
#'rf.auc <- unique(bva[, c("rf", "auc")])
#'rf.pg <- merge(rf.pg, rf.auc, by = "rf", all.x = TRUE)
#'#prioritized risk factors
#'rf.pg <- rf.pg[order(rf.pg$group, rf.pg$auc), ]
#'rf.pg <- rf.pg[order(rf.pg$group), ]
#'rf.pg
#'res <- stepRPC(start.model = Creditability ~ 1, 
#'		   risk.profile = rf.pg, 
#'		   p.value = 0.05, 
#'		   coding = "WoE",
#'		   db = loans)
#'summary(res$model)$coefficients
#'res$steps
#'head(res$dev.db)
#'@import monobin
#'@importFrom stats formula coef vcov
#'@export
stepRPC <- function(start.model, risk.profile, p.value = 0.05, coding = "WoE", coding.start.model = TRUE, 
			  check.start.model = TRUE, db, offset.vals = NULL) {
	#check arguments
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	if	(!all(c("rf", "group")%in%names(risk.profile))) {
		stop("risk.profile data frame has to contain columns: rf and group.")
		}
	if	(!all(risk.profile$rf%in%names(db))) {
		rp.rf.miss <- risk.profile$rf[!risk.profile$rf%in%names(db)]
		msg <- "Following risk factors from risk.profile are missing in supplied db: "
		msg <- paste0(msg, paste0(rp.rf.miss, collapse = ", "), ".")
		stop(msg)
		}
	if	(any(is.na(risk.profile$group))) {
		stop("Missing value(s) in risk.profile group.")
		}
	coding.opt <- c("WoE", "dummy")
	if	(!coding%in%coding.opt) {
		stop(paste0("coding argument has to be one of: ", paste0(coding.opt, collapse = ', '), "."))
		}
	if	((!is.numeric(p.value) | !length(p.value) == 1) |
		 !(p.value[1] > 0 & p.value[1] < 1)) {
		stop("p.value has to be of single numeric value vector greater than 0 and less then 1.")
		}
	if	(!is.logical(coding.start.model)) {
		stop("coding.start.model has to be logical (TRUE or FALSE).")
		}
	if	(!is.logical(check.start.model)) {
		stop("check.start.model has to be logical (TRUE or FALSE).")
		}
	if	(check.start.model) {coding.start.model <- TRUE}
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
	rf.rest <- unique(risk.profile$rf)
	#check supplied risk factors
	rf.restl <- length(rf.rest)
	if	(rf.restl == 0) {
		stop("Risk factors are missing. Check risk.profile argument.")
		}
	#correct names
	names.c <- check.names(x = names(db))
	names(db) <- unname(names.c[names(db)])
	target <- unname(names.c[target])
	if	(!is.null(rf.start)) {rf.start <- unname(names.c[rf.start])}
	rf.rest <- unname(names.c[rf.rest])
	risk.profile$rf <- unname(names.c[risk.profile$rf])
	#check coding, start model and rf.start types
	if	(check.start.model & coding%in%"dummy" & !is.null(rf.start)) {
		num.type.start <- sapply(db[, rf.start, drop = FALSE], is.numeric)
		if	(any(num.type.start)) {check.start.model <- FALSE}
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
	rf.a <- c(rf.start, rf.rest)
	rf.al <- length(rf.a)
	rf.woe.o <- vector("list", rf.al) 
	for	(i in 1:rf.al) {
		rf.a.l <- rf.a[i]
	 	woe.o <- woe.tbl(tbl = db, x = rf.a.l, y = target, y.check = FALSE)
		woe.o$bin <- as.character(woe.o$bin)
		woe.o <- cbind.data.frame(rf = rf.a.l, woe.o)
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
	#check coding and starting rf
	if	(coding.start.model) {
		if	(coding%in%"WoE" & length(rf.start) > 0) {
			for	(i in 1:length(rf.start)) {
				rf.start.l <- rf.start[i]
				woe.rep <- replace.woe(db = db[, c(target, rf.start.l)], target = target)
				woe.rep.check <- woe.rep[[2]]
				if	(nrow(woe.rep.check) > 0) {
					msg <- "Problem with the WoE calculations for the starting model. 
			  		Check the variable class and the following risk factors for NA or Inf values: "
					msg <- paste0(msg, paste(woe.rep.check$rf, collapse = ", "), ".")
					stop(msg)
					}
				db[, rf.start.l] <- woe.rep[[1]][, rf.start.l]
				}
			}
		}
	if	(coding%in%"WoE") {	
		woe.o <- rf.woe.o[rf.woe.o$rf%in%rf.rest, ]
		for	(i in 1:length(rf.rest)) {
			rf.rest.l <- rf.rest[i]
			woe.o.l <- woe.o[woe.o$rf%in%rf.rest.l, ]
			db[, rf.rest.l] <- replace.woe.aux(x = db[, rf.rest.l], woe.tbl = woe.o.l)
			}
		} else {
		rf.woe.o$mf <- paste0(rf.woe.o$rf, rf.woe.o$bin)
		}
	#summarize rf groups for priority estimations
	if	(!is.null(offset.vals)) {
		db <- cbind.data.frame(db, offset.vals = offset.vals)
		}
	rf.mod <- NULL
	pg <- unique(risk.profile$group)
	pgl <- length(pg)	
	#run stepwise per group
	steps <- vector("list", pgl)
	for	(i in 1:pgl) {
		pg.l <- group.summary(db = db, 
					    target = target, 
					    rp.tbl = risk.profile, 
					    g = pg[i], 
					    rf.mod = rf.mod, 
					    rf.start = rf.start, 
					    rf.rest = rf.rest,
					    p.value = p.value,
					    coding = coding,
					    coding.start.model = coding.start.model, 
					    check.start.model = check.start.model,
					    rf.woe.o = rf.woe.o,
					    offset.vals = offset.vals)
		steps[[i]] <- pg.l[["steps"]]
		rf.mod <-  pg.l[["rf.mod"]]
		}
	steps <- bind_rows(steps)
	if	(length(rf.mod) == 0) {rf.mod <- "1"}
	frm.f <- paste0(target, " ~ ", paste0(c(rf.start, rf.mod), collapse = " + "))
	if	(is.null(offset.vals)) {
		lr.mod <- glm(formula = as.formula(frm.f), family = "binomial", data = db)
		} else {
		lr.mod <- glm(formula = as.formula(frm.f), family = "binomial", data = db, offset = offset.vals)
		}
	res <- list(model = lr.mod, 
			steps = steps, 
			warnings = if (nrow(warn.tbl) > 0) {warn.tbl} else {data.frame(comment = "There are no warnings.")}, 
			dev.db = db)
return(res)
}

#group summary
group.summary <- function(db, target, rp.tbl, g, rf.mod, rf.start, rf.rest, p.value, coding, 
				  coding.start.model, check.start.model, rf.woe.o, offset.vals) {
	rf.g <- rp.tbl$rf[rp.tbl$group%in%g]
	rf.g <- rf.g[rf.g%in%rf.rest]	
	rf.mod <- c(rf.mod, rf.start)
	steps <- data.frame()	
	if	(length(rf.g) == 0) {
		return(list(rf.mod = rf.mod, steps = steps))
		}
	#initialte rf table for stepwise
	tbl.c <- data.frame(rf = rf.g, checked = FALSE)
	iter <- 1
	repeat	{
		message(paste0("Running iteration: ", iter, " for group: ", g))
		it.s <- iter.summary(target = target, 
					   rf.mod = rf.mod, 
					   rf.start = rf.start, 
					   check.start.model = check.start.model,
					   tbl.c = tbl.c, 
					   p.value = p.value, 
					   rf.woe.o = rf.woe.o, 
					   coding = coding, 
					   db = db,
					   offset.vals = offset.vals)
		step.i <- find.next(it.s = it.s, tbl.c = tbl.c)
		tbl.c <- step.i[["tbl.c"]]
		rf.mod <- c(rf.mod, step.i[["rf.next"]])
		steps <- bind_rows(steps, step.i[["step"]])
		if	(nrow(tbl.c) == 0 | all(tbl.c$checked)) {
			break
			}
		iter <- iter + 1
		}
	if	(nrow(steps) > 0) {
		steps <- cbind.data.frame(group = g, steps)
		}
return(list(rf.mod = rf.mod, steps = steps))	
}

