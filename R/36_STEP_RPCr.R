#' Stepwise regression based on risk profile concept and raw risk factors
#'
#' \code{stepRPCr} customized stepwise regression with p-value and trend check on raw risk factors which additionally takes into account 
#' the order of supplied risk factors per group when selects a candidate for the final regression model. Trend check is performed
#' comparing observed trend between target and analyzed risk factor and trend of the estimated coefficients. 
#' Note that procedure checks the column names of supplied \code{db} data frame therefore some 
#' renaming (replacement of special characters) is possible to happen. For details, please, check the help example.
#'@param start.model Formula class that represents the starting model. It can include some risk factors, but it can be
#'			   defined only with intercept (\code{y ~ 1} where \code{y} is target variable).
#'@param risk.profile Data frame with defined risk profile. It has to contain the following columns: \code{rf} and 
#'			    \code{group}. Column \code{group} defines order of groups that will be tested first as a candidate
#'			    for the regression model. Risk factors selected in each group are kept as a starting variables
#'			    for the next group testing. Column \code{rf} contains all candidate risk factors supplied for testing.
#'@param p.value Significance level of p-value of the estimated coefficients. For numerical risk factors this value is
#'		     is directly compared to the p-value of the estimated coefficients, while for categorical risk factors
#'		     multiple Wald test is employed and its value is used for comparison with selected threshold (\code{p.value}).
#'@param db Modeling data with risk factors and target variable. All risk factors (apart from the risk factors from the starting model) 
#'	    should be categorized and as of character type.
#'@param check.start.model Logical (\code{TRUE} or \code{FALSE}), if risk factors from the starting model should 
#'				   checked for p-value and trend in stepwise process.
#'@param offset.vals This can be used to specify an a priori known component to be included in the linear predictor during fitting. 
#'		    	   This should be \code{NULL} or a numeric vector of length equal to the number of cases. Default is \code{NULL}.
#'@return The command \code{stepRPCr} returns a list of four objects.\cr
#'	    The first object (\code{model}), is the final model, an object of class inheriting from \code{"glm"}.\cr
#'	    The second object (\code{steps}), is the data frame with risk factors selected at each iteration.\cr
#'	    The third object (\code{warnings}), is the data frame with warnings if any observed.
#'	    The warnings refer to the following checks: if risk factor has more than 10 modalities or
#'	    if any of the bins (groups) has less than 5% of observations.\cr
#'	    The final, fourth, object \code{dev.db} returns the model development database.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#create risk factor priority groups
#'rf.all <- names(loans)[-1]
#'set.seed(6422)
#'rf.pg <- data.frame(rf = rf.all, group = sample(1:3, length(rf.all), rep = TRUE))
#'rf.pg <- rf.pg[order(rf.pg$group), ]
#'head(rf.pg)
#'res <- stepRPCr(start.model = Creditability ~ 1, 
#'                risk.profile = rf.pg, 
#'                p.value = 0.05, 
#'                db = loans)
#'summary(res$model)$coefficients
#'res$steps
#'head(res$dev.db)
#'@importFrom stats formula coef vcov
#'@export
stepRPCr <- function(start.model, risk.profile, p.value = 0.05, db, check.start.model = TRUE, offset.vals = NULL) {
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
	if	((!is.numeric(p.value) | !length(p.value) == 1) |
		 !(p.value[1] > 0 & p.value[1] < 1)) {
		stop("p.value has to be of single numeric value vector greater than 0 and less then 1.")
		}
	if	(!is.logical(check.start.model)) {
		stop("check.start.model has to be logical (TRUE or FALSE).")
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
	#define warning table
	warn.tbl <- data.frame()
	#check num of modalities per risk factor
	num.type <- sapply(db[, rf.rest, drop = FALSE], is.numeric)
	num.rf <- names(num.type)[num.type]
	unique.mod <- sapply(db[, rf.rest[!rf.rest%in%num.rf], drop = FALSE], function(x) length(unique(x)))
	check.mod <- names(unique.mod)[unique.mod > 10]
	if	(length(check.mod) > 0) {
		warn.rep <- data.frame(rf = check.mod, comment = "More than 10 modalities.")
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		}
	#check for missing values in categorical risk factors
	cat.check <- sapply(db[, rf.rest[!rf.rest%in%num.rf], drop = FALSE], 
				  function(x) sum(is.na(x)))
	check.miss.cat <- names(cat.check)[cat.check > 0]
	if	(length(check.miss.cat) > 0) {
		warn.rep <- data.frame(rf = cat.check, comment = "Contains missing values.")
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		}	
	#check for missing values in numeric risk factors
	num.check <- sapply(db[, rf.rest[rf.rest%in%num.rf], drop = FALSE], 
				  function(x) sum(x%in%c(NA, NaN, Inf, -Inf)))
	check.miss.num <- names(num.check)[num.check > 0]
	if	(length(check.miss.num) > 0) {
		warn.rep <- data.frame(rf = check.miss.num, comment = "Contains special cases (NA, NaN, Inf, -Inf),")
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		}		
	#generate summary table for categorical risk factors
	rf.a <- c(rf.start, rf.rest)
	rf.n <- sapply(db[, rf.a, drop = FALSE], is.numeric)
	rf.c <- rf.a[!rf.a%in%names(rf.n)[rf.n]]
	rf.cl <- length(rf.c)
	rf.cat.o <- vector("list", rf.cl) 
	if	(length(rf.c) > 0) {
		for	(i in 1:rf.cl) {
			rf.c.l <- rf.c[i]
	 		cat.o <- summary.tbl(tbl = db, x = rf.c.l, y = target)
			cat.o$bin <- as.character(cat.o$bin)
			cat.o <- cbind.data.frame(rf = rf.c.l, cat.o)
			pct.check <- any(cat.o$pct.o < 0.05)
			cat.o$pct.check <- pct.check
			rf.cat.o[[i]] <- cat.o
			}
		rf.cat.o <- bind_rows(rf.cat.o)
		} else {
		rf.cat.o <- data.frame()
		}
	#observed correlation for numerical risk factor
	rf.n <- names(rf.n)[rf.n]
	rf.nl <- length(rf.n)
	if	(rf.nl > 0) {
		rf.num.o <- vector("list", rf.nl)
		for	(i in 1:rf.nl) {
			rf.n.l <- rf.n[i]
			cor.obs <- cor(x = db[!db[, rf.n.l]%in%c(NA, NaN, Inf, -Inf), rf.n.l],
					   y = db[!db[, rf.n.l]%in%c(NA, NaN, Inf, -Inf), target],
					   method = "spearman")
			num.o <- data.frame(rf = rf.n.l, cor = cor.obs)
			rf.num.o[[i]] <- num.o
			}
	
		} else {
		rf.num.o <- data.frame()
		}
	rf.num.o <- bind_rows(rf.num.o)
	#check pct of obs per bin
	check.pct <- unique(rf.cat.o$rf[rf.cat.o$pct.check]) 
	if	(length(check.pct) > 0) {
		warn.rep <- data.frame(rf = check.pct, comment = "At least one pct per bin less than 5%.")
		warn.tbl <- bind_rows(warn.tbl, warn.rep)
		}
	if	(nrow(rf.cat.o) > 0) {
		rf.cat.o$mf <- paste0(rf.cat.o$rf, rf.cat.o$bin)
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
		pg.l <- group.summary.r(db = db, 
					      target = target, 
					      rp.tbl = risk.profile, 
					      g = pg[i], 
					      rf.mod = rf.mod, 
					      rf.start = rf.start, 
					      rf.rest = rf.rest,
					      p.value = p.value,
					      rf.cat.o = rf.cat.o, 
					      rf.num.o = rf.num.o, 
					      check.start.model = check.start.model,
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
group.summary.r <- function(db, target, rp.tbl, g, rf.mod, rf.start, rf.rest, p.value,  rf.cat.o,  rf.num.o, 
				  check.start.model, offset.vals) {
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
		it.s <- iter.summary.r(target = target, 
					     rf.mod = rf.mod, 
					     rf.start = rf.start, 
					     check.start.model = check.start.model,
					     tbl.c = tbl.c, 
					     p.value = p.value, 
					     rf.cat.o = rf.cat.o, 
					     rf.num.o = rf.num.o, 
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

