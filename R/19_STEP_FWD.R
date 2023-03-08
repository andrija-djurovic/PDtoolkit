#' Customized stepwise regression with p-value and trend check
#'
#' \code{stepFWD} customized stepwise regression with p-value and trend check. Trend check is performed
#' comparing observed trend between target and analyzed risk factor and trend of the estimated coefficients within the 
#' logistic regression. Note that procedure checks the column names of supplied \code{db} data frame therefore some 
#' renaming (replacement of special characters) is possible to happen. For details check help example.
#'@param start.model Formula class that represents starting model. It can include some risk factors, but it can be
#'			   defined only with intercept (\code{y ~ 1} where \code{y} is target variable).
#'@param p.value Significance level of p-value of the estimated coefficients. For \code{WoE} coding this value is
#'		     is directly compared to the p-value of the estimated coefficients, while for \code{dummy} coding
#'		     multiple Wald test is employed and its p-value is used for comparison with selected threshold (\code{p.value}).
#'@param coding Type of risk factor coding within the model. Available options are: \code{"WoE"} (default) and
#'		    \code{"dummy"}. If \code{"WoE"} is selected, then modalities of the risk factors are replaced
#'		    by WoE values, while for \code{"dummy"} option dummies (0/1) will be created for \code{n-1} 
#'		    modalities where \code{n} is total number of modalities of analyzed risk factor.
#'@param coding.start.model Logical (\code{TRUE} or \code{FALSE}), if the risk factors from the starting model should be WoE coded. 
#'				    It will have an impact only for WoE coding option. Default is \code{TRUE}.
#'@param check.start.model Logical (\code{TRUE} or \code{FALSE}), if risk factors from the starting model should be 
#'				   checked for p-value and trend in stepwise process. Default is \code{TRUE}. 
#'				   If \code{FALSE} is selected, then \code{coding.start.model} is forced to \code{TRUE}.
#'@param db Modeling data with risk factors and target variable. All risk factors (apart from the risk factors from the starting model) 
#'	    should be categorized and as of character type.
#'@param offset.vals This can be used to specify an a priori known component to be included in the linear predictor during fitting. 
#'		    	   This should be \code{NULL} or a numeric vector of length equal to the number of cases. Default is \code{NULL}.
#'@return The command \code{stepFWD} returns a list of four objects.\cr
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
#'res <- stepFWD(start.model = Creditability ~ 1, 
#'		   p.value = 0.05, 
#'		   coding = "dummy",
#'		   db = loans)
#'summary(res$model)$coefficients
#'rf.check <- tapply(res$dev.db$Creditability, 
#'			 res$dev.db$Value_Savings_Stocks, 
#'			 mean)
#'rf.check
#'diff(rf.check)
#'res$steps
#'head(res$dev.db)
#'@import monobin
#'@importFrom stats formula coef vcov
#'@export
stepFWD <- function(start.model, p.value = 0.05, coding = "WoE", coding.start.model = TRUE, check.start.model = TRUE, 
			  db, offset.vals = NULL) {
	#check arguments
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
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
	rf.rest <- names(db)[!names(db)%in%c(c(target, rf.start))]
	#check supplied risk factors
	rf.restl <- length(rf.rest)
	if	(rf.restl == 0) {
		stop("Risk factors are missing. Check db argument.")
		}
	#correct names
	names.c <- check.names(x = names(db))
	names(db) <- unname(names.c[names(db)])
	target <- unname(names.c[target])
	if	(!is.null(rf.start)) {rf.start <- unname(names.c[rf.start])}
	rf.rest <- unname(names.c[rf.rest])
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
	#initialte rf table for stepwise
	if	(!is.null(offset.vals)) {
		db <- cbind.data.frame(db, offset.vals = offset.vals)
		}
	rf.mod <- NULL
	tbl.c <- data.frame(rf = rf.rest, checked = FALSE)
	steps <- data.frame()
	iter <- 1
	repeat	{
		message(paste0("Running iteration: ", iter))
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

#iteration summary
iter.summary <- function(target, rf.mod, rf.start, check.start.model, tbl.c, p.value, rf.woe.o, coding, 
				 db, offset.vals) {
	rf.mod <- c(rf.start, rf.mod) 
	frm.start <- paste0(target, " ~ ", ifelse(!is.null(rf.mod), 
								paste0(rf.mod, collapse = " + "), "1"))
	rfl <- nrow(tbl.c)
	res <- data.frame(matrix(nrow = rfl, ncol = 5))
	names(res) <- c("rf", "aic", "p.val", "p.val.check", "trend.check")
	for	(i in 1:rfl) {
		rf.l <- tbl.c[i, "rf"]
		res$rf[i] <- rf.l
		frm.check <- paste0(frm.start, " + ", rf.l)
		ms <- gms(formula = as.formula(frm.check), db = db, offset.vals = offset.vals) 
		lr.mod <- ms[["coef.tbl"]]
		res$aic[i] <- ms[["aic"]]
		if	(coding%in%"WoE") { 
			res$p.val[i] <- lr.mod$"Pr...z.."[nrow(lr.mod)]
			if	(any(is.na(coef(ms[["model"]])))) {
				res$p.val.check[i] <- FALSE
				res$trend.check[i] <- FALSE
				} else {
				if	(check.start.model) {
					res$p.val.check[i] <- all(lr.mod$"Pr...z.."[!lr.mod$rf%in%"(Intercept)"] < p.value)
					res$trend.check[i] <- all(lr.mod$Estimate[!lr.mod$rf%in%"(Intercept)"] < 0)
					} else {
					res$p.val.check[i] <- all(lr.mod$"Pr...z.."[!lr.mod$rf%in%c("(Intercept)", rf.start)] < p.value)
					res$trend.check[i] <- all(lr.mod$Estimate[!lr.mod$rf%in%c("(Intercept)", rf.start)] < 0)
					}
				}
			} else {
			checks <- ptc.dummy(model = ms[["model"]], 
						  lr.mod = lr.mod, 
						  rf.mod = rf.mod, 
						  rf.start = rf.start, 
						  check.start.model = check.start.model, 
						  rf.l = rf.l, 
						  rf.woe.o = rf.woe.o, 
						  p.value = p.value) 
			res$p.val[i] <- checks[["p.val"]]
			res[i, c("p.val.check", "trend.check")] <- unname(checks[["check.results"]])
			}
		}
return(res)
}	
#find next risk factor
find.next <- function(it.s, tbl.c) {
	rf.nc <- it.s[it.s$p.val.check & it.s$trend.check, ]
	if	(nrow(rf.nc) == 0) {
		tbl.c$checked <- TRUE
		rf.next <- NULL
		rf.next.all <- data.frame()
		} else {
		if	(length(unique(rf.nc$p.val)) == 1) {
			rf.next.all <- rf.nc[which.min(rf.nc$aic), ][1, ]
			} else {
			rf.next.all <- rf.nc[which.min(rf.nc$p.val), ][1, ]
			}
		rf.next <- rf.next.all$rf
		tbl.c <- tbl.c[!tbl.c$rf%in%rf.next, ]
		}
return(list(rf.next = rf.next, step = rf.next.all, tbl.c = tbl.c))
}	
#get model summary
gms <- function(formula, db, offset.vals) {
	mod.est <- glm(formula = formula, family = "binomial", data = db, offset = offset.vals)
	coef.tbl <- data.frame(summary(mod.est)$coefficients)
	coef.tbl <- cbind.data.frame(rf = rownames(coef.tbl), coef.tbl)
	row.names(coef.tbl) <- NULL
return(list(coef.tbl = coef.tbl, aic = mod.est$aic, model = mod.est))
}
#p-value and trend check for dummy coding
ptc.dummy <- function(model, lr.mod, rf.mod, rf.start, check.start.model, rf.l, rf.woe.o, p.value) {
	rf.mod <- c(rf.mod, rf.l)
	if	(!check.start.model) {
		rf.mod <- rf.mod[!rf.mod%in%rf.start]
		}
	rf.w <- rf.woe.o[rf.woe.o$rf%in%rf.mod, c("rf", "mf", "dr")]
	rf.est <- lr.mod[lr.mod$rf%in%rf.w$mf, c("rf", "Estimate", "Pr...z..")]
	rf.s <- merge(rf.w, rf.est, by.x = "mf", by.y = "rf",  all.x = TRUE)
	tc <- rf.s %>% 
		group_by(rf) %>%
		summarise(cc = cc.dummy(dr, Estimate), .groups = "drop")
	pcl <- length(rf.mod)
	pc <- rep(NA, pcl)
	for	(i in 1:pcl) {
		pc.l <- rf.mod[i]
		coefs <- rf.w$mf[rf.w$rf%in%pc.l]
		pc[[i]] <- wald.test(model = model, coefs = coefs)
		}
	#pc.res <- cbind.data.frame(rf = rf.mod, p.value = pc)
	c.res <- c(p.val.check = all(pc < p.value), 
		     trend.check = all(tc$cc))
return(list(p.val = pc[pcl], check.results = c.res))
}
cc.dummy <- function(dr, Estimate) {
	cc.cases <- complete.cases(dr, Estimate)
	if	(length(dr[is.na(Estimate)]) > 1) {return(FALSE)}
	ref.dir <- dr - dr[is.na(Estimate)]
	check.1 <- all(sign(ref.dir[cc.cases]) == sign(Estimate[cc.cases]))
	est <- ifelse(is.na(Estimate), 0, Estimate)
	check.2 <- all(sign(diff(dr)) == sign(diff(est)))
	if	(sum(cc.cases) > 1) {
		cc <- cor(Estimate, dr - dr[is.na(Estimate)], use = "complete.obs", method = "spearman")
		check.3 <- ifelse(round(cc, 5) == 1, TRUE, FALSE)
		} else {
		check.3 <- TRUE
		}
	cc <- check.1 & check.2 & check.3
return(cc)
}
wald.test <- function(model, coefs) {
	vcm <- vcov(model)
	coef.est <- coef(model)
	terms <- which(names(coef.est)%in%coefs)
	if	(any(is.na(coef.est[terms]))) {return(1)}
	w <- length(terms)
	h0 <- rep(0, w)
	l <- matrix(rep(0, length(coef.est) * w), ncol = length(coef.est))
	for	(i in 1:w) {l[i, terms[i]] <- 1}
	f <- l%*%coef.est
	stat <- t(f - h0)%*%solve(l%*%vcm%*%t(l))%*%(f - h0)
	p.val <- 1 - pchisq(c(stat), df = w)
return(p.val)
}
#check db names
check.names <- function(x = names(db)) {
	x.c <- gsub("[^[:alnum:][[^\\.]|[^\\_]]", " ", x)
	x.c <- trimws(x.c)
	x.c <- gsub(" ", "_", x.c)
	names(x.c) <- x
return(x.c)
}

