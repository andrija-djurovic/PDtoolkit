#' Customized stepwise regression with p-value and trend check on raw risk factors
#'
#' \code{stepFWDr} customized stepwise regression with p-value and trend check on raw risk factors. Trend check is performed
#' comparing observed trend between target and analyzed risk factor and trend of the estimated coefficients within the 
#' binomial logistic regression. Difference between \code{stepFWDr} and \code{\link{stepFWD}} is that this function
#' run stepwise regression on mixed risk factor types (numerical and/or categorical), while  \code{\link{stepFWD}} accepts only categorical risk factors.
#'Note that procedure checks the column names of supplied \code{db} data frame therefore some 
#' renaming (replacement of special characters) is possible to happen. For details check help example.
#'@param start.model Formula class that represents starting model. It can include some risk factors, but it can be
#'			   defined only with intercept (\code{y ~ 1} where \code{y} is target variable).
#'@param p.value Significance level of p-value of the estimated coefficients. For numerical risk factors this value is
#'		     is directly compared to the p-value of the estimated coefficients, while for categorical risk factors
#'		     multiple Wald test is employed and its p-value is used for comparison with selected threshold (\code{p.value}).
#'@param db Modeling data with risk factors and target variable. Risk factors can be categorized or continuous. 
#'@param check.start.model Logical (\code{TRUE} or \code{FALSE}), if risk factors from the starting model should be 
#'				   checked for p-value and trend in stepwise process. Default is \code{TRUE}. 
#'@param offset.vals This can be used to specify an a priori known component to be included in the linear predictor during fitting. 
#'		    	   This should be \code{NULL} or a numeric vector of length equal to the number of cases. Default is \code{NULL}.
#'@return The command \code{stepFWDr} returns a list of four objects.\cr
#'	    The first object (\code{model}), is the final model, an object of class inheriting from \code{"glm"}.\cr
#'	    The second object (\code{steps}), is the data frame with risk factors selected at each iteration.\cr
#'	    The third object (\code{warnings}), is the data frame with warnings if any observed.
#'	    The warnings refer to the following checks: if any categorical risk factor has more than 10 modalities or
#'	    if any of the bins (groups) has less than 5% of observations.\cr
#'	    The final, fourth, object \code{dev.db} returns the model development database.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'trf <- c("Creditability", "Account Balance", "Duration of Credit (month)",
#'         "Age (years)", "Guarantors", "Concurrent Credits")
#'res <- stepFWDr(start.model = Creditability ~ 1, 
#'                p.value = 0.05, 
#'	            db = loans[, trf],
#'                check.start.model = TRUE, 
#'                offset.vals = NULL)
#'summary(res$model)$coefficients
#'rf.check <- tapply(res$dev.db$Creditability, 
#'			 res$dev.db$Guarantors, 
#'			 mean)
#'rf.check
#'diff(rf.check)
#'res$steps
#'head(res$dev.db)
#'@importFrom stats formula coef vcov
#'@export
stepFWDr <- function(start.model, p.value = 0.05, db, check.start.model = TRUE, offset.vals = NULL) {
	#check arguments
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	if	((!is.numeric(p.value) | !length(p.value) == 1) |
		 !(p.value[1] > 0 & p.value[1] < 1)) {
		stop("p.value has to be of single numeric value vector greater than 0 and less then 1.")
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
		it.s <- iter.summary.r(target = target, 
					     rf.mod = rf.mod, 
					     rf.start = rf.start, 
					     tbl.c = tbl.c, 
					     p.value = p.value, 
					     rf.cat.o = rf.cat.o, 
					     rf.num.o = rf.num.o, 
					     db = db,
					     offset.vals = offset.vals,
					     check.start.model = check.start.model)
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
iter.summary.r <- function(target, rf.mod, rf.start, tbl.c, p.value, rf.cat.o, rf.num.o, 
				 db, offset.vals, check.start.model) {
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
		if	(any(is.na(coef(ms[["model"]])))) {
			res$p.val.check[i] <- FALSE
			res$trend.check[i] <- FALSE
			} else {
			rf.type <- is.numeric(db[, rf.l])
			checks <- ptc(model = ms[["model"]], 
					  lr.mod = lr.mod, 
					  rf.mod = rf.mod, 
					  rf.start = rf.start, 
					  check.start.model = check.start.model, 
					  rf.l = rf.l, 
					  rf.type = ifelse(rf.type, "numeric", "categorical"), 
					  rf.cat.o = rf.cat.o, 
					  rf.num.o = rf.num.o, 
					  p.value = p.value) 
			res$p.val[i] <- checks[["p.val"]]
			res[i, c("p.val.check", "trend.check")] <- unname(checks[["check.results"]])
			}
		}
return(res)
}	

#p-value and trend checks
ptc <- function(model, lr.mod, rf.mod, rf.start, check.start.model, rf.l, rf.type, rf.cat.o, 
		    rf.num.o, p.value) {
	rf.mod <- c(rf.mod, rf.l)
	if	(!check.start.model) {
		rf.mod <- rf.mod[!rf.mod%in%rf.start]
		}
	#categorical risk factors
	if	(nrow(rf.cat.o) > 0) {
		rf.cat <- rf.cat.o[rf.cat.o$rf%in%rf.mod, c("rf", "mf", "avg")]
		if	(nrow(rf.cat) > 0) {
			rf.est <- lr.mod[lr.mod$rf%in%rf.cat$mf, c("rf", "Estimate", "Pr...z..")]
			rf.cat <- merge(rf.cat, rf.est, by.x = "mf", by.y = "rf",  all.x = TRUE)
			tc.cat <- rf.cat %>% 
				    group_by(rf) %>%
				    summarise(cc = cc.cat(avg = avg, Estimate = Estimate), .groups = "drop")
			pc.cat.u <- unique(rf.cat$rf)
			pc.catl <- length(unique(pc.cat.u))
			pc.cat <- rep(NA, pc.catl)
			for	(i in 1:pc.catl) {
				pc.cat.l <- pc.cat.u[i]
				coefs <- rf.cat$mf[rf.cat$rf%in%pc.cat.l]
				pc.cat[i] <- wald.test(model = model, coefs = coefs)
				}
			} else {
			tc.cat <- data.frame(cc = TRUE)
			pc.cat <- -1
			}
		} else {
		tc.cat <- data.frame(cc = TRUE)
		pc.cat <- -1
		}	
	#numerical risk factors
	if	(nrow(rf.num.o) > 0) {
		rf.num <- rf.num.o[rf.num.o$rf%in%rf.mod, ]
		if	(nrow(rf.num) > 0) {
			rf.est <- lr.mod[lr.mod$rf%in%rf.num$rf, c("rf", "Estimate", "Pr...z..")]
			rf.num <- merge(rf.num, rf.est, by = "rf",  all.x = TRUE)
			tc.num <- data.frame(cc = all(sign(rf.num$cor) == sign(rf.num$Estimate)))
			pc.num <- rf.num$"Pr...z.."
			} else {
			tc.num <- data.frame(cc = TRUE)
			pc.num <- -1
			}
		} else {
		tc.num <- data.frame(cc = TRUE)
		pc.num <- -1
		}
	c.res <- c(p.val.check = all(c(pc.cat, pc.num) < p.value), 
		     trend.check = all(c(tc.cat$cc, tc.num$cc)))
return(list(p.val = ifelse(rf.type%in%"categorical", pc.cat[length(pc.cat)], pc.num[length(pc.num)]), 
		check.results = c.res))
}
cc.cat <- function(avg, Estimate) {
	cc.cases <- complete.cases(avg, Estimate)
	if	(length(avg[is.na(Estimate)]) > 1) {return(FALSE)} 
	ref.dir <- avg - avg[is.na(Estimate)]
	check.1 <- all(sign(ref.dir[cc.cases]) == sign(Estimate[cc.cases]))
	if	(sum(cc.cases) > 1) {
		cc <- cor(Estimate, avg - avg[is.na(Estimate)], use = "complete.obs", method = "spearman")
		check.2 <- ifelse(round(cc, 5) == 1, TRUE, FALSE)
		} else {
		check.2 <- TRUE
		}
	cc <- check.1 & check.2
return(cc)
}

#summary table 
summary.tbl <- function(tbl, x, y) {
	if	(!is.data.frame(tbl)) {
		stop("tbl is not a data frame.")
		}
	if	(!y%in%names(tbl)) {
		stop("y (target variable) does not exist in supplied tbl.")
		}
	if	(!x%in%names(tbl)) {
		stop("x (risk factor) does not exist in supplied tbl.")
		}
	res <- tbl %>% 
		 group_by_at(c("bin" = x)) %>%
		 summarise(no = n(),
			     avg = mean(!!sym(y), na.rm = TRUE)) %>%
		 ungroup() %>%
		 mutate(pct.o = no / sum(no, na.rm = TRUE))
return(data.frame(res))
}



