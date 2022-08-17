#' Model fairness validation
#'
#' \code{fairness.vld} performs fairness validation for a given sensitive attribute and selected outcome.
#' Sensitive attribute should be categorical variable with reasonable number of modalities, while
#' outcome can be categorical (e.g. reject/accept indicator or rating grade) or continuous (e.g. interest rate or amount).
#' Depending on model type outcome (see argument \code{mod.outcome.type}) Chi-square test or Wald test is applied.
#'@param db Data frame with sensitive attribute, observed outcome, model outcome and conditional attribute.
#'@param sensitive Name of sensitive attribute within \code{db}.
#'@param obs.outcome Name of observed outcome within \code{db}.
#'@param mod.outcome Name of model outcome within \code{db}.
#'@param conditional Name of conditional attribute within \code{db}. It is used for calculation of conditional
#'			   statistical parity. Default value is \code{NULL}.
#'@param mod.outcome.type Type of model outcome. Possible values are \code{disc} (discrete outcome) 
#'				   and \code{cont} (continuous).
#'@param p.value Significance level of applied statistical test (chi-square or Wald test).
#'@return The command \code{fairness.vld} returns a list of three data frames.\cr
#'	    The first object (\code{SP}), provides results of statistical parity testing.\cr
#'	    The second object (\code{CSP}), provides results of conditional statistical parity testing.
#'	    This object will be returned only if conditional attributed is supplied. \cr
#'	    The third object (\code{SP}), provides results of equal opportunity testing.\cr
#'@references
#'Hurlin, Christophe and Perignon, Christophe and Saurin, Sebastien (2022), 
#'The Fairness of Credit Scoring Models. HEC Paris Research Paper No. FIN-2021-1411
#'@examples
#'suppressMessages(library(PDtoolkit))
#'#build hypothetical model
#'data(loans)
#'#identify numeric risk factors
#'num.rf <- sapply(loans, is.numeric)
#'num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#'#discretized numeric risk factors using ndr.bin from monobin package
#'loans[, num.rf] <- sapply(num.rf, function(x) 
#'ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#'str(loans)
#'#run stepMIV
#'res <- stepMIV(start.model = Creditability ~ 1, 
#'	   miv.threshold = 0.02, 
#'	   m.ch.p.val = 0.05,
#'	   coding = "WoE",
#'	   coding.start.model = FALSE,
#'	   db = loans)
#'#print coefficients
#'summary(res$model)$coefficients
#'
#'#prepare data frame for fairness validation
#'db.fa <- data.frame(Creditability = loans$Creditability, 
#'			  mpred = predict(res$model, type = "response", newdata = res$dev.db))
#'#add hypothetical reject/accept indicator 
#'db.fa$rai <- ifelse(db.fa$mpred > 0.5, 1, 0)
#'#add hypothetical rating
#'db.fa$rating <- sts.bin(x = round(db.fa$mpred, 4), y = db.fa$Creditability)[[2]]
#'#add hypothetical interest rate
#'ir.r <- seq(0.03, 0.10, length.out = 6)
#'names(ir.r) <- sort(unique(db.fa$rating))
#'db.fa$ir <- ir.r[db.fa$rating]
#'#add hypothetical sensitive attribute
#'db.fa$sensitive.1 <- ifelse(loans$"Sex & Marital Status"%in%2, 1, 0) #not in a model
#'db.fa$sensitive.2 <- ifelse(loans$"Age (years)"%in%"03 [35,Inf)", 1, 0) #in a model
#'#add some attributes for calculation of conditional statistical parity
#'db.fa$"Credit Amount" <- loans$"Credit Amount" 
#'head(db.fa)
#'
#'#discrete model outcome - sensitive attribute not in a model
#'fairness.vld(db = db.fa, 
#'		 sensitive = "sensitive.1", 
#'		 obs.outcome = "Creditability", 
#'		 mod.outcome = "rai",
#'		 conditional = "Credit Amount", 
#'		 mod.outcome.type = "disc", 
#'		 p.value = 0.05)
#'#discrete model outcome - sensitive attribute in a model
#'fairness.vld(db = db.fa, 
#'		 sensitive = "sensitive.2", 
#'		 obs.outcome = "Creditability", 
#'		 mod.outcome = "rai",
#'		 conditional = "Credit Amount", 
#'		 mod.outcome.type = "disc", 
#'		 p.value = 0.05)
#'#continuous outcome - sensitive attribute not in a model
#'fairness.vld(db = db.fa, 
#'		 sensitive = "sensitive.1", 
#'		 obs.outcome = "Creditability", 
#'		 mod.outcome = "ir",
#'		 conditional = "Credit Amount", 
#'		 mod.outcome.type = "cont", 
#'		 p.value = 0.05)
#'#continuous outcome - sensitive attribute in a model
#'fairness.vld(db = db.fa, 
#'		 sensitive = "sensitive.2", 
#'		 obs.outcome = "Creditability", 
#'		 mod.outcome = "ir",
#'		 conditional = "Credit Amount", 
#'		 mod.outcome.type = "cont", 
#'		 p.value = 0.05)
#'@export
fairness.vld <- function(db, sensitive, obs.outcome, mod.outcome, conditional = NULL, mod.outcome.type, p.value) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	y <- db[, obs.outcome]
	cond.00 <- !sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])
	if	(cond.00) {
		stop("obs.outcome has to be 0/1 variable.")
		}
	if	(any(!c(sensitive, obs.outcome, mod.outcome, conditional)%in%names(db))) {
		stop("one of arguments sensitive, mod.outcome, obs.outcome or conditional cannot be found in the db data frame.")
		}
	mod.outcome.type.opt <- c("disc", "cont")
	if	(!mod.outcome.type%in%mod.outcome.type.opt) {
		stop(paste0("mod.outcome.type argument has to be one of: ", 
				paste0(mod.outcome.type.opt, collapse = ', '), "."))
		}
	if	((!is.numeric(p.value) | !length(p.value) == 1) |
		 !(p.value[1] > 0 & p.value[1] < 1)) {
		stop("p.value has to be of single numeric value vector greater than 0 and less then 1.")
		}
	cc <- complete.cases(db[, c(sensitive, obs.outcome, mod.outcome)])
	db <- db[cc, ]
	if	(nrow(db) == 0) {
		stop("No complete cases for db.")
		}
	if	(any(!cc)) {
		warning("There are some incomplete cases. Check sensitive, obs.outcome and mod.outcome columns.")
		}
	if	(mod.outcome.type%in%"disc")	{
		res <- disc.tests(db = db, 
				sensitive = sensitive, 
				obs.outcome = obs.outcome, 
				mod.outcome = mod.outcome, 
				conditional = conditional, 
				p.value = p.value)
		} else {
		res <- cont.tests(db = db, 
				sensitive = sensitive, 
				obs.outcome = obs.outcome, 
				mod.outcome = mod.outcome, 
				conditional = conditional, 
				p.value = p.value)
		}
return(res)
}

#discrete model outcome
disc.tests <- function(db, sensitive, obs.outcome, mod.outcome, conditional, p.value) {
	#statistical parity 
	sp <- disc.test.aux(db = db, 
			sensitive = sensitive, 
			mod.outcome = mod.outcome, 
			p.value = p.value)
	#conditional statistical parity
	csp <- NULL
	if	(!is.null(conditional)) {
		uc <- unique(conditional)
		ucl <- length(uc)
		csp <- vector("list", ucl)
		for	(i in 1:ucl) {
			conditional.l <- uc[i]
			csp.l <- disc.test.aux.c(db = db, 
						sensitive = sensitive, 
						mod.outcome = mod.outcome, 
						conditional = conditional.l, 
						p.value = p.value)
			csp.l <- cbind.data.frame(attribute = conditional.l, csp.l)
			csp[[i]] <- csp.l
			}
		csp <- bind_rows(csp)
		}
	#equal odds, equal opportunity, predictive equallity 
	eo <- disc.test.aux.c(db = db, 
			sensitive = sensitive, 
			mod.outcome = mod.outcome, 
			conditional = obs.outcome, 
			p.value = p.value)
	eo <- cbind.data.frame(metric = c("equal opportunity", "predictive equality", "equal odds"),
				     eo)	
	res <- list(SP = sp, CSP = csp, EO = eo)
return(res)
}
disc.test.aux <- function(db, sensitive, mod.outcome, p.value) {
	tbl <- table(db[, c(mod.outcome, sensitive)])
	test.res <-  chisq.test(x = tbl, correct = FALSE)
	df <- unname(test.res$parameter)
	statistic <- unname(test.res$statistic)
	p.val <- test.res$p.value
	res <- data.frame(nobs = sum(tbl), 
			statistic = statistic, 
			df = df, 
			p.val = p.val, 
			res = ifelse(p.val < p.value, "H0: independence assumption rejected.",
						      "H0: independence assumption cannot be rejected."))
return(res)
}
disc.test.aux.c <- function(db, sensitive, mod.outcome, conditional, p.value) {
	ucv <- sort(unique(db[, conditional]))
	ucvl <- length(ucv)
	res <- vector("list", ucvl)
	for	(i in 1:ucvl) {
		uc.l <- ucv[i]
		res.l <- disc.test.aux(db = db[db[, conditional]%in%uc.l, ], 
					sensitive = sensitive, 
					mod.outcome = mod.outcome, 
					p.value = p.value)
		res[[i]] <- cbind.data.frame(modality = as.character(uc.l), res.l)
		}
	res <- bind_rows(res)
	#overall test
	df.o <- sum(res$df)
	statistic.o <- sum(res$statistic)
	p.val.o <- pchisq(statistic.o, df.o, lower.tail = FALSE)
	res.o <- data.frame(modality = "overall",
			nobs = sum(res$nobs),
			statistic = statistic.o,
			df = df.o,
			p.val = p.val.o, 
			res = ifelse(p.val.o < p.value, "H0: independence assumption rejected.",
							"H0: independence assumption cannot be rejected."))
	res <- bind_rows(res, res.o)
return(res)
}
#continuous model outcome
cont.tests <- function(db, sensitive, obs.outcome, mod.outcome, conditional, p.value) {
	#statistical parity 
	sp <- cont.test.aux(db = db, 
			sensitive = sensitive, 
			mod.outcome = mod.outcome, 
			p.value = p.value)
	#conditional statistical parity
	csp <- NULL
	if	(!is.null(conditional)) {
		uc <- unique(conditional)
		ucl <- length(uc)
		csp <- vector("list", ucl)
		for	(i in 1:ucl) {
			conditional.l <- uc[i]
			csp.l <- cont.test.aux.c(db = db, 
						sensitive = sensitive, 
						mod.outcome = mod.outcome, 
						conditional = conditional.l, 
						p.value = p.value)
			csp.l <- cbind.data.frame(attribute = conditional.l, csp.l)
			csp[[i]] <- csp.l
			}
		csp <- bind_rows(csp)
		}
	#equal odds, equal opportunity, predictive equallity 
	eo <- cont.test.aux.c(db = db, 
				sensitive = sensitive, 
				mod.outcome = mod.outcome, 
				conditional = obs.outcome, 
				p.value = p.value)
	eo <- cbind.data.frame(metric = c("equal opportunity", "predictive equality", "equal odds"),
				     eo)	
	res <- list(SP = sp, CSP = csp, EO = eo)
return(res)
}
cont.test.aux <- function(db, sensitive, mod.outcome, p.value) {
	mod.frm <- paste0(mod.outcome, " ~ ", "as.factor(", eval(parse(text = "sensitive")), ")")	
	mod <- lm(formula = as.formula(mod.frm), data = db)
	res <- wald.test.aux(model = mod)
	res <- cbind.data.frame(res, 
				res = ifelse(res$p.val < p.value, "H0: average equality rejected.",
								"H0: average equality cannot be rejected."))
return(res)
}
wald.test.aux <- function(model) {
	vcm <- vcov(model)
	coef.est <- coef(model)
	terms <- 2:length(coef.est)
	if	(any(is.na(coef.est[terms]))) {return(1)}
	w <- length(terms)
	h0 <- rep(0, w)
	l <- matrix(rep(0, length(coef.est) * w), ncol = length(coef.est))
	for	(i in 1:w) {l[i, terms[i]] <- 1}
	f <- l%*%coef.est
	stat <- t(f - h0)%*%solve(l%*%vcm%*%t(l))%*%(f - h0)
	p.val <- pchisq(c(stat), df = w, lower.tail = FALSE)
	res <- data.frame(nobs = length(model$residuals), statistic = stat, df = w, p.val = p.val)
return(res)
}
cont.test.aux.c <- function(db, sensitive, mod.outcome, conditional, p.value) {
	ucv <- sort(unique(db[, conditional]))
	ucvl <- length(ucv)
	res <- vector("list", ucvl)
	for	(i in 1:ucvl) {
		uc.l <- ucv[i]
		res.l <- cont.test.aux(db = db[db[, conditional]%in%uc.l, ], 
					sensitive = sensitive, 
					mod.outcome = mod.outcome, 
					p.value = p.value)
		res[[i]] <- cbind.data.frame(modality = as.character(uc.l), res.l)
		}
	res <- bind_rows(res)
	#overall test
	df.o <- sum(res$df)
	statistic.o <- sum(res$statistic)
	p.val.o <- pchisq(statistic.o, df.o, lower.tail = FALSE)
	res.o <- data.frame(modality = "overall",
			nobs = sum(res$nobs),
			statistic = statistic.o,
			df = df.o,
			p.val = p.val.o, 
			res = ifelse(p.val.o < p.value, "H0: average equality rejected.",
							"H0: average equality cannot be rejected."))
	res <- bind_rows(res, res.o)
return(res)
}

