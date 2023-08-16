#' Constrained logistic regression
#'
#' \code{constrained.logit} performs estimation of logistic regression with constrains on values of the
#' estimated coefficient.
#'@param db Data set of risk factors and target variable.
#'@param x Character vector of risk factors (independent variables) used in logistic regression.
#'@param y Character vector of target (dependent variable) used in logistic regression.
#'@param lower Numeric vector of lower boundaries of the coefficients. This vector should contain value of the intercept,
#'		   therefore number of elements should be equal to number of elements of the argument \code{x} plus one.
#'@param upper Numeric vector of upper boundaries of the coefficients. This vector should contain value of the intercept,
#'		   therefore number of elements should be equal to number of elements of the argument \code{x} plus one.
#'@return The command \code{constrained.logit} returns list of two vectors. The first vector contains values of the 
#' estimated coefficients, while the second vector contains predictions of the constrained logistic regression.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#model 1
#'reg.1 <- glm(Creditability ~ `Account Balance`, family = "binomial", data = loans)
#'summary(reg.1)$coefficient
#'loans$pred.1 <-  unname(predict(reg.1, type = "response"))
#'#model 2
#'reg.2 <- glm(Creditability ~ `Age (years)`, family = "binomial", data = loans)
#'summary(reg.2)$coefficient
#'loans$pred.2 <-  unname(predict(reg.2, type = "response"))
#'#integration
#'fm <- glm(Creditability ~ pred.1 + pred.2, family = "binomial", data = loans)
#'summary(fm)$coefficient
#'fm.pred <- predict(fm, type = "response", newdata = loans)
#'auc.model(predictions = fm.pred, observed = loans$Creditability)
#'#constrained integration (regression)
#'cl.r <- constrained.logit(db = loans, 
#'			  x = c("pred.1", "pred.2"), 
#'			  y = "Creditability",
#'			  lower = c(-Inf, -Inf, -Inf), 
#'			  upper = c(Inf, 4.5, Inf))
#'names(cl.r)
#'cl.r[["beta"]]
#'auc.model(predictions = cl.r[["prediction"]], observed = loans$Creditability)
#'@export
constrained.logit <- function(db, x, y, lower, upper) {
	if	(!(all(is.numeric(lower)) & all(is.numeric(upper)))) {
		stop("arguments lower and upper has to be numeric vectors.")
		}
	if	(!all(c(x, y)%in%names(db))) {
		stop("x or y cannot be found in db data frame.")
		}
	frm <- paste0(y, " ~ ", paste0(x, collapse = " + "))
	lr <- glm(frm, family = "binomial", data = db)
	lr.coef <- coef(lr)
	if	(length(lr.coef) != length(lower) | length(lr.coef) != length(upper)) {
		stop("lower and upper bounds have to be equal to number of model parameters (including intercept).")
		}		
	lu <- bind_cols(l = lower, u = upper)
	check <- which(apply(lu, 1, function(x) !all(is.infinite(x)))) 
	if	(length(check) > 0) {
		if	(any(!upper[check] > lower[check])) {
			stop("Upper bound element(s) less or equal to lower bound element(s).")
			}
		}
	xm <- as.matrix(cbind.data.frame(intercept = 1, db[, x]))
	ym <- as.matrix(db[, y, drop = FALSE])
	b.start <- rep(0, length(lr.coef))
	opt.logit <- try(optim(par = b.start, 
				     fn = log_likelihood, 
				     x = xm, 
				     y = ym, 
				     method = "L-BFGS-B",
				     lower = lower, 
				     upper = upper),
			     silent = TRUE)
	if	(class(opt.logit)%in%"try-error") {
		msg <- "Error in optimization procedure."
		msg <- paste0(msg, " Check supplied arguments")
		msg <- paste0(msg, " and try manual estimation of unconstrained regression using glm.")
		stop(msg)
		}
	b.opt <- opt.logit$par
	names(b.opt) <- names(lr.coef)
	pred.opt <- c(xm%*%b.opt)
return(list(beta = b.opt, prediction = pred.opt))
}

log_likelihood <- function(b, x, y) {
	-sum(y * (x%*%b - log(1 + exp(x%*%b))) + (1 - y) * (-log(1 + exp(x%*%b)))) 
}

