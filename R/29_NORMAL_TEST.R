#' Multi-period predictive power test 
#'
#' \code{normal.test} performs multi-period testing of PD model predictive power. This procedure can be applied
#' on the level of the rating grade as well on the portfolio level. 
#'@param pdc Numeric vector of calibrated probabilities of default (PD).
#'@param odr Numeric vector of observed default rates.
#'@param alpha Significance level of p-value for implemented tests. Default is 0.05.
#'@return The command \code{normal.test} returns a data frame with estimated difference between \code{odr} and 
#'	    \code{pdc}, test statistics, standard error of the test statistics, selected significance level, 
#'	    p-value of test statistics and finally the test results. 
#'@references
#'Basel Committee on Banking Supervision (2005). Studies on the Validation of Internal
#' Rating Systems, working paper no. 14. 
#'@examples
#'set.seed(678)
#'normal.test(pdc = rep(0.02, 5), 
#'	odr = runif(5, 0.02, 0.03)) 
#'@export
normal.test <- function(pdc, odr, alpha = 0.05) {	
	if	(!(is.numeric(pdc) &  is.numeric(odr) & is.numeric(alpha))) {
		stop("All arguments have to be numeric.")
		}
	if	(length(pdc) != length(odr)) {
		stop("pdc and odr have to be of the same lenght.")
		}
	if	(any(pdc > 1 | pdc < 0 | odr > 1 | odr < 0)) {
		stop("pdc and odr have to be between 0 and 1.")
		}
	if	(alpha > 1 | alpha < 0) {
		stop("significance level (alpha) has to be between 0 and 1.")
		}
	T <- length(odr)
	se <- (sum((odr - pdc)^2) - ((sum(odr - pdc))^2 / T)) / (T - 1)
	test.stat <- sum(odr - pdc) / sqrt(T * se)
	p.value <- pnorm(test.stat, lower.tail = FALSE)
	estimate <- sum(odr - pdc)
	res <- data.frame(estimate = estimate,
				test.stat = test.stat,
				se = se, 
				alpha = alpha,
				p.value = p.value,
				res = ifelse(p.value >= alpha, "H0: ODR <= PD", "H1: ODR > PD"),
				stringsAsFactors = FALSE)
return(res)
}

