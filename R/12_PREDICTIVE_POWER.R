#' Testing the predictive power of PD rating model
#'
#' \code{pp.testing} performs testing of predictive power of the PD rating model. This procedure should be applied
#' on the level of the rating scale. 
#' Four tests are implemented: binomial, Jeffreys, z-score and Hosmer-Lemeshow test.
#' Only Hosmer-Lemeshow test refers to complete rating scale, while the remaining three are implemented on the 
#' rating grade level. The null hypothesis for all tests is that observed default rate 
#' \eqn{\frac{n_b}{n_o}} is less or equal to the calibrated PD (\code{pdc}).
#'@param rating.label Vector of rating labels.
#'@param pdc Vector of calibrated probabilities of default (PD).
#'@param no Number of observations per rating grade.
#'@param nb Number of defaults (bad cases) per rating grade.
#'@param alpha Threshold for p-value for implemented tests. Default is 0.05.
#'@details
#' Due to the fact that test of predictive power is usually implemented on the application portfolio,
#' certain prerequisites are needed to be fulfilled. In the first place model should be developed
#' and rating scale should be formed. In order to reflect appropriate role and right moment of 
#' tests application, presented simplified example covers all steps before test implementation.
#'@return The command \code{pp.testing} returns a data frame with input parameters along with 
#'	    p-value for each implemented test and the accepted hypothesis. Due to the fact that 
#'	    Hosmer-Lemeshow test is applied to complete rating scale, returned p-values are all equal between
#'	    the rating grades as well as the test results.
#'@references
#'Tasche, D. (2008). Validation of internal rating systems and PD estimates,
#'			   The Analytics of Risk Model Validation, Quantitative Finance, 
#'			   Elsevier B.V., <doi:10.1016/B978-075068158-2.50014-7>.\cr
#'Oesterreichische Nationalbank (2004). Rating Models and Validation,
#'						    Oesterreichische Nationalbank (OeNB). 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#estimate some dummy model
#'mod.frm <- `Creditability` ~ `Account Balance` + `Duration of Credit (month)` +
#'					`Age (years)`
#'lr.mod <- glm(mod.frm, family = "binomial", data = loans)
#'summary(lr.mod)$coefficients
#'#model predictions
#'loans$pred <-  unname(predict(lr.mod, type = "response", newdata = loans))
#'#scale probabilities
#'loans$score <- scaled.score(probs = loans$pred, score = 600, odd = 50/1, pdo = 20)
#'#group scores into rating
#'loans$rating <- sts.bin(x = round(loans$score), y = loans$Creditability, y.type = "bina")[[2]]
#'#create rating scale
#'rs <- loans %>%
#'	group_by(rating) %>%
#'	summarise(no = n(),
#'		    nb = sum(Creditability),
#'		    ng = sum(1 - Creditability)) %>%
#'	mutate(dr = nb / no)
#'rs
#'#calcualte portfolio default rate
#'sum(rs$dr * rs$no / sum(rs$no))
#'#calibrate rating scale to central tendency of 27% with minimum PD of 5%
#'ct <- 0.33
#'min.pd <- 0.05
#'rs$pd <- rs.calibration(rs = rs, 
#'				dr = "dr", 
#'				w = "no", 
#'				ct = ct, 
#'				min.pd = min.pd,
#'				method = "log.odds.ab")
#'#checks
#'rs
#'sum(rs$pd * rs$no / sum(rs$no))
#'#simulate some dummy application portfolio
#'set.seed(11)
#'app.port <- loans[sample(1:nrow(loans), 400), ]
#'#summarise application portfolio on rating level
#'ap.summary <- app.port %>%
#'		  group_by(rating) %>%
#'		  summarise(no = n(),
#'				nb = sum(Creditability),
#'				ng = sum(1 - Creditability)) %>%
#'		  mutate(dr = nb / no)
#'#bring calibrated pd as a based for predictive power testing
#'ap.summary <- merge(rs[, c("rating", "pd")], ap.summary, by = "rating", all.x = TRUE)
#'ap.summary
#'#perform predictive power testing
#'pp.res <- pp.testing(rating.label = ap.summary$rating,
#'			     pdc = ap.summary$pd,
#'			     no = ap.summary$no,
#'			     nb = ap.summary$nb, 
#'			     alpha = 0.05)
#'pp.res
#'@importFrom stats pbinom pbeta pnorm pchisq
#'@export
pp.testing <- function(rating.label, pdc, no, nb, alpha = 0.05) {
	l <- list(rating.label, pdc, no, nb)
 	l.check <- !length(table(sapply(l, length))) == 1
	if	(l.check) {
		stop("arguments rating.label, pdc, no and nb have to be of the same length.")
		}
	if	(any(!(is.numeric(pdc) | is.numeric(n) | is.numeric(nb)))) {
		stop("All arguments have to of numeric type.")
		}
	if	(any(pdc > 1 | pdc < 0)) {
		stop("pdc and odr have to be between 0 and 1.")
		}
	if	(any(nb > no)) {
		stop("Number of defaults cannot be greater than number of observations.")
		}
	if	(any(nb < 0 | no < 0)) {
		stop("no and nb arguments cannot be negative.")
		}
	bino.test <- pbinom(nb - 1, no, pdc, lower.tail = FALSE)
	bino.test.res <- ifelse(bino.test >= alpha, "H0: ODR <= PDC", "H1: ODR > PDC")
	jeff.test <- pbeta(pdc, nb + 0.5, no - nb + 0.5)
	jeff.test.res <- ifelse(jeff.test >= alpha, "H0: ODR <= PDC", "H1: ODR > PDC")
	odr <- nb / no
	zsco.test <- zscore.test(pdc = pdc, odr = odr, no = no)
	zsco.test.res <- ifelse(zsco.test >= alpha, "H0: ODR <= PDC", "H1: ODR > PDC")
	if	(length(pdc) == 1) {
		hosm.test <- NA
		hosm.test.res <- "Only one rating grade."
		} else {
		hosm.test <- hl.test(pdc = pdc, no = no, nb = nb)
		hosm.test.res <- ifelse(hosm.test >= alpha, "H0: ODR <= PDC", "H1: ODR > PDC")
		}
	res <- data.frame(rating = rating.label,
				no = no,
				nb = nb,
				odr = odr,
				pdc = pdc,
				alpha = alpha, 
				binomial = bino.test, binomial.res = bino.test.res, 
				jeffreys = jeff.test, jeffreys.res = jeff.test.res,
				zscore = zsco.test, zscore.res = zsco.test.res,
				hosmer.lemeshow = hosm.test, hosmer.lemeshow.res = hosm.test.res)
return(res)
}

zscore.test <- function(pdc, odr, no) {
	se <-  sqrt((pdc * (1 - pdc)) / no)
	test.stat <- (odr - pdc) / se
	p.val <- pnorm(test.stat, lower.tail = FALSE)
return(p.val)
}

hl.test <- function(pdc, no, nb) {
	k <- length(no)
	hl <- sum((nb - no * pdc)^2 / (no * pdc * (1 - pdc)), na.rm = T)
	p.val <- pchisq(q = hl, df = k, lower.tail = FALSE)
return(p.val)
}


