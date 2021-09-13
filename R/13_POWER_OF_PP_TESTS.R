#' Power of statistical test for predictive ability testing
#'
#' \code{power} performs Monte Carlo simulation of power of statistical test used for testing the predictive 
#' ability of the PD rating model. It covers fours tests: binomial, Jeffreys, z-score and Hosmer-Lemeshow test.
#' This procedure is applied under assumption that observed defaut rate is the true one and it make sense
#' to use it in order to check if calibrated PDs are underestimated. Therefore, for the cases where observed 
#' default rate is lower than calibrated PD, power calculation is not performed and will report the comment
#'@param rating.label Vector of rating labels.
#'@param pdc Vector of calibrated probabilities of default (PD).
#'@param no Number of observations per rating grade.
#'@param nb Number of defaults (bad cases) per rating grade.
#'@param alpha Threshold for p-value for implemented tests. Default is 0.05.
#'@param sim.num Number of Monte Carlo simulations. Default is 1000.
#'@param seed Random seed need to ensure the result reproducibility. Default is 2211.
#'@details
#' Due to the fact that test of predictive power is usually implemented on the application portfolio,
#' certain prerequisites are needed to be fulfilled. In the first place model should be developed
#' and rating scale should be formed. In order to reflect appropriate role and right moment of 
#' tests application, presented simplified example covers all steps before test implementation.
#'@return The command \code{power} returns a list with two objects. Both are the data frames and
#'	    while the first one presents power calculation of the tests applied usually on the 
#'	    rating level (binomial, Jeffreys and z-score test), the second one presents results of
#'	    the Hosmer-Lemeshow test which is applied on the complete rating scale.
#'	    For both level of the implementation (rating or complete scale) if the observed default 
#'	    rate is less than calibrated PD, function will return the comment and power simulation
#'	    will not be performed.
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
#'ct <- 0.27
#'min.pd <- 0.05
#'rs$pd <- rs.calibration(rs = rs, 
#'				dr = "dr", 
#'				w = "no", 
#'				ct = ct, 
#'				min.pd = min.pd,
#'				method = "log.odds.ab")
#'#check
#'rs
#'sum(rs$pd * rs$no / sum(rs$no))
#'#simulate some dummy application portfolio
#'set.seed(22)
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
#'power(rating.label = ap.summary$rating,
#'	  pdc = ap.summary$pd,
#'	  no = ap.summary$no,
#'	  nb = ap.summary$nb, 
#'	  alpha = 0.05,
#'	  sim.num = 1000,
#'	  seed = 2211)
#'@importFrom stats pbinom pbeta pnorm pchisq rbinom
#'@export
power <- function(rating.label, pdc, no, nb, alpha = 0.05, sim.num = 1000, seed = 2211) {
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
	odr <- nb / no
	rl <- length(rating.label)
	res.ie <- vector("list", rl)	
	for	(i in 1:rl) { 
		rating.l <- rating.label[i]
		pdc.l <- pdc[i]
		odr.l <- odr[i]
		no.l <- no[i]
		nb.l <- nb[i]
		res.l <- mc.sim.binom(pdc.r = pdc.l, 
					    odr.r = odr.l, 
					    no.r = no.l,  
					    alpha = alpha, 
					    sim.num = sim.num, 
					    seed = seed) 
		res.l <- cbind.data.frame(rating = rating.l, no = no.l, nb = nb.l, odr = odr.l, pdc = pdc.l, res.l)
		res.ie[[i]] <- res.l
		}
	res.ie <- bind_rows(res.ie)
	if	(length(pdc) == 1) {
		res.hl <- data.frame(rating = paste0(rating.label, collapse = " + "),
					   hosmer.lemeshow = NA, comment = "Only one rating grade.")
		} else {
		res.hl <- mc.sim.hl(pdc = pdc, odr = odr, no = no, nb = nb, 
					  alpha = alpha, sim.num = sim.num, seed = seed)
		res.hl <- cbind.data.frame(rating = paste0(rating.label, collapse = " + "),
						   res.hl)
		}
	res <- list(interval.estimator = res.ie, hosmer.lemeshow = res.hl)
return(res)
}

mc.sim.binom <- function(pdc.r, odr.r, no.r, alpha, sim.num, seed) {
	if	(odr.r <= pdc.r) {
		return(data.frame(comment = "ODR <= PDC"))
		}
	set.seed(seed)
	res.eb <- rep(NA, sim.num)
	res.jb <- rep(NA, sim.num)
	res.zs <- rep(NA, sim.num)
	for	(i in 1:sim.num){
		def.sim <- sum(rbinom(no.r, 1, prob = odr.r))
		res.eb[[i]] <- pbinom(def.sim - 1, no.r, pdc.r, lower.tail = FALSE)
		res.jb[[i]] <- pbeta(pdc.r, def.sim + 0.5, no.r - def.sim + 0.5)
		res.zs[[i]] <- zscore.test(pdc = pdc.r, odr = def.sim / no.r, no = no.r)
		}
	res <- data.frame(binomial = mean(res.eb < alpha),
				jeffreys = mean(res.jb < alpha),
				zscore = mean(res.zs < alpha))
return(res)
}

mc.sim.hl <- function(pdc, odr, no, nb, alpha, sim.num, seed) {
	port.odr <- sum(odr * no / sum(no))
	port.pdc <- sum(pdc * no / sum(no))
	if	(port.odr <= port.pdc) {
		return(data.frame(hosmer.lemeshow = NA, comment = "ODR <= PDC"))
		}
	res.hl <- rep(NA, sim.num)
	for	(i in 1:sim.num) {
		res.hl[[i]] <- hl.test(pdc = pdc, no = no, nb = rbin.aux(no = no, nb = nb, seed = seed + i))
		}
	res <- data.frame(hosmer.lemeshow = mean(res.hl < alpha))
return(res)
}

rbin.aux <- function(no, nb, seed) {
	rl <- length(no)
	def.sim <- rep(NA, rl)
	for	(i in 1:rl) {
		set.seed(seed)
		def.sim[i] <- sum(rbinom(no[i], 1, prob = nb[i] / no[i]))
		}
return(def.sim)
}




