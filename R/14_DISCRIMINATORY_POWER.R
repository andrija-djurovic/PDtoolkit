#' Testing the discriminatory power of PD rating model
#'
#' \code{dp.testing} performs testing of discriminatory power of the model in use applied to application portfolio
#' in comparison to the discriminatory power from the moment of development. Testing is performed based on area
#' under curve (AUC) from the application portfolio and development sample under assumption that latter is a 
#' deterministic (as given) and that test statistics follow the normal distribution. 
#' Standard error of AUC for application portfolio is calculated as proposed by
#' Hanley and McNeil (see References).
#'@param app.port Application portfolio (data frame) which contains default indicator (0/1) and
#'			calibrated probabilities of default (PD) in use.
#'@param def.ind Name of the column that represents observed default indicator (0/1).
#'@param pdc Name of the column that represent calibrated PD in use.
#'@param auc.test Value of tested AUC (usually AUC from development sample).
#'@param alternative Alternative hypothesis. Available options are: \code{"less", "greater", "two.sided"}.
#'@param alpha Significance level of p-value for hypothesis testing. Default is 0.05.
#'@details
#' Due to the fact that test of discriminatory power is usually implemented on the application portfolio,
#' certain prerequisites are needed to be fulfilled. In the first place model should be developed
#' and rating scale should be formed. In order to reflect appropriate role and right moment of 
#' tests application, presented simplified example covers all steps before test implementation.
#'@return The command \code{dp.testing} returns a data frame with input parameters along with 
#'	    hypothesis testing metrics such as estimated difference of observed (application portfolio) and testing AUC, 
#'	    standard error of observed AUC, p-value of testing procedure and accepted hypothesis.
#'@references
#'Hanley J. and McNeil B. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. 
#'				    Radiology (1982) 43 (1) pp. 29-36. 
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
#'				method = "log.odds.ab")[[1]]
#'#check
#'rs
#'sum(rs$pd * rs$no / sum(rs$no))
#'#bring calibrated PDs to the development sample
#'loans <- merge(loans, rs, by = "rating", all.x = TRUE)
#'#calculate development AUC
#'auc.dev <- auc.model(predictions = loans$pd, observed = loans$Creditability)
#'auc.dev
#'#simulate some dummy application portfolio
#'set.seed(321)
#'app.port <- loans[sample(1:nrow(loans), 400), ]
#'#calculate application portfolio AUC
#'auc.app <- auc.model(predictions = app.port$pd, observed = app.port$Creditability)
#'auc.app
#'#test deterioration of descriminatory power measured by AUC
#'dp.testing(app.port = app.port, 
#'	     def.ind = "Creditability", 
#'	     pdc = "pd", auc.test = 0.7557,
#'	     alternative = "less", 
#'	     alpha = 0.05)
#'@importFrom stats pnorm
#'@export
dp.testing <- function(app.port, def.ind, pdc, auc.test, alternative, alpha = 0.05) {
	if	(!is.data.frame(app.port)) {
		stop("app.port (application portfolio) is not a data frame.")
		}
	if	(any(!(c(def.ind, pdc)%in%names(app.port)))) {
		stop("arguments def.ind (default indicator) and/or pdc (calibrated pd) cannot be found 
		      in the app.port (application portfolio) data frame.")
		}
	if	(alpha < 0 | alpha > 1 | auc.test < 0 | auc.test > 1) {
		stop("alpha and auc.test have to be between 0 and 1.")
		}
	alt.opts <- c("less", "greater", "two.sided")
	if	(!alternative[1]%in%alt.opts) {
		msg <- paste0("alternative argument has to be one of: ", paste(alt.opts, collapse = ", "), ".")
		stop(msg)
		}
	y <- app.port[, def.ind]
	pd <- app.port[, pdc]
	cond.00 <- !sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])
	if	(cond.00) {
		stop("def.ind has to be 0/1 variable.")
		}
	cc <- complete.cases(app.port[, c(def.ind, pdc)])
	app.port <- app.port[cc, ]
	if	(nrow(app.port) == 0) {
		stop("No complete cases for app.port.")
		}
	if	(any(!cc)) {
		warning("There are some incomplete cases. Check def.ind and pdc columns.")
		}
	auc <- auc.model(predictions = pd, observed = y)
	q1 <- auc / (2 - auc) 
	q2 <- (2 * auc^2) / (1 + auc) 
	auc.se <- sqrt((auc * (1 - auc) + (sum(y) - 1) * (q1 - auc^2) + (sum(1-y) - 1) * (q2 - auc^2)) / 
			   (sum(y) * sum(1-y)))	
	test.stat <- (auc - auc.test) / auc.se
	p.val <- switch(alternative[1], "less" = pnorm(test.stat),
				  	        "greater" = pnorm(test.stat, lower.tail = FALSE), 
					        "two.sided" = 2 * pnorm(abs(test.stat), lower.tail = FALSE))
	h.sign <- switch(alternative[1], "less" = c(" >= ", " < "),
				  	     	   "greater" = c(" <= ", " > "), 
					    	   "two.sided" = c(" == ", " != "))
	res <- data.frame(auc = auc,
				auc.test = auc.test,
				estimate = auc - auc.test,
				auc.se = auc.se,
				test.stat = test.stat,
				p.val = p.val,
				alpha = alpha,
				res = ifelse(p.val >= alpha, 
					  	 paste0("H0: AUC", h.sign[1], " AUC test"),
						 paste0("H1: AUC", h.sign[2], " AUC test")))
return(res)
}




