#' Modelling the Economic Value of Credit Rating System
#'
#' \code{evrs} calculates the economic benefits of improved PD model based on increase of portfolio return.
#' Implemented algorithm replicates the framework presented in the Reference under assumption that 
#' bank adopts continuous PD rating scale. Despite this assumption, results are almost identical for scenarios
#' of base case portfolio from the Reference.
#'@param db Data frame with at least the following columns: default indicator (target), PDs of model in use, 
#'		PDs of benchmark model and LGD values.
#'@param pd Name of PD of model in use within db argument.
#'@param benchmark Name of PD of benchmark model within db argument.
#'@param lgd Name of LGD values within db argument.
#'@param target Name of target (default indicator 0/1) within db argument.
#'@param sigma Measurement error of model in use. If default value (\code{NA}) is passed, then measurement error
#'		   is calculated as standard deviation of PD difference of model in use and benchmark model.
#'@param r Risk-free rate.
#'@param elasticity Elasticity parameter used to define customer churn in case of loan overpricing. 
#'@param prob.to.leave.threshold Threshold for customers' probability to leave in case of loan overpricing. 
#'@param sim.num Number of simulations. Default is 500. 
#'@param seed Random seed to ensure reproducibility. Default is 991. 
#'@return The command \code{evrs} returns a list of two elements. The first element is data frame 
#' \code{summary.tbl} and it provides simulation summary: number of simulations, number of successful simulations,
#' population size (number of observations of supplied \code{db} data frame), measurement error, 
#' average churn value (number of customers that left the portfolio due to the overpricing), average return of simulated 
#' portfolios, return of benchmark portfolio and return difference (main result of the simulation). The second element is
#' numeric vector of return averages of simulated portfolios.
#'@references 
#' Jankowitsch at al. (2007). Modelling the economic value of credit rating systems.
#'	Journal of Banking & Finance, Volume 31, Issue 1, \doi{10.1016/j.jbankfin.2006.01.003}. 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#simulate model in use
#'miu.formula <- Creditability ~ `Age (years)` + `Duration of Credit (month)` +
#'	   			  `Value Savings/Stocks` + `Purpose`
#'miu <- glm(miu.formula, family = "binomial", data = loans)
#'miu.pd <- unname(predict(miu, type = "response", newdata = loans))
#'#simulate benchmark model with interaction.transformer support
#'bnm.pack <- stepFWDr(start.model = Creditability ~ 1, 
#'                     p.value = 0.05, 
#'                     db = loans,
#'                     check.start.model = TRUE, 
#'                     offset.vals = NULL)
#'bnm <- bnm.pack$model
#'bnm.pd <- unname(predict(bnm, type = "response", newdata = bnm.pack$dev.db))
#'#prepare data for evrs function
#'db <- data.frame("Creditability" = loans$Creditability, 	
#'	   pd = miu.pd, 
#'	   pd.benchmark = bnm.pd, 
#'	   lgd = 0.75)
#'#calculate the difference in portfolio return between model in use the benchmark model
#'res <- evrs(db = db, 
#'	pd = "pd", 
#'	benchmark = "pd.benchmark", 
#'	lgd = "lgd",
#'	target = "Creditability",
#'	sigma = NA, 
#'	r = 0.03, 
#'	elasticity = 100, 
#'	prob.to.leave.threshold = 0.5,
#'	sim.num = 500, 
#'	seed = 991)
#'names(res)
#'#print simulation summary table
#'res[["summary.tbl"]]
#'#portfolio return increase in case of using benchmark model
#'res[["summary.tbl"]][, "return.difference", drop = FALSE]
#'#summary of simulated returns
#'summary(res[["return.sim"]])
#'@importFrom stats rnorm
#'@export
evrs <- function(db, pd, benchmark, lgd, target, sigma = NA, r, elasticity, prob.to.leave.threshold, 
		     sim.num = 500, seed = 991) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	cols.comp <- c(pd, benchmark, lgd, target)
	if	(!sum(cols.comp%in%names(db)) == length(cols.comp)) {
		miss.cols <- cols.comp[!cols.comp%in%names(db)]
		msg <- paste0("Following column(s) does not exist in supplied db:",
				  paste(miss.cols, collapse = ", "), ".")
		stop(msg)
		}	
	db.sim <- db[, c(target, pd, benchmark, lgd)]
	names(db.sim) <- c("target", "pd", "benchmark", "lgd")
	db.sim <- db.sim[complete.cases(db.sim), ]
	if	(!all(db.sim$target%in%c(0, 1))) {
		stop("target is not 0/1 variable")
		}
	if	(!all(db.sim$pd > 0 & db.sim$pd < 1) |
		 !all(db.sim$bechmark > 0 & db.sim$bechmark < 1)) {
		stop("pd and bechmark has to be greater than 0 and less than 1.")
		}
	prob.to.leave.threshold <- prob.to.leave.threshold[1]
	if	(!(prob.to.leave.threshold[1] > 0 & prob.to.leave.threshold[1] < 1)) {
		stop("prob.to.leave.threshold has to be greater than 0 and less than 1.")
		}
	r <- r[1]; elasticity <- elasticity[1]; seed <- seed[1]
	if	(!(r > 0 & elasticity > 0 & seed > 0)) {
		stop("r, elasticity and seed have to be greater than 0.")
		}
	#measurement error
	if	(is.na(sigma)) {
		db.sim$pd.dif <- db.sim$pd - db.sim$benchmark
		sigma <- sd(db.sim$pd.dif)
		if	(sigma == 0) {
			stop("sigma cannot be calculated. Check pd.benchmark values.")
			}
		} else {
		if	(!is.numeric(sigma) | length(sigma) > 1) {
			stop("sigma has to be single numeric value greater than 0.")} 
		if	(sigma <= 0) {stop("sigma has to be single numeric value greater than 0.")}		
		}
	#calculate existing spreads
	db.sim$spread.exis <- spread(r = r, pd = db.sim$pd, lgd = db.sim$lgd)
	#simulations
	churn <- rep(NA, sim.num)
	r.sim <- rep(NA, sim.num)
	for	(i in 1:sim.num) {
		pd.sim <- pd.observed(pd = db.sim$pd, mean = 0, sigma = sigma, seed = seed + (i - 1))
		spread.sim <- spread(r = r, pd = pd.sim, lgd = db.sim$lgd)
		magnitude <- spread.sim - db.sim$spread.exis
		probs <- prob.to.leave(alpha = elasticity, 
					     magnitude = ifelse(magnitude < 0, 0, magnitude))
		sim.indx <- !probs > prob.to.leave.threshold
		churn.l <- sum(probs > prob.to.leave.threshold)
		churn[i] <- churn.l
		db.sim.l <- db.sim[sim.indx, ]
		r.sim[i] <- portfolio.return(def.ind = db.sim.l$target, 
						     r = r, 
						     spread = spread.sim[sim.indx], 
						     lgd = db.sim.l$lgd)
		}
	return.benchmark <- portfolio.return(def.ind = db.sim$target, 
					     r = r, 
					     spread = spread(r = r, 
							     pd = db.sim$benchmark, 
							     lgd = db.sim$lgd), 
					     lgd = db.sim$lgd)

	summary.tbl <- data.frame(sim.total = sim.num,
					  sim.succes = sum(!is.na(r.sim)),
					  population = nrow(db.sim), 
					  measurement.error = sigma,
					  churn.avg = mean(churn, na.rm = TRUE), 
					  return.sim = mean(r.sim, na.rm = TRUE),
					  return.benchmark = return.benchmark)
	summary.tbl$return.difference <- summary.tbl$return.benchmark - summary.tbl$return.sim	
	res <- list(summary.tbl = summary.tbl, return.sim = r.sim)
return(res)
}

score.trans <- function(x, type) {
	if	(type%in%"pd.to.score") {
		res <- log(x / (1 - x))
		} else {
		res <- exp(x) / ( 1 + exp(x))
		}
return(res)
}
pd.observed <- function(pd, mean = 0, sigma, seed) {
	set.seed(seed)
	score <- score.trans(pd, type = "pd.to.score")
	nobs <- length(pd)
	score.sim <- score + rnorm(nobs, mean = mean, sd = sigma)
	pd.sim <- score.trans(score.sim, type = "score.to.pd")
return(pd.sim)
}
spread <- function(r, pd, lgd) {
	res <- (1 + r) * (pd * lgd) / (1 - pd * lgd)
return(res)
}
prob.to.leave <- function(alpha, magnitude) {
	res <- 1 - exp(-alpha * magnitude) 
return(res)
} 
portfolio.return <- function(def.ind, r, spread, lgd) {
	ret <- ifelse(def.ind == 0, 1 + r + spread - 1, (1 - lgd) * (1 + r + spread) - 1)	
	ret.mean <- mean(ret)
return(ret.mean)
}



