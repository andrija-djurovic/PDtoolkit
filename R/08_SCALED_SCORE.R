#' Scaling the probabilities
#'
#' \code{scaled.score} performs scaling of the probabilities for a certain set up. User has to select
#' three parameters (\code{score, odd, pdo}), while the probabilities (\code{probs}) are usually  
#' predictions of the final model. 
#'@param probs Model predicted probabilities of default.
#'@param score Specific score for selected odd (for argument \code{odd}). Default is 600.
#'@param odd Odd (good/bad) at specific score (for argument \code{score}). Default is 50/1.
#'@param pdo Points for double the odds. Default is 20.
#'@return The command \code{scaled.score} returns a vector of scaled scores.
#'@references 
#'Siddiqi, N. (2012). Credit Risk Scorecards: Developing and Implementing Intelligent Credit Scoring,  
#'			    John Wiley & Sons, Inc., \doi{10.1002/9781119201731}. 
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#run stepMIV
#'res <- stepMIV(start.model = Creditability ~ 1, 
#'		   miv.threshold = 0.02, 
#'		   m.ch.p.val = 0.05,
#'		   coding = "WoE",
#'		   db = loans)
#extract the final model
#'final.model <- res$model
#'summary(final.model)$coefficients
#'#overview of development data base
#'head(res$dev.db)
#'#predict probabilities using the final model
#'loans$probs <- predict(final.model, type = "response", newdata = res$dev.db)
#'#scale probabilities to scores
#'loans$score <- scaled.score(probs = loans$probs, score = 600, odd = 50/1, pdo = 20)
#'#check AUC of probabilities and score
#'auc.model(predictions = loans$probs, observed = loans$Creditability)
#'auc.model(predictions = loans$score, observed = ifelse(loans$Creditability == 0, 1, 0))
#'#note that higher score indicates lower probability of default
#'@export
scaled.score <- function(probs, score = 600, odd = 50/1, pdo = 20) {
	check.00 <- !(is.numeric(probs) & is.numeric(score) & is.numeric(odd) & is.numeric(pdo))
	check.01 <- !(length(score) == 1 & length(odd) == 1 & length(pdo) == 1)
	check.02 <- !(odd > 0 & pdo > 0)
	if	(check.00) {stop("All arguments have to be of numeric type.")}
	if	(check.01) {stop("score, odd, pdo have to be of legnth 1.")}
	if	(check.02) {stop("odd and pdo have to be greater than 0.")}
	
	odds <- (1 - probs) / probs
	factor <- pdo / log(2)
	offset <- score - factor * log(odd) 
	ss <- offset + factor * log(odds)

return(ss)
}



