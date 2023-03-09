#' Herfindahl-Hirschman Index (HHI)
#'
#' \code{hhi} performs calculation on Herfindahl-Hirschman Index. 
#'@param x Numeric vector of input values (e.g. number of observations or sum of exposure per rating grade).
#'@return The command \code{hhinormal.test} returns single element numeric vector of HHI value.  
#'@examples
#'#simulate PD model and rating scale
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'res <- stepFWDr(start.model = Creditability ~ 1, 
#'	   p.value = 0.05, 
#'	   db = loans)
#'mod.predictions <- unname(predict(res$model, type = "response"))
#'rating.scale <- sts.bin(y = loans$Creditability, x = mod.predictions)[[1]]
#'#calculate HHI 
#'hhi(x = rating.scale$no)
#'@export
hhi <- function(x) {	
	if	(!is.numeric(x)) {
		stop("x has to be numeric vector.")
		}
	if	(sum(x%in%c(Inf, -Inf)) > 0) {
		stop("x contains Inf or -Inf values.")
		}
	cc <- complete.cases(x)
	x <- x[cc]
	s <- (x / sum(x))^2
	res <- sum(s)
return(res)
}

