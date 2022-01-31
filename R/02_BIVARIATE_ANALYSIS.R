#' Bivariate analysis
#'
#' \code{bivariate} returns the bivariate statistics for risk factors supplied in data frame \code{db}. \cr
#' Implemented procedure expects all risk factors to be categorical, thus numeric risk factors should be first
#' categorized. Additionally, maximum number of groups per risk factor is set to 10, so risk factors with more than
#' 10 categories will not be processed automatically, but manual inspection can be still done using \code{woe.tbl} 
#' and \code{auc.model} functions in order to produce the same statistics. Results of both checks (risk factor class and 
#' number of categories), if identified, will be reported in second element of function output - \code{info} data frame. \cr
#' Bivariate report (first element of function output - \code{results} data frame) includes:
#' \itemize{
#'   \item rf: Risk factor name.
#'   \item bin: Risk factor group (bin).
#'   \item no: Number of observations per bin.
#'   \item ng: Number of good cases (where target is equal to 0) per bin.
#'   \item nb: Number of bad cases (where target is equal to 1) per bin. 
#'   \item pct.o: Percentage of observations per bin. 
#'   \item pct.g: Percentage of good cases (where target is equal to 0) per bin. 
#'   \item pct.b: Percentage of bad cases (where target is equal to 1) per bin. 
#'   \item dr: Default rate per bin.   
#'   \item so: Number of all observations.   
#'   \item sg: Number of all good cases.  	
#'   \item sb: Number of all bad cases. 
#'   \item dist.g: Distribution of good cases per bin. 
#'   \item dist.b: Distribution of bad cases per bin. 
#'   \item woe: WoE value. 
#'   \item iv.b: Information value per bin. 
#'   \item iv.s: Information value of risk factor (sum of individual bins' information values). 
#'   \item auc: Area under curve of simple logistic regression model estimated as \code{y ~ x}, 
#'		    where \code{y} is selected target variable and \code{x} is categorical risk factor. 
#'}
#' Additional info report (second element of function output - \code{info} data frame), if produced, includes:
#' \itemize{
#'   \item rf: Risk factor name.
#'   \item reason.code: Reason code takes value 1 if inappropriate class of risk factor is identified, while 
#'				for check of maximum number of categories it takes value 2.
#'   \item comment: Reason description.
#'}
#'@param db Data frame of risk factors and target variable supplied for bivariate analysis.
#'@param target Name of target variable within \code{db} argument.
#'@return The command \code{bivariate} returns the list of two data frames. The first one contains bivariate metrics 
#'	  while the second data frame reports results of above explained validations 
#' (class of the risk factors and number of categories).
#'@seealso \code{\link{woe.tbl}} and \code{\link{auc.model}} for manual bivariate analysis.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'#categorize numeric risk factors
#'gcd$age.bin <- ndr.bin(x = gcd$age, y = gcd$qual)[[2]]
#'gcd$age.bin.1 <- cut(x = gcd$age, breaks = 20)
#'gcd$maturity.bin <- ndr.bin(x = gcd$maturity, y = gcd$qual, y.type = "bina")[[2]]
#'gcd$amount.bin <- ndr.bin(x = gcd$amount, y = gcd$qual)[[2]]
#'str(gcd)
#'#select target variable and categorized risk factors
#'gcd.bin <- gcd[, c("qual", "age.bin", "maturity.bin", "amount.bin")]
#'#run bivariate analysis on data frame with only categorical risk factors
#'bivariate(db = gcd.bin, target = "qual")
#'#run bivariate analysis on data frame with mixed risk factors (categorical and numeric). 
#'#for this example info table is produced
#'bivariate(db = gcd, target = "qual")
#'#run woe table for risk factor with more than 10 modalities
#'woe.tbl(tbl = gcd, x = "age.bin.1", y = "qual")
#'#calculate auc for risk factor with more than 10 modalities
#'lr <- glm(qual ~ age.bin.1, family = "binomial", data = gcd)
#'auc.model(predictions = predict(lr, type = "response", newdata = gcd),
#'	    observed = gcd$qual)
#'@import monobin
#'@import dplyr
#'@importFrom stats complete.cases glm predict
#'@export
bivariate <- function(db, target) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	if	(!target%in%names(db)) {
		stop("Target variable does not exist in supplied db.")
		}
	y <- db[, target]
	cond.00 <- !sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])
	if	(cond.00) {
		stop("Target is not 0/1 variable.")
		}
	rf <- names(db)[!names(db)%in%target]
	rfl <- length(rf)
	if	(rfl == 0) {
		stop("There are no risk factors in supplied db.")
		}
	res <- vector("list", rfl)
	info <- vector("list", rfl)
	for	(i in 1:rfl) {
		xl <- rf[i]
		x <- db[, xl]
		cond.01 <- !(is.character(x) | is.factor(x) | is.logical(x))
		cond.02 <- length(unique(x)) > 10
		if	(cond.01) {
			info[[i]] <- data.frame(rf = xl, 
							reason.code = 1, 
							comment = "Inappropriate class. It has to be one of: character, factor or logical.")
			next
			}
		if	(cond.02) {
			info[[i]] <- data.frame(rf = xl, 
							reason.code = 2,
							comment = "More than 10 categories.")
			next
			}
		woe.res <- woe.tbl(tbl = db, x = xl, y = target)
		mod.formula <- paste0(target, " ~ ", "`", xl, "`")
		lr <- try(glm(mod.formula, family = "binomial", data = db), silent = TRUE)	
		if	("try-error"%in%class(lr)) {
			auc <- NA
			} else {
			auc <- auc.model(predictions = unname(predict(lr, type = "response", newdata = db)), 
					     observed = y)
			}
		woe.res$auc <- auc
		woe.res <- cbind.data.frame(rf = xl, woe.res)
		res[[i]] <- woe.res
		}
	res <- data.frame(bind_rows(res))
	info <- data.frame(bind_rows(info))
return(list(results = res, info = info))
}

#' Weights of evidence (WoE) table
#'
#' \code{woe.tbl} calculates WoE and information value for given target variable and risk factor along with 
#' accompanied metrics needed for their calculation.
#' WoE table reports:
#' \itemize{
#'   \item bin: Risk factor group (bin).
#'   \item no: Number of observations per bin.
#'   \item ng: Number of good cases (where target is equal to 0) per bin.
#'   \item nb: Number of bad cases (where target is equal to 1) per bin. 
#'   \item pct.o: Percentage of observations per bin. 
#'   \item pct.g: Percentage of good cases (where target is equal to 0) per bin. 
#'   \item pct.b: Percentage of bad cases (where target is equal to 1) per bin. 
#'   \item dr: Default rate per bin.   
#'   \item so: Number of all observations.   
#'   \item sg: Number of all good cases.  	
#'   \item sb: Number of all bad cases. 
#'   \item dist.g: Distribution of good cases per bin. 
#'   \item dist.b: Distribution of bad cases per bin. 
#'   \item woe: WoE value. 
#'   \item iv.b: Information value per bin. 
#'   \item iv.s: Information value of risk factor (sum of individual bins' information values).  
#'}
#'@param tbl Data frame which contains target variable (\code{y}) and analyzed risk factor (\code{x}).
#'@param x Selected risk factor.
#'@param y Selected target variable.
#'@param y.check Logical, if target variable (\code{y}) should be checked for 0/1 values. 
#'		     Default value is \code{TRUE}.
#'		     Change of this parameter to \code{FALSE} can be handy for calculation of WoE based on model 
#'		     predictions. Concretely, it is used only in calculation of marginal information value (MIV) in \code{\link{stepMIV}}.
#'@return The command \code{woe.tbl} returns the data frame with WoE and information value calculations along with accompanied metrics.
#'@seealso \code{\link{bivariate}} for automatic bivariate analysis.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'#categorize numeric risk factors
#'gcd$age.bin <- woe.bin(x = gcd$age, y = gcd$qual, y.type = "bina")[[2]]
#'#generate woe table
#'woe.tbl(tbl = gcd, x = "age.bin", y = "qual")
#'@import monobin
#'@import dplyr
#'@export
woe.tbl <- function(tbl, x, y, y.check = TRUE) {
	if	(!is.data.frame(tbl)) {
		stop("tbl is not a data frame.")
		}
	if	(!y%in%names(tbl)) {
		stop("y (target variable) does not exist in supplied tbl.")
		}
	if	(!x%in%names(tbl)) {
		stop("x (risk factor) does not exist in supplied tbl.")
		}
	if	(!is.logical(y.check)) {
		stop("y.check has to be of logical type.")
		}
	target <- tbl[, y]
	if	(y.check) {
		cond.00 <- !sum(target[!is.na(target)]%in%c(0, 1)) == length(target[!is.na(target)])
		if	(cond.00) {
			stop("y (target variable) is not 0/1 variable.")
			}
		}
	res <- tbl %>% 
		 group_by_at(c("bin" = x)) %>%
		 summarise(no = n(),
			     ng = sum(1 - !!sym(y), na.rm = TRUE),
			     nb = sum(!!sym(y), na.rm = TRUE)) %>%
		 ungroup() %>%
		 mutate(pct.o = no / sum(no, na.rm = TRUE),
			  pct.g = ng / sum(ng, na.rm = TRUE),
			  pct.b = nb / sum(nb, na.rm = TRUE),
			  dr = nb / no,
			  so = sum(no, na.rm = TRUE),
			  sg = sum(ng, na.rm = TRUE),
			  sb = sum(nb, na.rm = TRUE), 
			  dist.g = ng / sg,
			  dist.b = nb / sb,
			  woe = log(dist.g / dist.b),
			  iv.b = (dist.g - dist.b) * woe,  
			  iv.s = sum(iv.b))
return(data.frame(res))
}

#' Area under curve (AUC)
#'
#' \code{auc.model} calculates area under curve (AUC) for a given predicted values and observed target variable.
#'@param predictions Model predictions.
#'@param observed Observed values of target variable.
#'@return The command \code{auc.model} returns value of AUC.
#'@seealso \code{\link{bivariate}} for automatic bivariate analysis.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'#categorize numeric risk factor
#'gcd$maturity.bin <- ndr.bin(x = gcd$maturity, y = gcd$qual, y.type = "bina")[[2]]
#'#estimate simple logistic regression model
#'lr <- glm(qual ~ maturity.bin, family = "binomial", data = gcd)
#'#calculate auc
#'auc.model(predictions = predict(lr, type = "response", newdata = gcd),
#'	    observed = gcd$qual)
#'@import monobin
#'@importFrom stats complete.cases glm predict
#'@export
auc.model <- function(predictions, observed) {
	cc <- complete.cases(predictions, observed)
	predictions <- predictions[cc]
	observed <- observed[cc]
	n1 <- sum(observed == 0)
	n2 <- sum(observed == 1)
	u  <- sum(rank(predictions)[observed == 0]) - n1 * (n1 + 1) / 2
	auc <- 1 - u / n1 / n2
return(auc)
}

#' Replace modalities of risk factor with weights of evidence (WoE) value
#'
#' \code{replace.woe} replaces modalities of risk factor with calculated WoE value. This function process only
#' categorical risk factors, thus it is assumed that numerical risk factors are previously categorized.
#' Additional info report (second element of function output - \code{info} data frame), if produced, includes:
#' \itemize{
#'   \item rf: Risk factor name.
#'   \item reason.code: Reason code takes value 1 if inappropriate class of risk factor is identified. 
#'				It takes value 2 if maximum number of categories exceeds 10, while 3 if 
#'				there are any problem with weights of evidence (WoE) calculations 
#'				(usually if any bin contains only good or bad cases).
#'				If validation 1 and 3 are observed, risk factor is not process for WoE replacement.
#'   \item comment: Reason description.
#'}
#'@param db Data frame of categorical risk factors and target variable supplied for WoE coding.
#'@param target Name of target variable within \code{db} argument..
#'@return The command \code{replace.woe} returns the list of two data frames. The first one contains WoE replacement 
#'	    of analyzed risk factors' modalities, while the second data frame reports results of above 
#'        mentioned validations regarding class of the risk factors, number of modalities and WoE calculation.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(gcd)
#'#categorize numeric risk factor
#'gcd$maturity.bin <- ndr.bin(x = gcd$maturity, y = gcd$qual, y.type = "bina")[[2]]
#'gcd$amount.bin <- ndr.bin(x = gcd$amount, y = gcd$qual, y.type = "bina")[[2]]
#'gcd$age.bin <- ndr.bin(x = gcd$age, y = gcd$qual, y.type = "bina")[[2]]
#'head(gcd)
#'#replace modalities with WoE values
#'woe.rep <- replace.woe(db = gcd, target = "qual")
#'#results overview
#'head(woe.rep[[1]])
#'woe.rep[[2]]
#'@import monobin
#'@import dplyr
#'@export
replace.woe <- function(db, target) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	if	(!target%in%names(db)) {
		stop("Target variable does not exist in supplied db.")
		}
	y <- db[, target]
	cond.00 <- !sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])
	if	(cond.00) {
		stop("Target is not 0/1 variable.")
		}
	rf <- names(db)[!names(db)%in%target]
	rfl <- length(rf)
	if	(rfl == 0) {
		stop("There are no risk factors in supplied db.")
		}
	res <- vector("list", rfl)
	info <- vector("list", rfl)
	for	(i in 1:rfl) {
		xl <- rf[i]
		x <- db[, xl]
		cond.01 <- !(is.character(x) | is.factor(x) | is.logical(x))
		cond.02 <- length(unique(x)) > 10
		if	(cond.01) {
			info[[i]] <- data.frame(rf = xl, 
							reason.code = 1, 
							comment = "Inappropriate class. It has to be one of: character, factor or logical.")
			res[[i]] <- data.frame(x)
			names(res[[i]]) <- xl
			next
			}
		if	(cond.02) {
			info[[i]] <- data.frame(rf = xl, 
							reason.code = 2,
							comment = "More than 10 categories.")
			}
		woe.res <- woe.tbl(tbl = db, x = xl, y = target)
		cond.03 <- any(woe.res$woe%in%c(NA, NaN, Inf))
		if	(cond.03) {
			info[[i]] <- data.frame(rf = xl, 
							reason.code = 3,
							comment = "Problem with WoE calculation (NA, NaN, Inf)")
			res[[i]] <- data.frame(x)
			names(res[[i]]) <- xl
			next
			}
		woe.val <- woe.res$woe
		names(woe.val) <- woe.res$bin	
		woe.rep <- unname(woe.val[x])
		res[[i]] <- data.frame(woe.rep)	
		names(res[[i]]) <- xl
		}
	res <- data.frame(bind_cols(res), check.names = FALSE)
	res <- cbind.data.frame(db[, target, drop = FALSE], res)
	info <- data.frame(bind_rows(info))
return(list(results = res, info = info))
}



