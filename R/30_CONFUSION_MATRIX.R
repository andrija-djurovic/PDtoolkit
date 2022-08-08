#' Confusion matrix
#'
#' \code{confusion.matrix} returns confusion matrix along with accompanied performance metrics.
#'@param predictions Model predictions.
#'@param observed Observed values of target variable.
#'@param cutoff Cutoff value. Single value numeric vector between 0 and 1.
#'@return The command \code{confusion.matrix} returns list of two objects. The first object is confusion matrix table,
#'	    while the second one is data frame with accompanied performance metrics.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#identify numeric risk factors
#'num.rf <- sapply(loans, is.numeric)
#'num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#'#discretized numeric risk factors using mdt.bin from monobin package
#'loans[, num.rf] <- sapply(num.rf, function(x) 
#'				  mdt.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#'str(loans)
#'res <- stepFWD(start.model = Creditability ~ 1, 
#'	   p.value = 0.05, 
#'	   coding = "WoE",
#'	   db = loans)
#'names(res)
#'summary(res$model)$coefficients
#'loans$model.pred <- predict(res$model, type = "response")
#'#confusion matrix
#'confusion.matrix(predictions = predict(res$model, type = "response"), 
#'		     observed = loans$"Creditability",
#'		     cutoff = 0.5)
#'@export
confusion.matrix <- function(predictions, observed, cutoff) {
	cc <- complete.cases(predictions, observed)
	predictions <- predictions[cc]; observed <- observed[cc]
	if	(!all(predictions >= 0 & predictions <= 1)) {
		stop("predictions should be betweeen 0 and 1")
		}
	if	(!sum(observed%in%c(0, 1)) == length(observed)) {
		stop("observed is not 0/1 variable.")
		}
	if	(!(cutoff[1] > 0 &  cutoff[1] < 1)) {
		stop("cutoff should be betweeen 0 and 1")
		}
	predicted <- ifelse(predictions <= cutoff, 0, 1)
	conf.mat <- table(observed, predicted)
	metrics <- cm.metrics(conf.mat = conf.mat)
	res <- list("confusion.matrix" = conf.mat, metrics = metrics)
return(res)
}

cm.metrics <- function(conf.mat) {
	total.cases <- sum(conf.mat)
	accuracy <- sum(diag(conf.mat)) / total.cases
	error.rate <- 1 - accuracy
	sensitivity <- conf.mat[2, 2] / sum(conf.mat[2, ])
	specificity <- conf.mat[1, 1] / sum(conf.mat[1, ])
	precision <- conf.mat[2, 2] / sum(conf.mat[, 2])
	f1.score <- 2 * (precision * sensitivity)/(precision + sensitivity)
	false.positive <- 1 - specificity
	false.discovery <- 1 - precision
	metrics <- data.frame(metric = c("accuracy", "error rate", "sensitivity",
						   "specificity", "precision", "f1.score",  
						   "false positive", "false discovery"),
				    value = c(accuracy, error.rate, sensitivity,
					      specificity, precision, f1.score, 
					      false.positive, false.discovery))
	metrics$metric <- factor(metrics$metric, levels = metrics$metric, ordered = TRUE)
return(metrics)
}

#' Palette of cutoff values that minimize and maximize metrics from the confusion matrix
#'
#' \code{cutoff.palette} returns confusion matrix along with accompanied performance metrics.
#'@param predictions Model predictions.
#'@param observed Observed values of target variable.
#'@param min.pct.obs Minimum percentage of observations. Used to select boundaries of cutoff values. Default value is 0.05.
#'@param min.pct.def Minimum percentage of default. Used to select boundaries of cutoff values. Default value is 0.01.
#'@return The command \code{cutoff.palette} returns data frame with minimum and maximum values of each confusion
#'	    matrix metric along with optimized cutoff itself.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#identify numeric risk factors
#'num.rf <- sapply(loans, is.numeric)
#'num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#'#discretized numeric risk factors using mdt.bin from monobin package
#'loans[, num.rf] <- sapply(num.rf, function(x) 
#'				  mdt.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#'str(loans)
#'res <- stepFWD(start.model = Creditability ~ 1, 
#'	   p.value = 0.05, 
#'	   coding = "WoE",
#'	   db = loans)
#'#run cutoff optimization
#'cop <- cutoff.palette(predictions = predict(res$model, type = "response"), 
#'				observed = loans$"Creditability",
#'				min.pct.obs = 0.05, 
#'				min.pct.def = 0.01)
#'cop
#run confustion matrix for optimized cutoff
#'confusion.matrix(predictions = predict(res$model, type = "response"), 
#'		     observed = loans$"Creditability",
#'		     cutoff = cop$cutoff.max[cop$metric%in%"f1.score"])
#'@export
cutoff.palette <- function(predictions, observed, min.pct.obs = 0.05, min.pct.def = 0.01) {
	cc <- complete.cases(predictions, observed)
	predictions <- predictions[cc]; observed <- observed[cc]
	if	(!all(predictions >= 0 & predictions <= 1)) {
		stop("predictions should be betweeen 0 and 1")
		}
	if	(!sum(observed%in%c(0, 1)) == length(observed)) {
		stop("observed is not 0/1 variable.")
		}
	if	(min.pct.obs < 0.05 | min.pct.obs > 0.5) {
		stop("min.pct.obs has to be between 0.05 and 0.5.")
		}
	if	(min.pct.def < 0.01 | min.pct.def > 0.99) {
		stop("min.pct.def has to be between 0.01 and 0.99.")
		}
	mdb.r <- length(observed)
	mdb.d <- sum(observed)
	min.obs <- ceiling(ifelse(mdb.r * min.pct.obs < 30, 30, mdb.r * min.pct.obs))
	min.rate <- ceiling(ifelse(mdb.d * min.pct.def < 1, 1, mdb.d * min.pct.def))
	pred.r <- range(predictions)
	cp.seq <- seq(pred.r[1], pred.r[2], length.out = 102)[-c(1, 102)]
	cpl <- length(cp.seq)
	#check candidates based on min.obs and min.def
	min.obs.lt <- rep(NA, cpl); min.obs.ut <- rep(NA, cpl)
	min.def.lt <- rep(NA, cpl); min.def.ut <- rep(NA, cpl); 
	for	(i in 1:cpl) {
		cp.l <- cp.seq[i]
		min.obs.lt[i] <- sum(predictions <= cp.l) >= min.obs
		min.obs.ut[i] <- sum(predictions >= cp.l) >= min.obs 
		min.def.lt[i] <- sum(observed[predictions <= cp.l]) >=  min.rate
		min.def.ut[i] <- sum(observed[predictions >= cp.l]) >= min.rate 
		}
	av <- which(min.obs.lt & min.obs.ut & min.def.lt & min.def.ut)
	lt <- min(av)
	ut <- max(av)
	if	(ut < lt) {
		stop("no cutoff candidates can be found for selected min.pct.obs & min.pct.def")
		}
	cp.seq <- cp.seq[lt:ut]
	cp.s <- lapply(cp.seq, function(x) { 
				     predicted <- ifelse(predictions <= x, 0, 1)
				     cm <- cm.metrics(conf.mat = table(observed, predicted))
				     cbind.data.frame(cm, cutoff = x)
				     })
	res <- cp.s %>% 
		 bind_rows() %>%
		 group_by(metric) %>%
		 summarise(min = min(value),
			     max = max(value),
			     cutoff.min = unique(cutoff[value == min(value)])[1],
			     cutoff.max = unique(cutoff[value == max(value)])[1])
	res <- data.frame(res)
return(res)
}

