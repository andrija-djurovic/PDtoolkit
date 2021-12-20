#' K-fold model cross-validation
#'
#' \code{kfold.vld} performs k-fold model cross-validation. 
#' The main goal of this procedure is to generate main model performance metrics such as absolute mean
#' square error, root mean square error or area under curve (AUC) based on resampling method.
#'@param model Model in use, an object of class inheriting from \code{"glm"}
#'@param k Number of folds. If \code{k} is equal or greater than the number of observations of 
#'	     modeling data frame, then validation procedure is equivalent to leave one out cross-validation (LOOCV)
#'	     method. For LOOCV, AUC is not calculated. Default is set to 10.
#'@param seed Random seed needed for ensuring the result reproducibility. Default is 1984.
#'@return The command \code{kfold.vld} returns a list of two objects.\cr
#'	    The first object (\code{iter}), returns iteration performance metrics.\cr
#'	    The second object (\code{summary}), is the data frame of iterations averages of performance metrics.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#identify numeric risk factors
#'num.rf <- sapply(loans, is.numeric)
#'num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#'#discretized numeric risk factors using ndr.bin from monobin package
#'loans[, num.rf] <- sapply(num.rf, function(x) 
#'	ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#'str(loans)
#'#run stepMIV
#'res <- stepMIV(start.model = Creditability ~ 1, 
#'		   miv.threshold = 0.02, 
#'		   m.ch.p.val = 0.05,
#'		   coding = "WoE",
#'		   db = loans)
#'#check output elements
#'names(res)
#'#extract the final model
#'final.model <- res$model
#'#print coefficients
#'summary(final.model)$coefficients
#'#print head of coded development data
#'head(res$dev.db)
#'#calculate AUC
#'auc.model(predictions = predict(final.model, type = "response", newdata = res$dev.db),
#'	    observed = res$dev.db$Creditability)
#'kfold.vld(model = final.model, k = 10, seed = 1984)
#'@import monobin
#'@importFrom stats as.formula
#'@export
kfold.vld <- function(model, k = 10, seed = 1984) {
	if	(any(!c("glm", "lm")%in%class(model))) {
		stop("model has to be of glm class.")
		}
	frm <- model$formula
	trg <- all.vars(as.formula(frm))[1]
	db <- model$data
	if	(k < 0) {
		stop("k cannot be negative.")
		}
	if	(k > nrow(db)) {
		k <- nrow(db)
		warning("k corrected to have LOOCV method.")
		}
	set.seed(seed)
	db <- db[sample(1:nrow(db), replace = FALSE), ]
	cv.folds <- cut(1:nrow(db), breaks = k, label = FALSE)
	if	(k == nrow(db)) {
		warning("Model equivalent to LOOCV method. AUC cannot be calculated.")
		}
	if	(k != nrow(db) & any(table(cv.folds) < 30)) {
		warning("AUC calculation can be unrealiable for the folds with less than 30 observations.")
		}
	k.seq <- 1:k
	res <- vector("list", k)
	for	(i in 1:k) {
		k.l <- k.seq[i]
		db.est <- db[!cv.folds%in%k.l, ]	
		db.vld <- db[cv.folds%in%k.l, ]
		vld.no <- nrow(db.vld)
		lrm <- glm(formula = frm, family = "binomial", data = db.est)
		lrm.p <- unname(predict(lrm, type = "response", newdata = db.vld))
		auc.l <- auc.model(predictions = lrm.p, observed = db.vld[, trg])
		res[[i]] <- data.frame(k = k.l, 
					     no = vld.no, 
					     amse = sum(abs(lrm.p - db.vld[, trg])^2) / vld.no,
					     rmse =  sqrt(sum((lrm.p - db.vld[, trg])^2) / vld.no),
					     auc = auc.l)
		}
	res <- bind_rows(res)		
	res.s <- res %>% summarise(amse = mean(amse), rmse = mean(rmse), auc = mean(auc))
return(list(iter = res, summary = res.s))
}


#' Bootstrap model validation
#'
#' \code{boots.vld} performs bootstrap model validation. 
#' The goal of this procedure is to generate main model performance metrics such as absolute mean
#' square error, root mean square error or area under curve (AUC) based on resampling method.
#'@param model Model in use, an object of class inheriting from \code{"glm"}.
#'@param B Number of bootstrap samples. Default is set to 1000. 
#'@param seed Random seed needed for ensuring the result reproducibility. Default is 1122.
#'@return The command \code{boots.vld} returns a list of two objects.\cr
#'	    The first object (\code{iter}), returns iteration performance metrics.\cr
#'	    The second object (\code{summary}), is the data frame of iterations averages of performance metrics.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#run stepMIV
#'res <- stepMIV(start.model = Creditability ~ 1, 
#'		   miv.threshold = 0.02, 
#'		   m.ch.p.val = 0.05,
#'		   coding = "WoE",
#'		   db = loans)
#'#check output elements
#'names(res)
#'#extract the final model
#'final.model <- res$model
#'#print coefficients
#'summary(final.model)$coefficients
#'#print head of coded development data
#'head(res$dev.db)
#'#calculate AUC
#'auc.model(predictions = predict(final.model, type = "response", newdata = res$dev.db),
#'	    observed = res$dev.db$Creditability)
#'boots.vld (model = final.model, B = 10, seed = 1122)
#'@importFrom stats as.formula
#'@export
boots.vld <- function(model, B = 1000, seed = 1122) {
	if	(any(!c("glm", "lm")%in%class(model))) {
		stop("model has to be of glm class.")
		}
	frm <- model$formula
	trg <- all.vars(as.formula(frm))[1]
	db <- model$data
	if	(B < 0) {
		stop("B cannot be negative.")
		}
	if	(B > 10e6) {
		warning("Number of bootstrap samples too high. Process can take too long to complete.")
		}
	res <- vector("list", B)
	for	(i in 1:B) {
		set.seed(seed + (i - 1))
		db.l <- db[sample(1:nrow(db), replace = TRUE), ]
		db.nr <- nrow(db.l)
		lrm <- glm(formula = frm, family = "binomial", data = db.l)
		lrm.p <- unname(predict(lrm, type = "response", newdata = db.l))
		auc.l <- auc.model(predictions = lrm.p, observed = db.l[, trg])
		res[[i]] <- data.frame(B = i,
					     amse = sum(abs(lrm.p - db.l[, trg])^2, na.rm = TRUE) / db.nr,
					     rmse =  sqrt(sum((lrm.p - db.l[, trg])^2, na.rm = TRUE) / db.nr),
					     auc = auc.l)
		}
	res <- bind_rows(res)		
	res.s <- res %>% summarise(amse = mean(amse), rmse = mean(rmse), auc = mean(auc))
return(list(iter = res, summary = res.s))
}







