#' Model segment validation 
#'
#' \code{segment.vld} performs model segment validation based on residuals. 
#' The main goal of this procedure is to identify segments where model in use overestimates or
#' underestimates true default rate. The procedure consists of a few steps. The first step is to 
#' calculate the model residuals (observed default indicator minus estimated probability). 
#' Then, on obtained residuals, the regression tree is fitted for segment identification.
#' Finally, one proportion test is applied in order to test overestimation or underestimation 
#' of the true default rate within these segments. Results of this validation can indicate
#' omission of some important risk factor(s) or some specific sub-portfolio for which model performs
#' worse than for the rest of the portfolio. 
#'@param model Model in use, an object of class inheriting from \code{"glm"}
#'@param db Modeling data with risk factors and target variable. Risk factors used for \code{model} development
#'		have to be of the same type (if WoE coding is used it has to be numeric with WoE values).
#'		Additionally, the rest of the risk factors (these that are supplied in \code{db}, but not used
#'		for \code{model} development will be used for segment validation.
#'@param alpha Threshold for p-value for one proportion test. Default is 0.05.
#'@return The command \code{segment.vld} returns a list of three objects.\cr
#'	    The first object (\code{segment.model}), returns regression tree results (\code{rpart} object).\cr
#'	    The second object (\code{segment.testing}), is the data frame with segment overview and testing results.\cr
#'	    The third object (\code{segment.rules}), is the data frame with average residual
#'	    rate and rules for segment identification. This elements is returned, only if the segments are
#'	    identified, otherwise it is\code{NULL}.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'library(rpart)
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
#'#run segment validation procedure
#'seg.analysis <- segment.vld(model = final.model, 
#'					db = res$dev.db,
#'					alpha = 0.05)
#'#check output elements
#'names(seg.analysis)
#'#print segment model - regression tree
#'seg.analysis$segment.model
#'#print segment summary and statistical testing
#'seg.analysis$segment.testing
#'#print segment identification rules
#'seg.analysis$segment.rules
#'@import rpart
#'@importFrom stats prop.test
#'@export
segment.vld <- function(model, db, alpha = 0.05) {
	model.vars <- names(model$model)
	if	(length(names(db)[!names(db)%in%model.vars]) == 0) {
		stop("No additional risk factors for analysis.")
		}
	target <- model.vars[1]
	rf.model <- model.vars[-1]
	db$mpred <- unname(predict(model, type = "response", newdata = db))
	db$error <- db[, target] - db$mpred
	tree.vars <- names(db)[!names(db)%in%c(target, "mpred", rf.model)]
	min.leaf <- round(0.03 * nrow(db)) 
	min.leaf <- ifelse(min.leaf < 30, 30, min.leaf)
	reg.tree <- rpart(error ~ ., method = "anova", data = db[, tree.vars, drop = FALSE],
				control = rpart.control(minsplit = min.leaf,
								minbucket = min.leaf))
	tree.rules <- extract.rules(model = reg.tree)
	if	(is.null(tree.rules)) {
		info <- "No significant split of residuals."
		seg.overview <- data.frame(info = info)
		} else {
		db$segment <- unname(reg.tree$where)
		seg.overview <- db %>% 
				    group_by(segment) %>%
				    summarise(no = n(),
						  ng.obs = sum(1 - !!sym(target)),
						  ng.mod = sum(1 - mpred),
						  nb.obs = sum(!!sym(target)),
						  nb.mod = sum(mpred)) %>% 
				    mutate(dr.obs = nb.obs / no,
					     dr.mod = nb.mod / no,
					     dr.diff = dr.mod - dr.obs)
		stat.test <- apply(seg.overview, 1, function(x) {
					 p.val = prop.test(x = x["nb.mod"], 
								 n = x["no"], 
							 	 p = x["dr.obs"],
								 alternative = ifelse(x["nb.mod"] <= x["nb.obs"], 
											    "less", "greater"),
								 correct = FALSE)$p.val})
		seg.overview <- cbind.data.frame(seg.overview, p.val = stat.test, alpha = alpha)	
		seg.overview$test.res <- ifelse(seg.overview$p.val < alpha,
					 	 ifelse(seg.overview$nb.mod > seg.overview$nb.obs,
						  	  "overestimate", "underestimate"), "equal")
		seg.overview <- data.frame(seg.overview)
		}
	res <- list(segment.model = reg.tree, 
			segment.testing = seg.overview,
			segment.rules = tree.rules)
return(res)
}

extract.rules <- function(model = reg.tree) {
	mod.frm <- model$frame
	mod.frm <- mod.frm[mod.frm[, 1]%in%"<leaf>", ]
	leaf.num <- row.names(mod.frm)
	nr <- nrow(mod.frm)
	if	(nr == 1) {
		return(NULL)
		}
	rules <- vector("list", nr)
	avgs <- rep(NA, nr)
	for	(i in 1:nr) {
		avgs[i] <- mod.frm[i, "yval"]
		rules[[i]] <- path.rpart(model, nodes = as.numeric(leaf.num[i]), print.it = FALSE)
		}
	rules <- lapply(rules, function(x) {
				     a <- unname(paste0(x[[1]][-1], collapse = " & "))
				     df.r <- cbind.data.frame(rule = a)
				     return(df.r)
				     })
	rules <- cbind.data.frame(avg = avgs, bind_rows(rules))
return(rules)
}



