rm(list = ls())
suppressMessages(library(PDtoolkit))
library(rpart)
data(loans)

source( "C:\\Users\\adjurovic\\Desktop\\App\\PDtoolkit\\R\\05_STEP_MIV.R")

#identify numeric risk factors
num.rf <- sapply(loans, is.numeric)
num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#discretized numeric risk factors using ndr.bin from monobin package
loans[, num.rf] <- sapply(num.rf, function(x) 
	ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
str(loans)

#run stepMIV
res <- stepMIV(start.model = Creditability ~ 1, 
		   miv.threshold = 0.02, 
		   m.ch.p.val = 0.05,
		   coding = "WoE",
		   db = loans)
#check output elements
names(res)
#extract the final model
final.model <- res$model
#print coefficients
summary(final.model)$coefficients
#calculate AUC
auc.model(predictions = predict(final.model, type = "response", newdata = res$dev.db),
	    observed = res$dev.db$Creditability)

#run segment validation procedure
seg.analysis <- segment.vld(final.model = final.model, 
		    alpha = 0.05,
		    db = res$dev.db)
#print segment model - regression tree
seg.analysis$segment.model
#print segment summary and statistical testing
seg.analysis$segment.testing
#print segment identification rules
seg.analysis$segment.rules

segment.vld <- function(final.model, alpha = 0.05, db) {
	model.vars <- names(final.model$model)
	if	(length(names(db)[!names(db)%in%model.vars]) == 0) {
		stop("No additional risk factors for analysis.")
		}
	target <- model.vars[1]
	rf.model <- model.vars[-1]
	db$mpred <- unname(predict(final.model, type = "response", newdata = db))
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



