#' Ensemble blocks regression
#'
#' \code{ensemble.blocks} performs blockwise regression where the predictions of each blocks' model are 
#' integrated into a final model. The final model is estimated in the form of logistic regression without
#' any check of the estimated coefficients (e.g. statistical significance or sign of the estimated coefficients).
#'@seealso \code{\link{staged.blocks}}, \code{\link{embedded.blocks}}, \code{\link{stepMIV}}, \code{\link{stepFWD}} and \code{\link{stepRPC}}.
#'@param method Regression method applied on each block. 
#'		    Available methods: \code{"stepMIV"}, \code{"stepFWD"} or \code{"stepRPC"}.
#'@param target Name of target variable within \code{db} argument.
#'@param db Modeling data with risk factors and target variable. 
#'@param coding Type of risk factor coding within the model. Available options are: \code{"WoE"} and
#'		    \code{"dummy"}. If \code{"WoE"} is selected, then modalities of the risk factors are replaced
#'		    by WoE values, while for \code{"dummy"} option dummies (0/1) will be created for \code{n-1} 
#'		    modalities where \code{n} is total number of modalities of analyzed risk factor.
#'@param blocks Data frame with defined risk factor groups. It has to contain the following columns: \code{rf} and 
#'		    \code{block}.
#'@param p.value Significance level of p-value for the estimated coefficient. For \code{WoE} coding this value is
#'		     is directly compared to p-value of the estimated coefficient, while for \code{dummy} coding
#'		     multiple Wald test is employed and its p-value is used for comparison with selected threshold (\code{p.value}).
#'		     This argument is applicable only for \code{"stepFWD"} and \code{"stepRPC"} selected methods.
#'@param miv.threshold MIV (marginal information value) entrance threshold applicable only for code{"stepMIV"} method. 
#'			     Only the risk factors with MIV higher than the threshold are candidate for the new model. 
#'			     Additional criteria is that MIV value should significantly separate
#'			     good from bad cases measured by marginal chi-square test. 
#'@param m.ch.p.val Significance level of p-value for marginal chi-square test applicable only for code{"stepMIV"} method. 
#'			  This test additionally supports MIV value of candidate risk factor for final decision.
#'@return The command \code{embeded.blocks} returns a list of three objects.\cr
#'	    The first object (\code{model}) is the list of the models of each block (an object of class inheriting from \code{"glm"}).\cr
#'	    The second object (\code{steps}), is the data frame with risk factors selected from the each block.\cr
#'	    The third object (\code{dev.db}), returns the list of block's model development databases.\cr
#'@references 
#'Anderson, R.A. (2021). Credit Intelligence & Modelling, Many Paths through the Forest of Credit Rating and Scoring,
#'			 OUP Oxford
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
#'#create risk factor priority groups
#'rf.all <- names(loans)[-1]
#'set.seed(22)
#'blocks <- data.frame(rf = rf.all, block = sample(1:3, length(rf.all), rep = TRUE))
#'blocks <- blocks[order(blocks$block), ]
#'blocks
#'#method: stepRPC
#'res <- ensemble.blocks(method = "stepRPC", 
#'			      target = "Creditability",
#'			      db = loans,
#'			      coding = "dummy",  
#'			      blocks = blocks, 
#'			      p.value = 0.05)
#'names(res)
#'nb <- length(res[["models"]])
#'res$models[[nb]]
#'auc.model(predictions = predict(res$models[[nb]], type = "response", 
#'					    newdata = res$dev.db[[nb]]),
#'	      observed = res$dev.db[[nb]]$Creditability)
#'@import monobin
#'@importFrom stats formula coef vcov
#'@export
ensemble.blocks <- function(method, target, db, coding = "WoE", blocks, 
				  p.value = 0.05, miv.threshold = 0.02, m.ch.p.val = 0.05) {
	method.opt <- c("stepMIV", "stepFWD", "stepRPC")
	if	(!method%in%method.opt) {
		stop(paste0("method argument has to be one of: ", paste0(method.opt, collapse = ', '), "."))
		}
		if	(!all(c("rf", "block")%in%names(blocks))) {
		stop("blocks data frame has to contain columns: rf and block.")
		}
	if	(!all(blocks$rf%in%names(db))) {
		rp.rf.miss <- blocks$rf[!blocks$rf%in%names(db)]
		msg <- "Following risk factors from blocks are missing in supplied db: "
		msg <- paste0(msg, paste0(rp.rf.miss, collapse = ", "), ".")
		stop(msg)
		}
	names.c <- check.names(x = names(db))
	names(db) <- unname(names.c[names(db)])
	target <- unname(names.c[target])
	blocks$rf <- unname(names.c[blocks$rf])

	start.model <- as.formula(paste0(target, " ~ 1"))
	if	(method%in%"stepMIV") {
		eval.exp <- "stepMIV(start.model = start.model, 
					   miv.threshold = miv.threshold, 
					   m.ch.p.val = m.ch.p.val,
					   coding = coding,
					   coding.start.model = TRUE,
					   db = db[, c(target, rf.b)])"
		}

	if	(method%in%"stepFWD") {
		eval.exp <- "stepFWD(start.model = start.model, 
					   p.value = p.value, 
					   coding = coding,
					   coding.start.model = TRUE, 
					   check.start.model = TRUE,
					   db = db[, c(target, rf.b)])"
		}
	if	(method%in%"stepRPC") {
		eval.exp <- "stepRPC(start.model = start.model, 
					   risk.profile = data.frame(rf = rf.b, group = 1:length(rf.b)),
					   p.value = p.value, 
					   coding = coding,
					   coding.start.model = TRUE, 
					   check.start.model = TRUE,
					   db = db[, c(target, rf.b)])"
		}

	#initiate procedure
	db.eb <- data.frame(db[, target, drop = FALSE])
	blocks <- blocks[complete.cases(blocks$rf, blocks$block), ]
	bid <- unique(blocks$block)
	bidl <- length(bid)
	steps <- vector("list", bidl)
	models <- vector("list", bidl + 1)
	dev.db <- vector("list", bidl + 1)
	for	(i in 1:bidl) {
		print(paste0("--------Block: ", i, "-------"))
		bid.l <- bid[i]
		rf.b <- blocks$rf[blocks$block%in%bid.l]
		res.l <- eval(parse(text = eval.exp))
		if	(nrow(res.l$steps) == 0) {next}
		steps[[i]] <- cbind.data.frame(block = i, res.l$steps)
		models[[i]] <- res.l$model
		names(models)[i] <- paste0("block_", i)
		b.l.p <- unname(predict(res.l$model, type = "link", newdata = res.l$dev.db))
		db.eb <- cbind.data.frame(db.eb, b.l.p)
		names(db.eb)[i + 1] <- paste0("block_", i)
		dev.db[[i]] <- res.l$dev.db
		names(dev.db)[i] <- paste0("block_", i)
		}
	steps <- bind_rows(steps)
	#ensemble model
	print(paste0("-----Ensemble block----"))
	bp <- names(db.eb)[!names(db.eb)%in%target]
	eb.frm <- paste0(target, " ~ ", paste(bp, collapse = " + "))	
	eb.mod <-  glm(formula = as.formula(eb.frm), family = "binomial", data = db.eb)
	models[[bidl + 1]] <- eb.mod
	names(models)[bidl + 1] <- "ensemble_block"
	dev.db[[bidl + 1]] <- db.eb
	names(dev.db)[bidl + 1] <- "ensemble_block"
	res <- list(models = models, steps = steps, dev.db = dev.db)
return(res)
}

