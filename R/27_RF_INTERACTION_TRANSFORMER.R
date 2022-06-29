#' Extract interactions from random forest
#'
#' \code{rf.interaction.transformer} extracts the interactions from random forest.
#' It implements customized random forest algorithm that takes into account different conditions (for single decision tree) such as minimum 
#' percentage of observations and defaults in each node, maximum tree depth and monotonicity condition 
#' at each splitting node. Gini index is used as metric for node splitting .
#'@param db Data frame of risk factors and target variable supplied for interaction extraction.
#'@param rf Character vector of risk factor names on which decision tree is run.
#'@param target Name of target variable (default indicator 0/1) within db argument.
#'@param num.rf Number of risk factors randomly selected for each decision tree. If default value (\code{NA}) is supplied, 
#'		then number of risk factors will be calculated as \code{sqrt(number of all supplied risk factors)}.
#'@param num.tree Number of decision trees used for random forest.
#'@param min.pct.obs Minimum percentage of observation in each leaf.
#'@param min.avg.rate Minimum percentage of defaults in each leaf.
#'@param max.depth Maximum number of splits.
#'@param monotonicity Logical indicator. If \code{TRUE}, observed trend between risk factor and target will be preserved
#'			    in splitting node.
#'@param create.interaction.rf Logical indicator. If \code{TRUE}, second element of the output will be data frame with
#'					 interaction modalities.
#'@param seed Random seed to ensure result reproducibility.
#'@return The command \code{rf.interaction.transformer} returns a list of two data frames. The first data frame provides
#' the trees summary. The second data frame is a new risk factor extracted from random forest.
#'@examples
#modify risk factors in order to show how the function works with missing values
#loans$"Account Balance"[1:10] <- NA
#loans$"Duration of Credit (month)"[c(13, 15)] <- NA
#rf.it <- rf.interaction.transformer(db = loans, 
#				     rf = names(loans)[!names(loans)%in%"Creditability"], 
#				     target = "Creditability",
#				     num.rf = NA, 
#				     num.tree = 3,
#				     min.pct.obs = 0.05,
#				     min.avg.rate = 0.01,
#				     max.depth = 2,
#				     monotonicity = TRUE,
#				     create.interaction.rf = TRUE,
#				     seed = 579)
#names(rf.it)
#rf.it[["tree.info"]]
#tail(rf.it[["interaction"]])
#table(rf.it[["interaction"]][, 1], useNA = "always")
#'@export
rf.interaction.transformer <- function(db, rf, target, num.rf = NA, num.tree, min.pct.obs, min.avg.rate, max.depth, 
						   monotonicity, create.interaction.rf, seed = 991) {
	if	(num.tree[1] < 1) {
		stop("num.tree argument has to be single integer greater than 0.")
		}
	num.tree <- round(num.tree)
	rfl <- length(rf)
	if	(is.na(num.rf)) {
		num.rf <- round(sqrt(rfl))
		} else {
		num.rf <- ifelse(num.rf > rfl, rfl, rfl)
		}
	nr.db <- nrow(db)
	obs.ss <- round(2 / 3 * nr.db)
	tree.info <- vector("list", num.tree )
	for	(i in 1:num.tree) {
		set.seed(seed + (i - 1))
		indx <- sample(1:nr.db, obs.ss, replace = TRUE)
		rf.l <- sample(rf, num.rf, replace = FALSE)
		tree.info.l <- interaction.transformer(db = db[indx, ],
						       rf = rf.l, 
						       target = target,
						       min.pct.obs = min.pct.obs,
						       min.avg.rate = min.avg.rate,
						       max.depth = max.depth,
						       monotonicity = monotonicity,
						       create.interaction.rf = FALSE)[[1]]
		tree.info.l <- cbind.data.frame(tree = i, tree.info.l)
		tree.info[[i]] <- tree.info.l
		}
	if	(create.interaction.rf) {
		interaction <- vector("list", num.tree) 
		for	(i in 1:1:num.tree) {
			ti.l <- tree.info[[i]]
			inter.l <- extract.interactions(db = db, tree.info = ti.l)
			names(inter.l) <- paste0("tree.", i)
			interaction[[i]] <- inter.l
			}
		} else {
		interaction <- data.frame()
		}
	tree.res <- bind_rows(tree.info)
	interaction <- bind_cols(interaction)
return(list(tree.info = tree.res, interaction = interaction))
}


