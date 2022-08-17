#' Indices for K-fold validation
#'
#' \code{kfold.idx} provides indices for K-fold validation.
#'@param target Binary target variable.
#'@param k Number of folds. If \code{k} is equal or greater than the number of observations of 
#'	     target variable, then validation procedure is equivalent to leave one out cross-validation (LOOCV)
#'	     method. For stratified sampling, k is compared with frequencies of 0 and 1 from target. 
#'         Default is set to 10.
#'@param type Sampling type. Possible options are \code{"random"} and \code{"stratified"}.
#'@param seed Random seed needed for ensuring the result reproducibility. Default is 2191.
#'@return The command \code{kfold.idx} returns a list of k folds estimation and validation indices.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#good-bad ratio
#'prop.table(table(loans$Creditability))
#'#random k-folds
#'kf.r <- kfold.idx(target = loans$Creditability, k = 5, type = "random", seed = 2191)
#'lapply(kf.r, function(x) prop.table(table(loans$Creditability[x[[2]]])))
#'#stratified k-folds
#'kf.s <- kfold.idx(target = loans$Creditability, k = 5, type = "stratified", seed = 2191)
#'lapply(kf.s, function(x) prop.table(table(loans$Creditability[x[[2]]])))
#'@export
kfold.idx <- function(target, k = 10, type, seed = 2191) {
	type.opt <- c("random", "stratified")
	if	(!type%in%type.opt) {
		stop(paste0("type.opt argument has to be one of: ", paste0(type.opt, collapse = ', '), "."))
		}
	if	(k < 0) {
		stop("k cannot be negative.")
		}
	cond.t <- !sum(target[!is.na(target)]%in%c(0, 1)) == length(target[!is.na(target)])
	if	(cond.t) {
		stop("target is not 0/1 variable")
		}
	target <- target[!is.na(target)]
	tl <- length(target)
	t.tbl <- table(target)
	if	(any(t.tbl < k)) {
		k <- min(t.tbl)
		warning("k corrected to minimum of 0 or 1 frequency.")
		}
	if	(k > tl) {
		k <- tl
		if	(type%in%"stratified") {type <- "random"}
		warning("k corrected to have LOOCV method and type overried to random.")
		}
	set.seed(seed)
	if	(type%in%"random") {
		idx <- sample(1:tl, tl, replace = FALSE)
		cv.folds <- cut(1:tl, breaks = k, label = FALSE)
		} else {
		idx <- ave(c(which(target%in%0), which(target%in%1)), target, FUN = function(x) sample(x, length(x), replace = FALSE))
		target <- target[idx]
		cv.folds <- ave(1:tl, target, FUN = function(x) cut(1:length(x), breaks = k, label = FALSE))
		}
	res <- vector("list", k)
	k.l <- 1:k
	for	(i in 1:k) {
		est.fold <- which(!cv.folds%in%k.l[i])
		vld.fold <- which(cv.folds%in%k.l[i])
		res[[i]] <- list(estimation = idx[est.fold],
				     validation = idx[vld.fold])
		} 
	names(res) <- paste0("k_", 1:k)
return(res)				
}





