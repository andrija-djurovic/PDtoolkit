#' Population Stability Index (PSI)
#'
#' \code{psi} calculates Population Stability Index (PSI) for a given base and target vectors. Function can be used for 
#' testing the stability of final model score, but also for testing a risk factor stability (aka Characteristic Stability Index).
#' Function also provides so-called critical values of z-score (based on normal distribution assumption) and chi-square
#' (based on Chi-square distribution) that can be used as alternatives for fixed "rule of thumb" thresholds 
#' (10% and 25%). For details see the Reference. 
#'@param base Vector of value from base sample. Usually this is training (model development) sample.
#'@param target Vector of value from target sample. Usually this is testing or portfolio application sample.
#'@param bin Number of bins. Applied only for numeric base and target and used for discretization of its values. Default is 10.
#'@param alpha Significance level used for calculation of statistical critical values 
#'		   (\code{cv.zscore} and \code{cv.chisq}). Default is 0.05, which refers to 0.95 confidence interval. 
#'@return The command \code{psi} returns a list of two data frames. The first data frame contains values of 
#' PSI along with statistical critical values for confidence level of \code{1 - alpha}, while second data frame
#' presents summary table used for the calculation of overall PSI. For numeric base and target vectors, summary 
#' table is presented on the bin (bucket level), while for the categorical modalities of base and target vectors
#' are tabulated.
#'@references 
#' Yurdakul, B. (2018). Statistical Properties of Population Stability Index . Dissertations. 3208.
#'				downloaded from \href{https://scholarworks.wmich.edu/dissertations/3208/}{here}  
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#split on training and testing data set
#'set.seed(1122)
#'tt.indx <- sample(1:nrow(loans), 700, replace = FALSE)
#'training <- loans[tt.indx, ]
#'testing <- loans[-tt.indx, ]
#'#calculate psi for numeric risk factor
#'psi(base = training[, "Age (years)"], target = testing[, "Age (years)"], 
#'    bin = 10, alpha = 0.05)
#'#calculate psi for categorical risk factor
#'psi(base = training[, "Account Balance"], target = testing[, "Account Balance"], 
#'    bin = 10, alpha = 0.05)
#'@importFrom stats qnorm qchisq
#'@import dplyr
#'@export
psi <- function(base, target, bin = 10, alpha = 0.05) {
	if	(class(base) != class(target)) {
		stop("base and target are of different class.")
		}
	base <- base[!is.na(base)]
	target <- target[!is.na(target)]
	if	(length(base) == 0 | length(target) == 0) {
		stop("base or target of 0 length.")
		}
	if	(!is.numeric(bin) | bin <= 1 | bin > 50) {
		stop("bin has to be numeric value between 2 and 50.")
		}
	if	(!is.numeric(alpha) | alpha < 0 | alpha > 1) {
		stop("alpha has to be numeric value between 2 and 50.")		
		}
	if	(is.numeric(base)) {
		tbl.s <- num.bt(base = base, target = target, bin = bin)
		}
	if	(!is.numeric(base)) {
		tbl.s <- cat.bt(base = base, target = target)
		}
	tbl.cols <- c("n.base", "pct.base", "n.target", "pct.target")
	tbl.s[, tbl.cols] <- sapply(tbl.s[, tbl.cols], function(x) ifelse(is.na(x), 0, x))
	res <- psi.aux(tbl.s = tbl.s, alpha = alpha)
return(res)
}

num.bt <- function(base, target, bin) {
	ops <- options(scipen = 20)
	on.exit(options(ops))
	bu <- unique(base)
	if	(length(bu) <= bin) {
		base.cut <- base
		} else {
		base.cut <- cut(base, breaks = bin, incluse.lowest = TRUE, labels = FALSE, right = FALSE)
		}
	base.df <- data.frame(base = base, bin = base.cut)
	base.tbl <- base.df %>%
			group_by(bin) %>%
			summarise(min.base = min(base),
				    max.base = max(base),
				    n.base = n()) %>%
			ungroup() %>%
			mutate(pct.base = n.base / sum(n.base))
	trg.cut.pts <- c(-Inf, base.tbl$min.base[-1], +Inf)
	target.cut <- cut(target, breaks = trg.cut.pts, incluse.lowest = TRUE, labels = FALSE, 
				right = FALSE)
	target.df <- data.frame(target = target, bin = target.cut)
	target.tbl <- target.df %>%
			  group_by(bin) %>%
			  summarise(min.target = min(target),
				      max.target = max(target),
					n.target = n()) %>%
			  ungroup() %>%
			  mutate(pct.target = n.target / sum(n.target))
	bt <- merge(base.tbl, target.tbl, by = "bin", all.x = TRUE)
	bt$min.base[1] <- -Inf
	bt$max.base[nrow(bt)] <- Inf
return(bt)
}
cat.bt <- function(base, target) {
	base.df <- data.frame(base = base, bin = base)
	base.tbl <- base.df %>%
			group_by(bin) %>%
			summarise(n.base = n()) %>%
			ungroup() %>%
			mutate(pct.base = n.base / sum(n.base))
	target.df <- data.frame(target = target, bin = target)
	target.tbl <- target.df %>%
			  group_by(bin) %>%
			  summarise(n.target = n()) %>%
			  ungroup() %>%
			  mutate(pct.target = n.target / sum(n.target))
	bt <- merge(base.tbl, target.tbl, by = "bin", all = TRUE)
return(bt)
}
psi.aux <- function(tbl.s, alpha) {
	tbl.s$psi.b <- (tbl.s$pct.base - tbl.s$pct.target) * 	
		          log(tbl.s$pct.base / tbl.s$pct.target)
	ci <- 1 - alpha
	n <- sum(tbl.s$n.base)
	m <- sum(tbl.s$n.target)
	b <- nrow(tbl.s)
	cv.zscore <- (1/n + 1/m) * (b - 1) + qnorm(p = ci) * (1/n + 1/m) * sqrt(2 * (b - 1))
	cv.chisq <- qchisq(p = ci, df = b - 1) * (1/n + 1/m)
	psi.s <- data.frame(psi = sum(tbl.s$psi.b), 
			    cv.zscore = cv.zscore, 
			    "psi**" = sum((tbl.s$pct.base - tbl.s$pct.target)^2 / tbl.s$pct.base)
			    cv.chisq = cv.chisq, 
			    ci = ci,
			    check.names = FALSE)
return(list(res = psi.s, tbl = tbl.s))
}





