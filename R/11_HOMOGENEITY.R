#' Testing homogeneity of the PD rating model
#'
#' \code{homogeneity} performs homogeneity testing of PD model based on the rating grades and selected segment.
#' This test is usually applied on application portfolio, but it can be applied also on model development sample. 
#' Additionally, this method requires higher number of observations per segment modalities within each rating in order
#' to produce available results. For segments with less than 30 observations, test is not performed.
#' If as a segment user selects numeric variable from the application portfolio, variable will be grouped in selected number of
#' groups (argument \code{segment.num}).
#'@param app.port Application portfolio (data frame) which contains default indicator (0/1),
#'			ratings in use and variable used as a segment.
#'@param def.ind Name of the column that represents observed default indicator (0/1).
#'@param rating Name of the column that represent rating grades in use.
#'@param segment Name of the column that represent testing segments. If it is of numeric type, than it is first grouped
#'		     into \code{segment.num} of groups otherwise is it used as supplied.
#'@param segment.num Number of groups used for numeric variables supplied as a segment. Only applicable if \code{segment}
#'			   is of numeric type.
#'@param alpha Significance level of p-value for two proportion test. Default is 0.05.
#'@details
#' Testing procedure is implemented for each rating separately comparing default rate from one segment modality to  
#' the default rate from the rest of segment modalities.
#'@return The command \code{homogeneity} returns a data frame with the following columns:
#' \itemize{
#'   \item segment.var: Variable used as a segment.
#'   \item rating: Unique values of rating grades from application portfolio..
#'   \item segment.mod: Tested segment modality. Default rate from this segment is compared with default rate from the
#'				rest of the modalities within the each rating.
#'   \item no: Number of observations of the analyzed rating.
#'   \item nb: Number of defaults (bad cases) of the analyzed rating.
#'   \item no.segment: Number of observations of the analyzed segment modality.
#'   \item no.rest: Number of observations of the rest of the segment modalities.
#'   \item nb.segment: Number of defaults of the analyzed segment modality.
#'   \item nb.rest: Number of defaults of the rest of the segment modalities.
#'   \item p.val: Two proportion test (two sided) p-value.
#'   \item alpha: Selected significance level.
#'   \item res: Accepted hypothesis.
#'}
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#estimate some dummy model
#'mod.frm <- `Creditability` ~ `Account Balance` + `Duration of Credit (month)` +
#'					`Age (years)` + `Value Savings/Stocks` + 
#'					`Duration in Current address`
#'lr.mod <- glm(mod.frm, family = "binomial", data = loans)
#'summary(lr.mod)$coefficients
#'#model predictions
#'loans$pred <-  unname(predict(lr.mod, type = "response", newdata = loans))
#'#scale probabilities
#'loans$score <- scaled.score(probs = loans$pred, score = 600, odd = 50/1, pdo = 20)
#'#group scores into ratings
#'loans$rating <- ndr.bin(x = round(loans$score), y = loans$Creditability, y.type = "bina")[[2]]
#'#simulate dummy application portfolio (oversample loans data) 
#'set.seed(2211)
#'app.port <- loans[sample(1:nrow(loans), 2500, rep = TRUE), ]
#'#run homogeneity test on ratings based on the Credit Amount segments
#'homogeneity(app.port = app.port, 
#'		def.ind = "Creditability", 
#'		rating = "rating", 
#'		segment = "Credit Amount",
#'		segment.num = 4, 
#'		alpha = 0.05)
#'@importFrom stats prop.test complete.cases
#'@export


homogeneity <- function(app.port, def.ind, rating, segment, segment.num, alpha = 0.05) {
	if	(!is.data.frame(app.port)) {
		stop("app.port is not a data frame.")
		}
	if	(any(!c(def.ind, rating, segment)%in%names(app.port))) {
		stop("def.ind and/or rating and/or segment do not exist in supplied app.port data frame.")
		}
	if	(alpha < 0 | alpha > 1) {
		stop("alpha has to be between 0 and 1.")
		}
	y <- app.port[, def.ind]
	cond.00 <- !sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])
	if	(cond.00) {
		stop("def.ind has to be 0/1 variable.")
		}
	cc <- complete.cases(app.port[, c(def.ind, rating, segment)])
	app.port <- app.port[cc, ]
	if	(nrow(app.port) == 0) {
		stop("No complete cases for app.port.")
		}
	if	(any(!cc)) {
		warning("There are some incomplete cases. Check def.ind, rating and segment columns.")
		}
	if	(length(segment) > 1) {
		stop("segment argument has to be of length one.")
		}
	seg <- app.port[, segment]
	if	(is.numeric(seg)) {
		seg.ul <- length(unique(seg))
		if	(seg.ul > 4) {
			seg <- cut(seg, breaks = segment.num, include.lowest = TRUE, dig.lab = 6)
			}
		}
	ratings <- app.port[, rating]
	defaults <- app.port[, def.ind]
	seg.unique <- sort(unique(seg))
	rat.unique <- sort(unique(app.port[, rating]))
	ru.l <- length(rat.unique)
	res <- vector("list", ru.l)
	for	(i in 1:ru.l) {
		rat.g = ratings[ratings%in%rat.unique[i]] 
		def = defaults[ratings%in%rat.unique[i]]
		rat.s = seg[ratings%in%rat.unique[i]]
		res[[i]] <- t2p(rat.g = rat.g, def = def, rat.s = rat.s, alpha = alpha)
		}
	res <- bind_rows(res)
	res <- cbind.data.frame(segment.var = segment, res)

return(res)
}

t2p <- function(rat.g, def, rat.s, alpha) {
	su <- sort(unique(rat.s))
	sul <- length(su)
	res <- vector("list", sul)
	for	(i in 1:sul) {
		su.l <- su[i]
		nb1 <- sum(def[rat.s%in%su.l])
		nb2 <- sum(def[!rat.s%in%su.l])
		no1 <- sum(rat.s%in%su.l)
		no2 <- sum(!rat.s%in%su.l)
		if	(no1 < 30 | no2 < 30) {
			p.val <- NA
			com <- "Less than 30 observations."
			} else {
			p.val <- prop.test(x = c(nb1, nb2), n = c(no1, no2), alternative = "two.sided", 
						 correct = FALSE)$p.value
			com <- ifelse(p.val >= alpha,
				        paste0("H0: DR(", su.l, ") =="," DR(rest)"),
					  paste0("H1: DR(", su.l, ") !="," DR(rest)"))

			}
		res.l <- data.frame(rating = unique(rat.g), segment.mod = su.l, no = length(def), nb = sum(def), 
					  no.segment = no1, no.rest = no2,
					  nb.segment = nb1, nb.rest = nb2, p.val = p.val, alpha = alpha, 
					  res = com)
		res[[i]] <- res.l
		}
	res <- bind_rows(res)
return(res)
}




