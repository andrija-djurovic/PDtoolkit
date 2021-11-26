#' Testing heterogeneity of the PD rating model
#'
#' \code{heterogeneity} performs heterogeneity testing of PD model based on the rating grades.
#' This test is usually applied on application portfolio, but it can be applied also on model development sample. 
#'@param app.port Application portfolio (data frame) which contains default indicator (0/1) and
#'			ratings in use.
#'@param def.ind Name of the column that represents observed default indicator (0/1).
#'@param rating Name of the column that represent rating grades in use.
#'@param alpha Threshold for p-value for two proportion test. Default is 0.05.
#'@details
#' Testing procedure starts with summarizing the number of observations and defaults per rating grade.
#' After that, two proportion test is applied on adjacent rating grades. Testing hypothesis is that 
#' default rate of grade \code{i} is less or greater than default rate of grade \code{i - 1}, where \code{i}
#' takes the values from 2 to the number of unique grades. 
#' Direction of alternative hypothesis (less or greater) is determined automatically based on correlation direction
#' of observed default on rating grades. 
#' Incomplete cases, identified based on default indicator (\code{def.ind}) and rating grade (\code{rating }) 
#' columns are excluded from the summary table and testing procedure. If identified, warning will be returned.
#'@return The command \code{heterogeneity} returns a data frame with the following columns:
#' \itemize{
#'   \item rating: Unique values of rating grades from application portfolio.
#'   \item no: Number of complete observations.
#'   \item nb: Number of defaults (bad cases) in complete observations.
#'   \item p.val: Test p-value (two proportion test of adjacent rating grades).
#'   \item alpha: Selected significance level.
#'   \item res: Accepted hypothesis.
#'}
#'@examples
#'suppressMessages(library(PDtoolkit))
#'data(loans)
#'#estimate some dummy model
#'mod.frm <- `Creditability` ~ `Account Balance` + `Duration of Credit (month)` +
#'					`Age (years)` + `Value Savings/Stocks`
#'lr.mod <- glm(mod.frm, family = "binomial", data = loans)
#'summary(lr.mod)$coefficients
#'#model predictions
#'loans$pred <-  unname(predict(lr.mod, type = "response", newdata = loans))
#'#scale probabilities
#'loans$score <- scaled.score(probs = loans$pred, score = 600, odd = 50/1, pdo = 20)
#'#group scores into ratings
#'loans$rating.1 <- sts.bin(x = round(loans$score), y = loans$Creditability, y.type = "bina")[[2]]
#'#group probabilities into ratings
#'loans$rating.2 <- sts.bin(x = round(loans$pred, 4), y = loans$Creditability, y.type = "bina")[[2]]
#'#simulate dummy application portfolio 
#'set.seed(1984)
#'app.port <- loans[sample(1:nrow(loans), 400, rep = TRUE), ]
#'#run heterogeneity test on ratings based on the scaled score
#'#higher score lower default rate
#'heterogeneity(app.port = app.port, 
#'		   def.ind = "Creditability", 
#'		   rating = "rating.1", 
#'		   alpha = 0.05)
#'#run test on predicted default rate - direction of the test is changed
#'heterogeneity(app.port = app.port, 
#'		   def.ind = "Creditability", 
#'		   rating = "rating.2", 
#'		   alpha = 0.05)
#'@importFrom stats prop.test complete.cases
#'@export

heterogeneity <- function(app.port, def.ind, rating, alpha = 0.05) {
	if	(!is.data.frame(app.port)) {
		stop("app.port is not a data frame.")
		}
	if	(any(!c(def.ind, rating)%in%names(app.port))) {
		stop("def.ind and/or rating do not exist in supplied app.port data frame.")
		}
	if	(alpha < 0 | alpha > 1) {
		stop("alpha has to be between 0 and 1.")
		}
	y <- app.port[, def.ind]
	cond.00 <- !sum(y[!is.na(y)]%in%c(0, 1)) == length(y[!is.na(y)])
	if	(cond.00) {
		stop("def.ind has to be 0/1 variable.")
		}
	cc <- complete.cases(app.port[, c(def.ind, rating)])
	app.port <- app.port[cc, ]
	if	(nrow(app.port) == 0) {
		stop("No complete cases for app.port.")
		}
	if	(any(!cc)) {
		warning("There are some incomplete cases. Check def.ind and rating columns.")
		}
	rs <- app.port %>% 
	      group_by_at(c("rating" = rating)) %>%
	      summarise(no = n(),
			    nb = sum(!!sym(def.ind))) %>%
	 	ungroup()%>%
	 	mutate(dr = nb / no)
	cor.df <- app.port[, c(def.ind, rating)]
	cor.df[, rating] <- as.numeric(factor(cor.df[, rating], 
							  levels = sort(unique(cor.df[, rating])), 
							  ordered = TRUE))
	cor.s <- sign(cor(x = cor.df[ ,rating], y = cor.df[ ,def.ind], use = "complete.obs"))
	sts <- ifelse(cor.s <= 0, "less", "greater")
	res <- t2p.neighbors(rs = rs, sts = sts, alpha = alpha)
	res <- data.frame(res)
return(res)
}

t2p.neighbors <- function(rs, sts, alpha) {
	test.exp <- "prop.test(x = c(nb2, nb1), 
				     n = c(no2, no1), alternative = sts, 
				     correct = FALSE)$p.value"
	h0 <- ifelse(sts == "less", ">=", "<=")
	h1 <- ifelse(sts == "less", "<", ">")
	rs$p.val <- NA
	rs$alpha <- alpha
	rs$res <- NA
	for	(i in 2:nrow(rs)) {
		nb1 <- rs$nb[i - 1]
		nb2 <- rs$nb[i]
		no1 <- rs$no[i - 1]
		no2 <- rs$no[i]
		rc1 <- rs$rating[i - 1]
		rc2 <- rs$rating[i]
		p.val.l <- eval(parse(text = test.exp))
		rs$p.val[i] <- p.val.l
		rs$res[i] <- ifelse(p.val.l >= alpha, 
					  paste0("H0: DR(", rc2, ") ", h0, " DR(", rc1, ")"),
					  paste0("H1: DR(", rc2, ") ", h1, " DR(", rc1, ")"))
		}	
return(rs)
} 


