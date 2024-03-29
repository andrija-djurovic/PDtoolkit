% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/10_HETEROGENEITY.R
\name{heterogeneity}
\alias{heterogeneity}
\title{Testing heterogeneity of the PD rating model}
\usage{
heterogeneity(app.port, def.ind, rating, alpha = 0.05)
}
\arguments{
\item{app.port}{Application portfolio (data frame) which contains default indicator (0/1) and
ratings in use.}

\item{def.ind}{Name of the column that represents observed default indicator (0/1).}

\item{rating}{Name of the column that represent rating grades in use.}

\item{alpha}{Significance level of p-value for two proportion test. Default is 0.05.}
}
\value{
The command \code{heterogeneity} returns a data frame with the following columns:
\itemize{
\item rating: Unique values of rating grades from application portfolio.
\item no: Number of complete observations.
\item nb: Number of defaults (bad cases) in complete observations.
\item p.val: Test p-value (two proportion test of adjacent rating grades).
\item alpha: Selected significance level.
\item res: Accepted hypothesis.
}
}
\description{
\code{heterogeneity} performs heterogeneity testing of PD model based on the rating grades.
This test is usually applied on application portfolio, but it can be applied also on model development sample.
}
\details{
Testing procedure starts with summarizing the number of observations and defaults per rating grade.
After that, two proportion test is applied on adjacent rating grades. Testing hypothesis is that
default rate of grade \code{i} is less or greater than default rate of grade \code{i - 1}, where \code{i}
takes the values from 2 to the number of unique grades.
Direction of alternative hypothesis (less or greater) is determined automatically based on correlation direction
of observed default on rating grades.
Incomplete cases, identified based on default indicator (\code{def.ind}) and rating grade (\code{rating })
columns are excluded from the summary table and testing procedure. If identified, warning will be returned.
}
\examples{
suppressMessages(library(PDtoolkit))
data(loans)
#estimate some dummy model
mod.frm <- `Creditability` ~ `Account Balance` + `Duration of Credit (month)` +
				`Age (years)` + `Value Savings/Stocks`
lr.mod <- glm(mod.frm, family = "binomial", data = loans)
summary(lr.mod)$coefficients
#model predictions
loans$pred <-  unname(predict(lr.mod, type = "response", newdata = loans))
#scale probabilities
loans$score <- scaled.score(probs = loans$pred, score = 600, odd = 50/1, pdo = 20)
#group scores into ratings
loans$rating.1 <- sts.bin(x = round(loans$score), y = loans$Creditability, y.type = "bina")[[2]]
#group probabilities into ratings
loans$rating.2 <- sts.bin(x = round(loans$pred, 4), y = loans$Creditability, y.type = "bina")[[2]]
#simulate dummy application portfolio 
set.seed(1984)
app.port <- loans[sample(1:nrow(loans), 400, rep = TRUE), ]
#run heterogeneity test on ratings based on the scaled score
#higher score lower default rate
heterogeneity(app.port = app.port, 
	   def.ind = "Creditability", 
	   rating = "rating.1", 
	   alpha = 0.05)
#run test on predicted default rate - direction of the test is changed
heterogeneity(app.port = app.port, 
	   def.ind = "Creditability", 
	   rating = "rating.2", 
	   alpha = 0.05)
}
