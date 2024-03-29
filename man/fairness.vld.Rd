% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/33_FAIRNESS_VLD.R
\name{fairness.vld}
\alias{fairness.vld}
\title{Model fairness validation}
\usage{
fairness.vld(
  db,
  sensitive,
  obs.outcome,
  mod.outcome,
  conditional = NULL,
  mod.outcome.type,
  p.value
)
}
\arguments{
\item{db}{Data frame with sensitive attribute, observed outcome, model outcome and conditional attribute.}

\item{sensitive}{Name of sensitive attribute within \code{db}.}

\item{obs.outcome}{Name of observed outcome within \code{db}.}

\item{mod.outcome}{Name of model outcome within \code{db}.}

\item{conditional}{Name of conditional attribute within \code{db}. It is used for calculation of conditional
statistical parity. Default value is \code{NULL}.}

\item{mod.outcome.type}{Type of model outcome. Possible values are \code{disc} (discrete outcome)
and \code{cont} (continuous).}

\item{p.value}{Significance level of applied statistical test (chi-square or Wald test).}
}
\value{
The command \code{fairness.vld} returns a list of three data frames.\cr
The first object (\code{SP}), provides results of statistical parity testing.\cr
The second object (\code{CSP}), provides results of conditional statistical parity testing.
This object will be returned only if conditional attributed is supplied. \cr
The third object (\code{EO}), provides results of equal opportunity testing.\cr
}
\description{
\code{fairness.vld} performs fairness validation for a given sensitive attribute and selected outcome.
Sensitive attribute should be categorical variable with reasonable number of modalities, while
outcome can be categorical (e.g. reject/accept indicator or rating grade) or continuous (e.g. interest rate or amount).
Depending on model type outcome (see argument \code{mod.outcome.type}) Chi-square test or Wald test is applied.
}
\examples{
suppressMessages(library(PDtoolkit))
#build hypothetical model
data(loans)
#numeric risk factors
#num.rf <- sapply(loans, is.numeric)
#num.rf <- names(num.rf)[!names(num.rf)\%in\%"Creditability" & num.rf]
num.rf <- c("Credit Amount", "Age (years)")
#discretized numeric risk factors using ndr.bin from monobin package
loans[, num.rf] <- sapply(num.rf, function(x) 
ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
str(loans)
#run stepMIV
rf <- c("Account Balance", "Payment Status of Previous Credit", 
       "Purpose", "Value Savings/Stocks", "Credit Amount",
       "Age (years)", "Instalment per cent", "Foreign Worker")
res <- stepMIV(start.model = Creditability ~ 1, 
   miv.threshold = 0.02, 
   m.ch.p.val = 0.05,
   coding = "WoE",
   coding.start.model = FALSE,
   db = loans[, c("Creditability", rf)])
#print coefficients
summary(res$model)$coefficients

#prepare data frame for fairness validation
db.fa <- data.frame(Creditability = loans$Creditability, 
		  mpred = predict(res$model, type = "response", newdata = res$dev.db))
#add hypothetical reject/accept indicator 
db.fa$rai <- ifelse(db.fa$mpred > 0.5, 1, 0)
#add hypothetical rating
db.fa$rating <- sts.bin(x = round(db.fa$mpred, 4), y = db.fa$Creditability)[[2]]
#add hypothetical interest rate
ir.r <- seq(0.03, 0.10, length.out = 6)
names(ir.r) <- sort(unique(db.fa$rating))
db.fa$ir <- ir.r[db.fa$rating]
#add hypothetical sensitive attribute
db.fa$sensitive.1 <- ifelse(loans$"Sex & Marital Status"\%in\%2, 1, 0) #not in a model
db.fa$sensitive.2 <- ifelse(loans$"Age (years)"\%in\%"03 [35,Inf)", 1, 0) #in a model
#add some attributes for calculation of conditional statistical parity
db.fa$"Credit Amount" <- loans$"Credit Amount" 
head(db.fa)

#discrete model outcome - sensitive attribute not in a model
fairness.vld(db = db.fa, 
	 sensitive = "sensitive.1", 
	 obs.outcome = "Creditability", 
	 mod.outcome = "rai",
	 conditional = "Credit Amount", 
	 mod.outcome.type = "disc", 
	 p.value = 0.05)
##discrete model outcome - sensitive attribute in a model
#fairness.vld(db = db.fa, 
#		 sensitive = "sensitive.2", 
#		 obs.outcome = "Creditability", 
#		 mod.outcome = "rai",
#		 conditional = "Credit Amount", 
#		 mod.outcome.type = "disc", 
#		 p.value = 0.05)
##continuous outcome - sensitive attribute not in a model
#fairness.vld(db = db.fa, 
#		 sensitive = "sensitive.1", 
#		 obs.outcome = "Creditability", 
#		 mod.outcome = "ir",
#		 conditional = "Credit Amount", 
#		 mod.outcome.type = "cont", 
#		 p.value = 0.05)
#continuous outcome - sensitive attribute in a model
fairness.vld(db = db.fa, 
	 sensitive = "sensitive.2", 
	 obs.outcome = "Creditability", 
	 mod.outcome = "ir",
	 conditional = "Credit Amount", 
	 mod.outcome.type = "cont", 
	 p.value = 0.05)
}
\references{
Hurlin, Christophe and Perignon, Christophe and Saurin, Sebastien (2022),
The Fairness of Credit Scoring Models. HEC Paris Research Paper No. FIN-2021-1411
}
