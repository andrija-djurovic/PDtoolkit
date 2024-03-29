% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/18_INTERACTION_TRANSFORMER.R
\name{interaction.transformer}
\alias{interaction.transformer}
\title{Extract risk factors interaction from decision tree}
\usage{
interaction.transformer(
  db,
  rf,
  target,
  min.pct.obs,
  min.avg.rate,
  max.depth,
  monotonicity,
  create.interaction.rf
)
}
\arguments{
\item{db}{Data frame of risk factors and target variable supplied for interaction extraction.}

\item{rf}{Character vector of risk factor names on which decision tree is run.}

\item{target}{Name of target variable (default indicator 0/1) within db argument.}

\item{min.pct.obs}{Minimum percentage of observation in each leaf.}

\item{min.avg.rate}{Minimum percentage of defaults in each leaf.}

\item{max.depth}{Maximum number of splits.}

\item{monotonicity}{Logical indicator. If \code{TRUE}, observed trend between risk factor and target will be preserved
in splitting node.}

\item{create.interaction.rf}{Logical indicator. If \code{TRUE}, second element of the output will be data frame with
interaction modalities.}
}
\value{
The command \code{interaction.transformer} returns a list of two data frames. The first data frame provides
the tree summary. The second data frame is a new risk factor extracted from decision tree.
}
\description{
\code{interaction.transformer} extracts the interaction between supplied risk factors from decision tree.
It implements customized decision tree algorithm that takes into account different conditions such as minimum
percentage of observations and defaults in each node, maximum tree depth and monotonicity condition
at each splitting node. Gini index is used as metric for node splitting .
}
\examples{
suppressMessages(library(PDtoolkit))
data(loans)
#modify risk factors in order to show how the function works with missing values
loans$"Account Balance"[1:10] <- NA
loans$"Duration of Credit (month)"[c(13, 15)] <- NA
it <- interaction.transformer(db = loans,
				 rf = c("Account Balance", "Duration of Credit (month)"), 
				 target = "Creditability",
				 min.pct.obs = 0.05,
				 min.avg.rate = 0.01,
				 max.depth = 2,
			 	 monotonicity = TRUE,
				 create.interaction.rf = TRUE)
names(it)
it[["tree.info"]]
tail(it[["interaction"]])
table(it[["interaction"]][, "rf.inter"], useNA = "always")
}
