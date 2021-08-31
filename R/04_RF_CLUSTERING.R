#' Risk factor clustering
#'
#' \code{rf.clustering} implements correlation based clustering of risk factors. 
#' Clustering procedure is base on \link[stats]{hclust} from \code{stats} package.
#'@param db Data frame of risk factors supplied for clustering analysis.
#'@param metric Correlation metric used for distance calculation. Available options are:
#'\itemize{
#'	  \item \code{"raw pearson"} - calculated distance \code{dist(cor(db, method = "pearson"))};
#'	  \item \code{"raw spearman"} - calculated distance \code{dist(cor(db, method = "spearman"))};
#'	  \item \code{"common pearson"} - calculated distance \code{dist((1 - cor(db, method = "pearson")) / 2)};
#'	  \item \code{"common spearman"} - calculated distance \code{dist((1 - cor(db, method = "spearman")) / 2)};
#'	  \item \code{"absolute pearson"} - calculated distance \code{dist(1 - abs(cor(db, method = "pearson")))};
#'	  \item \code{"absolute spearman"} - calculated distance \code{dist(1 - abs(cor(db, method = "spearman")))};
#'	  \item \code{"sqrt pearson"} - calculated distance \code{dist(sqrt(1 - cor(db, method = "pearson")))};
#'	  \item \code{"sqrt spearman"} - calculated distance \code{dist(sqrt(1 - cor(db, method = "spearman")))};
#'	  \item \code{"x2y"} - calculated distance \code{dist(dx2y(d = db)[[2]]))}.
#'}
#'\code{x2y} metric is proposed by Professor Rama Ramakrishnan and details can be found on this 
#' \href{https://rama100.github.io/lecture-notes/x2y.nb.html}{link}. This metric is especially handy if 
#' analyst wants to perform clustering before any binning procedures and to decrease number of risk factors. Additionally, 
#' \code{x2y} algorithm process numerical and categorical risk factors at once and it is able to identify
#' non-linear relationship between the pairs. Metric \code{x2y} is not symmetric with respect to inputs - \code{x, y},
#' therefore arithmetic average of values between \code{xy} and \code{yx} is used to produce the final value for each pair.
#'@param k Number of clusters. If default value (\code{NA}) is passed, then automatic elbow method
#' will be used to determine the optimal number of clusters, otherwise selected number of clusters will be used.  
#'@return The function \code{rf.clustering} returns a data frame with: risk factors, clusters assigned and 
#'	    distance to centroid (ordered from smallest to largest). 
#'	    The last column (distance to centroid) can be used for selection of one or more risk factors per
#'	    cluster.
#'@examples
#'suppressMessages(library(PDtoolkit))
#'library(rpart)
#'data(loans)
#'#risk factors clustering using x2y metric
#'cr <- rf.clustering(db = loans[, -which(names(loans)%in%"Creditability")], 
#'			  metric = "x2y", 
#'			  k = 15)
#'cr
#select one risk factor per cluster with min distance to centorid
#'cr %>% group_by(clusters) %>% 
#'	 slice(which.min(dist.to.centroid))
#'#clustering using common spearman metric
#'#first we need to categorize numeric risk factors
#'num.rf <- sapply(loans, is.numeric)
#'num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#'loans[, num.rf] <- sapply(num.rf, function(x) 
#'				  sts.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#'#replace woe in order to convert to all numeric factors 
#'loans.woe <- replace.woe(db = loans, target = "Creditability")[[1]]
#'cr <- rf.clustering(db = loans.woe[, -which(names(loans.woe)%in%"Creditability")], 
#'			  metric = "common spearman", 
#'			  k = NA)
#'cr
#'#select one risk factor per cluster with min distance to centorid
#'cr %>% group_by(clusters) %>% 
#'	 slice(which.min(dist.to.centroid))
#'@import monobin
#'@import dplyr
#'@import rpart
#'@importFrom stats hclust cor cutree dist pchisq pnorm xtabs
#'@importFrom utils combn
#'@export

rf.clustering <- function(db, metric, k = NA) {
	db.ncol <- ncol(db)
	if	(db.ncol < 4) {
		stop("At least 4 risk factors have to be available in db argument.")
		}
	metric.opt <- c("raw pearson", "raw spearman", "common pearson", "common spearman", 
			    "absolute pearson", "absolute spearman", "absolute pearson", "absolute spearman", 
			    "sqrt pearson", "sqrt spearman", "x2y")
	if	(!metric%in%metric.opt) {
		msg <- paste0("metric argument has to be one of: ", paste(metric.opt, collapse = ", "), ".")
		stop(msg)
		}
	if	(!is.na(k)) {
		k <- as.numeric(k)
		if	(is.na(k)) {
			stop("")
			}
		} 
	if	(!metric%in%"x2y") {
		cond <- any(!sapply(db, is.numeric))
		if	(cond) {
			msg <- paste0("For metric ", metric, " all risk factors have to be of numeric type.")
			stop(msg)
			}
		}
	distance <- switch(metric, "raw pearson" = dist(cor(db, method = "pearson")), 
					   "raw spearman" = dist(cor(db, method = "spearman")),
					   "common pearson" = dist((1 - cor(db, method = "pearson")) / 2), 
					   "common spearman" = dist((1 - cor(db, method = "spearman")) / 2),
					   "absolute pearson" = dist(1 - abs(cor(db, method = "pearson"))), 
					   "absolute spearman" = dist(1 - abs(cor(db, method = "spearman"))),
					   "sqrt pearson" = dist(sqrt(1 - cor(db, method = "pearson"))), 
					   "sqrt spearman" = dist(sqrt(1 - cor(db, method = "spearman"))),
					   "x2y" = dist(dx2y(d = db)[[2]]))
	clust <- hclust(distance, method = "centroid")
	if	(is.na(k)) {
		min.g <- 3
		max.g <- min(c(30, db.ncol))
		clusters <- cutree(clust, k = min.g:max.g)
		k <- automatic.elbow(distance = distance, clusters = clusters)
		} else {
		k <- ifelse(k > 30, 30, k)
		k <- ifelse(k > db.ncol, db.ncol, k)	
		}
	distance <- as.matrix(distance)
	distance <- cbind.data.frame(rf = row.names(distance), distance)
	rownames(distance) <- NULL
	clusters <- cutree(clust, k = k)
	clusters <- as.data.frame(clusters, check.names = FALSE)
	clusters <- cbind.data.frame(rf = row.names(clusters), clusters)
	rownames(clusters) <- NULL
	dc <- merge(distance, clusters, by = "rf")
	dc <- split(x = dc, f = dc[, ncol(dc)])
	ced <- lapply(dc, function(x) clust.dist(dd = x))
	ced <- unname(c(ced, recursive = TRUE))
	res <- cbind.data.frame(bind_rows(dc), dist.to.centroid = ced)
	res <- res[order(res$clusters, res$dist.to.centroid), 
		     c("rf", "clusters", "dist.to.centroid")]
return(res)
}

calc.mae.reduction <- function(y.hat, y.actual) {
	model.error <- mean(abs(y.hat - y.actual))
	baseline <- mean(y.actual, na.rm = TRUE)
	baseline.error <-  mean(abs(baseline - y.actual))
	result <- 1 - model.error / baseline.error
	result <- max(0.0, min(result, 1))
return(result)
}

calc.misclass.reduction <- function(y.hat, y.actual) {
	tab <- table(y.hat, y.actual)
	model.error <- 1 - sum(diag(tab)) / sum(tab)
	majority.class <- names(which.max(table(y.actual)))
	baseline.preds <- rep(majority.class, length(y.actual))
	baseline.error <- mean(baseline.preds != y.actual)
	result <- 1 - model.error / baseline.error
	result <- max(0.0, min(result, 1.0))
return(result)
}

x2y.inner <- function(x, y) {
	if	(length(unique(x)) == 1 | length(unique(y)) == 1) {
    		return(NA)
		} 
	#if y is continuous
	if	(is.numeric(y)) {
		preds <- predict(rpart(y ~ x, method = "anova"), type = "vector")
		calc.mae.reduction(y.hat = preds, y.actual = y)
		}
	#if y is categorical
		else {
		preds <- predict(rpart(y ~ x, method = "class"), type = "class")
		calc.misclass.reduction(y.hat = preds, y.actual = y)
		}
}


x2y <- function(x, y) {
	missing <-  is.na(x) | is.na(y)
  	x <- x[!missing]
	y <- y[!missing]
  	x2y.val <- x2y.inner(x, y)
return(x2y.val)
}

dx2y <- function(d) {
	rfl <- ncol(d)
	rfn <- names(d)
	pairs.comb <- combn(rfl, 2)
	pairs <- cbind(pairs.comb, pairs.comb[2:1, ])
  	n <- ncol(pairs)
  	res <- data.frame(x = rfn[pairs[1,]],
				y = rfn[pairs[2,]],
				x2y = rep(0.00, n),
				x2y.avg = rep(0.00, n))
	#calculate x2y metric
	for	(i in 1:n) {
		x <- d %>% pull(pairs[1, i])
		y <- d %>% pull(pairs[2, i])
		res[i, 3] <- x2y(x, y)
    		}
	#add average x2y metric for each pair
	for	(i in 1:ncol(pairs.comb)) {
		rf.l.x <- rfn[pairs.comb[1, i]]
		rf.l.y <- rfn[pairs.comb[2, i]]
		cond.x <- res$x%in%c(rf.l.x, rf.l.y)
		cond.y <- res$y%in%c(rf.l.x, rf.l.y)
		cond <- cond.x & cond.y
		res$x2y.avg[cond] <- mean(res$x2y[cond])
		}
	#add diagonal elements
	diag.2.append <- data.frame(x = rfn, y = rfn, x2y = 1, x2y.avg = 1)
	res <- bind_rows(res, diag.2.append)
	res <- res %>% arrange(desc(x2y))
	x2y.ori <- as.data.frame.matrix(xtabs(x2y ~ x + y, res))
	x2y.avg <- as.data.frame.matrix(xtabs(x2y.avg ~ x + y, res))
return(list(raw = x2y.ori, averaged = x2y.avg))
}

automatic.elbow <- function(distance, clusters) {
	distance <- as.matrix(distance)
	distance <- cbind.data.frame(rf = row.names(distance), distance)
	rownames(distance) <- NULL
	clusters <- as.data.frame(clusters, check.names = FALSE)
	clusters <- cbind.data.frame(rf = row.names(clusters), clusters)
	rownames(clusters) <- NULL	
	nc <- ncol(clusters)
	wss.res <- vector("list", nc - 1)
	for	(i in 2:nc) {
		clusters.l <- clusters[, c(1, i), drop = FALSE]
		nc.l <- as.numeric(names(clusters.l)[2])
		dc <- merge(distance, clusters.l, by = "rf")
		dc <- split(x = dc, f = dc[, ncol(dc)])
		wss.l <- sum(sapply(dc, function(x) wss(dd = x)))
		wss.res[[i]] <- data.frame(nc = nc.l, wss = wss.l)
		}
	wss.res <- data.frame(bind_rows(wss.res))
	elbow.points <- nrow(wss.res)
	dist2l.res <- rep(NA, elbow.points)
	b <- c(wss.res$nc[1], wss.res$wss[1])
	c <- c(wss.res$nc[elbow.points], wss.res$wss[elbow.points])
	for	(i in 1:elbow.points) {
		a <- c(wss.res$nc[i], wss.res$wss[i])
		dist2l.res[i] <- dist2l(a = a, b = b, c = c)
		}
	elbow.point <- which.max(dist2l.res)
	res <- wss.res[elbow.point, "nc"]
return(res)
}

wss <- function(dd) {
	dd <- dd[, -c(1, ncol(dd))]
	clust.centroid <- apply(dd, 2, mean)
	res <- sum(sweep(dd, 2, clust.centroid, FUN = "-")^2)
return(res)
}

dist2l <- function(a, b, c) {
	v1 <- b - c
	v2 <- a - b
	m <- cbind(v1, v2)
	d <- abs(det(m))/sqrt(sum(v1 * v1))
return(d)
} 

clust.dist <- function(dd) {
	dd <- dd[, -c(1, ncol(dd))]
	clust.centroid <- apply(dd, 2, mean)
	res <- apply((dd - clust.centroid)^2, 1, function(x) sqrt(sum(x)))
return(res)
}




