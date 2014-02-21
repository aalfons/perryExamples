# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

## LAD-lasso with tuning parameter selection
# x ........ predictor matrix (without constant term)
# y ........ response vector
# lambda ... vector of penalty parameters
#' @export

ladlasso <- function(x, y, lambda, standardize = TRUE, intercept = TRUE, 
                     splits = foldControl(), cost = rmspe, 
                     selectBest = c("hastie", "min"), seFactor = 1, 
                     ncores = 1, cl = NULL, seed = NULL, ...) {
  # initializations
  if(!is.numeric(lambda) || length(lambda) == 0 || any(!is.finite(lambda))) {
    stop("missing or invalid value of 'lambda'")
  }
  if(any(negative <- lambda < 0)) {
    lambda[negative] <- 0
    warning("negative value for 'lambda', using no penalization")
  }
  lambda <- sort.int(unique(lambda), decreasing=TRUE)
  if(length(lambda) > 1) names(lambda) <- seq_along(lambda)
  selectBest <- match.arg(selectBest)
  call <- call("ladlasso.fit", lambda=lambda, intercept=intercept, 
               standardize=standardize)
  # estimate the prediction error for each value of the penalty parameter 
  # and add the final model
  perryTuning(call, x=x, y=y, tuning=list(lambda=lambda), splits=splits, 
              cost=cost, costArgs=list(...), selectBest=selectBest, 
              seFactor=seFactor, final=TRUE, envir=parent.frame(), 
              ncores=ncores, cl=cl, seed=seed)
}


## fit function for LAD-lasso
# x ........ predictor matrix (without constant term)
# y ........ response vector
# lambda ... penalty parameter
#' @export
#' @import quantreg

ladlasso.fit <- function(x, y, lambda, standardize = TRUE, 
                         intercept = TRUE, ...) {
  # initializations
  matchedCall <- match.call()
  n <- length(y)
  x <- addColnames(as.matrix(x))
  d <- dim(x)
  if(!isTRUE(n == d[1])) stop(sprintf("'x' must have %d rows", n))
  lambda <- rep_len(lambda, length.out=1)
  if(!is.numeric(lambda) || !is.finite(lambda)) {
    stop("missing or invalid value of 'lambda'")
  }
  if(lambda < 0) {
    lambda <- 0
    warning("negative value for 'lambda', using no penalization")
  }
  standardize <- isTRUE(standardize)
  intercept <- isTRUE(intercept)
  # center the data
  if(intercept) {
    muX <- apply(x, 2, median)
    muY <- median(y)
    xs <- sweep(x, 2, muX, check.margin=FALSE)  # sweep out column centers
    z <- y - muY
  } else {
    muX <- rep.int(0, d[2])
    muY <- 0
    xs <- x
    z <- y
  }
  # scale the predictors
  if(standardize) {
    sigmaX <- apply(xs, 2, mad, center=0)
    xs <- sweep(xs, 2, sigmaX, "/", check.margin=FALSE)# sweep out column scales
  } else sigmaX <- rep.int(1, d[2])
  # compute LAD-lasso solution
  rqlasso <- function(..., tau) rq.fit.lasso(..., tau=0.5)
  fit <- rqlasso(xs, z, lambda=rep_len(lambda, d[2]), ...)
  # back-transform coefficients
  coef <- backtransform(coef(fit), muX=muX, sigmaX=sigmaX, muY=muY, 
                        intercept=intercept)
  # compute fitted values and residuals
  residuals <- drop(residuals(fit))
  fitted <- y - residuals
  # construct return object
  fit <- list(lambda=lambda, coefficients=coef, fitted.values=fitted, 
              residuals=residuals, intercept=intercept, 
              standardize=standardize, muX=muX, sigmaX=sigmaX, muY=muY, 
              call=matchedCall)
  class(fit) <- "ladlasso"
  fit
}
