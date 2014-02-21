# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

## ridge regression with tuning parameter selection
# x ........ predictor matrix (without constant term)
# y ........ response vector
# lambda ... vector of penalty parameters
#' @export

lmridge <- function(x, y, lambda, standardize = TRUE, intercept = TRUE, 
                    splits = foldControl(), cost = rmspe, 
                    selectBest = c("hastie", "min"), seFactor = 1, 
                    ncores = 1, cl = NULL, seed = NULL, ...) {
  # initializations
  selectBest <- match.arg(selectBest)
  call <- call("lmridge.fit", lambda=lambda, intercept=intercept, 
               standardize=standardize)
  # estimate the prediction error for all values of the penalty parameter
  pe <- perryFit(call, x=x, y=y, splits=splits, cost=cost, costArgs=list(...), 
                 envir=parent.frame(), ncores=ncores, cl=cl, seed=seed)
  # select the optimal value of the penalty parameter
  pe <- perryReshape(pe, tuning=list(lambda=lambda), selectBest=selectBest, 
                     seFactor=seFactor)
  optLambda <- lambda[pe$best]
  # add final model and return object
  pe$finalModel <- lmridge.fit(x, y, lambda=optLambda, intercept=intercept, 
                               standardize=standardize)
  pe
}


## fit function for ridge regression
# x ........ predictor matrix (without constant term)
# y ........ response vector
# lambda ... vector of penalty parameters
#' @export

lmridge.fit <- function(x, y, lambda, standardize = TRUE, 
                        intercept = TRUE, ...) {
  # initializations
  matchedCall <- match.call()
  n <- length(y)
  x <- addColnames(as.matrix(x))
  d <- dim(x)
  if(!isTRUE(n == d[1])) stop(sprintf("'x' must have %d rows", n))
  if(!is.numeric(lambda) || length(lambda) == 0 || any(!is.finite(lambda))) {
    stop("missing or invalid value of 'lambda'")
  }
  if(any(negative <- lambda < 0)) {
    lambda[negative] <- 0
    warning("negative value for 'lambda', using no penalization")
  }
  lambda <- sort.int(unique(lambda), decreasing=TRUE)
  if(length(lambda) > 1) names(lambda) <- seq_along(lambda)
  standardize <- isTRUE(standardize)
  intercept <- isTRUE(intercept)
  # center the data
  if(intercept) {
    muX <- colMeans(x)
    muY <- mean(y)
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
    f <- function(v) sqrt(sum(v^2) / max(1, length(v)-1))
    sigmaX <- apply(xs, 2, f)
    xs <- sweep(xs, 2, sigmaX, "/", check.margin=FALSE)# sweep out column scales
  } else sigmaX <- rep.int(1, d[2])
  # compute ridge solution
  SVD <- svd(xs)
  sv <- SVD$d  # singular values
  a <- sv * drop(crossprod(SVD$u, z)) / (sv^2 + rep(lambda, each=length(sv)))
  dim(a) <- c(length(sv), length(lambda))
  coef <- SVD$v %*% a
  if(length(lambda) == 1) {
    coef <- drop(coef)
    names(coef) <- colnames(x)
  } else dimnames(coef) <- list(colnames(x), names(lambda))
  # back-transform coefficients
  coef <- backtransform(coef, muX=muX, sigmaX=sigmaX, muY=muY, 
                        intercept=intercept)
  # compute fitted values and residuals
  fitted <- if(intercept) addIntercept(x) %*% coef else x %*% coef
  fitted <- drop(fitted)
  residuals <- y - fitted
  # construct return object
  fit <- list(lambda=lambda, coefficients=coef, fitted.values=fitted, 
              residuals=residuals, intercept=intercept, 
              standardize=standardize, muX=muX, sigmaX=sigmaX, muY=muY, 
              call=matchedCall)
  class(fit) <- "lmridge"
  fit
}
