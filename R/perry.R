# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Resampling-based prediction error for fitted models
#'
#' Estimate the prediction error of a fitted model via (repeated) \eqn{K}-fold
#' cross-validation, (repeated) random splitting (also known as random
#' subsampling or Monte Carlo cross-validation), or the bootstrap.  Methods are
#' available for least squares fits computed with \code{\link[stats]{lm}} as
#' well as for the following robust alternatives: MM-type models computed with
#' \code{\link[robustbase]{lmrob}} and least trimmed squares fits computed with
#' \code{\link[robustbase]{ltsReg}}.
#'
#' @name perry-methods
#'
#' @param object  the fitted model for which to estimate the prediction error.
#' @param splits  an object of class \code{"cvFolds"} (as returned by
#' \code{\link[perry]{cvFolds}}) or a control object of class
#' \code{"foldControl"} (see \code{\link[perry]{foldControl}}) defining the
#' folds of the data for (repeated) \eqn{K}-fold cross-validation, an object of
#' class \code{"randomSplits"} (as returned by
#' \code{\link[perry]{randomSplits}}) or a control object of class
#' \code{"splitControl"} (see \code{\link[perry]{splitControl}}) defining
#' random data splits, or an object of class \code{"bootSamples"} (as returned
#' by \code{\link[perry]{bootSamples}}) or a control object of class
#' \code{"bootControl"} (see \code{\link[perry]{bootControl}}) defining
#' bootstrap samples.
#' @param fit  a character string specifying for which fit to estimate the
#' prediction error.  Possible values are \code{"reweighted"} (the default) for
#' the prediction error of the reweighted fit, \code{"raw"} for the prediction
#' error of the raw fit, or \code{"both"} for the prediction error of both
#' fits.
#' @param cost  a cost function measuring prediction loss.  It should expect
#' the observed values of the response to be passed as the first argument and
#' the predicted values as the second argument, and must return either a
#' non-negative scalar value, or a list with the first component containing
#' the prediction error and the second component containing the standard
#' error.  The default is to use the root mean squared prediction error
#' for the \code{"lm"} method and the root trimmed mean squared prediction
#' error for the \code{"lmrob"} and \code{"lts"} methods (see
#' \code{\link[perry]{cost}}).
#' @param ncores  a positive integer giving the number of processor cores to be
#' used for parallel computing (the default is 1 for no parallelization).  If
#' this is set to \code{NA}, all available processor cores are used.
#' @param cl  a \pkg{parallel} cluster for parallel computing as generated by
#' \code{\link[parallel]{makeCluster}}.  If supplied, this is preferred over
#' \code{ncores}.
#' @param seed  optional initial seed for the random number generator (see
#' \code{\link{.Random.seed}}).  Note that also in case of parallel computing,
#' resampling is performed on the manager process rather than the worker
#' processes. On the parallel worker processes, random number streams are
#' used and the seed is set via \code{\link{clusterSetRNGStream}}.
#' @param \dots  additional arguments to be passed to the prediction loss
#' function \code{cost}.
#'
#' @return An object of class \code{"perry"} with the following components:
#' \describe{
#'   \item{\code{pe}}{a numeric vector containing the estimated prediction
#'   errors.  For the \code{"lm"} and \code{"lmrob"} methods, this is a single
#'   numeric value.  For the \code{"lts"} method, this contains one value for
#'   each of the requested fits.  In case of more than one replication, those
#'   are average values over all replications.}
#'   \item{\code{se}}{a numeric vector containing the estimated standard
#'   errors of the prediction loss.  For the \code{"lm"} and \code{"lmrob"}
#'   methods, this is a single numeric value.  For the \code{"lts"} method,
#'   this contains one value for each of the requested fits.}
#'   \item{\code{reps}}{a numeric matrix containing the estimated prediction
#'   errors from all replications.  For the \code{"lm"} and \code{"lmrob"}
#'   methods, this is a matrix with one column.  For the \code{"lts"} method,
#'   this contains one column for each of the requested fits.  However, this
#'   is only returned in case of more than one replication.}
#'   \item{\code{splits}}{an object giving the data splits used to estimate the
#'   prediction error.}
#'   \item{\code{y}}{the response.}
#'   \item{\code{yHat}}{a list containing the predicted values from all
#'   replications.}
#'   \item{\code{call}}{the matched function call.}
#' }
#'
#' @note The \code{perry} methods extract the data from the fitted model and
#' call \code{\link[perry]{perryFit}} to perform resampling-based prediction
#' error estimation.
#'
#' @author Andreas Alfons
#'
#' @seealso \code{\link[perry]{perryFit}}
#'
#' @example inst/doc/examples/example-perry.R
#'
#' @keywords utilities
#'
#' @import perry

NULL


## LS regression
#' @rdname perry-methods
#' @method perry lm
#' @export
#' @import stats

perry.lm <- function(object, splits = foldControl(), cost = rmspe,
                     ncores = 1, cl = NULL, seed = NULL, ...) {
  ## initializations
  matchedCall <- match.call()
  # retrieve data from model fit
  if(is.null(data <- object$model)) {
    haveDataArgument <- !is.null(object$call$data)
    if(haveDataArgument) {
      # try to retrieve data from 'x' and 'y' components
      # this only works if the data argument was used to fit the model
      if(!is.null(x <- object[["x"]]) && !is.null(y <- object$y)) {
        x <- removeIntercept(x)
        data <- data.frame(y, x)
      }
    }
    if(!haveDataArgument || is.null(data)) {
      # try to retrieve data from terms component
      data <- try(model.frame(object$terms), silent=TRUE)
      if(inherits(data, "try-error")) stop("model data not available")
    }
  }
  if(is.null(y <- object$y)) y <- model.response(data)
  ## call function perryFit() to estimate the prediction error
  out <- perryFit(object, data=data, y=y, splits=splits, cost=cost,
                  costArgs=list(...), envir=parent.frame(), ncores=ncores,
                  cl=cl, seed=seed)
  out$call <- matchedCall
  out
}


## MM and SDMD regression
#' @rdname perry-methods
#' @method perry lmrob
#' @export
#' @import robustbase

perry.lmrob <- function(object, splits = foldControl(), cost = rtmspe,
                        ncores = 1, cl = NULL, seed = NULL, ...) {
  ## initializations
  matchedCall <- match.call()
  # retrieve data from model fit
  if(is.null(data <- object$model)) {
    haveDataArgument <- !is.null(object$call$data)
    if(haveDataArgument) {
      # try to retrieve data from 'x' and 'y' components
      # this only works if the data argument was used to fit the model
      if(!is.null(x <- object[["x"]]) && !is.null(y <- object$y)) {
        x <- removeIntercept(x)
        data <- data.frame(y, x)
      }
    }
    if(!haveDataArgument || is.null(data)) {
      # try to retrieve data from terms component
      data <- try(model.frame(object$terms), silent=TRUE)
      if(inherits(data, "try-error")) stop("model data not available")
    }
  }
  if(is.null(y <- object$y)) y <- model.response(data)
  ## call function perryFit() to estimate the prediction error
  out <- perryFit(object, data=data, y=y, splits=splits, cost=cost,
                  costArgs=list(...), envir=parent.frame(), ncores=ncores,
                  cl=cl, seed=seed)
  out$call <- matchedCall
  out
}


## LTS regression
#' @rdname perry-methods
#' @method perry lts
#' @export
#' @import robustbase

perry.lts <- function(object, splits = foldControl(),
                      fit = c("reweighted", "raw", "both"), cost = rtmspe,
                      ncores = 1, cl = NULL, seed = NULL, ...) {
  ## initializations
  matchedCall <- match.call()
  object <- object
  if(is.null(x <- object$X) || is.null(y <- object$Y)) {
    if(is.null(data <- object$model)) {
      if(is.null(x)) x <- try(model.matrix(object$terms), silent=TRUE)
      if(is.null(y)) y <- try(model.response(object$terms), silent=TRUE)
      if(inherits(x, "try-error") || inherits(y, "try-error")) {
        stop("model data not available")
      }
    } else {
      x <- model.matrix(object$terms, data)
      y <- model.response(data)
    }
  }
  # predictor matrix is stored with column for intercept (if any)
  x <- removeIntercept(x)
  ## prepare cross-validation
  # extract function call for model fit
  call <- object$call
  call[[1]] <- as.name("ltsReg")
  # if the model was fitted with formula method, 'formula' and 'data'
  # arguments are removed from call and 'x' and 'y' are used instead
  call$formula <- NULL
  call$data <- NULL
  call$intercept <- object$intercept
  ## call function perryFit() to estimate the prediction error
  out <- perryFit(call, x=x, y=y, splits=splits, predictArgs=list(fit=fit),
                  cost=cost, costArgs=list(...), envir=parent.frame(),
                  ncores=ncores, cl=cl, seed=seed)
  out$call <- matchedCall
  out
}
