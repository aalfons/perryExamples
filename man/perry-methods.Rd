% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perry.R
\name{perry-methods}
\alias{perry-methods}
\alias{perry.lm}
\alias{perry.lmrob}
\alias{perry.lts}
\title{Resampling-based prediction error for fitted models}
\usage{
\method{perry}{lm}(
  object,
  splits = foldControl(),
  cost = rmspe,
  ncores = 1,
  cl = NULL,
  seed = NULL,
  ...
)

\method{perry}{lmrob}(
  object,
  splits = foldControl(),
  cost = rtmspe,
  ncores = 1,
  cl = NULL,
  seed = NULL,
  ...
)

\method{perry}{lts}(
  object,
  splits = foldControl(),
  fit = c("reweighted", "raw", "both"),
  cost = rtmspe,
  ncores = 1,
  cl = NULL,
  seed = NULL,
  ...
)
}
\arguments{
\item{object}{the fitted model for which to estimate the prediction error.}

\item{splits}{an object of class \code{"cvFolds"} (as returned by
\code{\link[perry]{cvFolds}}) or a control object of class
\code{"foldControl"} (see \code{\link[perry]{foldControl}}) defining the
folds of the data for (repeated) \eqn{K}-fold cross-validation, an object of
class \code{"randomSplits"} (as returned by
\code{\link[perry]{randomSplits}}) or a control object of class
\code{"splitControl"} (see \code{\link[perry]{splitControl}}) defining
random data splits, or an object of class \code{"bootSamples"} (as returned
by \code{\link[perry]{bootSamples}}) or a control object of class
\code{"bootControl"} (see \code{\link[perry]{bootControl}}) defining
bootstrap samples.}

\item{cost}{a cost function measuring prediction loss.  It should expect
the observed values of the response to be passed as the first argument and
the predicted values as the second argument, and must return either a
non-negative scalar value, or a list with the first component containing
the prediction error and the second component containing the standard
error.  The default is to use the root mean squared prediction error
for the \code{"lm"} method and the root trimmed mean squared prediction
error for the \code{"lmrob"} and \code{"lts"} methods (see
\code{\link[perry]{cost}}).}

\item{ncores}{a positive integer giving the number of processor cores to be
used for parallel computing (the default is 1 for no parallelization).  If
this is set to \code{NA}, all available processor cores are used.}

\item{cl}{a \pkg{parallel} cluster for parallel computing as generated by
\code{\link[parallel]{makeCluster}}.  If supplied, this is preferred over
\code{ncores}.}

\item{seed}{optional initial seed for the random number generator (see
\code{\link{.Random.seed}}).  Note that also in case of parallel computing,
resampling is performed on the manager process rather than the worker
processes. On the parallel worker processes, random number streams are
used and the seed is set via \code{\link{clusterSetRNGStream}}.}

\item{\dots}{additional arguments to be passed to the prediction loss
function \code{cost}.}

\item{fit}{a character string specifying for which fit to estimate the
prediction error.  Possible values are \code{"reweighted"} (the default) for
the prediction error of the reweighted fit, \code{"raw"} for the prediction
error of the raw fit, or \code{"both"} for the prediction error of both
fits.}
}
\value{
An object of class \code{"perry"} with the following components:
\describe{
  \item{\code{pe}}{a numeric vector containing the estimated prediction
  errors.  For the \code{"lm"} and \code{"lmrob"} methods, this is a single
  numeric value.  For the \code{"lts"} method, this contains one value for
  each of the requested fits.  In case of more than one replication, those
  are average values over all replications.}
  \item{\code{se}}{a numeric vector containing the estimated standard
  errors of the prediction loss.  For the \code{"lm"} and \code{"lmrob"}
  methods, this is a single numeric value.  For the \code{"lts"} method,
  this contains one value for each of the requested fits.}
  \item{\code{reps}}{a numeric matrix containing the estimated prediction
  errors from all replications.  For the \code{"lm"} and \code{"lmrob"}
  methods, this is a matrix with one column.  For the \code{"lts"} method,
  this contains one column for each of the requested fits.  However, this
  is only returned in case of more than one replication.}
  \item{\code{splits}}{an object giving the data splits used to estimate the
  prediction error.}
  \item{\code{y}}{the response.}
  \item{\code{yHat}}{a list containing the predicted values from all
  replications.}
  \item{\code{call}}{the matched function call.}
}
}
\description{
Estimate the prediction error of a fitted model via (repeated) \eqn{K}-fold
cross-validation, (repeated) random splitting (also known as random
subsampling or Monte Carlo cross-validation), or the bootstrap.  Methods are
available for least squares fits computed with \code{\link[stats]{lm}} as
well as for the following robust alternatives: MM-type models computed with
\code{\link[robustbase]{lmrob}} and least trimmed squares fits computed with
\code{\link[robustbase]{ltsReg}}.
}
\note{
The \code{perry} methods extract the data from the fitted model and
call \code{\link[perry]{perryFit}} to perform resampling-based prediction
error estimation.
}
\examples{
## load data
data("Bundesliga")
n <- nrow(Bundesliga)

## fit linear model
Bundesliga$logMarketValue <- log(Bundesliga$MarketValue)
fit <- lm(logMarketValue ~ Contract + Matches + Goals + Assists, 
          data=Bundesliga)

## perform K-fold cross-validation
perry(fit, foldControl(K = 5, R = 10), seed = 1234)

## perform random splitting
perry(fit, splitControl(m = n/3, R = 10), seed = 1234)

## perform bootstrap prediction error estimation
# 0.632 estimator
perry(fit, bootControl(R = 10, type = "0.632"), seed = 1234)
# out-of-bag estimator
perry(fit, bootControl(R = 10, type = "out-of-bag"), seed = 1234)
}
\seealso{
\code{\link[perry]{perryFit}}
}
\author{
Andreas Alfons
}
\keyword{utilities}
