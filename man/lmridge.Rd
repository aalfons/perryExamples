\name{lmridge}
\alias{lmridge}
\alias{lmridge.fit}
\title{Ridge regression with penalty parameter selection}
\usage{
  lmridge(x, y, lambda, standardize = TRUE,
    intercept = TRUE, splits = foldControl(), cost = rmspe,
    selectBest = c("hastie", "min"), seFactor = 1,
    ncores = 1, cl = NULL, seed = NULL, ...)

  lmridge.fit(x, y, lambda, standardize = TRUE,
    intercept = TRUE, ...)
}
\arguments{
  \item{x}{a numeric matrix containing the predictor
  variables.}

  \item{y}{a numeric vector containing the response
  variable.}

  \item{lambda}{a numeric vector of non-negative values to
  be used as penalty parameter.}

  \item{standardize}{a logical indicating whether the
  predictor variables should be standardized to have unit
  variance (the default is \code{TRUE}).}

  \item{intercept}{a logical indicating whether a constant
  term should be included in the model (the default is
  \code{TRUE}).}

  \item{splits}{an object giving data splits to be used for
  prediction error estimation (see
  \code{\link[perry]{perryTuning}}).}

  \item{cost}{a cost function measuring prediction loss
  (see \code{\link[perry]{perryTuning}} for some
  requirements).  The default is to use the root mean
  squared prediction error (see
  \code{\link[perry]{cost}}).}

  \item{selectBest,seFactor}{arguments specifying a
  criterion for selecting the best model (see
  \code{\link[perry]{perryTuning}}).  The default is to use
  a one-standard-error rule.}

  \item{ncores,cl}{arguments for parallel computing (see
  \code{\link[perry]{perryTuning}}).}

  \item{seed}{optional initial seed for the random number
  generator (see \code{\link{.Random.seed}} and
  \code{\link[perry]{perryTuning}}).}

  \item{\dots}{for \code{lmridge}, additional arguments to
  be passed to the prediction loss function \code{cost}.
  For \code{lmridge.fit}, additional arguments are
  currently ignored.}
}
\value{
  For \code{lmridge}, an object of class
  \code{"perryTuning"}, see
  \code{\link[perry]{perryTuning}}).  It contains
  information on the prediction error criterion, and
  includes the final model with the optimal tuning paramter
  as component \code{finalModel}.

  For \code{lmridge.fit}, an object of class \code{lmridge}
  with the following components:

  \item{lambda}{a numeric vector containing the values of
  the penalty parameter.}

  \item{coefficients}{a numeric vector or matrix containing
  the coefficient estimates.}

  \item{fitted.values}{a numeric vector or matrix
  containing the fitted values.}

  \item{residuals}{a numeric vector or matrix containing
  the residuals.}

  \item{standardize}{a logical indicating whether the
  predictor variables were standardized to have unit
  variance.}

  \item{intercept}{a logical indicating whether the model
  includes a constant term.}

  \item{muX}{a numeric vector containing the means of the
  predictors.}

  \item{sigmaX}{a numeric vector containing the standard
  deviations of the predictors.}

  \item{muY}{numeric; the mean of the response.}

  \item{call}{the matched function call.}
}
\description{
  Fit ridge regression models and select the penalty
  parameter by estimating the respective prediction error
  via (repeated) \eqn{K}-fold cross-validation, (repeated)
  random splitting (also known as random subsampling or
  Monte Carlo cross-validation), or the bootstrap.
}
\author{
  Andreas Alfons
}
\references{
  Hoerl, A.E. and Kennard, R.W. (1970) Ridge regression:
  biased estimation for nonorthogonal problems.
  \emph{Technometrics}, \bold{12}(1), 55--67.
}
\seealso{
  \code{\link[perry]{perryTuning}}
}
\keyword{regression}

