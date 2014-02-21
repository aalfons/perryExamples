# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

## add intercept column to design matrix
addIntercept <- function(x, check = FALSE) {
  if(!check || all(is.na(match(c("Intercept","(Intercept)"), colnames(x))))) {
    cbind("(Intercept)"=rep.int(1, nrow(x)), x)
  } else x
}

## remove intercept column from design matrix
removeIntercept <- function(x, pos) {
  if(missing(pos)) {
    pos <- match(c("Intercept","(Intercept)"), colnames(x), nomatch = 0)
    if(any(pos > 0)) x[, -pos, drop=FALSE] else x
  } else x[, -pos, drop=FALSE]
}
