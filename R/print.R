# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @S3method print ladlasso
#' @S3method print lasso
print.ladlasso <- print.lasso <- function(x, zeros = FALSE, ...) {
  # print coefficients
  cat("\nCoefficients:\n")
  print(coef(x, zeros=zeros), ...)
  # return object invisibly
  invisible(x)
}

#' @S3method print ridge
print.ridge <- function(x, ...) {
  # print coefficients
  cat("\nCoefficients:\n")
  print(coef(x), ...)
  # return object invisibly
  invisible(x)
}
