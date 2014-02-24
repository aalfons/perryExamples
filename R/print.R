# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' @S3method print ladlasso
#' @S3method print lmridge
print.ladlasso <- print.lmridge <- function(x, ...) {
  # print coefficients
  cat("\nCoefficients:\n")
  print(coef(x), ...)
  # return object invisibly
  invisible(x)
}

# ## @S3method print ladlasso
# print.ladlasso <- function(x, ...) {
#   # print function call
#   if(!is.null(call <- x$call)) {
#     cat("\nCall:\n")
#     dput(call)
#   }
#   # print coefficients
#   cat("\nCoefficients:\n")
#   print(coef(x), ...)
#   # print penalty parameter
#   lambda <- as.matrix(x$lambda)
#   dimnames(lambda) <- list("Penalty parameter:", "")
#   print(lambda, ...)
#   # return object invisibly
#   invisible(x)
# }
#
# ## @S3method print lmridge
# print.lmridge <- function(x, ...) {
#   # print function call
#   if(!is.null(call <- x$call)) {
#     cat("\nCall:\n")
#     dput(call)
#   }
#   # print coefficients
#   cat("\nCoefficients:\n")
#   print(coef(x), ...)
#   # print penalty parameters
#   lambda <- x$lambda
#   if(length(lambda) == 1) {
#     lambda <- as.matrix(lambda)
#     dimnames(lambda) <- list("Penalty parameter:", "")
#     print(lambda, ...)
#   } else {
#     cat("\nPenalty parameters:\n")
#     print(lambda, ...)
#   }
#   # return object invisibly
#   invisible(x)
# }
