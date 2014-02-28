\name{prostate}
\alias{prostate}
\docType{data}
\title{
Prostate cancer data
}
\description{
The data set contains information on the level of prostate-specific antigen in 
men who were about to receive a radical prostatectomy, together with several 
clinical measures.
}
\usage{data("prostate")}
\format{
  A data frame with 97 observations on the following 9 variables.
  \describe{
    \item{\code{lcavol}}{numeric; logarithm of cancer volume.}
    \item{\code{lweight}}{numeric; logarithm of prostate weight.}
    \item{\code{age}}{integer; age in years.}
    \item{\code{lbph}}{numeric; logarithm of the amount of benign prostatic 
    hyperplasia.}
    \item{\code{svi}}{integer; seminal vesicle invasion.}
    \item{\code{lcp}}{numeric; logarithm of capsular penetration.}
    \item{\code{gleason}}{integer; Gleason score.}
    \item{\code{pgg45}}{integer; percent of Gleason score 4 or 5.}
    \item{\code{lpsa}}{numeric; level of prostate-specific antigen.}
  }
}
\source{
Stamey, T., Kabalin, J., McNeal, J., Johnstone, I., Freiha, F., Redwine, E. and 
Yang, N. (1989) Prostate specific antigen in the diagnosis and treatment of 
adenocarcinoma of the prostate II. Radical prostatectomy treted patients. 
\emph{Journal of Urology}, \bold{141}(5), 1076--1083.
}
\examples{
data("prostate")
plot(prostate)
}
\keyword{datasets}
