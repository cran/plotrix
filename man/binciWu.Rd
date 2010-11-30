\name{binciWu}
\alias{binciWu}
\title{Upper binomial confidence interval}
\description{
 Calculates an upper binomial confidence interval using the Wilson approximation.
}
\usage{
 binciWu(x,n,alpha=0.05,trueval=NA,na.rm=TRUE)
}
\arguments{
 \item{x}{The number of successes or failures for which the CI is to be calculated.}
 \item{n}{The number of trials as above.}
 \item{alpha}{The desired coverage - 0.05 produces 95 percent coverage}
 \item{trueval}{The value representing the outcome of interest for the CI.}
 \item{na.rm}{Argument needed to make this work}
}
\details{
 \samp{binciWu} calculates the upper binomial confidence interval for the
 given number of successes and trials. It is mainly to allow binomial confidence
 intervals to be calculated in the \samp{brkdnNest} function, which is why
 the upper and lower CIs areg calculated separately.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{binciWl}}
\examples{
 binciWu(5,42)
}
\keyword{misc}
