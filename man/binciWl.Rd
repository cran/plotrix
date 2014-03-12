\name{binciWl}
\alias{binciWl}
\title{Lower binomial confidence limit}
\description{
 Calculates a lower binomial confidence limit using the Wilson approximation.
}
\usage{
 binciWl(x,n,alpha=0.05,trueval=TRUE,na.rm=TRUE)
}
\arguments{
 \item{x}{The number of successes or failures for which the CI is to be calculated.}
 \item{n}{The number of trials as above.}
 \item{alpha}{The desired coverage - 0.05 produces 95 percent coverage}
 \item{trueval}{The value representing the outcome of interest for the CI.}
 \item{na.rm}{Argument needed to make this work}
}
\details{
 \samp{binciWl} calculates the lower binomial confidence limit for the
 given number of successes and trials. It is mainly to allow binomial
 confidence limits to be calculated in the \samp{brkdnNest} function,
 which is why the upper and lower CIs are calculated separately.
}
\value{The lower binomial confidence limit}
\author{Jim Lemon}
\seealso{\link{binciWu}}
\examples{
 binciWl(5,42)
}
\keyword{misc}
