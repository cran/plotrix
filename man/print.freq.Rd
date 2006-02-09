\name{print.freq}
\alias{print.freq}
\title{Display frequency table(s)}
\description{
 Displays one or more frequency tables produced by \code{freq}.
}
\usage{
 print.freq(x,show.pc=TRUE,...)
}
\arguments{
  \item{x}{a frequency table produced by \code{\link{freq}}}
  \item{show.pc}{whether to display percentages as well as counts.}
  \item{...}{additional arguments passed to \code{print}.}
}
\details{
 \code{print.freq} displays frequency tables produced by \code{freq}.
 If \code{show.pc} is TRUE and there is a value in the frequency table with
 the label "NA", an additional set of percentages ignoring that value will
 also be displayed.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{freq}}}
\examples{
 test.df<-data.frame(A=c(sample(1:10,99,TRUE),NA),C=sample(LETTERS,100,TRUE))
 test.freq<-freq(test.df)
 print(test.freq)
}
\keyword{misc}
