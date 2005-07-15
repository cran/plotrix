\name{freq}
\alias{freq}
\title{Produce a frequency table}
\description{
 Calculates one or more frequency table(s) from a vector, matrix or data frame.
}
\usage{
 freq(x,variable.labels=NULL,display.na=TRUE)
}
\arguments{
  \item{x}{a vector, matrix or data frame.}
  \item{variable.labels}{optional labels for the variables. The default is
   the names of the variable passed or the \code{names} attribute if the
   variable has more than 1 dimension.}
  \item{display.na}{logical - whether to display counts of NAs.}
}
\details{
 \code{freq} calls \code{tabulate} to get the frequency counts and builds
 a list with one or more components containing the value labels and counts.
 Note that a limit on the number of bins has been removed, so passing a
 numeric vector with many levels may produce a huge, useless "frequency"
 table.
}
\value{A list with one or more components. Each component includes the 
 values of the relevant variable as the names.}
\author{Jim Lemon}
\seealso{\code{\link{print.freq}}}
\examples{
 test.df<-data.frame(A=sample(1:10,130,TRUE),C=sample(LETTERS[1:14],130,TRUE))
 freq(test.df)
}
\keyword{misc}
