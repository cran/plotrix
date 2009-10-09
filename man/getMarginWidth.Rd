\name{getMarginWidth}
\alias{getMarginWidth}
\title{Find the margin width necessary to fit text or a legend next to a plot.}
\description{
 Calculates the margin width necessary to fit text or a legend next to a plot.
}
\usage{
 getMarginWidth(side=4,labels,is.legend=FALSE)
}
\arguments{
 \item{side}{Which side of the plot (as in axis).}
 \item{labels}{The text to place next to the plot.}
 \item{is.legend}{Whether the text is in a legend or not.}
}
\details{
 \samp{getMarginWidth} reads parameters about the current plot and calculates the
 left or right (default) margin necessary to fit the strings passed as 
 \samp{labels} or a legend containing those strings.
}
\value{
 A two element list containing the number of margin lines necessary to
 fit the text or legend and the horizontal center of the margin in user units.
}
\author{Jim Lemon}
\examples{
 plot(rnorm(10))
 getMarginWidth(labels=c("Long label","Even longer label"))
}
\keyword{misc}
