\name{thigmophobe}
\title{Find the direction away from the closest point}
\description{Find the direction away from the closest point}
\usage{
 thigmophobe(x,y,plot.span=par("usr"),xlog=par("xlog"),ylog=par("ylog"))
}
\alias{thigmophobe}
\arguments{
 \item{x,y}{Numeric data vectors. Typically the x/y coordinates of
  plotted points. If arrays are passed, they will be silently coerced to
  numeric vectors.}
 \item{plot.span}{The extent of the plot in user units.}
 \item{xlog,ylog}{Flags for logarithmic axes. See Note.}
}
\details{
 \samp{thigmophobe} returns the direction (as 1|2|3|4 - see pos= in 
 \samp{text}) away from the nearest point to each of the points 
 described by \samp{x} and \samp{y}.
}
\note{
 \samp{thigmophobe} is typically used to get the offset to automatically
  place labels on a scatterplot or similar using \samp{thigmophobe.labels}
 to avoid overlapping labels. The name means "one who fears being touched".
 
 The \samp{plot.span}, \samp{xlog} and \samp{ylog} arguments were added to
 allow \samp{thigmophobe} to be used outside of base graphics.
}
\value{
 A vector of directions away from the point nearest to each point.
}
\keyword{misc}
\author{Jim Lemon - thanks to Gustaf Rydevik for the "names" bug fix,
 Steve Ellison for the suggestion about arrays and Duncan Murdoch for 
 the additional arguments.}
\seealso{\link{thigmophobe.labels}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 thigmophobe(x,y)
}
