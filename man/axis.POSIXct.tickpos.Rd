\name{axis.POSIXct.tickpos}
\alias{axis.POSIXct.tickpos}
\title{Positions of the ticks on a POSIXt date/time axis}
\description{
 Calculates the positions of the ticks on a POSIXt date/time axis.
}
\usage{axis.POSIXct.tickpos(side,x,format,...}
\arguments{
 \item{side}{The side of the plot.}
 \item{x}{A POSIXct date object.}
 \item{format}{The optional date format.}
}
\value{
 The positions of the axis ticks in user (i.e. date/time) units.
}
\details{
 This function is only used to get the positions for the vertical grid
 lines used in \code{gantt.chart}. It is basically a copy of the 
 \code{axis.POSIXct} function without the call to \code{axis}.}
}
\author{Jim Lemon}
\seealso{\code{\link{gantt.chart}}}
\keyword{misc}
