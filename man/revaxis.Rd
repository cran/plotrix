\name{revaxis}
\alias{revaxis}
\title{ Plot with axis direction(s) reversed. }
\description{
   Reverses the sense of either or both the \code{x} and \code{y} axes.
}
\usage{
revaxis(x, y, xrev=FALSE, yrev=TRUE, xside=if (yrev) 3 else 1,
       yside=if (xrev) 4 else 2, xlab=NULL, ylab=NULL, bty=NULL, ...)

}
\arguments{
   \item{x}{ Vector of \code{x}-coordinates of the data to be plotted. }
   \item{y}{ Vector of \code{y}-coordinates of the data to be plotted. }
   \item{xrev}{ Logical scalar; should the sense of the \code{x}-axis
    be reversed?}
   \item{yrev}{ Logical scalar; should the sense of the \code{y}-axis
    be reversed?}
   \item{xside}{ The side of the plot on which the \code{x}-axis labels
   should go.}
   \item{yside}{ The side of the plot on which the \code{y}-axis labels
   should go.}
   \item{xlab}{ Character string for labelling the \code{x}-axis. }
   \item{ylab}{ Character string for labelling the \code{y}-axis. }
   \item{bty}{ Single letter indicating the type of box to be drawn
   around the plot. See \code{\link{par}()} for the possible letters
   and their meaning.}
   \item{\dots}{ Other arguments to be passed to plot.}
}

\value{ nil }
\author{Rolf Turner}
\seealso{ \code{\link{plot}}, \code{\link{box}}, \code{\link{par}}}

\examples{
x <- runif(20)
y <- runif(20)
revaxis(x,y,yside=4)
}
\keyword{ hplot }
