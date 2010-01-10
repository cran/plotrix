\name{ladderplot}
\Rdversion{1.1}
\alias{ladderplot}
\alias{ladderplot.default}
\title{
Ladder Plot
}
\description{
Makes a ladder plot, similar to \code{\link[MASS]{parcoord}} but with more
flexibility and graphical options.
}
\usage{
ladderplot(x, ...)
\method{ladderplot}{default}(x, scale=FALSE, col=1, pch=19, lty=1, 
xlim=c(0.5, ncol(x) + 0.5), ylim=range(x), vertical = TRUE, ordered=FALSE,...)
}
\arguments{
  \item{x}{
A matrix or data frame with at least 2 columns.
}
  \item{scale}{
Logical, if the original data columns should be scaled to the unit (0-1) interval.
}
  \item{col}{
Color values to use for rows of \code{x}. If longer than 1, its value is recycled.
}
  \item{pch}{
Point type to use. If longer than 1, its value is recycled.
}
  \item{lty}{
Line type to use. If longer than 1, its value is recycled.
}
  \item{xlim, ylim}{
Limits for axes.
}
  \item{vertical}{
Logical, if the orientation of the ladderplot should be vertical or horizontal.
}
  \item{ordered}{
Logical, if the columns in \code{x} should be ordered.
}
  \item{\dots}{
Other arguments passed to the function \code{\link{stripchart}}.
}
}
\details{
The function uses \code{\link{stripchart}} to plot 1-D scatter plots for each column in \code{x}.
Then points are joined by lines for each rows of \code{x}.
}
\value{
Makes a plot as a side effect.
Returns \code{NULL} invisibly.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{lines}}, \code{\link{points}}, \code{\link{stripchart}}

Almost identical function: \code{\link[MASS]{parcoord}}
}
\examples{
x<-data.frame(A=c(1:10), B=c(2:11)+rnorm(10))
y<-data.frame(x, C=c(1:10)+rnorm(10))
opar <- par(mfrow=c(1,3))
ladderplot(x)
ladderplot(x, col=1:10, vertical=FALSE)
ladderplot(y, col=1:10)
par(opar)

## examples from parcoord
\dontrun{
if (require(MASS)) {
opar <- par(mfrow=c(2,3))
z1 <- state.x77[, c(7, 4, 6, 2, 5, 3)]
parcoord(z1, main="parcoord state.x77")
ladderplot(z1, pch=NA, scale=TRUE, main="ladderplot state.x77 original")
ladderplot(z1, main="ladderplot state.x77 original")
ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
z2 <- log(ir)[, c(3, 4, 2, 1)]
parcoord(z2, col = 1 + (0:149)%/%50, main="parcoord iris")
ladderplot(z2, scale=TRUE, col = 1 + (0:149)%/%50, 
    main="ladderplot iris original")
ladderplot(z2, col = 1 + (0:149)%/%50, main="ladderplot iris original")
par(opar)
}
}
}
\keyword{ aplot }
