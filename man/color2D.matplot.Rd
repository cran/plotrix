\name{color2D.matplot}
\alias{color2D.matplot}
\title{Display a numeric matrix as color matrix}
\description{
 Display the values of a numeric 2D matrix or data frame as colored
  rectangles or hexagons.
}
\usage{
 color2D.matplot(x,redrange=c(0,1),greenrange=c(0,1),bluerange=c(0,1),
  extremes=NA,show.legend=FALSE,nslices=10,xlab="Column",ylab="Row",
  do.hex=FALSE,axes=TRUE,show.values=FALSE,vcol="white",vcex=1,...)
}
\arguments{
 \item{x}{data values}
 \item{redrange, greenrange, bluerange}{the ranges of red, green and blue
  that will be scaled to represent the range of numeric values}
 \item{extremes}{The colors for the extreme values of \code{x}. Takes
  precedence over the color ranges.}
 \item{show.legend}{whether to display a color range rectangle with the
  extreme numeric values in the lower left corner of the plot}
 \item{nslices}{The number of color "slices" in the legend.}
 \item{xlab,ylab}{axis labels for the plot.}
 \item{do.hex}{plot packed hexagons instead of rectangles.}
 \item{axes}{Whether to suppress the default axis labelling.}
 \item{show.values}{Whether to display the numeric values of \code{x}.}
 \item{vcol}{The color for the value display.}
 \item{vcex}{The character expansion for the value display.}
 \item{...}{arguments passed to \code{plot}.}
}
\value{nil}
\details{
 Displays a plot with the same number of rectangular cells as there are 
 numeric values in the matrix or data frame. Each rectangle is colored to
 represent its corresponding value. The rectangles are arranged in the
 conventional display of a 2D matrix with rows beginning at the top and
 columns at the left. The color scale defaults to black for the minimum
 value and white for the maximum.
 
 The user will have to adjust the plot device dimensions to get regular
 squares or hexagons, especially when the matrix is not square. As the
 margins are not equivalent for all display devices, this is currently
 a matter of trial and error.
}
\note{
 The function \code{\link{image}} performs almost the same when passed a
 matrix of values without grid positions, except that it assigns values to
 a specified list of colors rather than calculating a color for each distinct
 value.
}
\author{Jim Lemon (thanks to Ashoka Polpitiya for \code{axes})}
\seealso{\code{\link{color.scale}}, \code{\link{image}}}
\examples{
 x<-matrix(rnorm(1024)+sin(seq(0,2*pi,length=1024)),nrow=32)
 if(dev.interactive()) par(ask=TRUE)
 color2D.matplot(x,c(1,0),c(0,0),c(0,1),show.legend=TRUE,
  xlab="Columns",ylab="Rows",main="2D matrix plot")
 # now do hexagons
 color2D.matplot(x,c(1,0),c(0,0),c(0,1),show.legend=TRUE,
  xlab="Columns",ylab="Rows",do.hex=TRUE,main="2D matrix plot (hexagons)")
 # do a color only association plot
 xt<-table(sample(1:10,100,TRUE),sample(1:10,100,TRUE))
 observed<-xt[,rev(1:dim(xt)[2])]
 expected<-outer(rowSums(observed),colSums(observed),"*")/sum(xt)
 deviates<-(observed-expected)/sqrt(expected)
 color2D.matplot(x=round(deviates,2),redrange=c(1,0.5,0),
  greenrange=c(0,0.5,0),bluerange=c(0,1),
  show.values=TRUE,main="Association plot")
 par(ask=FALSE)
}
\keyword{misc}
