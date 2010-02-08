\name{color2D.matplot}
\alias{color2D.matplot}
\title{Display a numeric matrix as color matrix}
\description{
 Display the values of a numeric 2D matrix or data frame as colored
  rectangles or hexagons.
}
\usage{
 color2D.matplot(x,redrange=c(0,1),greenrange=c(0,1),bluerange=c(0,1),
  extremes=NA,cellcolors=NA,show.legend=FALSE,nslices=10,xlab="Column",
  ylab="Row",do.hex=FALSE,axes=TRUE,show.values=FALSE,vcol=NA,vcex=1,
  border="black",na.color=NA,...)
}
\arguments{
 \item{x}{data values}
 \item{redrange, greenrange, bluerange}{the ranges of red, green and blue
  that will be scaled to represent the range of numeric values}
 \item{extremes}{The colors for the extreme values of \samp{x}. Takes
  precedence over the color ranges.}
 \item{cellcolors}{A precalculated vector of cell colors. This must have
  exactly the same number of colors as there are values in the matrix or
  it will be uninformative. Takes precedence over both \samp{extremes}
  and color ranges.}
 \item{show.legend}{whether to display a color legend with the
  extreme numeric values in the lower left corner of the plot. If the
  default is not suitable, call \samp{color.legend} separately.}
 \item{nslices}{The number of color "slices" in the legend.}
 \item{xlab,ylab}{axis labels for the plot.}
 \item{do.hex}{plot packed hexagons instead of rectangles.}
 \item{axes}{Whether to suppress the default axis labelling.}
 \item{show.values}{Whether to display the numeric values of \samp{x}.
  This also controls the number of decimal places displayed.}
 \item{vcol}{The color for the value display. If NA, the values are displayed
  in black or white depending upon the darkness of the cell color.}
 \item{vcex}{The character expansion for the value display.}
 \item{border}{The color(s) for the borders of the cells. Pass NA
  if no border is  wanted.}
 \item{na.color}{The color to use for NA values of \samp{x}.}
 \item{...}{arguments passed to \samp{plot}.}
}
\value{nil}
\details{
 Displays a plot with the same number of rectangular or hexagonal cells as
 there are numeric values in the matrix or data frame. Each rectangle is
 colored to represent its corresponding value. The rectangles are arranged
 in the conventional display of a 2D matrix with rows beginning at the top
 and columns at the left. The color scale defaults to black for the minimum
 value and white for the maximum.
 
 The user will have to adjust the plot device dimensions to get regular
 squares or hexagons, especially when the matrix is not square. As the
 margins are not equivalent for all display devices, this is currently
 a matter of trial and error. Drawing hexagons is quite slow.
 
 \samp{show.values} and \samp{show.legend} are also used to control the
 number of decimal places displayed if the values or legend are shown.
 \samp{TRUE} will give one decimal place, \samp{2} two, and so on.
}
\note{
 The function \link{image} performs almost the same when passed a
 matrix of values without grid positions, except that it assigns values to
 a specified list of colors rather than calculating a color for each distinct
 value.
}
\author{Jim Lemon (thanks to Ashoka Polpitiya for \samp{axes})}
\seealso{\link{color.scale}, \link{image}}
\examples{
 x<-matrix(rnorm(1024),nrow=32)
 # simulate a correlation matrix with values -0.5 to 0.5
 x<-rescale(x,c(-0.5,0.5))
 # add a column with the extreme values (-1,1) to calculate
 # the colors, then drop the extra column in the result
 cellcol<-color.scale(cbind(x,c(-1,rep(1,31))),c(0,1),0,c(1,0))[,1:32]
 color2D.matplot(x,cellcolors=cellcol,main="Blue to red correlations")
 # do the legend call separately to get the full range
 color.legend(0,-5,6.5,-3,legend=c(-1,-0.5,0,0.5,1),
  rect.col=color.scale(c(-1,-0.5,0,0.5,1),c(0,1),0,c(1,0)),align="rb")
 x<-matrix(rnorm(100),nrow=10)
 # generate colors that show negative values in red to brown
 # and positive in blue-green to green
 cellcol<-matrix(rep("#000000",100),nrow=10)
 cellcol[x<0]<-color.scale(x[x<0],c(1,0.8),c(0,0.8),0)
 cellcol[x>0]<-color.scale(x[x>0],0,c(0.8,1),c(0.8,0))
 # now do hexagons without borders
 color2D.matplot(x,cellcolors=cellcol,xlab="Columns",ylab="Rows",
  do.hex=TRUE,main="2D matrix plot (hexagons)",border=NA)
 # for this one, we have to do the color legend separately
 # because of the two part color scaling
 legval<-seq(min(x),max(x),length.out=6)
 legcol<-rep("#000000",6)
 legcol[legval<0]<-color.scale(legval[legval<0],c(1,0.8),c(0,0.8),0)
 legcol[legval>0]<-color.scale(legval[legval>0],0,c(0.8,1),c(0.8,0))
 color.legend(0,-1.8,3,-1.4,round(c(min(x),0,max(x)),1),rect.col=legcol)
 # do a color only association plot
 xt<-table(sample(1:10,100,TRUE),sample(1:10,100,TRUE))
 observed<-xt[,rev(1:dim(xt)[2])]
 expected<-outer(rowSums(observed),colSums(observed),"*")/sum(xt)
 deviates<-(observed-expected)/sqrt(expected)
 cellcol<-matrix(rep("#000000",100),nrow=10)
 cellcol[deviates<0]<-
  color.scale(deviates[deviates<0],c(1,0.8),c(0,0.5),0)
 cellcol[deviates>0]<-
  color.scale(deviates[deviates>0],0,c(0.7,0.8),c(0.5,0))
 color2D.matplot(x=round(deviates,2),cellcolors=cellcol,
  show.values=TRUE,main="Association plot")
}
\keyword{misc}
