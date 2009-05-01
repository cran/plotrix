\name{barp}
\alias{barp}
\title{A bar plotting routine}
\description{Display a bar plot}
\usage{
 barp(height,width=0.4,names.arg=NULL,legend.lab=NULL,legend.pos="e",
 col=NULL,border=par("fg"),main=NULL,xlab="",ylab="",xlim=NULL,ylim=NULL,
 staxx=FALSE,staxy=FALSE, height.at=NULL,height.lab=NULL,
 cex.axis=par("cex.axis"),cylindrical=FALSE,shadow=FALSE,do.first=NULL)
}
\arguments{
 \item{height}{A numeric vector, matrix or data frame that will be represented
  as the heights of bars.}
 \item{width}{Half the width of a single bar or group of bars in X axis units.}
 \item{names.arg}{The labels for the bars or groups of bars.}
 \item{legend.lab}{Labels for an optional legend. If NULL, no legend is
  displayed.}
 \item{legend.pos}{Optional position for the legend as a list with \samp{x}
  and \samp{y} components. The default is to call \samp{\link{emptyspace}}
  to position the legend. If this is NA, \samp{locator} will be called.}
 \item{col}{The fill colors for the bars. The default is no fill.}
 \item{border}{The border for the bars.}
 \item{main}{The title at the top of the plot.}
 \item{xlab,ylab}{The labels for the X and Y axes respectively.}
 \item{xlim,ylim}{Optional horizontal and vertical limits for the plot.}
 \item{staxx,staxy}{Whether to use \samp{\link{staxlab}} to stagger the 
  X or Y axis tick labels.}
 \item{height.at}{Optional positions of the tick marks on the Y axis.}
 \item{height.lab}{Optional tick labels for the Y axis.}
 \item{cex.axis}{Character expansion for the axis labels.}
 \item{cylindrical}{Whether to give the bars a cylindrical appearance
  by shading them.}
 \item{shadow}{Whether to place a shadow behind the bars.}
 \item{do.first}{An optional expression that will be evaluated before anything
  else is displayed on the plot. Useful background colors or lines.}
}
\value{
 A list containing two components of the same form as \samp{height}:
 \item{x}{The centers of the bars displayed.}
 \item{y}{The heights of the bars.}
}
\details{
 \samp{barp} displays a bar plot similar to \samp{barplot} but with axes and
 horizontal bar positions more like \samp{plot}. Bars or groups of bars are
 centered on integral X values, and so both the width and spacing of the bars
 are controlled by a single number. If \samp{height} is a vector, single bars
 representing each value will be displayed centered at \samp{1:length(height)}.
 If \samp{height} is a matrix or data frame, a group of bars will be drawn for
 each column, with the values of the group taken from the rows of that column.
 The values from \samp{freq} or \samp{brkdn} in the prettyR package can be
 used as the \samp{height} argument. The value from \samp{table} can also be
 passed as \samp{height}.
 
 Bars are empty by default but fill colors can be defined in several ways.
 If a single color is passed, all bars will be the same color. If \samp{height}
 is a vector, colors will be recycled or some will be ignored if the length of
 \samp{col} is
 not equal to that of \samp{height}. If \samp{height} is a matrix or data frame,
 the user may pass a vector of colors equal to the number of rows in \samp{height}
 or a matrix of colors of the same dimensions as \samp{height}. Other sequences
 of color will probably not result in an easy to interpret plot.

 \samp{barp} is intended to simplify illustrating categorical data for which both
 the variable designations and the categories are names, as on many multiple
 choice questions.  \samp{height.at} and \samp{height.lab} allow the user to place
 labels on the vertical axis, usually representing the options. If \samp{staxx}
 or \samp{staxy} are TRUE, the labels on the horizontal or vertical axes
 respectively will be staggered, allowing the user to use many or lengthy variable
 or value labels.

 \samp{barp} allows two enhancements that may be useful in those areas where fancy
 plots are appreciated. One is to give the bars a cylindrical look by shading the
 color. The other is to place an apparent shadow behind each bar. Both of these
 effects appear as though the light is coming from the upper left, and this is
 hard coded. You can add error bars by calling \samp{dispbars}, but many advise against
 this.

 If \samp{legend.lab} is not NULL, a legend will be displayed. If 
 \samp{legend.pos} is NA, \samp{locator} is called to place the legend. On 
 Windows, the alert may not appear on the console, and the function will 
 appear to hang unless the user clicks on the console window or the plot.
}
\author{Jim Lemon}
\seealso{\samp{\link{staxlab}}, \samp{\link{barplot}}, \samp{\link{cylindrect}},
 \samp{\link{gradient.rect}}}
\examples{
 # get some extra room on the left
 par(mar=c(5,5,4,2))
 # make up some happiness data, as so many seem to do
 happyday<-data.frame(Monday=c(2.3,3.4),Tuesday=c(2.8,3.3),Wednesday=c(3.2,3.1),
 Thursday=c(3.6,2.8),Friday=c(4.2,2.6),Saturday=c(4.5,2.9),Sunday=c(4.1,2.8))
 happylabels<-c("Utterly dashed","Rather mopey","Indifferent","Somewhat elated",
  "Euphoric")
 barp(happyday,names.arg=names(happyday),legend.lab=c("Slaves","Unemployed"),
  legend.pos="e",col=c("#ee7700","#3333ff"),main="9AM happiness by weekday",
  xlab="Day of week",ylab="Happiness rating",ylim=c(1,5),staxx=TRUE,staxy=TRUE,
  height.at=1:5,height.lab=happylabels,cex.axis=0.9,cylindrical=TRUE,
  shadow=TRUE)
 # now do a plot with colors scaled to the sex ratio (real data!)
 # notice how zero and one have been added to get the full proportion range
 sexratio<-c(0,1,0.24,0.35,0.09,0.59,0.63,0.34,0.7,0.6)
 # the fun ratings are once again a pack of lies
 funrating<-c(3.2,3.5,1.5,5.4,4.5,2.7,6.8,4.9)
 funstudy<-c("Astronomy","Chemistry","Economics","Anthropology","Linguistics",
  "Math/Stats","Psychology","Sociology")
 funlabels<-c("Torture","Agony","Boredom","Neutral","Entertaining","Exhilarating",
  "Maniacal")
 # in the call to color.scale, the leading zero and one are dropped
 barp(funrating,names.arg=funstudy,main="Fun ratings for various areas of study",
  col=color.scale(sexratio[-c(1,2)],c(0.2,1),c(0.2,0.4),c(1,0.4)),xlab="Study",
  ylab="Rating",height.at=1:7,height.lab=funlabels,ylim=c(1,7),staxx=TRUE,
  staxy=TRUE,cex.axis=0.9)
 # here we want the full scale from zero to one
 color.legend(2,6,4,6.4,legend=c("100\% guys","100\% girls"),
  rect.col=color.scale(seq(0,1,by=0.25),c(0.2,1),c(0.2,0.4),c(1,0.4)))
 par(mar=c(5,4,4,2))
 # use barp to display a multiple histogram
 h1<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
 h2<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
 h3<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
 hmat<-matrix(c(h1,h2,h3),nrow=3,byrow=TRUE)
 barp(hmat,names.arg=names(h1),width=0.45,col=2:4,
  main="Multiple histogram using barp",xlab="Bins",ylab="Frequency")
 legend(3.8,50,c("h1","h2","h3"),fill=2:4)
}
\keyword{misc}
