\name{panes}
\alias{panes}
\title{Prepare a "panel" type layout}
\description{
 Split the graphics device into a "panel" type layout for a group of plots
}
\usage{
 panes(mat=NULL,widths=rep(1,ncol(mat)),heights=rep(1,nrow(mat)),
  nrow=2,ncol=2,mar=c(0,0,1.6,0),oma=c(2.5,1,1,1))
}
\arguments{
 \item{mat}{A matrix representing the number of panes to be created and
  their order of plotting.}
 \item{widths,heights}{The widths and heights of the panes.
  See \samp{layout}.}
 \item{nrow,ncol}{The numbers of rows and columns in the layout.
  See \samp{par(mfrow)}.}
 \item{mar}{The margins for each plot in the panes.}
 \item{oma}{The outer margins for the entire group of panes.}
}
\details{
 \samp{panes} combines the information for displaying a set of plots in
 a "panel" layout. The default values will usually produce the desired
 result by calling \samp{par(mfrow)}. If \samp{mat} is not NULL, the
 \samp{layout} function will be called instead of \samp{par(mfrow)}.
 The two methods are included for the convenience of the user.

 Note that \samp{panes} does not produce any plots and that the user
 must call \samp{tab.title} to get the "look" of the panel plot. The
 overall title is usually centered at the left edge (as in the example)
 or in the center of one of the plots in the bottom row.
}
\value{
 The values of \samp{par} options that existed when \samp{panes} was
 called. This list is usually used to restore those values.
}
\author{Jim Lemon}
\seealso{\link{par},\link{layout}}
\examples{
 y<-runif(8)
 oldpar<-panes()
 boxplot(y,axes=FALSE)
 box()
 tab.title("Boxplot of y",tab.col="#88dd88")
 barplot(y,axes=FALSE,col=2:9)
 box()
 tab.title("Barplot of y",tab.col="#88dd88")
 pie(y,col=2:9)
 tab.title("Pie chart of y",tab.col="#88dd88")
 box()
 plot(y,xaxs="i",xlim=c(0,9),axes=FALSE,col=2:9)
 box()
 tab.title("Scatterplot of y",tab.col="#88dd88")
 # center the title at the left edge of the last plot
 mtext("Test of panes function",at=0,side=1,line=0.8,cex=1.5)
 par(oldpar)
}
\keyword{misc}
