\name{centipede.plot}
\alias{centipede.plot}
\title{Display a centipede plot}
\description{
 Displays a centipede plot on the current graphics device.
}
\usage{
 centipede.plot(segs,midpoint="mean",lower.limit="std.error",
  upper.limit="std.err",left.labels=NULL,right.labels=NULL,sort.segs=TRUE,
  main="",xlab="",vgrid=NA,mar=NA,col=par("fg"),bg="green",...)
}
\arguments{
  \item{segs}{a matrix of midpoints and limits calculated by
   \code{\link{get.segs}}.}
  \item{midpoint}{The function to use in calculating the midpoint of each
   segment.}
  \item{lower.limit}{The functions to use in calculating the
   lower limits for each subset of the data.}
  \item{upper.limit}{The functions to use in calculating the
   upper limits.}
  \item{left.labels}{The variable or subset labels to place 
   at the left margin of the plot. Default values are provided.}
  \item{right.labels}{The variable or subset labels to place 
   at the right margin of the plot.}
  \item{sort.segs}{Whether to sort the segments in ascending order.}
  \item{main}{Optional title for the plot.}
  \item{xlab}{Optional x axis label for the plot.}
  \item{vgrid}{Optional vertical line(s) to display on the plot.}
  \item{mar}{Margin widths for the plot. Defaults to c(4,5,1,4) or 
   c(4,5,3,4) if there is a title.}
  \item{col}{The color(s) of the limit lines and borders of the midpoint
   markers.}
  \item{bg}{The color(s) to fill the midpoint markers.}
  \item{...}{additional arguments passed to \code{plot}.}
}
\details{
 \code{centipede.plot} displays one or more midpoints and limits as 
  filled circles with horizontal error bars. It places labels on the left and
  right sides of the plot. If these labels are long, it may be necessary to
  pass explicit values to the \code{mar} argument to leave enough room.
  
  Similarly, centipede plots typically have a large number of subsets, and 
  it may be necessary to start the graphics device with an aspect ratio that
  will prevent crowding of the labels when over 30 segments are displayed.
  
  The matrix \code{segs} may be entered manually or read from a file.
  The first row specifies midpoints, the second and third rows the lower
  and upper limits respectively and the fourth row the number of valid
  observations.
}
\note{
 This type of plot is also known as a caterpillar plot or a league table.
}
\value{nil.}
\author{Jim Lemon}
\seealso{\code{\link{get.segs}}}
\examples{
 testcp<-list("",40)
 for(i in 1:40) testcp[[i]]<-rnorm(sample(1:8,1)*50)
 segs<-get.segs(testcp)
 centipede.plot(segs,main="Test centipede plot",vgrid=0)
}
\keyword{misc}
