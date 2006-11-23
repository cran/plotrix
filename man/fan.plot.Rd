\name{fan.plot}
\alias{fan.plot}
\title{ Display a fan plot. }
\description{
 Displays numerical values as overlapping sectors.
}
\usage{
fan.plot(x,edges=200,radius=1,col=NULL,align.at=NULL,labels=NULL,
 labelpos=NULL,label.radius=1.1,align="left",shrink=0.02,main="",
 ticks=NULL,include.sumx=FALSE,...)
}
\arguments{
 \item{x}{Vector of numbers.}
 \item{edges}{The number of edges with which to draw a circle.}
 \item{radius}{The radius of the sectors.}
 \item{col}{The colors with which to fill the sectors.}
 \item{align.at}{Where to align the sectors (see Details).}
 \item{labels}{Labels placed around the sector arcs.}
 \item{labelpos}{Optional circumferential positions for the labels.}
 \item{label.radius}{How far away from the sectors the labels will
  be placed.}
 \item{align}{Position of the alignment of sectors (see Details).}
 \item{shrink}{How much to shrink each successive sector in user units.}
 \item{main}{Optional title for the plot.}
 \item{ticks}{The number of ticks that would appear on the circumference
  of the complete circle. Default is no ticks, TRUE gives the number of
  ticks equal to the integer sum of \code{x}, which is fairly sensible if 
  \code{x} is a vector of integers.}
 \item{include.sumx}{Whether to include the sum of all \code{x} values
  as the largest sector.}
 \item{...}{Additional arguments passed to \code{polygon}.}
}
\details{
 The fan plot is a variant of the pie chart that places the sectors
 "on top" of each other from the largest to the smallest. By default,
 the largest sector is centered with its circumferential arc upwards,
 giving the plot the appearance of a folding fan. Passing a value for
 \code{align.at} will place the point of alignment at that angle in 
 radians. The sectors may be aligned at either the left or right edges
 or in the center. Note that \code{align} must be one of \code{left right}
 or \code{center}. Each successive sector is radially "shrunk" by a
 constant amount so that two equal sectors will both be visible.
 
 In cases where there are several segments with very small differences,
 the labels may be crowded. There is a simple routine in the function to
 spread out crowded labels, but it may not be sufficient for severe
 crowding. By capturing the return value and manually altering 
 the label positions, the crowded labels can be separated. This new
 vector of positions may then be passed as \code{labelpos}.
}
\value{The circumferential positions of the labels in radians.}
\author{Jim Lemon, Anupam Tyagi}
\seealso{\code{\link{floating.pie}}}
\examples{
 # IUCN counts of threatened species by geographical area
 iucn.df<-data.frame(area=c("Africa","Asia","Europe","N&C America",
  "S America","Oceania"),threatened=c(5994,7737,1987,4716,5097,2093))
 fan.plot(iucn.df$threatened,labels=iucn.df$area,main="Test of Fan Plot",
  ticks=276)
}
\keyword{misc}
