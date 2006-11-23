\name{oz.windrose}
\alias{oz.windrose}
\title{Display an Australian wind rose.}
\description{Displays a wind rose in the style used by the Australian
 Bureau of Meteorology.}
\usage{
 oz.windrose(windagg,speed.col=c("#dab286","#fe9a66","#ce6733","#986434"),
  speed.width=c(0.2,0.4,0.6,0.8),show.legend=TRUE,legend.pos=27,...)
}
\arguments{
 \item{windagg}{A matrix of percentages with the rows representing
  speed ranges and the columns indicating wind directions.}
 \item{speed.col}{Colors representing speed ranges.}
 \item{speed.width}{Half widths of the bars representing speed ranges.}
 \item{show.legend}{Logical indicating whether to display a legend.}
 \item{legend.pos}{The vertical position of the wind rose legend. The
  Australian Bureau of Meteorology displays the legend at the top of
  the plot}
 \item{...}{additional arguments passed to \code{plot}.}
}
\details{
 \code{oz.windrose} displays a wind rose in the style used by the Australian
  Bureau of Meteorology. Each limb represents a bin of wind directions, and 
  there are conventionally eight bins. If \code{windagg} has more than eight
  columns, more limbs will be displayed. The rows of \code{windagg} represent
  the speed ranges used by the Australian Bureau of Meteorology (0, 0-10, 
  10-20, 20-30 and over 30 in km/hour). The diameter of the central circle is
  calculated as (percent calm observations)/(number of direction bins). The
  remaining grid circles are spaced from the circumference of the "Calm"
  circle.
}
\note{
 If a title is desired, remember to move the legend to the bottom 
  of the plot. If the function is passed values that do not sum to 100, the
  resulting plot will at best be misleading.}
}
\value{nil}
\author{Jim Lemon (thanks to Anna in the Sydney BoM office)}
\seealso{\code{\link{oz.windrose.legend}}, \code{\link{draw.circle}}, 
 \code{\link{bin.wind.records}}}
\examples{
 windagg<-matrix(c(8,0,0,0,0,0,0,0,4,6,2,1,6,3,0,4,2,8,5,3,5,2,1,1,
  5,5,2,4,1,4,1,2,1,2,4,0,3,1,3,1),nrow=5,byrow=TRUE)
 oz.windrose(windagg)
}
\keyword{misc}