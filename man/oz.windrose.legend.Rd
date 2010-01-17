\name{oz.windrose.legend}
\alias{oz.windrose.legend}
\title{Display an Australian wind rose legend.}
\description{
 Displays a wind rose legend in the style used by the Australian Bureau 
 of Meteorology.
}
\usage{
 oz.windrose.legend(speed.col=c("#dab286","#fe9a66","#ce6733","#986434"),
  speed.width=c(0.2,0.4,0.6,0.8),legend.pos=27)
}
\arguments{
 \item{speed.col}{Colors representing speed ranges.}
 \item{speed.width}{Half widths of the bars representing speed ranges.}
 \item{legend.pos}{The vertical position of the wind rose legend. The
  Australian Bureau of Meteorology displays the legend at the top of
  the plot}
}
\value{nil}
\author{Jim Lemon (thanks to Anna in the Sydney BoM office)}
\seealso{\link{oz.windrose}}
\examples{
 plot(0,xlim=c(-20,20),ylim=c(-20,20),type="n",axes=FALSE,xlab="",ylab="")
 par(xpd=TRUE)
 oz.windrose.legend()
 par(xpd=FALSE)
}
\keyword{misc}
