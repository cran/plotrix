\name{piper.points}
\alias{piper.points}
\title{Display the points on a Piper diagram.}
\description{Displays the points for the anions and cations in the Piper
 diagram.}
\usage{
 piper.points(ca,mg,so4,cl,ions=data.frame(ca=ca,mg=mg,so4=so4,cl=cl),sites,
  ppm=TRUE,chull=FALSE,pch=3,main="",cex.pch=1,col=NA,pch.lwd=0.7,plot.sep=0.15)
}
\arguments{
 \item{ca,mg,so4,cl}{Concentrations of the ions.}
 \item{ions}{data frame containing the above values.}
 \item{sites}{indices for ions.}
 \item{ppm}{Whether the concentrations are in milligrams/liter (parts per
 million) or milliequivalents.}
 \item{chull}{Whether to display points or polygons for the concentrations.}
 \item{pch}{Symbol to use in plotting points.}
 \item{main}{title for the diagram}
 \item{cex.pch}{Expansion for the points.}
 \item{col}{Colors for the points/polygons.}
 \item{pch.lwd}{Line widths.}
 \item{plot.sep}{The offsets for the ions.}
}
\details{  
 \samp{piper.points} displays the points/polygons in the Piper diagram.
}
\value{A list containing the coordinates of the points and optionally
 information about the indices and colors.}
\author{Mike Cheetham}
\seealso{\samp{\link{piper.diagram}}}
\keyword{misc}
