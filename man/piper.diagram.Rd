\name{piper.diagram}
\alias{piper.diagram}
\title{Display a Piper diagram.}
\description{Displays a Piper diagram.}
\usage{
 piper.diagram(ca,mg,so4,cl,ions=data.frame(ca=ca,mg=mg,so4=so4,cl=cl),
  sites=1:NROW(ions),new=FALSE,ppm=TRUE,chull=FALSE,tcsep=0.2,pch=3,
  main="",cex.lab=0.7,cex.tck=0.6,cex.pch=1,grid=TRUE,col=NA,ticklength=0.03,
  lwd.frame=1,pch.lwd=0.7,lwd.grid=lwd.frame,col.box="black",col.tck=col.box,
  col.grid="grey")
}
\arguments{
 \item{ca,mg,so4,cl}{Concentrations of the ions.}
 \item{ions}{data frame containing the above values.}
 \item{sites}{Indices for the ions.}
 \item{new}{Whether this is a new diagram.}
 \item{ppm}{Whether the concentrations are in milligrams/liter (parts per
 million) or milliequivalents.}
 \item{chull}{Whether to use the \samp{chull} function.}
 \item{tcsep}{Spacing for tick mark labels.}
 \item{pch}{Symbol to use for ion values.}
 \item{main}{Title for the diagram.}
 \item{cex.lab}{Expansion for the label text.}
 \item{cex.tck}{Expansion for the tick text.}
 \item{cex.pch}{Expansion for the point text.}
 \item{grid}{Whether to display a grid.}
 \item{col}{Colors for the points.}
 \item{ticklength}{Length for the ticks.}
 \item{lwd.frame}{Line width for the frame.}
 \item{pch.lwd}{Line width for the symbols.}
 \item{lwd.grid}{Line width for the grid.}
 \item{col.box}{Color for the box.}
 \item{col.tck}{Color for the ticks.}
 \item{col.grid}{Color for the grid.}
}
\value{The return value from piper.points.}
\author{Mike Cheetham}
\keyword{misc}
