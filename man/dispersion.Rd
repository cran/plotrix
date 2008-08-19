\name{dispersion}
\alias{dispersion}
\alias{dispbars}
\title{ Display a measure of dispersion }
\description{
 Display line/cap bars at specified points on a plot representing
  measures of dispersion.
}
\usage{
 dispersion(x,y,ulim,llim=ulim,arrow.cap=0.01,arrow.gap=NA,type="a",...)
 dispbars(x,y,ulim,llim=ulim,arrow.cap=0.01,arrow.gap=NA,...)
}
\arguments{
 \item{x,y}{x and y position of the centers of the bars}
 \item{ulim,llim}{The extent of the dispersion measures.}
 \item{arrow.cap}{The width of the cap at the outer end of each bar
  as a proportion of the width of the plot.}
 \item{arrow.gap}{The gap to leave at the inner end of each bar.
  Defaults to two thirds of the height of a capital "O".}
 \item{type}{What type of display to use.}
 \item{...}{additional arguments passed to \code{arrows} or \code{lines}
  depending upon \code{type}.}
}
\details{
 \code{dispersion} displays a measure of dispersion on an existing plot.
 Currently it will display either vertical lines with caps (error bars) or lines
 that form an "confidence band" around a line of central tendency.
 \code{dispbars} is now a call to \code{dispersion} with \code{type} set to
 \code{a}.
 
 If \code{arrow.gap} is greater than or equal to the upper or lower
 limit for a bar, \code{segments} is used to draw the upper and
 lower caps with no bars to avoid zero length arrows.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{arrows}}, \code{\link{segments}}, code{\link{lines}}}
\keyword{misc}
