\name{radial.grid}
\title{Display a radial grid}
\usage{
 radial.grid(labels=NA,label.pos=NULL,radlab=FALSE,radial.lim=NULL,
  start=0,clockwise=FALSE,label.prop=1.15,grid.pos,
  grid.col="gray",grid.bg="transparent")
}
\alias{radial.grid}
\arguments{
 \item{labels}{The labels to display around the circumference of the grid.}
 \item{label.pos}{Radial positions for the labels.}
 \item{radlab}{Whether to rotate the labels to a radial orientation.}
 \item{radial.lim}{Optional radial limits for the circular plot. If specified,
  these must be the same as the radial limits of the original plot.}
 \item{start}{The zero position on the plot in the units of \samp{label.pos}.}
 \item{clockwise}{Whether to increase angles clockwise rather than the
  default counterclockwise.}
 \item{label.prop}{Proportion of \samp{radial.lim} to place the labels.}
 \item{grid.pos}{Radial positions for the circular grid lines.}
 \item{grid.col}{Color for the radial grid lines.}
 \item{grid.bg}{Background color for the radial grid.}
}
\description{
 \samp{radial.grid} displays a radial grid for the radial.pie function.
}
\value{nil}
\author{Jim Lemon}
\keyword{misc}
