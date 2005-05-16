\name{corner.loc}
\alias{corner.loc}
\alias{corner.label}
\title{Find corner points and add labels in corners}
\description{
 Finds the coordinates in user parameters of a specified corner of the
 figure region and/or puts a label there
}
\usage{
 corner.loc(x=-1,y=1,xoff=0.05,yoff=0.05)
 corner.label(label=NULL,x=-1,y=1,xoff=0.05,yoff=0.05,...)
}
\arguments{
 \item{label}{text to plot in the appropriate corner}
 \item{x}{an integer value: -1 for the left side of the plot, 1 for the
  right side}
 \item{y}{an integer value: -1 for the bottom side of the plot, 1 for
  the top side}
 \item{xoff}{horizontal offset as a proportion of the total plot size}
 \item{yoff}{vertical offset as a proportion of the total plot size}
 \item{...}{further arguments to the \code{text} command for the label}
}
\value{
 corner.loc returns a list of the x and y positions of the corner
}
\author{Ben Bolker}
\examples{
 plot(1:10,1:10)
 corner.label("A")
 corner.loc(1,1)
 corner.label("B",y=-1,x=1)
}
\keyword{aplot}
