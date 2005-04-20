\name{axis.break}
\alias{axis.break}
\title{ Place a "break" mark on an axis }
\description{
 Places a "break" mark on an axis on an existing plot
}
\usage{axis.break(axis=1,breakpos=NULL,bgcol="white",breakcol="black",
  style="slash",brw=0.02)}
\arguments{
 \item{axis}{which axis to break}
 \item{breakpos}{where to place the break in user units}
 \item{bgcol}{the color of the plot background}
 \item{breakcol}{the color of the "break" marker}
 \item{style}{Either \code{slash} or \code{zigzag}}
 \item{brw}{break width relative to plot width}
}
}
\value{nil}
\author{Jim Lemon and Ben Bolker}
\examples{
 plot(3:10)
 # put a break at the default axis and position
 axis.break()
 axis.break(2,2.9,style="zigzag")
}
\keyword{misc}
