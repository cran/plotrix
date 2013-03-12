\name{box.heresy}
\alias{box.heresy}
\title{Display a sort of box plot}
\usage{
 box.heresy(x,y,uinner,linner=uinner,ulim,llim=ulim,intervals=FALSE,
 arrow.cap=NA,pch=22,main="",xlab="",ylab="",xaxlab=NA,col="white",do.first=NULL,...)
}
\arguments{
 \item{x,y}{Vectors of numeric values representing measures of central tendency.}
 \item{uinner,linner}{Vectors of numeric values representing "inner" measures of
  dispersion.}
 \item{ulim,llim}{Vectors of numeric values representing "outer" measures of
  dispersion.}
 \item{intervals}{Whether the values for dispersion are intervals (TRUE) or
  absolute limits (FALSE).}
 \item{arrow.cap}{The width of the cap on the "whiskers" relative to the width
  of the plot. Defaults to the same width as the outer box.}
 \item{pch}{The symbol to be used to represent the measure(s) of central tencency
  in the box.}
 \item{main}{The title for the plot (i.e. \samp{main}).}
 \item{xlab,ylab}{The x and y axis labels.}
 \item{xaxlab}{Optional labels for the boxes.}
 \item{col}{The fill colors for the "inner" rectangles.}
 \item{do.first}{An expression that will be evaluated before anything is displayed.}
 \item{...}{additional arguments passed to the \samp{dispersion} function.}
}
\description{
 \samp{box.heresy} displays a box plot in which a symbol represents a measure of
 central tendency, a surrounding box that represents an "inner" measure of dispersion
 (e.g. standard error) and whiskers represent an "outer" measure of dispersion
 (e.g. standard deviation). The function is pretty basic at this time and will 
 probably change a bit.
}
\value{nil}
\author{Jim Lemon - thanks to Gianni Lavaredo for the suggestion}
\keyword{misc}
\seealso{\link{plot}, \link{boxplot}}
\examples{
 y<-runif(5)
 ulim<-runif(5)
 llim<-runif(5)
 uinner<-ulim/2
 linner<-llim/2
 box.heresy(y,uinner=uinner,linner=linner,ulim=ulim,llim=llim,
  intervals=TRUE,main="The heretical boxplot",
  xlab="Number of observations",ylab="Value")
}
