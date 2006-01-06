\name{axis.mult}
\alias{axis.mult}
\title{ Display an axis with values having a multiplier }
\description{
 An axis is displayed on an existing plot where the tick values are divided
 by a multiplier and the multiplier is displayed next to the axis.
}
\usage{
 axis.mult(side=1,at=NULL,labels,mult=1,mult.label,mult.line,
  mult.labelpos=NULL,...)
}
\arguments{
 \item{side}{which side to display}
 \item{at}{where to place the tick marks - defaults to \code{axTicks()}}
 \item{labels}{tick labels - defaults to at/mult}
 \item{mult}{the multiplier factor}
 \item{mult.label}{the label to show the multiplier - defaults to "x mult"}
 \item{mult.line}{the margin line upon which to show the multiplier}
 \item{mult.labelpos}{where to place \code{mult.label} - defaults to centered
  and outside the axis tick labels}
 \item{...}{additional arguments passed to \code{axis}.}
}
}
\value{nil}
\details{
 \code{axis.mult} automates the process of displaying an axis with a 
 multiplier applied to the tick values. By default it will divide the
 default axis tick labels by \code{mult} and place \code{mult.label}
 where \code{xlab} or \code{ylab} would normally appear. Thus the plot
 call should set the relevant label to an empty string in such cases.
 It is simplest to call \code{plot} with \code{axes=FALSE} and then 
 display the box and any standard axes before calling \code{axis.mult}.
 While \code{axis.mult} will try to display an axis on any side, the top
 and right margins will require adjustment using \code{par} for
 \code{axis.mult} to display properly.
}
\author{Jim Lemon}
\seealso{\code{\link{axis}}, \code{\link{mtext}}}
\examples{
 plot(1:10*0.001,1:10*100,axes=FALSE,xlab="",ylab="",main="Axis multipliers")
 box()
 axis.mult(1,mult=0.001)
 axis.mult(2,mult=100,mult.label="Y value/100")
}
\keyword{misc}
