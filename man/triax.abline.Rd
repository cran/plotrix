\name{triax.abline}
\alias{triax.abline}
\title{Lines for triangle plot}
\description{
 Display lines on a triangle plot.
}
\usage{
 triax.abline(b=NULL,r=NULL,l=NULL,col=par("col"),lty=par("lty"),
  cc.axes=FALSE)
}
\arguments{
 \item{b}{Lines relating to the bottom axis.}
 \item{r}{Lines relating to the right axis.}
 \item{l}{Lines relating to the left axis.}
 \item{col}{Color(s) of the lines.}
 \item{lty}{Type(s) of the lines.}
 \item{cc.axes}{Clockwise/counterclockwise axes and ticks.}
}
\details{
 \code{triax.abline} displays one or more lines on a triangle plot. Lines
  are oriented in the conventional way, horizontal for the left axis, 
  slanting up to the right for the right axis and up to the left for the 
  bottom axis. If \code{cc.axes} is TRUE, the orientation is up-left for
  the left axis, horizontal for the right axis and up-right for the bottom
  axis.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{triax.plot}}}
\examples{
 triax.plot(data.frame(bottom=0.4,right=0.3,left=0.3),main="Triax ablines")
 triax.abline(l=0.3,col="red")
 triax.abline(r=0.3,col="green")
 triax.abline(b=0.4,col="blue")
}
\keyword{misc}
