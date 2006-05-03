\name{triax.abline}
\alias{triax.abline}
\title{Lines for triangle plot}
\description{
 Display lines on a triangle plot.
}
\usage{
 triax.abline(l=NULL,r=NULL,b=NULL,col=par("col"),lty=par("lty"))
}
\arguments{
 \item{l}{Lines relating to the left axis.}
 \item{r}{Lines relating to the right axis.}
 \item{b}{Lines relating to the botton axis.}
 \item{col}{Color(s) of the lines.}
 \item{lty}{Type(s) of the lines.}
}
\details{
 \code{triax.abline} displays one or more lines on a triangle plot. Lines
  are oriented in the conventional way, horizontal for the left axis, 
  slanting up to the right for the right axis and up to the left for the 
  bottom axis.
}
\value{A list of the \code{x,y} positions plotted.}
\author{Jim Lemon}
\seealso{\code{\link{triax.plot}}}
\examples{
 triax.plot(data.frame(l=0.3,r=0.3,b=0.4),main="Triax ablines")
 triax.abline(l=0.2,col="red")
 triax.abline(r=0.4,col="green")
 triax.abline(b=0.6,col="blue")
}
\keyword{misc}
