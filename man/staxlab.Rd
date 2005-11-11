\name{staxlab}
\alias{staxlab}
\title{ Place staggered labels on an axis }
\description{
 Places labels on an axis in a regular staggered order
}
\usage{
 staxlab(side=1,at,labels,nlines=2,top.line=0.5,line.spacing=0.8,...)
}
\arguments{
 \item{side}{axis on which to place the labels, as in \code{axis}}
 \item{at}{where to place the labels in user units, as in \code{axis}}
 \item{labels}{text strings}
 \item{nlines}{How many lines to use to stagger the labels.}
 \item{top.line}{Distance from the axis to place the first line of text.}
 \item{line.spacing}{Spacing between lines of text labels.} 
 \item{...}{Additional arguments to be passed to \code{mtext}.}
}
\details{
 This function is mainly useful when either long axis labels or a large
 number of labels are to be placed without overlapping. It places the 
 labels regularly spaced and staggered along the axis specified. The 
 user may wish to increase the space beneath the plot using 
 \code{mar} before calling \code{staxlab}.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{mtext}}}
\examples{
 plot(rnorm(12),axes=FALSE)
 box()
 months<-c("January","February","March","April","May","June",
  "July","August","September","October","November","December")
 staxlab(1,1:12,months)
}
\keyword{misc}
