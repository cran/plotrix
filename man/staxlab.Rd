\name{staxlab}
\alias{staxlab}
\title{ Place staggered labels on an axis }
\description{
 Places labels on an axis in a regular staggered order
}
\usage{staxlab(side=1,at,labels,nlines=2)}
\arguments{
 \item{side}{axis on which to place the labels, as in \code{axis}}
 \item{at}{where to place the labels in user units, as in \code{axis}}
 \item{labels}{text strings}
 \item{nlines}{How many lines to use to stagger the labels.}
}
\details{
 This function is mainly useful when either long axis labels or a large
 number of labels are to be placed without overlapping. It places the 
 labels regularly spaced and staggered along the axis specified.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{mtext}}
\examples{
 plot(1:10,axes=FALSE)
 box()
 nums<-c("one","two","three","four","five","six","seven","eight","nine","ten")
 staxlab(1,1:10,nums)
}
\keyword{misc}
