\name{clplot}
\title{Plot lines with colors determined by values.}
\usage{
 clplot(x,y,ylab=deparse(substitute(y)),xlab=deparse(substitute(x)),
  levels=seq(min(y)+(max(y)-min(y))/5,max(y)-(max(y)-min(y))/5,length.out=4),
  cols=c("black","blue","green","orange","red"),showcuts=FALSE,...)
}
\alias{clplot}
\arguments{
 \item{x,y}{numeric data vectors.}
 \item{ylab,xlab}{Labels for the X and Y axes.}
 \item{levels}{Cut points to assign colors to the values of \code{x} and
  \code{y}.}
 \item{cols}{The colors to be assigned.}
 \item{showcuts}{Whether to show the positions of the cut points.}
 \item{...}{additional arguments passed to \code{plot} or \code{lines}.}
}
\description{
 \code{clplot} displays a plot of lines for which the colors are dependent
 upon the x and y values. \code{clplot} is similar to \code{color.scale.lines}
 except that while the latter calculates a color for each unique value,
 \code{clplot} assigns colors to groups of values within the cutpoints defined
 by \code{levels}.
}
\value{nil}
\author{Carl Witthoft}
\seealso{\code{\link{plot}}}
\examples{
 x<-seq(1,100)
 y<-sin(x/5)+x/20
 clplot(x,y,main="Test of clplot")
}
\keyword{misc}
