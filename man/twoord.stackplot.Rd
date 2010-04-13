\name{twoord.stackplot}
\alias{twoord.stackplot}
\title{ Plot with two ordinates allowing stacked bars }
\description{
 Two sets of values are displayed on the same plot with different ordinate
 scales on the left and right. Barplots can be stacked.
}
\usage{
 twoord.stackplot(lx,rx,ldata,rdata,lcol,rcol,ltype,rtype,
 border,rylab,lylab,xlab,...,incrylim=NULL,halfwidth=0.4,
 mar=c(5,4,4,4))
}
\arguments{
 \item{lx,rx}{y and optional x values for the plot}
 \item{ldata,rdata}{optional data frames from which to obtain the above values}
 \item{lcol,rcol}{colors to distinguish the two sets of values}
 \item{ltype,rtype}{as in the \samp{type} argument to \samp{plot}}
 \item{border}{border color for the bars}
 \item{rylab,lylab}{optional labels for the y-axes}
 \item{xlab}{optional labels for x-axis - useful for things like dates}
 \item{...}{additional arguments passed to \samp{plot}}
 \item{incrylim}{optional extra space for the y limits}
 \item{halfwidth}{Half the width of the bars in user units. The bars are
  centered on successive integers if no \samp{x} values are supplied.}
 \item{mar}{margins for the plot}
}
\value{nil}
\details{
 \samp{twoord.stackplot} is an extension of the \samp{twoord.plot} function.
 Its principal advantage is that barplots can be stacked. The arguments are
 somewhat different, but most of the actual functions of \samp{twoord.plot}
 are present in \samp{twoord.stackplot}.
}
\author{Christophe Dutang}
\seealso{\link{twoord.plot}}
\examples{
 time <- 0:25
 A <- 1+1/2*sin(time/2)
 B <- A + rnorm(length(A), sd=1/10)
 B <- B + rnorm(length(A), sd=1/10)
 sizeA <- floor(450*(1 + 1/4*sin(time/2+2))*(1+0.1))
 sizeB <- 1000-sizeA
 C <- (A*sizeA + B*sizeB)/(sizeA+sizeB)
par(twoord.stackplot(lx=time, rx=time, ldata=cbind(sizeA, sizeB),
  rdata=cbind(A, B, C),  lcol=c("grey80", "white"),
  rcol=c("blue", "red","black"), ltype="bar", rtype=c("l","p","o"),
  border="grey80", lylab="Size", rylab="A,B,C", xlab="Time",
  main="Test of twoord.stackplot", incrylim=2/100))
}
\keyword{misc}
