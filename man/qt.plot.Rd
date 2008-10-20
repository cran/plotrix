\name{qt.plot}
\alias{qt.plot}
\title{ Quantity by interval plot }
\description{
 Display the counts of numeric quantities against the intervals between 
 quantities that are equal when rounded to integers.
}
\usage{
 qt.plot(qnt,qtime=NA,col=NULL,border="lightgray",
  main="Quantity x interval",xlab="Interval",ylab="Quantity",
  mar=c(5,4,4,4),...)
}
\arguments{
 \item{qnt}{Numeric vector}
 \item{qtime}{Numeric vector - may be a date as an integer.}
 \item{col}{The colors to fill the strips. NA for none.}
 \item{border}{border color for the polygons}
 \item{main}{The title of the plot.}
 \item{xlab,ylab}{Axis labels.}
 \item{mar}{margins for the plot - defaults to leave space for scale}
 \item{...}{additional arguments passed to \code{plot}.}
}
\details{
 The intervals calculated from \code{qtime} are the x values and the counts
 of values of \code{qnt} are the y values of the plot displayed as the widths
 of sections of polygons running across the time intervals. This plot was
 devised to display the distribution of drinking, but may be useful for any
 situation in which it is desired to display the distribution of numerically
 coded quantities against the intervals between their occurrence.
 
 \code{qt.plot} assumes that the values in \code{qtime} represent interpretable
 intervals like seconds or days. The default is to assume sequential time
 intervals. If \code{qtime} contains dates, they must be translated to
 numeric format. These values will be sorted by the function. If \code{qtime} is NA,
 it will be assigned \code{1:length(qnt)}.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{polygon}}}
\examples{
 # first a moderate drinker with infrequent big sessions
 qnt<-rpois(365,2)
 qtdates<-seq(as.Date("2007-01-01"),as.Date("2007-12-31"),by=1)
 qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
  ylab="Standard drinks per session")
 # now add monthly big sessions
 qnt[c(30,60,90,120,150,180,210,240,270,300,330,360)]<-rep(8,length.out=12)
 qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
  ylab="Standard drinks per session")
}
\keyword{misc}
