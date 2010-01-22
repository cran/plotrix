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
 \item{...}{additional arguments passed to \samp{plot}.}
}
\details{
 The intervals calculated from \samp{qtime} are the x values and the counts
 of values of \samp{qnt} are the y values of the plot displayed as the widths
 of sections of polygons running across the time intervals. This plot was
 devised to display the distribution of drinking, but may be useful for any
 situation in which it is desired to display the distribution of numerically
 coded quantities against the intervals between their occurrence. Note that if
 there are many values and many intervals, the resulting plot will be mostly
 empty. Categorizing the values and intervals so that there are only three or
 four categories will often produce a more informative plot.
 
 \samp{qt.plot} assumes that the values in \samp{qtime} represent interpretable
 intervals like seconds or days. The default is to assume sequential time
 intervals. If \samp{qtime} contains dates, they must be translated to
 numeric format. These values will be sorted by the function. If \samp{qtime} is NA,
 it will be assigned \samp{1:length(qnt)}.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{polygon}}
\examples{
 # first a moderate drinker with frequent bigger sessions
 qnt<-sample(0:5,365,TRUE,prob=c(0.02,0.1,0.4,0.3,0.1,0.08))
 qtdates<-seq(as.Date("2007-01-01"),as.Date("2007-12-31"),by=1)
 qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
  ylab="Standard drinks per session")
 # now add monthly bigger sessions and notice how this
 qnt[c(30,60,90,120,150,180,210,240,270,300,330,360)]<-rep(4:5,length.out=12)
 qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
  ylab="Standard drinks per session")
}
\keyword{misc}
