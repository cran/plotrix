\name{gantt.chart}
\alias{gantt.chart}
\title{Display a Gantt chart}
\description{
 Displays a Gantt chart with priority coloring
}
\usage{
 gantt.chart(x=NULL,format="\%Y/\%m/\%d",xlim=NULL,taskcolors=NULL,
  main="",ylab="")
}
\arguments{
 \item{x}{a list of task labels, start/end times and
  task priorities as returned by \code{get.gantt.info}.}
 \item{format}{the format to be used in entering dates/times
  (see \code{\link{strptime}}).}
 \item{xlim}{the horizontal limits of the plot.}
 \item{taskcolors}{a vector of colors used to illustrate task priority.}
 \item{main}{the title of the plot - note that this is actually displayed
  using \code{mtext}.}
 \item{ylab}{vertical axis label - usually suppressed.}
}
\value{
 The list used to create the chart. This can be saved and reused rather 
 than manually entering the information each time the chart is displayed.
}
\author{Jim Lemon (original by Scott Waichler)}
\seealso{\code{\link{get.gantt.info}}, \code{\link{axis.POSIXct.tickpos}}}
\examples{
 Ymd.format<-"\%Y/\%m/\%d"
 gantt.info<-list(labels=
  c("First task","Second task","Third task","Fourth task","Fifth task"),
  starts=
  as.POSIXct(strptime(
  c("2004/01/01","2004/02/02","2004/03/03","2004/05/05","2004/09/09"),
  format=Ymd.format)),
  ends=
  as.POSIXct(strptime(
  c("2004/03/03","2004/05/05","2004/05/05","2004/08/08","2004/12/12"),
  format=Ymd.format)),
  priorities=c(1,2,3,4,5))
 gantt.chart(gantt.info,main="Calendar date Gantt chart")
}
\keyword{misc}
