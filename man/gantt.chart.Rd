\name{gantt.chart}
\alias{gantt.chart}
\title{Display a Gantt chart}
\description{
 Displays a Gantt chart with priority coloring
}
\usage{
 gantt.chart(x=NULL,format="\%Y/\%m/\%d",xlim=NULL,taskcolors=NULL,
  priority.legend=FALSE,vgridpos=NULL,vgridlab=NULL,
  vgrid.format="\%Y/\%m/\%d",half.height=0.25,hgrid=FALSE,main="",ylab="")
}
\arguments{
 \item{x}{a list of task labels, start/end times and task priorities
  as returned by \code{get.gantt.info}. If this is not present,
  \code{\link{get.gantt.info}} will be called.}
 \item{format}{the format to be used in entering dates/times
  (see \code{\link{strptime}}).}
 \item{xlim}{the horizontal limits of the plot.}
 \item{taskcolors}{a vector of colors used to illustrate task priority.}
 \item{priority.legend}{Whether to display a priority color legend.}
 \item{vgridpos}{optional positions of the vertical grid lines.}
 \item{vgridlab}{optional labels for the vertical grid lines.}
 \item{vgrid.format}{format for the vertical grid labels.}
 \item{half.height}{the proportion of the spacing between task bars that
  will be filled by the bar on each side - 0.5 will leave no space.}
 \item{hgrid}{logical - whether to display grid lines between the bars.}
 \item{main}{the title of the plot - note that this is actually displayed
  using \code{mtext}.}
 \item{ylab}{vertical axis label - usually suppressed.}
}
\value{
 The list used to create the chart - see \code{\link{get.gantt.info}} for
 details. This can be saved and reused rather than manually entering the 
 information each time the chart is displayed.
}
\details{
 If task priority colors are not wanted, simply mark every task as priority
 1 or set \code{taskcolors} to a single value to suppress the coloring.
}
\author{Jim Lemon (original by Scott Waichler - features by Ulrike Grömping)}
\seealso{\code{\link{get.gantt.info}}}
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
 vgridpos<-as.POSIXct(strptime(c("2004/01/01","2004/02/01","2004/03/01",
  "2004/04/01","2004/05/01","2004/06/01","2004/07/01","2004/08/01",
  "2004/09/01","2004/10/01","2004/11/01","2004/12/01"),format=Ymd.format))
 vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
 gantt.chart(gantt.info,main="Calendar date Gantt chart (2004)",
  priority.legend=TRUE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)
}
\keyword{misc}
