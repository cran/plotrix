\encoding{latin1}
\name{gantt.chart}
\alias{gantt.chart}
\title{Display a Gantt chart}
\description{
 Displays a Gantt chart with priority coloring
}
\usage{
 gantt.chart(x=NULL,format="\%Y/\%m/\%d",xlim=NULL,taskcolors=NULL,
  priority.legend=FALSE,vgridpos=NULL,vgridlab=NULL,vgrid.format="\%Y/\%m/\%d",
  half.height=0.25,hgrid=FALSE,main="",xlab="",cylindrical=FALSE)
}
\arguments{
 \item{x}{a list of task labels, start/end times and task priorities
  as returned by \samp{get.gantt.info}. If this is not present,
  \samp{\link{get.gantt.info}} will be called.}
 \item{format}{the format to be used in entering dates/times
  (see \samp{\link{strptime}}).}
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
  using \samp{mtext}.}
 \item{xlab}{horizontal axis label - usually suppressed.}
 \item{cylindrical}{Whether to give the bars a cylindrical appearance.}
}
\value{
 The list used to create the chart - see \samp{\link{get.gantt.info}} for
 details. This can be saved and reused rather than manually entering the 
 information each time the chart is displayed.
}
\details{
 If task priority colors are not wanted, set \samp{taskcolors} to a single
 value to suppress the coloring. If this is not done, \samp{rainbow} will
 be called to generate a different color for each task.
 
 There can now be more than one time interval for each task. If there is,
 more than one bar will be displayed for each "task", which may not be a
 task at all, but intervals of some attribute for each entity. See the last
 example.
}
\author{Jim Lemon (original by Scott Waichler - features by Ulrike Gromping)}
\seealso{\samp{\link{get.gantt.info}}}
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
 # if both vgidpos and vgridlab are specified,
 # starts and ends don't have to be dates
 info2<-list(labels=c("Jim","Joe","Jim","John","John","Jake","Joe","Jed","Jake"),
  starts=c(8.1,8.7,13.0,9.1,11.6,9.0,13.6,9.3,14.2),
  ends=c(12.5,12.7,16.5,10.3,15.6,11.7,18.1,18.2,19.0))
 gantt.chart(info2,vgridlab=8:19,vgridpos=8:19,
  main="Sitting at desk",taskcolors="lightgray")
}
\keyword{misc}
