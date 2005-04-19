\name{get.gantt.info}
\alias{get.gantt.info}
\title{Gather the information to create a Gantt chart}
\description{
 Allows the user to enter the information for a Gantt chart.
}
\usage{get.gantt.info(format="\%Y/\%m/\%d")
}
\arguments{
 \item{format}{the format to be used in entering dates/times. Defaults to
  YYYY/mm/dd. See \code{\link{strptime}} for various date/time formats.}
}
\value{
 The list used to create the chart. Elements are:
 \item{labels}{The task labels to be displayed at the left of the chart.}
 \item{starts,ends}{The task starts/ends as POSIXct dates/times.}
 \item{priorities}{Task priorities as integers in the range 1 to 10.
  There can be less than 10 levels of priority, but if priorities do
  not start at 1 (assumed the highest), the default priority colors
  will be calculated from 1.}
}
\author{Jim Lemon (original by Scott Waichler)}
\seealso{\code{\link{gantt.chart}}}
\examples{
 # get task times using HH:MM (hour:minute) format
 get.gantt.info("\%H:\%M")
}
\keyword{misc}
