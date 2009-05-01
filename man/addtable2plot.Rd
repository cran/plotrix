\name{addtable2plot}
\alias{addtable2plot}
\title{Add a table of values to a plot}
\description{
 Displays a table of values at a user-specified position on an existing plot}
\usage{
 addtable2plot(x,y=NULL,table,lwd=par("lwd"),bty="n",bg=par("bg"),
  cex=1,xjust=0,yjust=1,box.col=par("fg"),text.col=par("fg"),
  display.colnames=TRUE,display.rownames=FALSE,hlines=FALSE,title=NULL)
}
\arguments{
 \item{x,y}{Either x and y coordinates to locate the table or an
  \samp{xy.coords} object.}
 \item{table}{A data frame, matrix or similar object that will be displayed.}
 \item{lwd}{The line width for the box and horizontal dividers.}
 \item{bty}{Whether to draw a box around the table ("o") or not ("n").}
 \item{bg}{The background color for the table.}
 \item{cex}{Character expansion for the table.}
 \item{xjust,yjust}{Positioning for the table relative to \samp{x,y}.}
 \item{box.col}{The color for the box and lines.}
 \item{text.col}{The color for the text.}
 \item{display.colnames}{Whether to display the column names in the table.}
 \item{display.rownames}{Whether to display the row names in the table.}
 \item{hlines}{Whether to draw horizontal lines between each row of the table.}
 \item{title}{Optional title placed over the table.}
}
\value{nil}
\details{
 \samp{addtable2plot} displays the values in \samp{table} at a position in user
 coordinates specified by \samp{x,y}. The two justification arguments,
 \samp{xjust} and \samp{yjust} are the same as in the \samp{legend} function,
 and \samp{addtable2plot} has been programmed to be as similar to \samp{legend}
 as possible. The defaults are those that were most popular in scientific
 journals at the time of programming.
}
\author{Original by John Kane, mods by Jim Lemon and Brian Diggs}
\seealso{\samp{\link{legend}}}
\examples{
 testdf<-data.frame(Before=c(10,7,5),During=c(8,6,2),After=c(5,3,4))
 rownames(testdf)<-c("Red","Green","Blue")
 barp(testdf,main="Test addtable2plot",ylab="Value",
  names.arg=colnames(testdf),col=2:4)
 # show most of the options
 addtable2plot(2,8,testdf,bty="o",display.rownames=TRUE,hlines=TRUE,
  title="The table")
}
\keyword{misc}
