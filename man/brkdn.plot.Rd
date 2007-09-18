\name{brkdn.plot}
\alias{brkdn.plot}
\title{A point/line plotting routine}
\description{Display a point/line plot of breakdowns of one or more variables.}
\usage{
 brkdn.plot(vars,groups=NA,obs=NA,data,mct="mean",md="std.error",stagger=NA,
 dispbar=TRUE,main="Breakdown plot",xlab=NA,ylab=NA,xaxlab=NA,
 ylim=NA,type="b",pch=1,lty=1,col=par("fg"),...)
}
\arguments{
 \item{vars}{The names or indices of one or more columns in a data frame.
  The columns must contain numeric data.}
 \item{groups}{The name or index of a column in a data frame that classifies
  the values in \code{vars} into different, usually fixed effect, levels.}
 \item{obs}{The name or index of a column in a data frame that classifies
  the values in \code{vars} into different, usually random effect, levels.}
 \item{data}{The data frame.}
 \item{mct}{The measure of central tendency to calculate for each group.}
 \item{md}{The measure of dispersion to calculate, NA for none.}
 \item{stagger}{The amount to offset the successive values at each horizontal
  position as a proportion of the width of the plot. The calculated default
  is usually adequate. Pass zero for none.}
 \item{dispbar}{Whether to display the measures of dispersion as bars.}
 \item{main}{The title at the top of the plot.}
 \item{xlab,ylab}{The labels for the X and Y axes respectively. There are
  defaults, but they are basic.}
 \item{xaxlab}{Optional labels for the horizontal axis ticks.}
 \item{ylim}{Optional vertical limits for the plot.}
 \item{type}{Whether to plot symbols, lines or both (as in \code{plot}).}
 \item{pch}{Symbol(s) to plot.}
 \item{lty}{Line type(s) to plot.}
 \item{col}{Color(s) for the symbols and lines.}
 \item{...}{additional arguments passed to \code{plot}.}
}
\value{
 A list of two matrices of dimension \code{length(levels(b))} by 
 \code{length(levels(c))}. The first contains the measures of central
 tendency calculated and its name is the name of the function passed as
 \code{mct}. The second contains the measures of dispersion and its name
 is the name of the function passed as \code{md}.
 
 If both \code{groups} and \code{obs} are not NA, the rows of each matrix
 will be the \code{groups} and the columns the \code{obs}. If \code{obs}
 is NA, the rows will be the \code{groups} and the columns the \code{vars}.
 If \code{groups} is NA, the rows will be the \code{vars} and the columns
 the \code{obs}. That is, if \code{vars} has more than one element, if
 \code{obs} is NA, the elements of \code{vars} will be considered to
 represent observations, while if \code{groups} is NA, they will be
 considered to represent groups. At least one of \code{groups} and \code{obs}
 must be not NA or there is no point in using \code{brkdn.plot}.
}
\details{
 \code{brkdn.plot} displays a plot useful for visualizing the breakdown of a
 response measure by two factors, or more than one response measure by either
 a factor representing something like levels of treatment (\code{groups}) or
 something like repeated observations (\code{obs}). For example, if
 observations are made at different times on data objects that receive
 different treatments, the \code{groups} factor will display the measures
 of central tendency as points/lines with the same color, symbol and line type,
 while the \code{obs} factor will be represented as horizontal positions on the
 plot. This is a common way of representing changes over time intervals for
 experimental groups.
}
\author{Jim Lemon}
\seealso{\code{\link{dispbars}}}
\examples{
 test.df<-data.frame(a=rnorm(80)+4,b=rnorm(80)+4,c=rep(LETTERS[1:4],each=20),
  d=rep(rep(letters[1:4],each=4),5))
 # first use the default values
 brkdn.plot("a","c","d",test.df,pch=1:4,col=1:4)
 # now jazz it up a bit using medians and median absolute deviations
 # and some enhancements
 bp<-brkdn.plot("a","c","d",test.df,main="Test of the breakdown plot",
  mct="median",md="mad",xlab="Temperature range", ylab="Cognition",
  xaxlab=c("10-15","16-20","21-25","25-30"),pch=1:4,lty=1:4,col=1:4)
 es<-emptyspace(bp)
 legend(es,legend=c("Sydney","Gosford","Karuah","Brisbane"),pch=1:4,
  col=1:4,lty=1:4,xjust=0.5,yjust=0.5)
}
\keyword{misc}
