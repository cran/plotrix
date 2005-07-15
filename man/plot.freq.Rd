\name{plot.freq}
\alias{plot.freq}
\title{Plot a frequency table}
\description{
 Plots a horizontal bar plot of a "freq" object.
}
\usage{
 plot.freq(x,main=NULL,col=NULL,show.prop=TRUE,show.pc=FALSE,show.labels=FALSE,
  show.axis=FALSE,...)
}
\arguments{
  \item{x}{a frequency table produced by \code{freq}}
  \item{main}{main title of the plot}
  \item{col}{vector of colors to use as fills for the bars - defaults
   to \code{1:nbars+1}.}
  \item{show.prop}{whether to display the proportions or percentages on
   each bar}
  \item{show.pc}{whether to display percentages instead of proportions}
  \item{show.labels}{whether to display the value labels on each bar}
  \item{show.axis}{whether to display a horizontal axis below the bars}
  \item{...}{additional arguments passed to \code{plot}.}
}
\details{
 \code{plot.freq} will accept a list of frequencies produced by 
 \code{\link{freq}} and display a horizontal barplot in which each 
 horizontal stack of bars represents one component of the list. The
 length of each bar is proportional to the frequency of that value
 in the list.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{freq}}}
\examples{
 testdat.df<-data.frame(Sex=sample(c("Male","Female"),142,TRUE),
  Marital=sample(c("Divorced","Married","Single","De facto","Widowed"),142,TRUE),
  SES=c(sample(c("Low","Middle","High"),130,TRUE),rep(NA,12)),
  Age=sample(c("0-20","21-40","41-60","61-80","over 80"),142,TRUE))
 testdat.freq<-freq(testdat.df)
 plot.freq(testdat.freq,show.labels=TRUE,show.axis=TRUE)
}
\keyword{misc}
