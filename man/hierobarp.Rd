\name{hierobarp}
\alias{hierobarp}
\title{Display a nested breakdown of numeric values.}
\description{Breaks down a numeric element of a data frame by one or more
 categorical elements and displays the breakdown as a plot.}
\usage{
 hierobarp(formula,data,maxlevels=10,num.desc=c("mean","std.error","valid.n"),
 x=NULL,xinfo=NULL,space=0.1,xlim=NULL,ylim=NULL,main="",xlab="",ylab="",
 errbars=FALSE,col=NA,lineht=NA,showall=FALSE,showbrklab=FALSE,firstcall=TRUE)
}
\arguments{
 \item{formula}{A formula with a numeric element of a data frame on the left and
  one or more categorical elements on the right.}
 \item{data}{A data frame containing the elements in \samp{formula}.}
 \item{maxlevels}{The maximum number of levels in any categorical element. Mainly to
  prevent the mess caused by breaking down by a huge number of categories.}
 \item{num.desc}{The summary functions to use. The defaults are usually appropriate,
  but the user can pass other summary functions if desired.}
 \item{x}{This becomes the result of the breakdown after the first call. Currently
  anything passed as this argument will be ignored.}
 \item{xinfo}{The result of \samp{brkdnCrawler} to be passed to recursive calls.
  As with \samp{x}, anything passed as this argument will be ignored.}
 \item{space}{The amount of space to leave at each side of the bar as a proportion
  of the bar width.}
 \item{xlim,ylim}{Optional x and y limits for the plot.}
 \item{main}{Title for the plot.}
 \item{xlab,ylab}{Axis labels for the plot.}
 \item{errbars}{Whether to display error bars on the lowest level of breakdown.}
 \item{col}{The colors to use to fill the bars. See Details.}
 \item{lineht}{The height of a line of text in the lower margin of the plot in user
  units. This will be calculated by the function if a value is not passed.}
 \item{showall}{Whether to display bars for the entire breakdown.}
 \item{showbrklab}{Whether to display the labels for the lowest level of breakdown.}
 \item{firstcall}{This is used to keep track of recursive calls. Don't change it.}
}
\value{nil}
\details{
 \samp{hierobarp} displays a bar plot illustrating the breakdown of a numeric
 element of a data frame by one or more categorical elements. The breakdown is
 performed by \samp{brkdn.num}. Typically, the mean of each category specified
 by the formula is displayed as the height of a bar. If \samp{showall} is TRUE,
 the entire nested breakdown will be displayed. This can be useful in
 visualizing the relationship between groups and subgroups in a compact format.

 If there are more than three functions named in \samp{num.desc} and
 \samp{errbars} is TRUE, \samp{hierobarp} will assume that the second and third
 functions define the upper and lower confidence limits of the estimate and
 display appropriate error bars. This allows the user to define asymmetric
 confidence limits using quantiles.

 The colors of the bars are determined by \samp{col}. If \samp{showall} is FALSE,
 the user only need pass a vector of colors, usually the same length as the number
 of categories in the final (first on the right side) element in the formula. If
 \samp{showall} is TRUE and the user wants to color all of the bars, a list with
 as many elements as there are levels in the breakdown should be passed. Each
 element should be a vector of colors, again usually the same length as the number
 of categories.
}
\author{Jim Lemon and Ofir Levy}
\examples{
 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("M","F"),100,TRUE),
  Marital=sample(c("D","M","S","W"),100,TRUE),
  Employ=sample(c("Full Time","Part Time","Unemployed"),100,TRUE))
 test.col<-list(Overall="green",Employ=c("purple","orange","brown"),
  Marital=c("#1affd8","#caeecc","#f7b3cc","#94ebff"),Sex=c(2,4))
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Show only the final breakdown",errbars=TRUE,col=test.col$Sex)
 # set up functions for 20 and 80 percentiles
 q20<-function(x,na.rm=TRUE) return(quantile(x,probs=0.2,na.rm=TRUE))
 q80<-function(x,na.rm=TRUE) return(quantile(x,probs=0.8,na.rm=TRUE))
 # show the asymmetric dispersion measures
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Use median and quantiles for dispersion",
  num.desc=c("median","q80","q20","valid.n"),errbars=TRUE,col=test.col$Sex)
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Show the entire hierarchical breakdown",col=test.col,showall=TRUE,
  showbrklab=TRUE)
 \dontrun{
  # example of a legend that might be included, needs a lot of space
  legend(2.5,34,c("Overall","Full time","No work","Part time","Divorced",
  "Married","Single","Widowed","Female","Male"),
   fill=unlist(test.col))
 }
}
\keyword{misc}
