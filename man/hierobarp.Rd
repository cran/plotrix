\name{hierobarp}
\alias{hierobarp}
\title{Display a nested breakdown of numeric values.}
\description{Breaks down a numeric element of a data frame by one or more
 categorical elements and displays the breakdown as a bar plot.}
\usage{
 hierobarp(formula=NULL,data=NULL,maxlevels=10,mct=mean,lmd=std.error,umd=lmd,
 x=NULL,xlim=NULL,ylim=NULL,main="",xlab="",ylab="",start=0,end=1,shrink=0.02,
 errbars=FALSE,col=NA,labelcex=1,lineht=NA,showall=FALSE,barlabels=NULL,
 showbrklab=TRUE,mar=NULL,arrow.cap=NA)
}
\arguments{
 \item{formula}{A formula with a numeric element of a data frame on the left and
  one or more categorical elements on the right.}
 \item{data}{A data frame containing the elements in \samp{formula}.}
 \item{maxlevels}{The maximum number of levels in any categorical element. Mainly to
  prevent the mess caused by breaking down by a huge number of categories.}
 \item{mct}{The measure of central tendency function to use.}
 \item{lmd}{The lower measure of dispersion function to use.}
 \item{umd}{The upper measure of dispersion function to use.}
 \item{x}{This becomes the result of the breakdown after the first call. Currently
  anything passed as this argument will be ignored.}
 \item{xlim,ylim}{Optional x and y limits for the plot.}
 \item{main}{Title for the plot.}
 \item{xlab,ylab}{Axis labels for the plot.}
 \item{start,end}{The start and end values of the initial display. The user will
  almost certainly not want to change these.}
 \item{shrink}{The proportion to shrink the width of the bars as more levels are
  added. This proportion increases with the number of levels.}
 \item{errbars}{Whether to display error bars on the lowest level of breakdown.}
 \item{col}{The colors to use to fill the bars. See Details.}
 \item{barlabels}{Optional group labels that may be useful if the factors used to
  break down the numeric variable are fairly long strings.}
 \item{labelcex}{Character size for the group labels.}
 \item{lineht}{The height of a line of text in the lower margin of the plot in user
  units. This will be calculated by the function if a value is not passed.}
 \item{showall}{Whether to display bars for the entire breakdown.}
 \item{showbrklab}{Whether to display the labels for the lowest level of breakdown.}
 \item{mar}{If not NULL, a four element vector to set the plot margins. If new margins
  are set, the user must reset the margins after the function exits.}
 \item{arrow.cap}{The width of the "cap" on error bars in user units, defaulting to 0.01.}
}
\value{The summary arrays produced by hierobrk.}
\details{
 \samp{hierobarp} displays a bar plot illustrating the breakdown of a numeric
 element of a data frame by one or more categorical elements. The breakdown is
 performed by \samp{hierobrk}. Typically, the mean of each category specified
 by the formula is displayed as the height of a bar. If \samp{showall} is TRUE,
 the entire nested breakdown will be displayed. This can be useful in
 visualizing the relationship between groups and subgroups in a compact format.

 The colors of the bars are determined by \samp{col}. If \samp{showall} is FALSE,
 the user only need pass a vector of colors, usually the same length as the number
 of categories in the final (first on the right side) element in the formula. If
 \samp{showall} is TRUE and the user wants to color all of the bars, a list with
 as many elements as there are levels in the breakdown should be passed. Each
 element should be a vector of colors, again usually the same length as the number
 of categories. As the categorical variables are likely to be factors, it is
 important to remember that the colors must be in the correct order for the levels
 of the factors. When the levels are not in the default alphanumeric order, it is
 quite easy to get this wrong.
}
\author{Jim Lemon and Ofir Levy}
\seealso{\samp{\link{hierobrk}}, \samp{\link{barp}}}
\examples{
 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("M","F"),100,TRUE),
  Marital=sample(c("D","M","S","W"),100,TRUE),
  Employ=sample(c("Full Time","Part Time","Unemployed"),100,TRUE))
 test.col<-list(Overall="gray",Employ=c("#1affd8","#caeecc","#f7b3cc"),
  Marital=c("mediumpurple","orange","tan","lightgreen"),Sex=c("pink","lightblue"))
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Show only the final breakdown",errbars=TRUE,col=test.col$Sex)
 # set up functions for 20 and 80 percentiles - must be offsets, not limits
 q20<-function(x,na.rm=TRUE) return(mean(x)-quantile(x,probs=0.2,na.rm=TRUE))
 q80<-function(x,na.rm=TRUE) return(quantile(x,probs=0.8,na.rm=TRUE)-mean(x))
 # show the asymmetric dispersion measures
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Use median and quantiles for dispersion",mct=median,lmd=q20,umd=q80,
  errbars=TRUE,col=test.col$Sex)
 \dontrun{
  # start a wide plot window for this one
  x11(width=10)
  hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
   main="Show the entire hierarchical breakdown",col=test.col,showall=TRUE,
   showbrklab=TRUE,mar=c(5,4,4,8))
  # example of a legend that might be included, needs a lot of space
  par(xpd=TRUE)
  legend(1.02,27,c("Overall","Full time","Part time","No work","Divorced",
   "Married","Single","Widowed","Female","Male"),
   fill=unlist(test.col))
  par(xpd=FALSE,mar=c(5,4,4,2))
 }
}
\keyword{misc}
