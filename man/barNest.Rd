\name{barNest}
\alias{barNest}
\title{Display a nested breakdown of numeric values}
\description{Breaks down a numeric element of a data frame by one or more
 categorical elements and displays the breakdown as a bar plot.}
\usage{
 barNest(formula=NULL,data=NULL,maxlevels=10,mct=mean,lmd=std.error,umd=lmd,
 x=NULL,ylim=NULL,main="",xlab="",ylab="",shrink=0.1,errbars=FALSE,col=NA,
 labelcex=1,lineht=NA,showall=FALSE,barlabels=NULL,showlabels=TRUE,mar=NULL,
 arrow.cap=0.01,trueval=NA)
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
 \item{x}{This becomes the result of the breakdown after the first call. If a list
  of arrays of values of the same form as that produced by \samp{brkdnNest} is
  passed, it will be used to determine the heights of the nested bars.}
 \item{ylim}{Optional y limits for the plot.}
 \item{main}{Title for the plot.}
 \item{xlab,ylab}{Axis labels for the plot. The x axis label is typically blank}
 \item{shrink}{The proportion to shrink the width of the bars at each level.}
 \item{errbars}{Whether to display error bars on the lowest level of breakdown.}
 \item{col}{The colors to use to fill the bars. See Details.}
 \item{labelcex}{Character size for the group labels.}
 \item{lineht}{The height of a line of text in the lower margin of the plot in user
  units. This will be calculated by the function if a value is not passed.}
 \item{showall}{Whether to display bars for the entire breakdown.}
 \item{barlabels}{Optional group labels that may be useful if the factors used to
  break down the numeric variable are fairly long strings.}
 \item{showlabels}{Whether to display the labels below the bars.}
 \item{mar}{If not NULL, a four element vector to set the plot margins. If new
  margins are set, the user must reset the margins after the function exits.}
 \item{arrow.cap}{The width of the "cap" on error bars in user units,
  defaulting to 0.01 of the width of the plot.}
 \item{trueval}{If this is not NA, the call to \samp{brkdnNest} will return the
  proportions of the response variable that are equal to \samp{trueval}.
  See Details.}
}
\value{The summary arrays produced by brkdnNest.}
\details{
 \samp{barNest} displays a bar plot illustrating the breakdown of a numeric
 element of a data frame by one or more categorical elements. The breakdown is
 performed by \samp{brkdnNest} and the actual display is performed by
 \samp{drawNestedBars}. Typically, the mean of each category specified
 by the formula is displayed as the height of a bar. If \samp{showall} is TRUE,
 the entire nested breakdown will be displayed. This can be useful in
 visualizing the relationship between groups and subgroups in a compact format.

 If \samp{trueval} is not NA and brkdnNest is called to calculate the values for
 the heights of the bars, the proportions of the response variable that are
 equal to \samp{trueval} will be returned. Currently the value of \samp{errbars}
 will be forced to FALSE in this case, as the confidence limits are meaningless.
 This may change if a suitable method of calculating CIs becomes available.

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
\seealso{\link{brkdnNest}, \link{drawNestedBars}, superbarplot(UsingR)}
\examples{
 # start a wide plot window
 x11(width=10)
 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("Male","Female"),100,TRUE),
  Marital=sample(c("Div","Mar","Sing","Wid"),100,TRUE),
  Employ=sample(c("FT","PT","Un"),100,TRUE))
 test.col<-list(Overall="gray",Sex=c("pink","lightblue"),
  Marital=c("mediumpurple","orange","tan","lightgreen"),
  Employ=c("#1affd8","#caeecc","#ff90d0"))
 barNest(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Show only the final breakdown",errbars=TRUE,col=test.col$Employ)
 # set up functions for 20 and 80 percentiles - must be offsets, not limits
 q20<-function(x,na.rm=TRUE) return(-quantile(x,probs=0.2,na.rm=TRUE)+median(x))
 q80<-function(x,na.rm=TRUE) return(quantile(x,probs=0.8,na.rm=TRUE)-median(x))
 # show the asymmetric dispersion measures
 barNest(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Use median and quantiles for dispersion",mct=median,lmd=q20,umd=q80,
  errbars=TRUE,col=test.col$Employ)
 \dontrun{
  barNest(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
   main="Show the entire hierarchical breakdown",col=test.col,showall=TRUE,
   showlabels=TRUE,mar=c(5,4,4,8))
  # example of a legend that might be included, needs a lot of space
  par(xpd=TRUE)
  legend(1.05,27,c("Overall","Female","Male","Divorced",
   "Married","Single","Widowed","Full time","Part time","No work"),
   fill=unlist(test.col))
  par(xpd=FALSE,mar=c(5,4,4,2))
 }
 dev.off()
}
\keyword{misc}
