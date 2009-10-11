\name{hierobarp.svymean}
\alias{hierobarp.svymean}
\title{Display a nested breakdown of means.}
\description{Displays a nested breakdown as a bar plot of means from
 the output of svyby (survey package) using svymean as the function.}
\usage{
 hierobarp.svymean(x,meanvar,dispvar,xlim=NULL,ylim=NULL,
  main="",xlab="",ylab="",yticks=NULL,start=0,end=1,shrink=0.02,
  errbars=FALSE,col=NA,labelcex=1,lineht=NA,showall=TRUE,barlabels=NULL,
  showbrklab=TRUE,mar=NULL)
}
\arguments{
 \item{x}{The output of the \samp{svyby} function using \samp{svymean}. See Details.}
 \item{meanvar}{The name of the column containing the mean values.}
 \item{dispvar}{The name of the column containing the SE values.}
 \item{xlim,ylim}{Optional x and y limits for the plot.}
 \item{main}{Title for the plot.}
 \item{xlab,ylab}{Axis labels for the plot. X axis labels are usually left out}
 \item{yticks}{Optional tick labels for the y-axis.}
 \item{start,end}{The start and end values of the initial display. The user will
  almost certainly not want to change these.}
 \item{shrink}{The proportion to shrink the width of the bars as more levels are
  added. This proportion increases with the number of levels.}
 \item{errbars}{Whether to display error bars on the lowest level of breakdown.}
 \item{col}{The colors to use to fill the bars. See Details.}
 \item{labelcex}{Character size for the group labels.}
 \item{lineht}{The height of a line of text in the lower margin of the plot in user
  units. This will be calculated by the function if a value is not passed.}
 \item{showall}{Whether to display bars for the entire breakdown.}
 \item{barlabels}{Optional group labels that may be useful if the factors used to
  break down the numeric variable are fairly long strings.}
 \item{showbrklab}{Whether to display the labels for the lowest level of breakdown.}
 \item{mar}{If not NULL, a four element vector to set the plot margins. If new margins
  are set, the user must reset the margins after the function exits.}
}
\value{nil}
\details{
 \samp{hierobarp.svymean} displays a bar plot illustrating the breakdown of
 a numeric variable as group means. This can be useful in visualizing the relationship
 between groups and subgroups in a compact format. While the function can be used
 with just the output of the \samp{svyby} and \samp{svymean} functions, the user will
 almost certainly have the \pkg{survey} package installed.

 The colors of the bars are determined by \samp{col}. A list with
 as many elements as there are levels in the breakdown should be passed. Each
 element should be a vector of colors, again usually the same length as the number
 of categories. As the categorical variables are likely to be factors, it is
 important to remember that the colors must be in the correct order for the levels
 of the factors. When the levels are not in the default alphanumeric order, it is
 quite easy to get this wrong.
}
\author{Jim Lemon}
\seealso{\samp{\link{hierobarp}}, \samp{\link{barp}}}
\keyword{misc}
