\name{barhier}
\alias{barhier}
\title{Display a set of hierarchically associated variables}
\description{Display a data frame in which the values in each successive
 column represent subcategories of the previous column as a block of stacked
 rectangles.}
\usage{
 barhier(x,left=0,top,right=1,showval=TRUE,showcount=TRUE,firstcall=TRUE,
  col=NA,fade=FALSE,...)
}
\arguments{
 \item{x}{A data frame in which each successive column represents
  subcategories of the previous column.}
 \item{left}{The left edge of the current stack of rectangles in user units.}
 \item{top}{The top of the current stack of rectangles in user units.}
 \item{right}{The right edge of the current stack of rectangles in user units.}
 \item{showval}{Whether to display the values representing the categories.}
 \item{showcount}{Whether to display the count for the categories.}
 \item{firstcall}{A flag for the function - do not alter this.}
 \item{col}{Optional fill colors for the rectangles.}
 \item{fade}{Whether to incrementally decrease the saturation of the bar
  colors at each level.}
 \item{...}{additional arguments passed to \samp{plot}.}
}
\value{ nil }
\details{
 \samp{barhier} displays hierarchical categories as stacked rectangles.
 It accepts a data frame in which the values in the first column represent
 categories, the values in the second column represent subcategories of the
 first column, and so on. The first column will be displayed as a stack of
 rectangles, the height of each proportional to the count for each category.
 Each substack of rectangles in the second stack will represent the breakdown
 of counts for its superordinate category and so on through the columns.
 Empty categories are ignored and NAs will produce gaps.
 
 Typically, the user will simply pass the data frame, which should only
 contain columns that are hierarchical categories, set \samp{showval} and
 \samp{showcount} to the desired values, and pass colors for the top level
 categories if these are wanted. If colors are passed, it is best to have at
 least as many colors as there are categories in the first column or some will
 be recycled. Currently, these colors will be "faded out" in successive stacks
 of rectangles toward white.
 
 The \samp{firstcall} argument is necessary for the function to initialize the
 plot, as each breakdown involves a recursive call. If it is changed, the best
 that can be expected is an uninformative plot.
}
\author{Jim Lemon}
\seealso{\samp{\link{plot}},\samp{\link{smoothColors}}}
\examples{
 cat1<-sample(LETTERS[1:4],40,TRUE)
 cat2<-paste(cat1,sample(1:4,40,TRUE),sep="")
 cat3<-paste(cat2,sample(letters[1:4],40,TRUE),sep="")
 hcats<-data.frame(cat1,cat2,cat3)
 barhier(hcats,col=c("#ff8080","#dddd80","#80ff80","#8080ff"),
  main="Hierarchical count chart",fade=TRUE)
}
\keyword{misc}
