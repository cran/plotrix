\name{sizetree}
\alias{sizetree}
\title{Display a hierarchical breakdown of disjunct categories}
\description{Display a data frame in which the values in each successive
 column represent subcategories of the previous column as stacked
 rectangles.}
\usage{
 sizetree(x,left=0,top,right=1,lastcenter=NA,showval=TRUE,showcount=TRUE,
  firstcall=TRUE,col=NA,colorindex=1,...)
}
\arguments{
 \item{x}{A data frame in which each successive column represents
  subcategories of the previous column.}
 \item{left}{The left edge of the current stack of rectangles in user units.}
 \item{top}{The top of the current stack of rectangles in user units.}
 \item{right}{The right edge of the current stack of rectangles in user units.}
 \item{lastcenter}{The center of the previous rectangle from which the next
  breakdown of categories arises. There is almost no reason to change it.}
 \item{showval}{Whether to display the values representing the categories.}
 \item{showcount}{Whether to display the count for the categories.}
 \item{firstcall}{A flag for the function - do not alter this.}
 \item{col}{Optional fill colors for the rectangles. See Details}
 \item{colorindex}{The index for \samp{col} if it is a list of color vectors.}
 \item{...}{additional arguments passed to \samp{plot}.}
}
\value{ nil }
\details{
 \samp{sizetree} displays disjunct hierarchical categories as stacked rectangles.
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
 be recycled. If different colors are desired for different levels, as in the
 example, pass a list of colors.
 
 The \samp{firstcall} argument is necessary for the function to initialize the
 plot, as each breakdown involves a recursive call. If it is changed, the best
 that can be expected is an uninformative plot.
}
\author{Jim Lemon}
\seealso{\samp{\link{plot}},\samp{\link{smoothColors}}}
\examples{
 cat1<-sample(c("None","Low","Medium","High"),40,TRUE)
 cat2<-sample(c("None","Low","Medium","High"),40,TRUE)
 cat3<-sample(c("None","Low","Medium","High"),40,TRUE)
 hcats<-data.frame(cat1,cat2,cat3)
 bhcol<-list(c("#ff8080","#dddd80","#80ff80","#8080ff"),
  c("red","green","lightblue","yellow"),
  c("#ffffff","#bbbbbb","#999999","#666666"))
 sizetree(hcats,col=bhcol,main="Hierarchical count chart")
}
\keyword{misc}
