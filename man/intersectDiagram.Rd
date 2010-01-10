\name{intersectDiagram}
\alias{intersectDiagram}
\title{Display set intersections}
\description{Display set intersections as rows of rectangles.}
\usage{
 intersectDiagram(x,pct=FALSE,show.nulls=FALSE,xnames=NULL,namesep="-",
  mar=c(0,0,3,0),main="Intersection Diagram",cex=1,col=NULL,minspacing=0.1)
}
\arguments{
 \item{x}{A list containing as many numeric vectors as there are sets. The
  first vector contains the counts or percentages of the elements that are
  only in one set, the next vector contains the counts or percentages of 
  elements that are in two sets and so on. A matrix of set membership
  indicators can be passed - see Details.}
 \item{pct}{Whether to display counts (FALSE) or percentages (TRUE) of the
  number of entities.}
 \item{show.nulls}{Whether to display the number of original objects that were
  not members of any set.}
 \item{xnames}{Optional user supplied names for the set categories (see
  Details).}
 \item{namesep}{The separator to use between category names.}
 \item{mar}{The margins for the diagram. The margins that were in effect when
  the function is called are restored.}
 \item{main}{The title for the diagram.}
 \item{col}{Colors for the sets (see Details).}
 \item{cex}{Character expansion for the intersection labels.}
 \item{minspacing}{The minimum spacing between the rectangles as a proportion.}
}
\details{
 \samp{intersectDiagram} displays rows of optionally colored rectangles.
 The topmost row represents the elements that only belong to one set, the
 next row down represents elements belonging to two sets, and so on to the
 bottom row of one rectangle representing the elements that belong to all
 the sets. More than three intersecting sets generally produce a complex and
 difficult to interpret Venn diagram, and this provides an alternative way to
 display the intersections between larger numbers of sets. 
 
 Each set is assigned a color if \samp{col} is not NA. \samp{rainbow} is
 called if \samp{col} is NULL, otherwise the colors passed are used. For each
 intersection, the colors representing the sets intersecting are included in
 the rectangle.
 
 The strings displayed on each rectangle are taken from the argument
 \samp{xnames} unless that is NULL, then the \samp{names} of the first element
 of the intersectList object passed as \samp{x} or returned from the call to
 \samp{makeIntersectList} and if this is also NULL, capital letters are assigned
 to each category in \samp{x}.
 
 If there were objects in the original data set that were not members of any
 set, any percentages calculated will reflect this. By setting \samp{show.nulls}
 to TRUE, the counts or percentages of such objects will be displayed below the
 intersections over an empty rectangle scaled to the count or percentage.
 
 If a matrix of set membership indicators is passed as \samp{x}, it will be
 passed to \link{makeIntersectList} for conversion.
}
\value{nil}
\keyword{misc}
\author{Jim Lemon}
\seealso{\link{makeIntersectList}, \link{getIntersectList}}
\examples{
 # create a matrix where each row represents an element and
 # a 1 (or TRUE) in each column indicates that the element is a member
 # of that set.
 druguse<-matrix(c(sample(c(0,1),200,TRUE),
  sample(c(0,1),200,TRUE),
  sample(c(0,1),200,TRUE),
  sample(c(0,1),200,TRUE)),ncol=4)
 colnames(druguse)<-c("Alc","Tob","THC","Amp")
 druglist<-makeIntersectList(druguse)
 # first display it as counts
 intersectDiagram(druglist)
 # then as percent with non.members, passing the initial matrix
 intersectDiagram(druguse,pct=TRUE,show.nulls=TRUE)
 # alter the data to have more multiple intersections
 druguse[which(as.logical(druguse[,1]))[1:40],2]<-1
 druguse[which(as.logical(druguse[,1]))[31:70],3]<-1
 druguse[,4]<-sample(c(0,1),200,TRUE,prob=c(0.9,0.1))
 intersectDiagram(druguse,col=c("gray20","gray40","gray60","gray80"))
}
