\name{plot.dendrite}
\alias{plot.dendrite}
\title{Plot a dendrogram of a dendrite object.}
\description{Plot a dendrogram for two or more mutually exclusive attributes.}
\usage{
 \method{plot}{dendrite}(x,xlabels=NULL,main="",mar=c(1,0,3,0),cex=1,...)
}
\arguments{
 \item{x}{A \samp{dendrite} object containing the counts of objects having
  combinations of mutually exclusive attributes.}
 \item{xlabels}{The category labels that will be displayed beneath the
  dendrogram.}
 \item{main}{The title of the plot.}
 \item{mar}{Margins for the plot.}
 \item{cex}{Character expansion for the leaves of the dendrogram.}
 \item{...}{Additional arguments passed to \samp{plot}.}
}
\details{  
 \samp{plot.dendrite} sets up a plot for a dendrogram. The actual plotting of
 the dendrogram is done by \samp{furc}.
}
\value{nil}
\author{Jim Lemon}
\seealso{\samp{\link{furc}}}
\examples{
 sex<-sample(c("M","F"),100,TRUE)
 hair<-sample(c("Blond","Black","Brown","Red"),100,TRUE)
 eye<-sample(c("Blue","Black","Brown","Green"),100,TRUE)
 charac<-data.frame(sex=sex,hair=hair,eye=eye)
 characlist<-makeDendrite(charac)
 plot.dendrite(characlist,names(charac),main="Test dendrogram",cex=0.8)
}
\keyword{misc}
