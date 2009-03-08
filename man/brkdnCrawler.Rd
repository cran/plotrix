\name{brkdnCrawler}
\alias{brkdnCrawler}
\title{Crawl a list containing a nested breakdown of numeric values.}
\description{Breaks down a numeric element of a data frame by one or more
 categorical elements.}
\usage{
 brkdnCrawler(x,errbars=FALSE,
  retval=list(indx=vector("numeric",0),element=NULL,value=NA,depth=0))
}
\arguments{
 \item{x}{The output of \samp{brkdn.num}.}
 \item{errbars}{Whether to include the second row of \samp{x} (the dispersion) in
  the calculation of the maximum value.}
 \item{retval}{A list containing useful information about the list.}
}
\value{
 A list containing the indices of the list element that is not itself a list with the
 largest numeric value. That element should be a matrix of summary results produced by
 \samp{brkdn.num}. The list also contains the matrix, the above value, and the maximum
 depth of the list.
}
\details{
 \samp{brkdnCrawler} crawls a list produced by \samp{brkdn.num} and returns the
 information needed by \samp{hierobarp}.
}
\author{Jim Lemon}
\examples{
 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("M","F"),100,TRUE),
  Marital=sample(c("M","X","S","W"),100,TRUE),
  Employ=sample(c("FT","PT","NO"),100,TRUE))
 test.brk<-hierobrk(formula=Age~Sex+Marital+Employ,data=test.df)
 brkdnCrawler(test.brk)
}
\keyword{misc}