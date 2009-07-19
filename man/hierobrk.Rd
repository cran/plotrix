\name{hierobrk}
\alias{hierobrk}
\title{Perform a nested breakdown of numeric values.}
\description{Breaks down a numeric element of a data frame by one or more
 categorical elements.}
\usage{
 hierobrk(formula,data,maxlevels=10,mct=mean,lmd=NULL,umd=lmd)
}
\arguments{
 \item{formula}{A formula with a numeric element of a data frame on the left and
  one or more categorical elements on the right.}
 \item{data}{A data frame containing the elements in \samp{formula}.}
 \item{maxlevels}{The maximum number of levels in any categorical element. Mainly to
  prevent the mess caused by breaking down by a huge number of categories.}
 \item{mct}{The measure of central tendency function to use - defaults to the
  normal standard error.}
 \item{lmd}{The lower measure of dispersion function to use.}
 \item{umd}{The upper measure of dispersion function to use.}
}
\value{
 A list with four elements:
 \item{mctlist}{The array produced by the function passed as the \samp{mct} argument.}
 \item{lcllist}{The array produced by the function passed as the \samp{lmd} argument.}
 \item{ucllist}{The array produced by the function passed as the \samp{umd} argument.}
 \item{barlabels}{A list containing the unique elements of the variables on the right
  side of the formula (or the levels if they are factors), in the order in which they appear in the formula. These will be the default labels for the \samp{hierobarp} function.}
 This function is similar to \samp{brkdn} in the \pkg{prettyR} package, but
 is structured to be used with the \samp{hierobarp} function.
}
\details{
 \samp{hierobrk} performs the breakdown of a numeric element of a data frame by one
 or more categorical elements. For each category and optionally subcategories, the
 variable on the left of the formula is summarized as specified by the functions
 named in \samp{num.desc}.

 The user should take care when specifying different summary functions.
 \samp{hierobarp} expects a measure of central tendency as the first function and
 measures of dispersion as the second and third, if "error bars" are to be displayed.
}
\author{Jim Lemon}
\examples{
 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("M","F"),100,TRUE),
  Marital=sample(c("M","X","S","W"),100,TRUE),
  Employ=sample(c("FT","PT","NO"),100,TRUE))
 hierobrk(formula=Age~Sex+Marital+Employ,data=test.df)
}
\keyword{misc}
