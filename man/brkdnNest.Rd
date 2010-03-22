\name{brkdnNest}
\alias{brkdnNest}
\title{Perform a nested breakdown of numeric values}
\description{Breaks down a numeric or categorical element of a data frame
 by one or more categorical elements.}
\usage{
 brkdnNest(formula,data,maxlevels=10,mct=mean,lmd=std.error,umd=lmd,trueval=NA)
}
\arguments{
 \item{formula}{A formula with a numeric element of a data frame on the left and
  one or more categorical elements on the right.}
 \item{data}{A data frame containing the elements in \samp{formula}.}
 \item{maxlevels}{The maximum number of levels in any categorical element. Mainly to
  prevent the mess caused by breaking down by a huge number of categories.}
 \item{mct}{The measure of central tendency function to use (default is the mean).}
 \item{lmd}{The lower measure of dispersion function to use (default is standard
  error).}
 \item{umd}{The upper measure of dispersion function to use.}
 \item{trueval}{The value to use in calculating proportions of a categorical
  response variable. See Details.}
}
\value{
 A list with four elements:
 \item{mctlist}{The list of arrays produced by the function passed as the
  \samp{mct} argument.}
 \item{lcllist}{The list of arrays produced by the function passed as the
  \samp{lmd} argument.}
 \item{ucllist}{The list of arrays produced by the function passed as the
  \samp{umd} argument.}
 \item{barlabels}{A list containing the unique elements of the variables on
  the right side of the formula (or the levels if they are factors), in the
  order in which they appear in the formula. These will be the default labels
  for the \samp{barNest} function.}
 This function is similar to \samp{brkdn} in the \pkg{prettyR} package, but
 is structured to be used with the \samp{barNest} function. It produces a set
 of measures for the overall data, then the subsets of the data defined by
 the first variable to the right of the tilde, then the subsets defined by
 the first and second variable, and so on.
}
\details{
 \samp{brkdnNest} performs a nested breakdown of an element of a data frame
 by one or more categorical elements. For each category and optionally
 subcategories, the variable on the left of the formula is summarized as
 specified by the functions named in \samp{mct}, \samp{lmd} and \samp{umd}.
 If \samp{trueval} is not NA, brkdnNest will calculate the proportion of
 \samp{trueval} values in the response variable out of the total valid
 responses. Remember that \samp{lcllist} and \samp{ucllist} will be
 meaningless in this case.

 The user should take care when specifying different summary functions.
 \samp{barNest} expects a measure of central tendency as the first function
 and measures of dispersion as the second and third, if "error bars" are to
 be displayed. The user must also decide whether the "error bars" have any
 useful meaning.
}
\author{Jim Lemon}
\seealso{\link{by}}
\examples{
 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("M","F"),100,TRUE),
  Marital=sample(c("M","X","S","W"),100,TRUE),
  Employ=sample(c("FT","PT","NO"),100,TRUE))
 brkdnNest(formula=Age~Sex+Marital+Employ,data=test.df)
}
\keyword{misc}
