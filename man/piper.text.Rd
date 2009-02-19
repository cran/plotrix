\name{piper.text}
\alias{piper.text}
\title{Display a string on a Piper diagram.}
\description{Displays a character string on the Piper diagram.}
\usage{
 piper.text(ca,mg,so4,cl,ions=data.frame(ca=ca,mg=mg,so4=so4,cl=cl),ptext,
  ppm=TRUE,cex=1,col=1,plot.sep=0.15,pos=4)
}
\arguments{
 \item{ca,mg,so4,cl}{Concentrations of the ions.}
 \item{ions}{data frame containing the above values.}
 \item{ptext}{The character string.}
 \item{ppm}{Whether the concentrations are in milligrams/liter (parts per
 million) or milliequivalents.}
 \item{cex}{Expansion for the text.}
 \item{col}{Colors for the text.}
 \item{plot.sep}{The offsets.}
 \item{pos}{The position of the string relative to the data point.}
}
\value{nil}
\author{Mike Cheetham}
\seealso{\samp{\link{piper.diagram}}}
\keyword{misc}
