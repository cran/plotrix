\name{convert.meq}
\alias{convert.meq}
\title{Convert milligrams/liter to milliequivalents/liter.}
\description{Convert milligrams/liter of ions used in the Piper diagram
 to milliequivalents/liter.}
\usage{
 convert.meq(mgpl,parameter)
}
\arguments{
 \item{mgpl}{A data frame containing the concentrations of ions used in the
  Piper diagram in milligrams/liter.}
 \item{parameter}{The names of the ions if necessary.}
}
\details{  
 \code{convert.meq} does the conversion with a lookup table of molecular weights
 and valences.
}
\value{The concentration of the ions in milliequivalents.}
\author{Mike Cheetham}
\seealso{\code{\link{piper.diagram}}}
\keyword{misc}
