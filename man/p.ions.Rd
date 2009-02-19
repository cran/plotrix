\name{p.ions}
\alias{p.ions}
\title{Calculate ion/total(ion) ratios.}
\description{Calculates the ion/total(ions) ratios of ions used in the Piper
 diagram.}
\usage{
 p.ions(meq,short.output=TRUE)
}
\arguments{
 \item{meq}{A data frame containing the concentrations of ions used in the
  Piper diagram in milliequivalents/liter.}
 \item{short.output}{Whether to return the proportions of four ions (TRUE)
  or include carbonate-bicarbonate and sodium-potassium (FALSE).}
}
\details{  
 \samp{p.ions} calculates the proportions of anions or cations to total anions
 or cations for the ions used in the Piper diagram.
}
\value{The proportion of the ions.}
\author{Mike Cheetham}
\seealso{\samp{\link{piper.diagram}}}
\keyword{misc}
