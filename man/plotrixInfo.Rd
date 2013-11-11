\name{plotrixInfo}
\alias{plotrixInfo}
\title{ Save information from a plot }
\description{ \samp{plotrixInfo} is a list filled by some functions with an
 \samp{add} argument. }
\details{
  Because \samp{plotrixInfo} is created in the parent environment of a
 function, it may be read if the same function is called with \samp{add=TRUE}.
 This allows some plot parameters to be passed from the initial plotting
 function to subsequent calls that add more values.
}
\keyword{misc}
