\name{getFigCtr}
\title{Find the center of the figure region}
\description{Calculates the center of the figure region.}
\usage{
 getFigCtr()
}
\alias{getFigCtr}
\details{
 \samp{getFigCtr} reads parameters about the current plot and calculates the
 vertical and horizontal centers of the figure region. This is typically useful
 for placing a centered title on plots where the left and right margins are very
 different.
}
\value{A two element vector containing the coordinates of the center of the
 figure region in user units.}
\author{Jim Lemon}
\keyword{misc}
