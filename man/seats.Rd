\name{seats}
\alias{seats}
\title{Arrange N seats in M semicircular rows}
\description{Compute seat positions in a semicircular parliament}
\usage{seats(N, M, r0 = 2.5)}
\arguments{
 \item{N}{Total number of seats}
 \item{M}{Number of semicircular arcs on which to distribute the seats}
 \item{r0}{Radius of the inner arc in user units}
}
\details{
 Parliaments and other assemblies are often graphically shown as
 semicircular rows of coloured dots, each dot coloured for a party or
 grouping for the member of the assembly. These graphics can be seen on
 many Wikipedia articles on national governments. However, it seems that
 many of those graphics do not tally with the numbers given for the
 constituents of that assembly. One can perhaps assume that the editors
 who changed the numbers couldn't be bothered to update the graphic.
 Let's maybe make that easier for them.

 This function computes the positions of the seats in a semicircular
 parliament. The number of seats in each row is computed to be similar
 to the adjacent rows, giving a pleasing effect.

 The resulting seat locations are ordered by angle and radius, so that
 colouring by party colours produces a natural grouping. Plots of
 assemblies by party colours can be made with the election function.
}
\value{
A data frame including:
 \item{x}{The x positions of the seats to be plotted on semi-circular arcs}
 \item{y}{The y positions of the seats to be plotted on semi-circular arcs}
 \item{r}{The row numbers for each seat}
 \item{theta}{The angle of each seat, going from pi to zero radians}
}
\author{
 Duncan Murdoch, Barry Rowlingson
}
\seealso{election}
\keyword{misc}
