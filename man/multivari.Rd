\name{multivari}
\alias{multivari}
\title{
Function to draw a multivari chart
}
\description{
A multivari chart of one quantitative response variable depending on two
to four categorical variables can be drawn.
}
\usage{
multivari(var, fac1, fac2, fac3 = NULL, fac4 = NULL, data, sort = FALSE,
 fun = mean, pch = list(15, 16, 17), col = list("black", "blue", "red"),
 col0 = "black", cex = 1, fac.cex = 2, xlab.depth = 3, legend = FALSE,
 main = paste("multivari chart of", var), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{variable name (character string) or column index of response
  variable, required
}
  \item{fac1}{variable name (character string) or column index of first
  level factor, required
}
  \item{fac2}{variable name (character string) or column index of second
  level factor, required
}
  \item{fac3}{variable name (character string) or column index of third
  level factor, optional
}
  \item{fac4}{
variable name (character string) or column index of fourth level factor,
optional; can only be specified if there is also a third level factor;
separate multivari charts for the first three factors are drawn for each
level of this factor}
  \item{data}{
a data frame, required
}
  \item{sort}{
logical, specifying whether or not levels are sorted, when converting
character vectors to factors (a single choice for all factors is needed); default: \code{FALSE}
}
  \item{fun}{
a function to be used in aggregation; default: \code{mean}
}
  \item{pch}{
  a list of length 2 or 3, depending on whether or not \code{fac3} is
  specified; the ith list element can be an individual plotting symbol
  (like the usual \code{pch} entry) or a vector of plot symbols for each
  level of \code{fac_i}
}
  \item{col}{
  a list of length 2 or 3, depending on whether or not \code{fac3} is
  specified; the ith list element can be an individual color or a vector
  of colors for each level of \code{fac_i}; this color specification is
  used for the plot symbols of \code{fac_i} levels and for the lines
  connecting the symbols for the next level in the hierarchy
}
  \item{col0}{
the color for the first line to be drawn
}
  \item{cex}{
the size of the annotation text below the horizontal axis
(annotation of the fourth level header is 1.5 times this size)
}
  \item{fac.cex}{
a multiplier for \code{cex}; plot symbol sizes are \code{fac.cex*cex};
default: 2
}
  \item{xlab.depth}{
labels for the horizontal axis are printed down to this level of the
hierarchy (default: 3); if the depth is reduced, different plot symbols
should be used, and a legend should be drawn
}
  \item{legend}{
logical determining whether or not a legend should be drawn (default:
FALSE); the function determines wether top right or bottom right yields
a better position (it is not guaranteed that there is no overlab); if
this does not work well, one can manually draw a legend in the outer
margin
}
  \item{main}{
title, as usual; a default is provided
}
  \item{\dots}{
further arguments to functions \code{plot}, \code{lines}, \code{points}, 
   \code{mtext}
}
}
\details{
A multivari chart as implemented here mainly serves exploratory purposes.
It works particularly well with balanced data, and it serves the purpose
to visualize an interaction of degree up to four. The order of the
factors can make a big difference in the display. If there is no natural
order, it may be worthwhile inspect several orders.

The function is inspired by Minitabs behavior for multivari charts. It
does not attempt to visualize individual observations.

For interactions with two factors only, it is often preferrable to use
function \code{\link{interaction.plot}}.
}
\value{
a list of (lists of) data frames with summary statistics to be plotted
}

\author{
Ulrike Groemping
}
\seealso{
See also \code{\link{interaction.plot}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\dontrun{
require(car)
multivari("cycles", "len", "load", "amp", data=Wool, 
    col=list("black","red",c("grey70","grey45","grey20")),
    pch=list(15,17,8), legend=TRUE, xlab.depth = 2, lwd=2)
multivari("cycles", "load", "len", "amp", data=Wool, 
    col=list("black",c("red","blue","darkgreen"),
    c("grey70","grey45","grey20")),
    pch=list(15,17,8), legend=TRUE, xlab.depth = 2, lwd=2)
    
## create a fake fourth factor
fakedat <- rbind(cbind(newfac="blabla",Wool),cbind(newfac="albalb",Wool))
## make it character for demonstrating the effect of sort option
fakedat$newfac <- as.character(fakedat$newfac)

## default: sort order in the data is respected (order of unique is used)
multivari("cycles", "load", "len", "amp", "newfac", data=fakedat, 
    col=list("black",c("red","blue","darkgreen"),
    c("grey70","grey45","grey20")),
    pch=list(15,17,8), legend=TRUE, xlab.depth = 2, lwd=2, cex=0.8)
    
## sort=TRUE: levels are sorted (order of sort(unique))
multivari("cycles", "load", "len", "amp", "newfac", data=fakedat, 
    col=list("black",c("red","blue","darkgreen"),
    c("grey70","grey45","grey20")),
    pch=list(15,17,8), legend=TRUE, xlab.depth = 2, lwd=2, cex=0.8,
    sort=TRUE)
}
}
\keyword{ hplot }
\keyword{ design }
