\name{arctext}
\title{Display text on a circular arc}
\usage{
 arctext(x,center=c(0,0),radius=1,start=NA,middle=pi/2,stretch=1,cex=1,...)
}
\alias{arctext}
\arguments{
 \item{x}{A character string.}
 \item{center}{The center of the circular arc in x/y user units.}
 \item{radius}{The radius of the arc in user units.}
 \item{start}{The starting position of the string in radians.}
 \item{middle}{The middle position of the string in radians.}
 \item{stretch}{How much to stretch the string for appearance.}
 \item{cex}{The character expansion factor.}
 \item{...}{additional arguments passed to \samp{text}.}
}
\description{
 \samp{arctext} displays a string along a circular arc, rotating each
 letter. This may not work on all devices, as not all graphic devices
 can rotate text to arbitrary angles. The output looks best on a 
 Postscript or similar device that can rotate text without distortion.
 Rotated text often looks very ragged on small bitmaps.
 
 If the user passes a value for
 \samp{start}, this will override any value passed to \samp{middle}.
 If the plot area is not square, see \samp{par(pty="s")}, the arc will
 be somewhat elliptical.
}
\value{nil}
\author{Jim Lemon - Thanks to Suhas Parandekar for the idea.}
\seealso{\link{text}}
\examples{
 plot(0,xlim=c(1,5),ylim=c(1,5),main="Test of arctext",xlab="",ylab="",
  type="n")
 arctext("bendy like spaghetti",center=c(3,3),col="blue")
 arctext("bendy like spaghetti",center=c(3,3),radius=1.5,start=pi,cex=2)
 arctext("bendy like spaghetti",center=c(3,3),radius=0.5,
  start=pi/2,stretch=1.2)
}
\keyword{misc}
