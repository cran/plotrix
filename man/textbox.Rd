\name{textbox}
\alias{textbox}
\title{Add text box}
\description{
 Add text to plot, justified, in a box
}
\usage{
 textbox(x,y,textlist,justify=TRUE,cex=1,leading=0.5,box=TRUE)
}
\arguments{
 \item{x}{x position: a vector with min. and max. x position}
 \item{y}{y position: location of the top of the box}
 \item{textlist}{a vector of text strings}
 \item{justify}{justify text in box?}
 \item{cex}{character size}
 \item{leading}{inter-line spacing}
 \item{box}{draw a box around the text?}
}
\details{
 Justifies text in the box by pasting the vector together,
 splitting it into words, and then adding words to the current
 line until the line is wide enough
}
\value{
 nil
}
\author{Ben Bolker}
\examples{
 plot.new()
 textbox(c(0,0.2),1,c("many words","more words","why not?",
                      "keep going",rep("and going",10)))
}
\keyword{aplot}
