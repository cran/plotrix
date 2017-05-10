\name{violin_plot}
\alias{violin_plot}
\title{Display a violin plot}
\description{Displays violin plot(s)}
\usage{
 violin_plot(X = rnorm(50), at, add=FALSE, na.rm = TRUE, bw = 0.35,
  violin_width = 1, violin_end_width = 0.005, equal_width = FALSE,
  box_width = 0.03, box_col = "black", show_outliers = FALSE, pch = 1,
  range = 1.5, xlim, ylim, xlab = "", ylab = "", x_axis_labels,
  main = "Violin Plot", col = "red", median_col = "white", plot_mean = FALSE,
  mean_pch = 19, mean_pch_col = "yellow", ...)
}
\arguments{
 \item{X}{A vector or matrix or data frame of numeric values.}
 \item{at}{Horizontal position(s) for the violins.}
 \item{add}{Whether this violin should be added to an existing plot.}
 \item{na.rm}{Remove NA values. Passed to functions such as \samp{boxplot}.}
 \item{bw}{Bandwidth value for \samp{density}.}
 \item{violin_width}{Multiplier to scale the widths of the violins.}
 \item{violin_end_width}{Multiplier to scale the width of the ends of the
  violins.}
 \item{equal_width}{Should all violin widths be equal?}
 \item{box_width}{Multiplier for the width of internal boxes.}
 \item{box_col}{Fill color for the internal rectangle.}
 \item{show_outliers}{Whether to display outliers as points.}
 \item{pch}{Symbol for displaying outliers.}
 \item{range}{Argument for \samp{boxplot}.}
 \item{xlim,ylim}{Explicitly set the plot limits.}
 \item{xlab,ylab}{Axis labels.}
 \item{x_axis_labels}{Labels for the violins.}
 \item{main}{Title for the plot,}
 \item{col}{Fill color for the violins. Will be recycled.}
 \item{median_col}{Fill color for the median mark.}
 \item{plot_mean}{Whether to plot the mean as well as the median.}
 \item{mean_pch}{Symbol to use for the mean mark.}
 \item{mean_pch_col}{Fill color for the mean mark.}
 \item{...}{Extra arguments passed to \samp{polygon}.}
}
\details{
 \samp{violin_plot} displays one or more violin plots by drawing the
 rotated kernel density on each side of the boxes.
}
\value{nil}
\author{Darshan Baral}
\examples{
 normvar<-c(rnorm(49),-4)
 unifvar<-runif(50,-2,2)

 violin_plot(matrix(c(normvar,unifvar),ncol=2),
  main="Default plot",x_axis_labels=c("Normal","Uniform"))

 violin_plot(matrix(c(normvar,unifvar),ncol=2),at=1:3,
  main="Different colors and extra space",
  x_axis_labels=c("Normal","Uniform","Normal"))

 normvar2<-rnorm(45)

 violin_plot(normvar2,at=3,add=TRUE,col="green")
}
\keyword{misc}
