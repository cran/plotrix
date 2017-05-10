violin_plot = function(X = rnorm(50), at, add = FALSE, na.rm = TRUE,
bw = 0.35, violin_width = 1, violin_end_width = 0.005, equal_width = FALSE,
box_width = 0.03, box_col = "black", show_outliers = FALSE, pch = 1, range = 1.5,
xlim, ylim, xlab = "", ylab = "", x_axis_labels, main = "Violin Plot", 
col = "red", median_col = "white", plot_mean = FALSE, mean_pch = 19,
mean_pch_col = "yellow", ...) {
    
 if (missing(at)) at <- sequence(NCOL(X))
 if (missing(xlim)) xlim <- c(min(at) - violin_width/2, max(at) + violin_width/2)
 if (missing(ylim)) ylim <- c(min(X, na.rm = TRUE), max(X, na.rm = TRUE))
 if (missing(x_axis_labels)) {
  if (is.null(names(X))) x_axis_labels <- at
  else x_axis_labels <- names(X)
 }
 if (length(col) != length(at)){  #NEW, made color vector of violins recyclable
  col = rep(col, length.out = length(at))
 }
 if (add == FALSE) {
  #graphics.off()  #NEW
  plot(x = 1,
   y = 1,
   type = "n",
   xlim = xlim,
   ylim = ylim,
   xlab = xlab,
   ylab = ylab,
   xaxt = "n")
  axis(1, at = at, labels = x_axis_labels)
  title(main = main)
 }
    
 for (i in 1:NCOL(X)) {
  if(NCOL(X) == 1) x <- X
  else x <- X[,i]
  if (na.rm == TRUE) x <- x[!is.na(x)]
  d <- density(x, bw = bw)        
  b <- boxplot.stats(x, coef = range)
  violin_y <- min(x) + ((d$x - min(d$x)) * diff(range(x)))/diff(range(d$x)) #NEW, Rescaled to the range of x compared to b$stats
  if (equal_width == FALSE) width_2 <- violin_width * max(d$y)
  else width_2 <- violin_width
          
  violin_x <- 0 + (d$y - min(d$y)) * ((0.5 - violin_end_width) *
   width_2)/diff(range(d$y))
 
  polygon(x = c(at[i] + violin_end_width * violin_width,
   at[i] - violin_end_width * violin_width,
   at[i] - violin_x - violin_end_width * violin_width,
   at[i] - violin_end_width * violin_width,
   at[i] + violin_end_width * violin_width,
   rev(at[i] + violin_x + violin_end_width * violin_width)),
   y = c(min(violin_y),
   min(violin_y),
   violin_y,
   max(violin_y),
   max(violin_y),
   rev(violin_y)),
   col = col[i], ...)
 
   lines(x = c(at[i], at[i]), y = b$stats[1:2], lwd = 1, lend = "butt")
   lines(x = c(at[i], at[i]), y = b$stats[4:5], lwd = 1, lend = "butt")
 
   polygon(x = c(at[i] - box_width * violin_width,
    at[i] + box_width * violin_width,
    at[i] + box_width * violin_width,
    at[i] - box_width * violin_width),
    y = c(b$stats[2],
    b$stats[2],
    b$stats[4],
    b$stats[4]),
    col = box_col)
 
   lines(x = c(at[i] - box_width * violin_width,
    at[i] + box_width * violin_width),
    y = c(b$stats[3], b$stats[3]),
    lwd = 4,
    lend = "butt",
    col = median_col)
        
  if (plot_mean)
   points(x = at[i], y = mean(x), pch = mean_pch, col = mean_pch_col)
  
   if (show_outliers)
    points(x = rep(at[i], length(b$out)), y = b$out, pch = pch)
 }
}
