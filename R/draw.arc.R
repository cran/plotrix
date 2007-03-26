# center and radius are x, y, radius
# specify either angle1 and angle2 in radians or deg1 and deg2 in degrees
# n is number of pieces arc is divided into for the approximation
# ... is passed to segments
# e.g.
# plot(1:10)
# draw.arc(5, 6, deg1 = 45, deg2 = 90, col = "red")
#	
draw.arc <- function(x = 1, y = NULL, radius = 1, 
   angle1 = deg1 * pi / 180, angle2 = deg2 * pi / 180, 
   deg1 = 0, deg2 = 45, n = 35, col = 1, ...) {
   draw.arc.0 <- function(x, y, radius, angle1, angle2, n, col = col, ...) {
      angle <- angle1 + seq(0, length = n) * (angle2 - angle1) / n
      p1x <- x + radius * cos(angle)
      p1y <- y + radius * sin(angle)
      angle <- angle1 + seq(length = n) * (angle2 - angle1) / n
      p2x <- x + radius * cos(angle)
      p2y <- y + radius * sin(angle)
      segments(p1x, p1y, p2x, p2y, col = col, ...)
   }
   xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
   a1 <- pmin(angle1, angle2); a2 <- pmax(angle1, angle2)
   angle1 <- a1; angle2 <- a2
   args <-
    data.frame(x,y,radius,angle1,angle2,n,col,stringsAsFactors=FALSE)
   for(i in 1:nrow(args)) do.call("draw.arc.0", c(args[i, ], ...))
   invisible(args)
}

