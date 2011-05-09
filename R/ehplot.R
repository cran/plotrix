ehplot <-
function(data, groups, intervals=50, offset=0.1, log=FALSE, 
                   median=TRUE, box=FALSE, boxborder="grey50", 
                   xlab="groups", ylab="values", col="black", ...)
{
  stopifnot(length(data) == length(groups), is.numeric(data), intervals > 0, offset < 0.4)
  grps <- split(data, groups, drop=TRUE)
  dr <- range(data, finite = TRUE)
  if (log)
    seps <- dr[1] * exp((0:intervals) * log(dr[2]/dr[1]) / intervals)
  else 
    seps <- (0:intervals) * (diff(dr) / intervals) + dr[1]
  
  # prepare x-shift for odd and even number of datapoints per interval
  inc <- rep(1:(0.4/offset), each=2)
  xshift <- list(even=c((inc-0.5) * offset * (-1)^(1:length(inc))), # 1
                 odd=c(0, inc * offset * (-1)^(1:length(inc))))     # 2
  pnts_a <- list() # accumulate datapoint offsets before plotting.
  for (i in 1:length(grps)) {
    histo <- hist(grps[[i]], breaks=seps, plot=FALSE)$counts
    ixof <- unlist(sapply(histo, function(j){rep(xshift[[j%%2 + 1]], length.out=j)}))
    pnts_a[[i]] <- i + ixof[rank(grps[[i]], ties.method="first")]
    if (anyDuplicated(na.omit(cbind(grps[[i]], pnts_a[[i]]))))
      warning("Some points are overplotted in group ",names(grps)[i],
              ". Please consider using a lower offset-value.")
  }
  plot(data, xlim=c(0.5, length(grps)+0.5), xaxt="n", type="n", xlab=xlab, ylab=ylab, log=ifelse(log,"y",""), ...)
  axis(1, at=1:length(grps), labels=names(grps), ...)
  if (box) 
    boxplot(data ~ groups, border=boxborder, add=TRUE, axes=FALSE, outline=FALSE)
  # draw datapoints at once
  points(unsplit(pnts_a, groups, drop=TRUE), data, col=col, ...)
  if (median) 
    lines(rep(1:length(grps),each=3)+c(-0.4, 0.4, NA),
          rep(sapply(grps, median, na.rm=TRUE), each=3)+c(0, 0, NA), lwd=3)

}

