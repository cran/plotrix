get.gantt.info<-function(format="%Y/%m/%d") {
 cat("Enter the label, start and finish time for each task.\n")
 cat("Default format for time is year/month/day e.g. 2005/2/22\n")
 cat("Enter a blank label to end.\n")
 nextlabel<-"dummy"
 tasklabels<-NA
 taskstarts<-NA
 taskends<-NA
 priorities<-NA
 while(nchar(nextlabel)) {
  nextlabel<-readline("Task label - ")
  if(nchar(nextlabel)) {
   if(is.na(tasklabels[1])) tasklabels<-nextlabel
   else tasklabels<-c(tasklabels,nextlabel)
   nextstart<-as.POSIXct(strptime(readline("Task begins - "),format=format))
   if(is.na(taskstarts[1])) taskstarts<-nextstart
   else taskstarts<-c(taskstarts,nextstart)
   nextend<-nextstart-1
   while(nextend < nextstart) {
    nextend<-as.POSIXct(strptime(readline("Task ends - "),format=format))
    if(nextend < nextstart) cat("Task cannot end before it starts!\n")
    else {
     if(is.na(taskends[1])) taskends<-nextend
     else taskends<-c(taskends,nextend)
    }
   }
   nextpriority<-0
   while(nextpriority < 1 || nextpriority > 10)
    nextpriority<-as.numeric(readline("Task priority (1-10) - "))
   if(is.na(priorities[1])) priorities<-nextpriority
   else priorities<-c(priorities,nextpriority)
  }
 }
 return(list(labels=tasklabels,starts=taskstarts,ends=taskends,
  priorities=priorities))
}

axis.POSIXct.tickpos<-function (side,x,format,...) {
 x <- as.POSIXct(x)
 range <- par("usr")[if(side%%2) 1:2 else 3:4]
 d <- range[2] - range[1]
 z <- c(range, x[is.finite(x)])
 if (d < 1.1 * 60) {
  sc <- 1
  if(missing(format))
  format <- "%S"
 }
 else if (d < 1.1 * 60 * 60) {
  sc <- 60
  if(missing(format)) format <- "%M:%S"
 }
 else if (d < 1.1 * 60 * 60 * 24) {
  sc <- 60 * 24
  if(missing(format)) format <- "%H:%M"
 }
 else if (d < 2 * 60 * 60 * 24) {
  sc <- 60 * 24
  if(missing(format)) format <- "%a %H:%M"
 }
 else if (d < 7 * 60 * 60 * 24) {
  sc <- 60 * 60 * 24
  if(missing(format)) format <- "%a"
 }
 else {
  sc <- 60 * 60 * 24
 }
 if (d < 60 * 60 * 24 * 50) {
  zz <- pretty(z/sc)
  z <- zz * sc
  class(z) <- c("POSIXt", "POSIXct")
  if(missing(format)) format <- "%b %d"
 }
 else if (d < 1.1 * 60 * 60 * 24 * 365) {
  class(z) <- c("POSIXt", "POSIXct")
  zz <- as.POSIXlt(z)
  zz$mday <- 1
  zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
  zz$mon <- pretty(zz$mon)
  m <- length(zz$mon)
  m <- rep.int(zz$year[1], m)
  zz$year <- c(m, m + 1)
  z <- as.POSIXct(zz)
  if (missing(format)) format <- "%b"
 }
 else {
  class(z) <- c("POSIXt", "POSIXct")
  zz <- as.POSIXlt(z)
  zz$mday <- 1
  zz$isdst <- zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
  zz$year <- pretty(zz$year)
  z <- as.POSIXct(zz)
  if(missing(format)) format <- "%Y"
 }
 z <- z[z >= range[1] & z <= range[2]]
 return(z)
}

gantt.chart<-function(x=NULL,format="%Y/%m/%d",xlim=NULL,
 taskcolors=NULL,main="",ylab="") {
 oldpar<-par(no.readonly=TRUE)
 if(is.null(x)) x<-get.x(format=format)
 ntasks<-length(x$labels)
 plot.new()
 charheight<-strheight("M",units="inches")
 maxwidth<-max(strwidth(x$labels,units="inches"))*1.5
 if(is.null(xlim)) xlim=range(c(x$starts,x$ends))
 if(is.null(taskcolors))
  taskcolors<-color.gradient(c(255,0),c(0,0),c(0,255),max(x$priorities))
 par(mai=c(0,maxwidth,charheight*5,0.1))
 par(omi=c(0.1,0.1,0.1,0.1))
 plot(x$starts,1:ntasks,xlim=xlim,ylim=c(0.5,ntasks+0.5),
     main="",xlab="",ylab=ylab,axes=FALSE,type="n")
 box()
 if(nchar(main)) mtext(main,3,2)
 axis.POSIXct(3,xlim)
 topdown<-seq(ntasks,1)
 axis(2,at=topdown,labels=x$labels,las=2)
 xrange<-par("usr")[1:2]
 abline(v=axis.POSIXct.tickpos(3,xlim),col="darkgray",lty=3)
 half.height <- 0.25
 for(i in 1:ntasks) {
  rect(x$starts[i],topdown[i]-half.height,
   x$ends[i],topdown[i]+half.height,
   col=taskcolors[x$priorities[i]],
   border=FALSE)
 }
 par(oldpar)
 invisible(x)
}
