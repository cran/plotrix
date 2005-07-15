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
 tickpos<-axis.POSIXct(3,xlim)
 topdown<-seq(ntasks,1)
 axis(2,at=topdown,labels=x$labels,las=2)
 xrange<-par("usr")[1:2]
 abline(v=tickpos,col="darkgray",lty=3)
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
