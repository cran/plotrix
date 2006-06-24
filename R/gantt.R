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

gantt.chart<-function(x=NULL,format="%Y/%m/%d",xlim=NULL,taskcolors=NULL,
 priority.legend=FALSE,vgridpos=NULL,vgridlab=NULL,half.height=0.25,
 hgrid=FALSE,main="",ylab="") {

 oldpar<-par(no.readonly=TRUE)
 if(is.null(x)) x<-get.gantt.info(format=format)
 ntasks<-length(x$labels)
 plot.new()
 charheight<-strheight("M",units="inches")
 maxwidth<-max(strwidth(x$labels,units="inches"))*1.5
 if(is.null(xlim)) xlim=range(c(x$starts,x$ends))
 npriorities<-max(x$priorities)
 if(is.null(taskcolors))
  taskcolors<-color.gradient(c(255,0),c(0,0),c(0,255),npriorities)
 else {
  if(length(taskcolors) < npriorities)
   taskcolors<-rep(taskcolors,length.out=npriorities)
 }
 bottom.margin<-ifelse(priority.legend,0.5,0) 
 par(mai=c(bottom.margin,maxwidth,charheight*5,0.1))
 par(omi=c(0.1,0.1,0.1,0.1),xaxs="i",yaxs="i")
 plot(x$starts,1:ntasks,xlim=xlim,ylim=c(0.5,ntasks+0.5),
  main="",xlab="",ylab=ylab,axes=FALSE,type="n")
 box()
 if(nchar(main)) mtext(main,3,2)
 if(is.null(vgridpos)) tickpos<-axis.POSIXct(3,xlim)
 else tickpos<-vgridpos
 # if no tick labels, use the grid positions if there
 if(is.null(vgridlab) && !is.null(vgridpos))
  vgridlab<-format.POSIXct(vgridpos,"%y/%m/%d")
 # if vgridpos wasn't specified, use default axis ticks
 if(is.null(vgridlab)) axis.POSIXct(3,xlim)
 else axis(3,at=tickpos,labels=vgridlab)
 topdown<-seq(ntasks,1)
 axis(2,at=topdown,labels=x$labels,las=2)
 abline(v=tickpos,col="darkgray",lty=3)
 for(i in 1:ntasks) {
  rect(x$starts[i],topdown[i]-half.height,
  x$ends[i],topdown[i]+half.height,
  col=taskcolors[x$priorities[i]],
  border=FALSE)
 }
 if(hgrid)
  abline(h=(topdown[1:(ntasks-1)]+topdown[2:ntasks])/2,col="darkgray",lty=3)
 if(priority.legend) {
  par(xpd=TRUE)
  plim<-par("usr")
  gradient.rect(plim[1],0,plim[1]+(plim[2]-plim[1])/4,0.3,col=taskcolors)
  text(plim[1],0.2,"Priorities  ",adj=c(1,0.5))
  text(c(plim[1],plim[1]+(plim[2]-plim[1])/4),c(0.4,0.4),c("High","Low"))
 }
 par(oldpar)
 invisible(x)
}
