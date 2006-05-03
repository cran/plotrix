get.triprop<-function(use.percentages=FALSE,cnames=c("1st","2nd","3rd")) {
 cat("Enter the label and ")
 cat(ifelse(use.percentages,"percentages ","proportions "))
 cat("of",cnames[1],cnames[2],"and",cnames[3],"for each observation.\n")
 cat("Enter a blank observation label to end.\n")
 nextlabel<-"dummy"
 nextprop<-0
 proplabels<-NA
 prop1<-NA
 prop2<-NA
 prop3<-NA
 nprop<-0
 totprop<-ifelse(use.percentages,100,1)
 tolerance<-ifelse(use.percentages,1,0.01)
 while(nchar(nextlabel)) {
  nextlabel<-readline("Observation label - ")
  if(nchar(nextlabel)) {
   if(is.na(proplabels[1])) proplabels<-nextlabel
   else proplabels<-c(proplabels,nextlabel)
   cat(cnames[1],"- ")
   nextprop<-as.numeric(readline())
   if(is.na(prop1[1])) prop1<-nextprop
   else prop1<-c(prop1,nextprop)
   cat(cnames[2],"- ")
   nextprop<-as.numeric(readline())
   if(is.na(prop2[1])) prop2<-nextprop
   else prop2<-c(prop2,nextprop)
   cat(cnames[3],"- ")
   nextprop<-as.numeric(readline())
   if(is.na(prop3[1])) prop3<-nextprop
   else prop3<-c(prop3,nextprop)
   nprop<-nprop+1
  }
  sumprop<-prop1[nprop]+prop2[nprop]+prop3[nprop]
  if(abs(totprop-sumprop) > tolerance)
   cat("Warning - sum not equal to",totprop,"\n")
 }
 triprop<-cbind(prop1,prop2,prop3)
 rownames(triprop)<-proplabels
 colnames(triprop)<-cnames
 return(triprop)
}

triax.abline<-function(l=NULL,r=NULL,b=NULL,col=par("col"),lty=par("lty")) {
 sin60<-sin(pi/3)
 if(!is.null(l)) {
  if(any(l>1)) l<-l/100
  lx1<-l*0.5
  ly<-l*sin60
  segments(lx1,ly,1-lx1,ly,col=col,lty=lty)
 }
 if(!is.null(r)) {
  if(any(r>1)) l<-r/100
  rx1<-0.5*(r+1)
  ry1<-sin60*(1-r)
  rx2<-1-r
  segments(rx1,ry1,r,0,col=col,lty=lty)
 }
 if(!is.null(b)) {
  if(any(b>1)) l<-b/100
  bx2<-0.5*(1-b)
  by2<-sin60*(1-b)
  segments(1-b,0,bx2,by2,col=col,lty=lty)
 }
}

triax.points<-function(x,show.legend=FALSE,label.points=FALSE,
 point.labels=NULL,col.symbols=par("fg"),pch=par("pch"),
 bg.symbols=par("bg"),...) {
 
 if(dev.cur() == 1)
  stop("Cannot add points unless the triax.frame has been drawn")
 if(missing(x))
  stop("Usage: triax.points(x,...)\n\twhere x is a 3 column array of proportions or percentages")
 if(!is.matrix(x) && !is.data.frame(x))
  stop("x must be a matrix or data frame with at least 3 columns and one row.")
 if(any(x > 1) || any(x < 0)) {
  if(any(x < 0))
   stop("All proportions must be between zero and one.")
  if(any(x > 100))
   stop("All percentages must be between zero and 100.")
  x<-x/100
 }
 if(any(abs(rowSums(x)-1) > 0.01))
  warning("At least one set of proportions does not equal one.")
 ypos<-x[,3]*sin(pi/3)
 xpos<-1-(x[,1]+x[,3]*0.5)
 nobs<-dim(x)[1]
 points(x=xpos,y=ypos,pch=pch,col=col.symbols,bg=bg.symbols,...)
 if(is.null(point.labels)) point.labels<-rownames(x)
 if(label.points) thigmophobe.labels(xpos,ypos,point.labels)
 if(show.legend) {
  legend(0.16-0.02*max(nchar(point.labels)),0.75+0.04*length(point.labels),
   legend=point.labels,pch=pch,col=col.symbols)
 }
 invisible(list(x=xpos,y=ypos)) 
}

triax.frame<-function(main="",
 at=list(l=seq(0.1,0.9,by=0.1),r=seq(0.1,0.9,by=0.1),b=seq(0.1,0.9,by=0.1)),
 axis.labels=NULL,tick.labels=NULL,col.axis="black",
 show.grid=FALSE,col.grid="gray",lty.grid=par("lty")) {

 sin60<-sin(pi/3)
 # bottom ticks
 bx1<-at$b
 bx2<-bx1+0.01
 by1<-rep(0,9)
 by2<-rep(-0.02*sin60,9)
 # left ticks
 ly1<-at$l*sin60
 lx1<-bx1*0.5
 lx2<-lx1-0.02
 ly2<-ly1
 # right ticks
 rx1<-at$r*0.5+0.5
 rx2<-rx1+0.01
 ry1<-rev(ly1)
 ry2<-rev(ly2)+0.02*sin60
 if(show.grid) {
  par(fg=col.grid)
  segments(bx2,by2,lx1,ly1,lty=lty.grid)
  segments(lx1,ly1,rev(rx1),rev(ry1),lty=lty.grid)
  segments(rx1,ry1,bx1,by1,lty=lty.grid)
 }
 par(fg=col.axis)
 if(is.null(tick.labels)) tick.labels<-at
 text(lx1-0.05,ly1,tick.labels$l)
 par(srt=57)
 text(0.13,0.5,axis.labels[1])
 text(rx2+0.02,ry1+0.04,tick.labels$r)
 par(srt=303)
 text(0.86,0.52,axis.labels[2])
 text(bx1+0.025,by1-0.05,rev(tick.labels$b))
 par(srt=0)
 text(0.5,-0.14,axis.labels[3])
 # draw the triangle and ticks
 x1<-c(0,0,0.5)
 x2<-c(1,0.5,1)
 y1<-c(0,0,sin60)
 y2<-c(0,sin60,0)
 par(fg=col.axis)
 segments(x1,y1,x2,y2)
 # bottom ticks
 segments(bx1,by1,bx2,by2)
 # left ticks
 segments(lx1,ly1,lx2,ly2)
 # right ticks
 segments(rx1,ry1,rx2,ry2)
 if(nchar(main)) {
  par(cex=1.5)
  text(0.5,1.1,main)
  par(cex=1)
 }
}

triax.plot<-function(x=NULL,main="",
 at=list(l=seq(0.1,0.9,by=0.1),r=seq(0.1,0.9,by=0.1),b=seq(0.1,0.9,by=0.1)),
 axis.labels=NULL,tick.labels=NULL,col.axis="black",
 show.grid=FALSE,col.grid="gray",lty.grid=par("lty"),
 show.legend=FALSE,label.points=FALSE,point.labels=NULL,
 col.symbols="black",pch=par("pch"),...) {
 
 par(xpd=TRUE)
 currentfg<-par("fg")
 plot(0.5,type="n",axes=FALSE,xlim=c(0,1.1),ylim=c(0,1),main="",xlab="",ylab="")
 if(is.null(axis.labels)) axis.labels<-colnames(x)[1:3]
 triax.frame(main=main,at=at,axis.labels=axis.labels,tick.labels=tick.labels,
  col.axis=col.axis,show.grid=show.grid,col.grid=col.grid,lty.grid=lty.grid)
 if(is.null(x)) xypos<-NULL
 else xypos<-triax.points(x,show.legend=show.legend,label.points=label.points,
   point.labels=point.labels,col.symbols=col.symbols,pch=pch,...)
 par(fg=currentfg,xpd=FALSE)
 invisible(xypos) 
}
