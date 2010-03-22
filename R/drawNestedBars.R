drawNestedBars<-function(x,start,end,shrink=0.1,errbars=FALSE,
 col=NA,labelcex=1,labely=NA,showlabels=TRUE,arrow.cap=0.01) {

 dimx<-dim(x[[1]])
 if(is.null(dimx) || length(dimx) == 1) {
  lenx<-length(x[[1]])
  # assume start and end are the edges of the bar within which
  # the new bars must fit. First calculate the total space
  barspace<-(end-start)*shrink
  # then calculate the width of each bar
  barwidth<-((end-start)-barspace)/lenx
  # calculate the space between bars and at the edges
  barspace<-barspace/(lenx+1)
  start<-start+barspace
  arrow.gap<-strheight("O")/2
  caplen<-arrow.cap*par("pin")[1]
  barlabels<-x[[4]]
  if(length(col) < lenx) col<-rep(col,length.out=lenx)
  for(xbar in 1:lenx) {
   barcenter<-start+barwidth/2
   if(!is.null(x[[1]][[xbar]]))
    rect(start,0,start+barwidth,x[[1]][[xbar]],col=col[xbar])
   if(errbars) {
    if(!is.null(x[[2]][[xbar]]) && !is.null(x[[3]][[xbar]]) &&
       !is.na(x[[2]][[xbar]]) && !is.na(x[[3]][[xbar]])) {
     if(arrow.gap >= x[[2]][[xbar]] * 0.9 || arrow.gap >= x[[3]][[xbar]] * 0.9) {
      x0<-rep(barcenter-arrow.cap, 2)
      x1<-rep(barcenter+arrow.cap, 2)
      y0<-rep(c(x[[1]][[xbar]]-x[[2]][[xbar]],x[[1]][[xbar]]+x[[3]][[xbar]]),2)
      y1<-rep(c(x[[1]][[xbar]]-x[[2]][[xbar]],x[[1]][[xbar]]+x[[3]][[xbar]]),2)
      segments(x0,y0,x1,y1)
     }
     else {
      x0<-x1<-rep(barcenter,2)
      y0<-c(x[[1]][[xbar]]+arrow.gap,x[[1]][[xbar]]-arrow.gap)
      y1<-c(x[[1]][[xbar]]+x[[3]][[xbar]],x[[1]][[xbar]]-x[[2]][[xbar]])
      arrows(x0,y0,x1,y1,length=caplen,angle=90)
     }
    }
   }
   if(showlabels) {
    par(xpd=TRUE)
    segments(c(start,start+barwidth,start),c(0,0,labely),
     c(start,start+barwidth,start+barwidth),rep(labely,3))
    boxed.labels(barcenter,labely,barlabels[xbar],
     bg=ifelse(is.na(col[xbar]),"white",col[xbar]),cex=labelcex)
    par(xpd=FALSE)
   }
   start<-start+barwidth+barspace
  }
 }
 else {
  ndim<-length(dimx)
  # assume start and end are the edges of the bar within which
  # the new bars must fit. First calculate the total space
  barspace<-(end-start)*shrink
  # then calculate the width of each bar
  barwidth<-((end-start)-barspace)/dimx[1]
  # calculate the space between bars and at the edges
  barspace<-barspace/(dimx[1]+1)
  sliceargs<-vector("list",ndim+1)
  xslice<-vector("list",4)
  start<-start+barspace
  for(slice in 1:dimx[1]) {
   for(stat in 1:3) {
    sliceargs[[1]]<-x[[stat]]
    sliceargs[[2]]<-slice
    for(arg in 3:(ndim+1)) sliceargs[[arg]]<-TRUE
    xslice[[stat]]<-do.call('[',sliceargs)
   }
   xslice[[4]]<-x[[4]]
   # send the sliced list as x to drawNestedBars
   drawNestedBars(xslice,start=start,end=start+barwidth,
    shrink=shrink,errbars=errbars,col=col,labelcex=labelcex,labely=labely,
    showlabels=showlabels,arrow.cap=arrow.cap)
   start<-start+barwidth+barspace
  }
 }
}
