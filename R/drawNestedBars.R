drawNestedBars<-function(x,start,end,shrink=0.1,errbars=FALSE,
 col=NA,labelcex=1,lineht=NA,showall=TRUE,showlabels=TRUE,arrow.cap=0.01) {

 barcol<-ifelse(is.list(col),col[[1]],col)
 # should only be one bar per call
 if(!is.null(x[[1]][[1]][1,2]) && (showall | length(x[[1]]) == 1))
  rect(start,0,end,x[[1]][[1]][1,2],col=barcol)
 if(showlabels && !is.null(x[[1]][[1]][1,2])) {
  barlabel<-ifelse(names(x[[1]][[1]])[1]=="Overall","Overall",
   as.character(x[[1]][[1]][1,1]))
  labely<--lineht*length(x[[1]])
  par(xpd=TRUE)
  segments(c(start,end,start),c(0,0,labely),c(start,end,end),rep(labely,3))
  boxed.labels((start+end)/2,labely,barlabel,ypad=1.4,
   bg=ifelse(is.na(barcol),"white",barcol),cex=labelcex)
  par(xpd=FALSE)
 }
 if(errbars && length(x[[1]])==1)
  dispersion((start+end)/2,x[[1]][[1]][1,2],x[[2]][[1]][1,2],x[[3]][[1]][1,2],
   arrow.cap=arrow.cap)
 # now set up each bar in the next level and call
 lenx<-length(x)
 # remove the first component of each element of x
 for(xcomp in 1:lenx) x[[xcomp]][[1]]<-NULL
 if(length(x[[1]])) {
  if(is.list(col)) col[[1]]<-NULL
  xlevels<-unlist(x[[1]][[1]][1])
  nlevels<-length(xlevels)
  barspace<-(end-start)*shrink
  barwidth<-((end-start)-barspace)/nlevels
  barspace<-barspace/(nlevels+1)
  start<-start+barspace
  for(xlev in 1:nlevels) {
   newx<-vector("list",lenx)
   newcol<-col
   for(xcomp in 1:lenx) {
    newx[[xcomp]][[1]]<-x[[xcomp]][[1]][xlev,]
    lenxcomp<-length(x[[xcomp]])
    if(lenxcomp > 1) {
     for(xsub in 2:lenxcomp) {
      newx[[xcomp]][[xsub]]<-x[[xcomp]][[xsub]][x[[xcomp]][[xsub]][,1]==xlevels[xlev],]
      newx[[xcomp]][[xsub]]<-newx[[xcomp]][[xsub]][,-1]
     }
    }
   }
   if(is.list(col)) newcol[[1]]<-col[[1]][xlev]
   if(length(x[[1]])>0)
    drawNestedBars(newx,start,start+barwidth,shrink=shrink,
     errbars=errbars,col=newcol,labelcex=labelcex,lineht=lineht,
     showall=showall,showlabels=showlabels,arrow.cap=arrow.cap)
   else print(newx)
   start<-start+barwidth+barspace
  }
 }
}
