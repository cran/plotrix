dendroPlot<-function(x,breaks=NULL,pch=1,cex=1,nudge=NA,setlabels=NA,...) {
 if(!is.null(breaks)) {
  for(list_element in 1:length(x)) {
   if(length(breaks[[list_element]]) > 1)
    x[[list_element]]<-cut(x[[list_element]],breaks=breaks[[list_element]])
  }
 }
 plot(c(0.5,length(x)+0.5),range(unlist(sapply(x,as.numeric))),
  type="n",xaxt="n",...)
 lenx<-length(x)
 if(is.na(setlabels[1])) setlabels<-paste("Group",1:lenx)
 axis(1,at=1:lenx,labels=setlabels)
 if(is.na(nudge[1])) nudge<-strwidth("o")
 if(length(pch) < lenx) pch<-rep(pch,length.out=lenx)
 for(list_element in 1:length(x)) {
  for(bin in 1:length(levels(x[[list_element]]))) {
   thisbin<-which(as.numeric(x[[list_element]])==bin)
   npoints<-length(x[[list_element]][thisbin])
   if(npoints) {
    if(npoints==1) offsetx<-offsety<-0
    else {
     offsetx<-(c(0,trunc(seq(1,npoints/2,by=0.5)))*nudge[1])[1:npoints]
     if(length(nudge) > 1)
      offsety<-c(0,rep(c(nudge[2],nudge[2],-nudge[2],-nudge[2]),
       length.out=npoints-1))
     else offsety<-rep(0,length(offsetx))
    }
    if(length(offsetx) > 3 )
     offsetx[seq(2,length(offsetx),by=2)]<-
      -offsetx[seq(2,length(offsetx),by=2)]
    else if(length(offsetx) == 3 ) offsetx[2]<--offsetx[2]
    points(list_element+offsetx,
     sort(as.numeric(x[[list_element]][thisbin]))+offsety,
     pch=pch[list_element])
   }
  }
 }
}
