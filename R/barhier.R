barhier<-function(x,left=0,top,right=1,showval=TRUE,showcount=TRUE,
 firstcall=TRUE,col=NA,...) {
 
 dimx<-dim(x)
 if(firstcall) {
  x<-x[do.call(order,x),]
  oldmar<-par("mar")
  par(mar=c(1,1,2,1))
  plot(0,xlim=c(0,dimx[2]),ylim=c(0,dimx[1]),type="n",axes=FALSE,
   xlab="",ylab="",...)
  top<-dimx[1]
 }
 xfreq<-table(x[,1])
 lenxf<-length(xfreq)
 # careful here, as the table may contain empty entries due to levels
 if(length(col) < lenxf) col<-rep(col,length.out=lenxf)
 labels<-names(xfreq)
 for(bar in 1:lenxf) {
  if(length(xfreq[bar])) {
   if(!is.na(xfreq[bar])) {
    if(xfreq[bar] > 0) {
     rect(left,top-xfreq[bar],right,top,col=col[bar])
     labelheight<-strheight(labels[bar])
     cex<-ifelse((1.5*labelheight) > xfreq[bar],0.75*xfreq[bar]/labelheight,1)
     if(showval) {
      bartext<-ifelse(showcount,paste(labels[bar]," (",xfreq[bar],")",sep=""),
       labels[bar])
      text((left+right)/2,top-xfreq[bar]/2,bartext,cex=cex)
     }
    }
   }
  }
  xvalue<-ifelse(is.numeric(x[,1]),as.numeric(labels[bar]),labels[bar])
  if(dimx[2] > 1) {
   prevcol<-ifelse(bar>1,col[bar-1],NA)
   nextcol<-ifelse(bar<lenxf,col[bar+1],NA)
   nextx<-subset(x,x[,1]==xvalue,2:dimx[2])
   barhier(nextx,right,top,right+1,showval=showval,firstcall=FALSE,
    col=rep(smoothColors(col[bar],1,"#ffffff")[2],length(nextx[,1])))
  }
  top<-top-xfreq[bar]
 }
 if(firstcall) par(mar=oldmar)
}
