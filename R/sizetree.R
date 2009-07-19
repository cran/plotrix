sizetree<-function(x,left=0,top,right=1,lastcenter=NA,showval=TRUE,showcount=TRUE,
 firstcall=TRUE,col=NA,colorindex=1,...) {
 
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
 if(is.list(col)) {
  barcol<-col[[colorindex]]
  colorindex<-colorindex+1
 }
 else barcol<-col
 # careful here, as the table may contain empty entries due to levels
 if(length(barcol) < lenxf) barcol<-rep(col,length.out=lenxf)
 labels<-names(xfreq)
 squeeze<-(right-left)/10
 for(bar in 1:lenxf) {
  if(length(xfreq[bar])) {
   if(!is.na(xfreq[bar])) {
    if(xfreq[bar] > 0) {
     rect(left+squeeze,top-xfreq[bar],right-squeeze,top,col=barcol[bar])
     labelheight<-strheight(labels[bar])
     cex<-ifelse((1.5*labelheight) > xfreq[bar],0.75*xfreq[bar]/labelheight,1)
     if(showval) {
      bartext<-ifelse(showcount,paste(labels[bar]," (",xfreq[bar],")",sep=""),
       labels[bar])
      text((left+right)/2,top-xfreq[bar]/2,bartext,cex=cex)
     }
     if(!is.na(lastcenter))
      segments(left+squeeze,top-xfreq[bar]/2,left-squeeze,lastcenter)
    }
   }
  }
  xvalue<-ifelse(is.numeric(x[,1]),as.numeric(labels[bar]),labels[bar])
  if(dimx[2] > 1) {
   prevcol<-ifelse(bar>1,col[bar-1],NA)
   nextcol<-ifelse(bar<lenxf,col[bar+1],NA)
   nextx<-subset(x,x[,1]==xvalue,2:dimx[2])
   sizetree(nextx,right,top,right+1,lastcenter=top-xfreq[bar]/2,
    showval=showval,firstcall=FALSE,col=col,colorindex=colorindex)
  }
  top<-top-xfreq[bar]
 }
 if(firstcall) par(mar=oldmar)
}
