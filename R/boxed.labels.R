boxed.labels<-function(x,y=NA,labels,bg="white",border=TRUE,xpad=1.2,
 ypad=1.2,srt=0,cex=1,adj=0.5,...) {

 if(is.na(y) && is.list(x)) {
  y<-unlist(x[[2]])
  x<-unlist(x[[1]])
 }
 box.adj<-adj+(xpad-1)*(0.5-adj)
 if(srt==90 || srt==270) {
  bheights<-strwidth(labels)
  theights<-bheights*(1-box.adj)
  bheights<-bheights*box.adj
  lwidths<-rwidths<-strheight(labels)*0.5
 }
 else{
  lwidths<-strwidth(labels)
  rwidths<-lwidths*(1-box.adj)
  lwidths<-lwidths*box.adj
  bheights<-theights<-strheight(labels)*0.5
 }
 rect(x-lwidths*xpad,y-bheights*ypad,x+rwidths*xpad,y+theights*ypad,
  col=bg,border=border)
 text(x,y,labels,srt=srt,cex=cex,adj=adj,...)
}
