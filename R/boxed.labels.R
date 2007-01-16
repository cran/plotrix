# x,y values are the center of the labels
boxed.labels<-function(x,y=NA,labels,bg="white",border=TRUE,xpad=0.6,
 ypad=0.6,srt=0,...) {

 if(is.na(y) && is.list(x)) {
  y<-unlist(x[[2]])
  x<-unlist(x[[1]])
 }
 if(srt==90 || srt==270) {
  heights<-strwidth(labels)
  widths<-strheight(labels)
 }
 else{
  widths<-strwidth(labels)
  heights<-strheight(labels)
 }
 rect(x-widths*xpad,y-heights*ypad,x+widths*xpad,y+heights*ypad,
  col=bg,border=border)
 text(x,y,labels,srt=srt,...)
}
