# x,y values are the center of the labels
boxed.labels<-function(x,y=NA,labels,bg="white",border=TRUE,
 xpad=0.6,ypad=0.6,...) {

 if(is.na(y) && is.list(x)) {
  y<-unlist(x[[2]])
  x<-unlist(x[[1]])
 }
 widths<-strwidth(labels)
 heights<-strheight(labels)
 rect(x-widths*xpad,y-heights*ypad,x+widths*xpad,y+heights*ypad,
  col=bg,border=border)
 text(x,y,labels,...)
}
