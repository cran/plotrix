boxed.labels<-function(x,y,labels,col="white",border=TRUE,xpad=0.6,ypad=0.6,...) {
 widths<-strwidth(labels)
 heights<-strheight(labels)
 rect(x-widths*xpad,y-heights*ypad,x+widths*xpad,y+heights*ypad,
  col=col,border=border)
 text(x,y,labels,...)
}
