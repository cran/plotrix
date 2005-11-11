boxed.labels<-function(x,y,labels,bg="white",border=TRUE,xpad=0.6,ypad=0.6,...) {
 widths<-strwidth(labels)
 heights<-strheight(labels)
 rect(x-widths*xpad,y-heights*ypad,x+widths*xpad,y+heights*ypad,
  col=bg,border=border)
 text(x,y,labels,...)
}
