addtable2plot<-function(x,y=NULL,table,lwd=par("lwd"),bty="n",
 bg=par("bg"),cex=1,xjust=0,yjust=1,box.col=par("fg"),text.col=par("fg"),
 display.colnames=TRUE,display.rownames=FALSE,hlines=FALSE,vlines=FALSE,
 title=NULL) {

 # make sure that there is a plot device there
 if(dev.cur() == 1)
  stop("Cannot add table unless a graphics device is open")
 # check for an xy.coords structure
 if(is.null(y)) {
  if(is.null(x$y)) stop("both x and y coordinates must be given")
  y<-x$y
  x<-x$x
 }
 tabdim<-dim(table)
 column.names<-colnames(table)
 if(is.null(column.names) && display.colnames)
  column.names<-1:tabdim[1]
 row.names<-rownames(table)
 if(is.null(row.names) && display.rownames)
 row.names<-1:tabdim[1]
 mwidth<-strwidth("M",cex=cex)
 if(par("xlog")) x<-log10(x)
 if(display.colnames) {
  cellwidth<-max(strwidth(c(column.names,row.names,
   as.vector(unlist(table))),cex=cex))+mwidth
  nvcells<-tabdim[1]+1
 }
 else {
  nvcells<-tabdim[1]
  cellwidth<-
   max(strwidth(c(row.names,as.vector(unlist(table))),cex=cex))+mwidth
 }
 if(display.rownames) nhcells<-tabdim[2]+1
 else nhcells<-tabdim[2]
 if(par("ylog")) y<-log10(y)
 cellheight<-
  max(strheight(c(column.names,row.names,as.vector(unlist(table))),
   cex=cex))*1.5
 xleft<-x-xjust*nhcells*cellwidth
 ytop<-y+yjust*nvcells*cellheight
 # adjust for logarithmic plotting and allow the table to extend beyond the plot
 oldpar<-par(ylog=FALSE,ylog=FALSE,xpd=TRUE)
 # draw the box if wanted
 if(bty=="o")
  rect(xleft,ytop-nvcells*cellheight,xleft+nhcells*cellwidth,ytop,
   lwd=lwd,col=bg,border=box.col)
 for(row in 1:tabdim[1]) {
  # draw the horizontal lines unless at the bottom
  if(row <= nvcells-1 && hlines)
   segments(xleft,ytop-row*cellheight,
    xleft+nhcells*cellwidth,ytop-row*cellheight,
    lwd=lwd,col=box.col)
  if(display.rownames)
   text(xleft+0.5*cellwidth,
    ytop-(row+display.colnames-0.5)*cellheight,
    row.names[row],cex=cex,col=text.col)
  for(col in 1:tabdim[2]) {
   text(xleft+(col+display.rownames-0.5)*cellwidth,
    ytop-(row+display.colnames-0.5)*cellheight,
    table[row,col],cex=cex,col=text.col)
   if(vlines) segments(xleft+(col+display.rownames-1)*cellwidth,
    ytop-(row+display.colnames)*cellheight,
    xleft+(col+display.rownames-1)*cellwidth,
    ytop-row*cellheight)
  }
 }
 if(display.colnames)
  for(col in 1:tabdim[2]) {
   text(xleft+(col+display.rownames-0.5)*cellwidth,
    ytop-0.5*cellheight,column.names[col],cex=cex,col=text.col)
  if(!hlines)
   segments(xleft,ytop-cellheight,
    xleft+nhcells*cellwidth,ytop-cellheight,
    lwd=lwd,col=box.col)
 }
 if(!is.null(title)) {
  text(xleft+(nhcells*cellwidth)/2,ytop+cellheight/2,title,
   cex=cex,col=text.col)
  if(bty=="n")
   segments(xleft,ytop,xleft+nhcells*cellwidth,ytop,lwd=lwd,col=box.col)
 }
 par(oldpar)
}
