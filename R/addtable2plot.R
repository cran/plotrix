addtable2plot<-function(x,y=NULL,table,lwd=par("lwd"),bty="n",
 bg=par("bg"),cex=1,xjust=0,yjust=1,xpad=0.1,ypad=0.5,box.col=par("fg"),
 text.col=par("fg"),display.colnames=TRUE,display.rownames=FALSE,
 hlines=FALSE,vlines=FALSE,title=NULL) {

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
 if(is.null(dim(bg))) bg<-matrix(bg,nrow=tabdim[1],ncol=tabdim[2])
 column.names<-colnames(table)
 if(is.null(column.names) && display.colnames)
  column.names<-1:tabdim[2]
 row.names<-rownames(table)
 if(is.null(row.names) && display.rownames)
 row.names<-1:tabdim[1]
 if(par("xlog")) x<-log10(x)
 cellwidth<-rep(0,tabdim[2])
 if(display.colnames) {
  for(column in 1:tabdim[2])
   cellwidth[column]<-max(strwidth(c(column.names[column],
    format(table[,column])),cex=cex))*(1+xpad)
  nvcells<-tabdim[2]+1
 }
 else {
  nvcells<-tabdim[2]
  for(column in 1:tabdim[2])
   cellwidth[column]<-max(strwidth(format(table[,column]),cex=cex))*(1+xpad)
 }
 if(display.rownames) {
  nhcells<-tabdim[2]+1
  rowname.width<-max(strwidth(row.names,cex=cex))*(1+xpad)
 }
 else {
  nhcells<-tabdim[2]
  rowname.width<-0
 }
 if(par("ylog")) y<-log10(y)
 cellheight<-
  max(strheight(c(column.names,row.names,as.vector(unlist(table))),
   cex=cex))*(1+ypad)
 ytop<-y+yjust*nvcells*cellheight
 # adjust for logarithmic plotting and allow the table to extend beyond the plot
 oldpar<-par(xlog=FALSE,ylog=FALSE,xpd=TRUE)
 for(row in 1:tabdim[1]) {
  xleft<-x-xjust*(sum(cellwidth)+rowname.width)
  # draw the horizontal lines unless at the bottom
  if(row <= nvcells-1 && hlines)
   segments(xleft+rowname.width,ytop-row*cellheight,
    xleft+sum(cellwidth)+rowname.width,ytop-row*cellheight,
    lwd=lwd,col=box.col)
  if(display.rownames) {
   text(xleft+0.5*rowname.width,
    ytop-(row+display.colnames-0.5)*cellheight,
    row.names[row],cex=cex,col=text.col)
   xleft<-xleft+rowname.width
  }
  for(column in 1:tabdim[2]) {
   rect(xleft,ytop-(row+display.colnames-1)*cellheight,
    xleft+cellwidth[column],ytop-(row+display.colnames)*cellheight,
    col=bg[row,column])
   text(xleft+0.5*cellwidth[column],
    ytop-(row+display.colnames-0.5)*cellheight,
    table[row,column],cex=cex,col=text.col)
   if(vlines) segments(xleft,ytop-(row+display.colnames)*cellheight,
    xleft+cellwidth[column],ytop-(row+display.colnames)*cellheight,col=box.col)
   xleft<-xleft+cellwidth[column]
  }
 }
 if(display.colnames)
  xleft<-x-xjust*(sum(cellwidth)+rowname.width)
 for(column in 1:tabdim[2]) {
  text(xleft+display.rownames*rowname.width+cellwidth[column]*0.5,
   ytop-0.5*cellheight,column.names[column],cex=cex,col=text.col)
  if(!hlines)
   segments(xleft+rowname.width,ytop-cellheight,
    xleft+cellwidth[column],ytop-cellheight,
    lwd=lwd,col=box.col)
  xleft<-xleft+cellwidth[column]
 }
 if(!is.null(title)) {
  xleft<-x-xjust*(sum(cellwidth)+rowname.width)
  text(xleft+(rowname.width+sum(cellwidth))/2,ytop+cellheight/2,title,
   cex=cex,col=text.col)
  if(bty=="n")
   segments(xleft,ytop,xleft+sum(cellwidth)+rowname.width,ytop,lwd=lwd,col=box.col)
 }
 par(oldpar)
}
