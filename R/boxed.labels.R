boxed.labels<-function(x,y=NA,labels,
 bg=ifelse(match(par("bg"),"transparent",0),"white",par("bg")),
 border=TRUE,xpad=1.2,ypad=1.2,srt=0,cex=1,adj=0.5,...) {

 oldcex<-par("cex")
 par(cex=cex)
 if(is.na(y) && is.list(x)) {
  y<-unlist(x[[2]])
  x<-unlist(x[[1]])
 }
 box.adj<-adj+(xpad-1)*cex*(0.5-adj)
 # boxes can only be rotated 90 degrees either way
 if(srt==90 || srt==270) {
  bheights<-strwidth(labels)
  theights<-bheights*(1-box.adj)
  bheights<-bheights*box.adj
  lwidths<-rwidths<-strheight(labels)*0.5
 }
 else {
  lwidths<-strwidth(labels)
  rwidths<-lwidths*(1-box.adj)
  lwidths<-lwidths*box.adj
  bheights<-theights<-strheight(labels)*0.5
 }
 # fix for adding a col argument to ... by Thorn Thaler
 args <- list(x = x, y = y, labels = labels, srt = srt, adj = adj, 
             col = ifelse(colSums(col2rgb(bg) * c(1, 1.4, 0.6)) < 
                          350, "white", "black"))
 args <- modifyList(args, list(...))
 rect(x - lwidths * xpad, y - bheights * ypad, x + rwidths * 
     xpad, y + theights * ypad, col = bg, border = border)
 do.call(text, args)
 par(cex=oldcex)
}
