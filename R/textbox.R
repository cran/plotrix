textbox<-function(x,y,textlist,justify=TRUE,cex=1,leading=0.5,box=TRUE) {
 par(adj = 0)
 textstr <- paste(textlist, collapse = " ")
 words <- strsplit(textstr, " ")[[1]]
 line.height <- strheight("hy", cex = cex) * (1 + leading)
 x.len <- diff(x)
 y.pos <- y
 x.pos <- x[1]
 curword <- 1
 while (curword < length(words)) {
  curline <- ""
  curline <- paste(curline, words[curword])
  curword <- curword + 1
  while (x.pos+strwidth(paste(curline,words[curword]),cex=cex) < x[2]) {
   curline<-paste(curline,words[curword])
   curword<-curword+1
  }
  text(x.pos,y.pos,curline)
  y.pos<-y.pos-line.height
 }
 if(box) rect(x[1],y+line.height,x[2],y.pos)
 par(adj=0.5)
}
