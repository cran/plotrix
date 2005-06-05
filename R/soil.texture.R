show.soil.texture<-function(soiltexture,pch=NULL,col.symbols=NULL,
 bg.symbols=NA,show.legend=FALSE) {
 
 if(dev.cur() == 1)
  stop("Cannot add points unless the soil.triangle has been drawn")
 if(missing(soiltexture))
  stop("Usage: soil.points(soiltexture,pch=NULL,col.symbols=NULL,show.legend=FALSE)")
 if(!is.matrix(soiltexture) && !is.data.frame(soiltexture))
  stop("soiltexture must be a matrix with at least three columns and one row.")
 if(any(soiltexture > 1) || any(soiltexture < 0)) {
  if(any(soiltexture < 0))
   stop("All soil proportions must be between zero and one.")
  if(any(soiltexture > 100))
   stop("All soil percentages must be between zero and 100.")
  soiltexture<-soiltexture/100
 }
 if(any(abs(rowSums(soiltexture)-1) > 0.01))
  warning("At least one set of soil proportions does not equal one.")
 ypos<-soiltexture[,3]*sin(pi/3)
 xpos<-1-soiltexture[,1]-soiltexture[,3]*0.5
 nobs<-dim(soiltexture)[1]
 if(is.null(pch)) pch<-1:nobs
 if(is.null(col.symbols)) col.symbols<-2:(nobs+1)
 points(x=xpos,y=ypos,pch=pch,col=col.symbols,bg=bg.symbols)
 if(show.legend) {
  sample.labels<-rownames(soiltexture)
  legend(0,0.65+0.04*length(sample.labels),legend=sample.labels,pch=pch,
   col=col.symbols)
 }
 invisible(list(x=xpos,y=ypos)) 
}

get.soil.texture<-function(use.percentages=FALSE) {
 cat("Enter the label and ")
 cat(ifelse(use.percentages,"percentages ","proportions "))
 cat("of sand, silt and clay for each soil sample.\n")
 cat("Enter a blank label to end.\n")
 nextlabel<-"dummy"
 nextprop<-0
 soillabels<-NA
 sandprop<-NA
 siltprop<-NA
 clayprop<-NA
 while(nchar(nextlabel)) {
  nextlabel<-readline("Soil sample label - ")
  if(nchar(nextlabel)) {
   if(is.na(soillabels[1])) soillabels<-nextlabel
   else soillabels<-c(soillabels,nextlabel)
   nextprop<-as.numeric(readline("Sand - "))
   if(is.na(sandprop[1])) sandprop<-nextprop
   else sandprop<-c(sandprop,nextprop)
   nextprop<-as.numeric(readline("Silt - "))
   if(is.na(siltprop[1])) siltprop<-nextprop
   else siltprop<-c(siltprop,nextprop)
   nextprop<-as.numeric(readline("Clay - "))
   if(is.na(clayprop[1])) clayprop<-nextprop
   else clayprop<-c(clayprop,nextprop)
  }
 }
 soiltexture<-cbind(sand=sandprop,silt=siltprop,clay=clayprop)
 rownames(soiltexture)<-soillabels
 colnames(soiltexture)<-c("sand","silt","clay")
 return(soiltexture)
}

soil.texture<-function(soiltexture=NULL,
 show.names=TRUE,show.lines=TRUE,show.grid=FALSE,bg.names="white",main="",
 col.axis="black",col.names="gray",col.lines="gray",col.grid="gray",...) {

 par(xpd=TRUE)
 plot(0.5,type="n",axes=FALSE,xlim=c(0,1.1),ylim=c(0,1),main="",xlab="",ylab="")
 sin60<-sin(pi/3)
 # bottom ticks
 bx1<-seq(0.1,0.9,by=0.1)
 bx2<-bx1+0.01
 by1<-rep(0,9)
 by2<-rep(-0.02*sin60,9)
 # left ticks
 ly1<-bx1*sin60
 lx1<-bx1*0.5
 lx2<-lx1-0.02
 ly2<-ly1
 # right ticks
 rx1<-rev(lx1+0.5)
 rx2<-rx1+0.01
 ry1<-ly1
 ry2<-ly2+0.02*sin60
 ten2ninety<-seq(10,90,by=10)
 currentfg<-par("fg")
 par(fg=col.axis)
 text(lx1-0.05,ly1,as.character(ten2ninety))
 par(srt=57)
 text(0.13,0.5,"percent clay")
 arrows(0.12,0.41,0.22,0.57,length=0.15)
 text(rx2+0.02,ry1+0.04,as.character(rev(ten2ninety)))
 par(srt=303)
 text(0.86,0.52,"percent silt")
 arrows(0.78,0.57,0.88,0.41,length=0.15)
 text(bx1+0.025,by1-0.05,as.character(rev(ten2ninety)))
 par(srt=0)
 text(0.5,-0.14,"percent sand")
 arrows(0.6,-0.1,0.38,-0.1,length=0.15)
 if(show.grid) {
  par(fg=col.grid)
  segments(bx2,by2,lx1,ly1,lty=3)
  segments(lx1,ly1,rx1,ry1,lty=3)
  segments(rev(rx1),rev(ry1),bx1,by1,lty=3)
 }
 if(show.lines) {
  par(fg=col.lines)
  # boundary of clay with extensions
  x1<-c(0.275,0.355,0.6)
  x2<-c(0.42,0.8,0.7)
  y1<-c(0.55*sin60,0.4*sin60,0.4*sin60)
  y2<-c(0.28*sin60,0.4*sin60,0.6*sin60)
  segments(x1,y1,x2,y2)
  # lower bound of clay loam & silty divider
  x1<-c(0.42,0.66)
  x2<-c(0.86,0.6)
  y1<-c(0.28*sin60,0.28*sin60)
  y2<-c(0.28*sin60,0.4*sin60)
  segments(x1,y1,x2,y2)
  x1<-c(0.175,0.1,0.38)
  x2<-c(0.383,0.38,0.42)
  y1<-c(0.35*sin60,0.2*sin60,0.2*sin60)
  y2<-c(0.35*sin60,0.2*sin60,0.28*sin60)
  segments(x1,y1,x2,y2)
  # sand corner
  x1<-c(0.05,0.075)
  x2<-c(0.12,0.3)
  y1<-c(0.1*sin60,0.15*sin60)
  y2<-c(0,0)
  segments(x1,y1,x2,y2)
  x1<-c(0.38,0.44,0.5,0.8,0.86)
  x2<-c(0.44,0.54,0.64,0.86,0.94)
  y1<-c(0.2*sin60,0.078*sin60,0,0,0.12*sin60)
  y2<-c(0.078*sin60,0.078*sin60,0.28*sin60,0.12*sin60,0.12*sin60)
  segments(x1,y1,x2,y2)
 }
 if(show.names) {
  par(fg=col.names)
  xpos<-c(0.5,0.7,0.7,0.73,0.73,0.5,0.275,0.275,0.27,0.27,0.25,0.127,
   0.155,0.055,0.49,0.72,0.9)
  ypos<-c(0.57,0.49*sin60,0.44*sin60,0.36*sin60,0.32*sin60,
   0.35*sin60,0.43*sin60,0.39*sin60,0.3*sin60,0.26*sin60,
   0.13*sin60,0.072*sin60,0.032*sin60,0.021,0.18*sin60,
   0.15*sin60,0.06*sin60)
  snames<-c("clay","silty","clay","silty clay","loam","clay loam",
   "sandy","clay","sandy clay","loam","sandy loam","loamy","sand",
   "sand","loam","silt loam","silt")
  boxed.labels(xpos,ypos,snames,border=FALSE,col=bg.names,xpad=0.5)
 }
 # finally draw the triangle and ticks
 x1<-c(0,0,0.5)
 x2<-c(1,0.5,1)
 y1<-c(0,0,sin60)
 y2<-c(0,sin60,0)
 par(fg=col.axis)
 segments(x1,y1,x2,y2)
 # bottom ticks
 segments(bx1,by1,bx2,by2)
 # left ticks
 segments(lx1,ly1,lx2,ly2)
 # right ticks
 segments(rx1,ry1,rx2,ry2)
 if(nchar(main)) {
  par(cex=1.5)
  text(0.5,1,main)
  par(cex=1)
 }
 par(xpd=FALSE,fg=currentfg)
 if(!is.null(soiltexture)) soilpoints<-show.soil.texture(soiltexture,...)
 else soilpoints<-NULL
 invisible(soilpoints)
}
