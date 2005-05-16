corner.loc<-function(x=-1,y=1,xoff=0.05,yoff=0.05) {
 par.usr <- par("usr")
 xrange <- diff(par.usr[1:2])
 yrange <- diff(par.usr[3:4])
 if (x == -1) xpos <- par.usr[1] + xoff * xrange
 else if (x == 1) xpos <- par.usr[2] - xoff * xrange
 if (y == -1) ypos <- par.usr[3] + yoff * yrange
 else if (y == 1) ypos <- par.usr[4] - yoff * yrange
 if (par("xlog")) xpos <- 10^xpos
 if (par("ylog")) ypos <- 10^ypos
 list(x = xpos, y = ypos)
}

corner.label<-function(label=NULL,x=-1,y=1,xoff=0.05,yoff=0.05,...) {
 corner<-corner.loc(x,y,xoff,yoff)
 text(corner$x,corner$y,label,...)
}
