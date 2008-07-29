#Written by Mike Cheetham, 2008
#micheetham@googlemail.com
# minor changes by Jim Lemon 02/06/08
#-------------------------------------------------------------------------------
# convert.meq
# converts mg/l to meq/l for common ions
# more can be added - B, P (various), NO3, Fe (various), Mn, Si02, Al
# Arguments:
# mgpl - concentrations in mg/l.
# parameter - If a data frame must have column names matching names in
# "molecule", set parameter=c('ca','mg','na','k','hco3','co3','so4','cl')
# Value: The initial values in "mgpl" converted to milliequivalents per liter

convert.meq<-function(mgpl,parameter) {
   if (missing(parameter)) parameter<-names(mgpl)
  #data frame containing molecular weights and valancy for molecules
  molecule<-data.frame(name=c('ca','mg','na','k','hco3','co3','so4','cl'),
                    weight=c(40.077,24.305,22.989,39.0983,61.0159,60.008,96.062,35.4527),
                    valence=c(2,2,1,1,1,2,2,1))

  #calculate equivalent weights by matching molecules to mgpl and making matrix
  #of equivalent weights of the same shape and size as mgpl
  m<-match(parameter, molecule$name)
  repetition<-length(c(mgpl, recursive=TRUE))/length(parameter)
  eq.weights.multiplier<-matrix(rep(molecule$valence[m]/molecule$weight[m],
                              repetition), ncol=NROW(parameter), byrow=TRUE)

  #calculate output, tidy and return
  meq<-data.frame(mgpl*eq.weights.multiplier)
  names(meq)<-parameter
  return(meq)  
}
#-------------------------------------------------------------------------------
# p.ions
# produce %anions/cations for piper
# note assumes that ion balance is acceptable - not checked
# note that it needs values in meq/l
# Arguments:
# meq - data frame/matrix or vector with length=8 with columns of meq
# typically = c('ca','mg','na','k','hco3','co3','so4','cl')
# name order of columns/rows of meq
# short.output - if TRUE returns a 4 column data frame required
# for input to piper diagram

p.ions<-function(meq,short.output=TRUE) {
  #checks on the form of the input data
  if (class(meq)!="data.frame") {
   warning("meq is not a data frame: coercing to data frame")
   meq<-data.frame(matrix(meq, ncol=8))
  }
  if(length(c(meq, recursive=TRUE))%%8!=0)
   stop("Not all ions present - note that bicarb and carbonate ions are required")
  
  sum.anion<-apply(data.frame(meq$mg, meq$ca, meq$na, meq$k), MARGIN=1, FUN=sum)
  mg<-meq$mg/sum.anion
  ca<-meq$ca/sum.anion
  naplusk<-(meq$na+meq$k)/sum.anion
  
  sum.cation<-apply(data.frame(meq$so4, meq$co3, meq$hco3, meq$cl),
   MARGIN=1, FUN=sum)
  so4<-meq$so4/sum.cation
  cl<-meq$cl/sum.cation
  hco3plusco3<-(meq$hco3+meq$co3)/sum.cation
  
  if(short.output) {
    return(data.frame(mg=mg, ca=ca, so4=so4, cl=cl))
  } else {
    return(data.frame(mg=mg, ca=ca, so4=so4, cl=cl, hco3plusco3=hco3plusco3,
     naplusk=naplusk))
  }
}

#-------------------------------------------------------------------------------
piper.set<-function(plot.sep=0.15){
  sin60<-sin(pi/3)
  cos60<-cos(pi/3)
  return(list(mg0.x=-(plot.sep*cos60+0.5), mg0.y=-(plot.sep*sin60+sin60),
   cl0.x=(plot.sep*cos60+0.5),cl0.y=-(plot.sep*sin60+sin60)))
}

#-------------------------------------------------------------------------------
#Piper plot data
piper.points<-function(ca, mg, so4, cl,
 ions=data.frame(ca=ca, mg=mg, so4=so4, cl=cl),
 sites,
 ppm=TRUE,chull=FALSE,
 pch=3,main="",
 cex.pch=1,col=NA,pch.lwd=0.7, plot.sep=0.15) {
 
 # some checks on the input data,
 # and if site labels are provided generate a list of colors
 if(missing(ions) && (missing(ca) || missing(mg) || missing(so4) || missing(cl)))
  stop("Must have some data...")
 if(ppm) ions<-p.ions(convert.meq(ions))
 if(any(apply(cbind(ions$ca, ions$mg), 1, sum) > 1 &&
  apply(cbind(ions$so4, ions$cl), 1, sum) > 1))
   stop("Sum of ion proportions greater than 1")
 if(missing(sites)) sites<-1:NROW(ions)
 if(isTRUE(is.na(col))) {
  u<-unique(sites)
  col<-match(sites, u)
 }
 #plot data
 coords<-piper.coords(ions)
 if(!chull) {
  points(coords$xfinal,coords$yfinal,pch=pch,cex=cex.pch,col=col,lwd=pch.lwd)
  points(coords$xan,coords$yan,pch=pch,cex=cex.pch,col=col,lwd=pch.lwd)
  points(coords$xcat,coords$ycat,pch=pch,cex=cex.pch,col=col,lwd=pch.lwd)
 }
 else {
  col<-1:NROW(u)
  for(i in 1:NROW(u)) {
   pick<-sites==u[i]
   cathull<-chull(coords$xcat[pick],coords$ycat[pick])
   anhull<-chull(coords$xan[pick],coords$yan[pick])
   finalhull<-chull(coords$xfinal[pick],coords$yfinal[pick])
   polygon(coords$xcat[pick][cathull], coords$ycat[pick][cathull],
    border=col[i])
   polygon(coords$xan[pick][anhull], coords$yan[pick][anhull],
    border=col[i])
   polygon(coords$xfinal[pick][finalhull],coords$yfinal[pick][finalhull],
    border=col[i])
  }
 }
 return(if(isTRUE(is.na(col))) {
         list(col=NA,coords=coords)
	}
        else {
	 list(col=data.frame(sites=u,col=rep(palette(),length.out=NROW(u))),
	  coords=coords)
	}
 )
}
#-------------------------------------------------------------------------------
# piper.coords
# Arguments:
# x - data frame with proportion of ca, mg, so4, cl
#  and names=c("ca","mg","so4","cl")
# plot.sep - separation between anion/cation plots and merged plot

piper.coords<-function(ions, plot.sep=0.15) {
 sin60<-sin(pi/3)
 cos60<-cos(pi/3)
 caplusmg<-ions$ca+ions$mg
 naplusk<-1-caplusmg
 so4pluscl<-ions$so4+ions$cl

 xfinal<-so4pluscl*cos60+naplusk*cos60
 yfinal<-so4pluscl*sin60-naplusk*sin60

 xan<-piper.set(plot.sep)$mg0.x+(1-ions$ca)-ions$mg*cos60
 yan<-piper.set(plot.sep)$mg0.y+ions$mg*sin60

 xcat<-piper.set(plot.sep)$cl0.x+ions$cl+ions$so4*cos60
 ycat<-piper.set(plot.sep)$cl0.y+ions$so4*sin60

 return(data.frame(xfinal=xfinal, yfinal=yfinal, xan=xan,
  yan=yan, xcat=xcat, ycat=ycat))

}
#-------------------------------------------------------------------------------

piper.text<-function(ca, mg, so4, cl,
                    ions=data.frame(ca=ca, mg=mg, so4=so4, cl=cl),
                    ptext,
                    ppm=TRUE,
                    cex=1,col=1, plot.sep=0.15, pos=4) {
 
 if(missing(ions) && (missing(ca) || missing(mg) ||
    missing(so4) || missing(cl)))
     stop("Must have some data...")
 if(ppm) ions<-p.ions(convert.meq(ions))
 coords<-piper.coords(ions)
 text(coords$xan, coords$yan, ptext, cex=cex, col=col, pos=pos)
 text(coords$xcat, coords$ycat, ptext, cex=cex, col=col, pos=pos)
 text(coords$xfinal, coords$yfinal, ptext, cex=cex, col=col, pos=pos)
 invisible()
}

#-------------------------------------------------------------------------------
#Piper diagram
piper.diagram<-function(ca, mg, so4, cl,
                    ions=data.frame(ca=ca, mg=mg, so4=so4, cl=cl),
		    sites=1:NROW(ions),
                    new=FALSE,ppm=TRUE,chull=FALSE,tcsep=0.2,
                    pch=3,main="",
                    cex.lab=0.7,cex.tck=0.6,cex.pch=1,grid=TRUE,
                    col=NA, ticklength=0.03,
                    lwd.frame=1, pch.lwd=0.7,lwd.grid=lwd.frame,
                    col.box="black", col.tck=col.box,
                    col.grid="grey") {
    
 if(missing(ions) && (missing(ca) || missing(mg) || missing(so4) || missing(cl)))
  stop("Must have some data...")
 sin60<-sin(pi/3)
 cos60<-cos(pi/3)
 off.lab<-0.07  #offset of labels
 plot.sep<-0.15 #seperation of plots (anions, cations, and combined)
 off.tic<-0.02  #offset of tickmark labels
 if(length(tcsep) == 1) seq.tick<-seq(0,1,tcsep)
 else seq.tick<-tcsep #sequence of tick mark labels
 ticval<-as.character(round(seq.tick,2)) #tickmark labels    
 #set up the plotting region
 par(new=new)
 plot(0.5, type = "n", axes = FALSE, xlim = c(-0.6, 1.6), ylim = c(-1.1, 1.1),
  main = main, xlab = "", ylab = "", asp=1, xaxs="i", yaxs="i")
 #draw rhombus
 segments(c(0, 0.5), c(0, -sin60), c(0.5, 1), c(-sin60, 0),
  lwd=lwd.frame, col=col.box)
 segments(c(0, 0.5), c(0, sin60), c(0.5, 1), c(sin60, 0),
  lwd=lwd.frame, col=col.box)
 #draw triangles
 #verticies of triangles
 x1<-c(0, 0, 0.5)
 x2<-c(1, 0.5, 1)
 y1<-c(0, 0, sin60)
 y2<-c(0, sin60, 0)
 #anion and cation triangle origins
 segments(x1+piper.set(plot.sep)$mg0.x, y1+piper.set(plot.sep)$mg0.y,
  x2+piper.set(plot.sep)$mg0.x, y2+piper.set(plot.sep)$mg0.y, cex=0.5,
  lwd=lwd.frame, col=col.box)
 segments(x1+piper.set(plot.sep)$cl0.x, y1+piper.set(plot.sep)$cl0.y,
  x2+piper.set(plot.sep)$cl0.x, y2+piper.set(plot.sep)$cl0.y, lwd=lwd.frame,
   col=col.box)
 #draw tickmarks
 #so4+cl
 x1.sop<-seq.tick*cos60
 y1.sop<-seq.tick*sin60
 x2.sop<-x1.sop-ticklength*cos60
 y2.sop<-y1.sop+ticklength*sin60
 segments(x1.sop,y1.sop, x2.sop, y2.sop, lwd=lwd.frame, col=col.tck)
 #ca+mg
 x1.cap<-1-seq.tick*cos60
 y1.cap<-seq.tick*sin60
 x2.cap<-x1.cap+ticklength*cos60
 y2.cap<-y1.cap+ticklength*sin60
 segments(x1.cap,y1.cap, x2.cap, y2.cap, lwd=lwd.frame, col=col.tck)
 #na+k
 x1.nap<-seq.tick*cos60
 y1.nap<-seq.tick*-sin60
 x2.nap<-x1.nap-ticklength*cos60
 y2.nap<-y1.nap-ticklength*sin60
 segments(x1.nap,y1.nap, x2.nap, y2.nap, lwd=lwd.frame, col=col.tck)
 #co3+hco3
 x1.carb<-1-seq.tick*cos60
 y1.carb<-seq.tick*-sin60
 x2.carb<-x1.carb+ticklength*cos60
 y2.carb<-y1.carb-ticklength*sin60
 segments(x1.carb,y1.carb, x2.carb, y2.carb, lwd=lwd.frame, col=col.tck)
 #mg
 x1.mg<-piper.set(plot.sep)$mg0.x+seq.tick*cos60
 y1.mg<-seq.tick*sin60+piper.set(plot.sep)$mg0.y
 x2.mg<-x1.mg-ticklength
 y2.mg<-y1.mg
 segments(x1.mg,y1.mg, x2.mg, y2.mg, lwd=lwd.frame, col=col.tck)
 #so4
 x1.so<-piper.set(plot.sep)$cl0.x+1-seq.tick*cos60
 y1.so<-seq.tick*sin60+piper.set(plot.sep)$cl0.y
 x2.so<-x1.so+ticklength
 y2.so<-y1.so
 segments(x1.so,y1.so, x2.so, y2.so, lwd=lwd.frame, col=col.tck)
 #ca
 x1.ca<-piper.set(plot.sep)$mg0.x+seq.tick
 y1.ca<-rep(piper.set(plot.sep)$mg0.y, NROW(x1.ca))    
 x2.ca<-x1.ca+ticklength*cos60
 y2.ca<-y1.ca-ticklength*sin60
 segments(x1.ca,y1.ca, x2.ca, y2.ca, lwd=lwd.frame, col=col.tck)
 #cl
 x1.cl<-piper.set(plot.sep)$cl0.x+seq.tick
 y1.cl<-rep(piper.set(plot.sep)$cl0.y, NROW(x1.cl))    
 x2.cl<-x1.cl-ticklength*cos60
 y2.cl<-y1.cl-ticklength*sin60
 segments(x1.cl,y1.cl, x2.cl, y2.cl, lwd=lwd.frame, col=col.tck)
 #na+k lower
 x1.napl<-piper.set(plot.sep)$mg0.x+0.5+rev(seq.tick)*cos60
 y1.napl<-seq.tick*sin60+piper.set(plot.sep)$mg0.y
 x2.napl<-x1.napl+ticklength*cos60
 y2.napl<-y1.napl+ticklength*sin60
 segments(x1.napl,y1.napl, x2.napl, y2.napl, lwd=lwd.frame, col=col.tck)
 #co3+hco3 lower
 x1.carbl<-piper.set(plot.sep)$cl0.x+0.5-rev(seq.tick)*cos60
 y1.carbl<-seq.tick*sin60+piper.set(plot.sep)$cl0.y
 x2.carbl<-x1.carbl-ticklength*cos60
 y2.carbl<-y1.carbl+ticklength*sin60
 segments(x1.carbl,y1.carbl, x2.carbl, y2.carbl, lwd=lwd.frame, col=col.tck)
 if(grid) {
  o.r<-(NROW(seq.tick)-1):2
  o<-2:(NROW(seq.tick)-1)    
  #rhombus
  segments(x1.nap[o], y1.nap[o], x1.cap[o.r], y1.cap[o.r],
   col=col.grid, lty='dashed', lwd=lwd.grid)
  segments(x1.sop[o], y1.sop[o], x1.carb[o.r], y1.carb[o.r],
   col=col.grid, lty='dashed', lwd=lwd.grid)
  #anions
  segments(x1.mg[o], y1.mg[o], x1.napl[o], y1.napl[o],
   col=col.grid, lty='dashed', lwd=lwd.grid)
  segments(x1.ca[o.r], y1.ca[o.r], x1.mg[o.r], y1.mg[o.r],
   col=col.grid, lty='dashed', lwd=lwd.grid)
  segments(x1.napl[o.r], y1.napl[o.r], x1.ca[o], y1.ca[o],
   col=col.grid, lty='dashed', lwd=lwd.grid)
  #cations
  segments(x1.cl[o], y1.cl[o], x1.carbl[o], y1.carbl[o],
   col=col.grid, lty='dashed', lwd=lwd.grid)
  segments(x1.cl[o.r], y1.cl[o.r], x1.so[o], y1.so[o],
   col=col.grid, lty='dashed', lwd=lwd.grid)
  segments(x1.so[o.r], y1.so[o.r], x1.carbl[o.r], y1.carbl[o.r],
   col=col.grid, lty='dashed', lwd=lwd.grid)
 }
 #tickmark values
 text(x2.mg-off.tic, y2.mg, ticval, cex=cex.tck, pos=2, offset=0)
 text(x2.so+off.tic, y2.so, ticval, cex=cex.tck, pos=4, offset=0)
 text(x2.ca+off.tic*cos60, y2.ca-off.tic*sin60, rev(ticval),
  cex=cex.tck, pos=4, offset=0)
 text(x2.cl-off.tic*cos60, y2.cl-off.tic*sin60, ticval,
  cex=cex.tck, pos=2, offset=0)
 text(x2.carb+(rev(x2.carbl)-x2.carb)/2, y2.carb+(rev(y2.carbl)-y2.carb)/2,
  ticval, cex=cex.tck, offset=0)
 text(x2.nap+(rev(x2.napl)-x2.nap)/2, y2.nap+(rev(y2.napl)-y2.nap)/2,
  ticval, cex=cex.tck, offset=0)
 text(x2.sop-off.tic*cos60, y2.sop+off.tic*sin60, ticval, cex=cex.tck,
  pos=2, offset=0)
 text(x2.cap+off.tic*cos60, y2.cap+off.tic*sin60, ticval, cex=cex.tck,
  pos=4, offset=0)
 #labels
 text(x1.napl+(x1.nap-x1.napl)/2, y1.napl+(y1.nap-y1.napl)/2,
  'Na+\nK', cex=cex.lab)
 text(x1.carbl+(x1.carb-x1.carbl)/2, y1.carbl+(y1.carb-y1.carbl)/2,
  'CO3+\nHCO3', cex=cex.lab)
 text(min(x1.sop)+(max(x1.sop)-min(x1.sop))/2-off.lab*sin60,
  min(y1.sop)+(max(y1.sop)-min(y1.sop))/2+off.lab*cos60, 'SO4+\nCl',
  cex=cex.lab)
 text(min(x1.cap)+(max(x1.cap)-min(x1.cap))/2+off.lab*sin60,
  min(y1.cap)+(max(y1.cap)-min(y1.cap))/2+off.lab*cos60, 'Ca+\nMg',
  cex=cex.lab)
 text(min(x1.mg)+(max(x1.mg)-min(x1.mg))/2-off.lab*sin60,
  min(y1.mg)+(max(y1.mg)-min(y1.mg))/2+off.lab*cos60, 'Mg', cex=cex.lab)
 text(min(x1.so)+(max(x1.so)-min(x1.so))/2+off.lab*sin60,
  min(y1.so)+(max(y1.so)-min(y1.so))/2+off.lab*cos60, 'SO4', cex=cex.lab)
 text(min(x1.ca)+(max(x1.ca)-min(x1.ca))/2, min(y1.ca)-off.lab,
  'Ca', cex=cex.lab) 
 text(min(x1.cl)+(max(x1.cl)-min(x1.cl))/2, min(y1.cl)-off.lab,
  'Cl', cex=cex.lab)      
 #plot data
 cols<-piper.points(ions=ions, sites=sites,ppm=ppm,pch=pch,
                    cex.pch=cex.pch,col=col,pch.lwd=pch.lwd,
		    plot.sep=plot.sep, chull=chull)
 invisible(cols)
}
