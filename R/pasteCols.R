pasteCols<-function(x) {
 pastestring<-paste("list(",paste("x","[",1:dim(x)[1],",]",
  sep="",collapse=","),")",sep="")
 return(do.call(paste,c(eval(parse(text = pastestring)),sep="")))
}
