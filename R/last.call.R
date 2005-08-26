last.call<-function(look.for="plot",look.back=10) {
 # grab the command history
 savehistory("snapshot.hst")
 tail.call<-paste("tail -n",look.back,"snapshot.hst",collapse=" ")
 last.calls<-system(tail.call,T)
 system("rm snapshot.hst")
 begin<-look.back
 found<-0
 lpc.length<-nchar(look.for)
 # this finds the last call line that matches the string
 while(begin >= 1 && !found) {
  # strip any leading spaces
  this.call<-gsub("^ .","",last.calls[begin])
  if(is.na(pmatch(look.for,this.call))) begin<-begin-1
  else found<-1
 }
 # check that a matching call was found
 if(found) return(last.calls[begin:look.back])
 else {
  cat("Can't find a call containing",look.for,"!\n")
  return(NULL)
 }
}
