FindInvent<-function(data,region,abbrev,Type="PC"){
  x<-data[[region]]
  if(Type=="PC"){
    Present<-x$Present[x$Present$element %in% abbrev,]
    Complete<-x$Complete[x$Complete$element %in% abbrev,]
    out<-list("Present"=Present,"Complete"=Complete)
  }else{if(Type=="P"){
    out<-x$Present[x$Present$element %in% abbrev,]
  }else{if(Type=="C"){
    out<-x$Present[x$Present$element %in% abbrev,]
  }else{out<-"Valid types = PC P and C"}}}
  out
}