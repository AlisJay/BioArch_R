CompareBioArch<-function(data,Type,Split){
  
}

#Spliters##############
ByID<-function(x){
  data.table("ID"=x$ID,"Group"=as.factor(x$ID))
}
ByIndividual<-function(x){
  data.table("ID"=x$ID,"Group"=as.factor(x$In))
}