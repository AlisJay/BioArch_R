#pp basics
#basic functions to use with paleopath data
#Tablize###############################################################
PPTablize<-function(x,Ids,warn="skipped, names and data different lengths (Check Commas)"){
  tablize<-function(x){
    x<-strsplit(x,split=":")[[1]]
    names<-strsplit(x[1],split=",")[[1]]
    data<-strsplit(x[2],split=",")[[1]]
    if(length(names)!=length(data)){table<-"Skip"
    }else{
    table<-data.frame(names,data)
    table$names<-as.character(table$names)}
    table}
  A<-0
  for(i in 1:length(x)){
    if(is.na(x[i])){x[i]<-"NA:NA"}
    Table1<-tablize(x[i])
    if(is.data.frame(Table1)){
      names(Table1)<-c("names",Ids[i])
      A<-A+1
      if(A>1){Table<-merge.data.frame(Table,Table1,by="names",all.x=TRUE,all.y=TRUE)}else{Table<-Table1}
      }else{warning(paste(Ids[i],warn))}
  }
  Table[Table$names!="NA",]
}
#Summary############################################################

PPSummary<-function(PP){}