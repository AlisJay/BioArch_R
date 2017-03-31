#pp basics
#basic functions to use with paleopath data
#Tablize###############################################################
PPTablize<-function(x,Ids){
  tablize<-function(x){
    x<-strsplit(x,split=":")[[1]]
    names<-strsplit(x[1],split=",")[[1]]
    data<-strsplit(x[2],split=",")[[1]]
    table<-data.frame(names,data)
    table$names<-as.character(table$names)
    table}
  for(i in 1:length(x)){
    Table1<-tablize(x[i])
    names(Table1)<-c("names",Ids[i])
    if(i>1){Table<-merge.data.frame(Table,Table1,by="names",all.x=TRUE,all.y=TRUE)}else{Table<-Table1}
  }
  Table
}
#Summary############################################################

PPSummary<-function(PP){}