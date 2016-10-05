InputBioArch<-function(infoType){
  library("shiny")
  InfoTypes<-c("Inventory","Profile","Library")
  if(!(infoType %in% InfoTypes)){stop("invalid info type! valid options are ",paste(InfoTypes,collapse=","))}
  if(infoType=="Profile"){shiny::runApp('BioProfile2')}
  if(infoType=="Inventory"){shiny::runApp('BAInventory')}
  if(infoType=="Library"){shiny::runApp('Library')}
}

ReadBioArch<-function(file){
  if(!(file.exists(file))){
    stop("File ",file," Does not exist please check the Population ID")
  }else{
    f<-readLines(file)
    head<-grep("^#",f,value=TRUE)
    table<-read.table(file,skip=length(head),header=TRUE,row.names=NULL,stringsAsFactors=FALSE)
    list(Head=head,Table=table)
}}
  