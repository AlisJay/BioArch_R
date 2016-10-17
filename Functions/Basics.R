#' Input Bioarchaeology data 
#'
#' @param infoType type of info to enter current options= Profile,Inventory,Metrics,Library,JInventory,JMetrics,Dental
#'
#' @return Opens the relevant shiny app
#' @export
#'
#' @examples InputBioArch("Profile)
InputBioArch<-function(infoType){
  library("shiny")
  InfoTypes<-c("Inventory","Profile","Library","JInventory","Metrics","JMetrics","Dental")
  if(!(infoType %in% InfoTypes)){stop("invalid info type! valid options are ",paste(InfoTypes,collapse=","))}
  if(infoType=="Profile"){shiny::runApp('Apps/BioProfile2')}
  if(infoType=="Inventory"){shiny::runApp('Apps/BAInventory')}
  if(infoType=="Library"){shiny::runApp('Apps/Library')}
  if(infoType=="Metrics"){shiny::runApp('Apps/Metrics')}
  if(infoType=="JInventory"){shiny::runApp('Apps/JInventory')}
  if(infoType=="JMetrics"){shiny::runApp('Apps/JMetrics')}
  if(infoType=="Dental"){shiny::runApp('Apps/Dental')}
}

#' read in BioArch files
#'
#' @param file filepath of bioarch file this supports .BP.txt, .OA.BP.txt and .SI.txt files
#'
#' @return a list containing Head and Table
#' @export
#'
#' @examples Bp<-ReadBioArch("data/EG001.BP.txt")
ReadBioArch<-function(file){
  if(!(file.exists(file))){
    stop("File ",file," Does not exist please check the Population ID")
  }else{
    f<-readLines(file)
    head<-grep("^#",f,value=TRUE)
    table<-read.table(file,skip=length(head),header=TRUE,row.names=NULL,stringsAsFactors=FALSE)
    list(Head=head,Table=table)
}}
  