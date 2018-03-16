#CorticalFlaking#### 
CorticalFlaking_UI<-tagList(h3("Cortical Flaking Description"))
CorticalFlaking_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#Burning####
Burning_UI<-tagList(h3("Burning Description"))
Burning_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#Discolouration####
Discolouration_UI<-tagList(h3("Discolouration Description"))
Discolouration_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#IncreasedMass####
IncreasedMass_UI<-tagList(h3("Increased Mass Description"))
IncreasedMass_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#ReducedMass####
ReducedMass_UI<-tagList(h3("Reduced Mass Description"))
ReducedMass_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#Shrinkage####
Shrinkage_UI<-tagList(h3("Shrinkage Description"))
Shrinkage_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#MineralDeposition####
MineralDeposition_UI<-tagList(h3("Mineral Deposit(s) Description"))
MineralDeposition_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#Inclusion####
Inclusion_UI<-tagList(h3("Inclusion Description"))
Inclusion_RC<-function(input){
  Table<-data.frame(NatureNames=NA,NatureValues=NA,Des1=NA)
}
#Other#### 
Other_UI<-tagList(h3("Other Description"),textInput("OtherTaph","Describe Other Taphonmic Abnormality",value=NA))
Other_RC<-function(input){
  Table<-data.frame(NatureNames="Other-Description",NatureValues=input$OtherTaph,Des1="Other Taphonomic Abnormality")
}