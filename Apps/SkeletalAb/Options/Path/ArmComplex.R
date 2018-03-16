#Custom#####
A_CC_UI<-tagList(h3("ID:A_CC"),h4("Description:Custom Complex Abnormality"))
A_CC_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
Table
}
#osteophyte, eburnation ect distal humerus#######
A_C01_UI<-tagList(fixedPage(h3("ID:A_C01"),h4("Description:Osteophytes/mushrooming/euburnation/pitting/distoration on distal humerus")))
A_C01_RC<-function(input=input){   
  Table<-data.frame(Des1="osteophytes/mushrooming/eburnation/pitting/distoration in distal humerus",Size=NA,Nature=NA,Heal=NA)
  Table }

#pitting and projection on radial tuberocity######
A_C02_UI<-tagList(h3("ID:A_C02"),h4("Description:Pitting and projection on radial tuberocity"))
A_C02_RC<-function(input=input){   
  Table<-data.frame(Des1="Pitting and projection on radail tuberocity",Size=NA,Nature=NA,Heal=NA)
  Table }

#Osteophytes ect radius########
A_C03_UI<-tagList(h3("ID:A_C03"),h4("Description:Osteophytes/eburnation/pitting on radial head"))
A_C03_RC<-function(input=input){   
  Table<-data.frame(Des1="Osteophytes/eburnation/pitting on radial head",Size=NA,Nature=NA,Heal=NA)
  Table }

#osteophytes ect ulna#######
A_C04_UI<-tagList(h3("ID:A_C04"),h4("Description:Osteophytes/pitting/eburnation on proximal ulna"))
A_C04_RC<-function(input=input){   
  Table<-data.frame(Des1="Osteophytes/putting/eburnation on proximal ulna",Size=NA,Nature=NA,Heal=NA)
  Table }

#osteophytes ect humeral head##########
A_C05_UI<-tagList(h3("ID:A_C05"),h4("Description:Eburnation of superior humeral head"))
A_C05_RC<-function(input=input){   
  Table<-data.frame(Des1="Eburnation of superior humeral head",Size=NA,Nature=NA,Heal=NA)
  Table }
