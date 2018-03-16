#Custom#####
V_CC_UI<-tagList(h3("ID:V_CC"),h4("Description:Custom Complex Abnormality"))
V_CC_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
Table
}
#Osteophytes, eburnation ect######
V_C01_UI<-tagList(h3("ID:V_C01"),h4("Description:Osteophytes/irregular margins/surface porosity/eburnation"))
V_C01_RC<-function(input=input){   
  Table<-data.frame(Des1="Osteophytes/irregular margins/surface porosity/eburnation",Size=NA,Nature=NA,Heal=NA)
  Table }

#Pitting and osteophytes######
V_C02_UI<-tagList(h3("ID:V_C02"),h4("Description:Pitting and osteophytes"))
V_C02_RC<-function(input=input){   
  Table<-data.frame(Des1="Pitting and osteophytes",Size=NA,Nature=NA,Heal=NA)
  Table }

#L5S1 abnormality######
V_C03_UI<-tagList(h3("ID:V_C03"),h4("Description:L5S1 abnormality"))
V_C03_RC<-function(input=input){   
  Table<-data.frame(Des1="L5S1 abnormality",Size=NA,Nature=NA,Heal=NA)
  Table }
