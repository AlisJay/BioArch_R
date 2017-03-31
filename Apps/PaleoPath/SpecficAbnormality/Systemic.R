SY_01_UI<-tagList(fixedPage(h3("ID:SY_01"),
                    h4("Description:Dwarfism")))
SY_01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SY_01",Type="Complex",Des="Dwarfism",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

SY_02_UI<-tagList(fixedPage(h3("ID:SY_02"),
                            h4("Description:Gigantism")))
SY_02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SY_02",Type="Complex",Des="Gigantism",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
##################################################################################################
library(data.table)
Systemic_DT<-data.table(ref=c("SY_01","SY_02"),
                    uioptions=c(SY_01_UI,SY_02_UI),
                    RecordCreator=c(SY_01_RC,SY_02_RC))