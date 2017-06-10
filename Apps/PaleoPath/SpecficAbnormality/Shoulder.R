#Separated acromion##############################################################################################
SH_S01_UI<-tagList(fixedPage(h3("ID:SH_S01"),
                            h4("Description:Separated acromion")))
SH_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_S01",Type="Shape",Des="Separated acromion",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Depression on ant clavicle##############################################################################################
SH_L01_UI<-tagList(fixedPage(h3("ID:SH_L01"),
                             h4("Description:Depression on anterior clavicle")))
SH_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_L01",Type="Loss",Des="Depression on anterior clavicle",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Raise growth on superiolateral clavicle##############################################################################################
SH_F01_UI<-tagList(fixedPage(h3("ID:SH_F01"),
                             h4("Description:Raised area on superiolateral clavicle")))
SH_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_F01",Type="Formation",Des="Raised area on superiolateral clavicle",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Circular plateau on coracoid##############################################################################################
SH_F02_UI<-tagList(fixedPage(h3("ID:SH_F02"),
                             h4("Description:Circular plateau on coracoid process")))
SH_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_F02",Type="Formation",Des="Circular plateau on coracoid process",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#raised margins and glenoid pitting##############################################################################################
SH_C01_UI<-tagList(fixedPage(h3("ID:SH_C01"),
                             h4("Description:Raised margins and pitting of glenoid")))
SH_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_C01",Type="Complex",Des="Raised nargins and pitting of glenoid",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Extended glenoid##############################################################################################
SH_C02_UI<-tagList(fixedPage(h3("ID:SH_C02"),
                             h4("Description:Extension of glenoid")))
SH_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_C02",Type="Complex",Des="Extension of glenoid",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#pitting around rotator cuff##############################################################################################
SH_C03_UI<-tagList(fixedPage(h3("ID:SH_C03"),
                             h4("Description:Pitting and new bone around rotator cuff insertion")))
SH_C03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_C03",Type="Complex",Des="Pitting and new bone around rotator cuff insertion",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Eburnation on acromion##############################################################################################
SH_C04_UI<-tagList(fixedPage(h3("ID:SH_C04"),
                             h4("Description:Eburnation on acromion")))
SH_C04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SH_C04",Type="Complex",Des="Eburnation on acromion",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
##################################################################################################
library(data.table)
Shoulder_DT<-data.table(ref=c("SH_S01","SH_L01","SH_F01","SH_F02","SH_C01","SH_C02","SH_C03","SH_C04"),
                    uioptions=c(SH_S01_UI,SH_L01_UI,SH_F01_UI,SH_F02_UI,SH_C01_UI,SH_C02_UI,SH_C03_UI,SH_C04_UI),
                    RecordCreator=c(SH_S01_RC,SH_L01_RC,SH_F01_RC,SH_F02_RC,SH_C01_RC,SH_C02_RC,SH_C03_RC,SH_C04_RC))