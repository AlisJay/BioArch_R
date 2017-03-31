A_S01_UI<-tagList(fixedPage(h3("ID:A_S01"),
                            h4("Description:Suppernumery bone")))
A_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_S01",Type="Shape",Des="Supernummery bone",Loc="Arm:Snum:NA",Feat="NA:NA",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_L01_UI<-tagList(fixedPage(h3("ID:A_L01"),
                    h4("Description:Porous groove on humeral shaft")))
A_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_L01",Type="Loss",Des="Porous groove on humeral shaft",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_L02_UI<-tagList(fixedPage(h3("ID:A_L02"),
                            h4("Description:Perforation in olecranon fossa")))
A_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_L02",Type="Loss",Des="Perforation in olecranon fossa",Loc=NA,Feat="olecranon fos",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_L03_UI<-tagList(fixedPage(h3("ID:A_L03"),
                            h4("Description:Erosion of styloid")))
A_L03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_L03",Type="Loss",Des="Erosion of styloid process",Loc=NA,Feat="styloid pro",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_F01_UI<-tagList(fixedPage(h3("ID:A_F01"),
                            h4("Description:Bone spur superior to medial epicondyle")))
A_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_F01",Type="Formation",Des="Bone spur superior to medial epicondyle",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_F02_UI<-tagList(fixedPage(h3("ID:A_F02"),
                            h4("Description:Thickened line(s) across diaphysis/metaphysis")))
A_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_F02",Type="Formation",Des="Thickened line(s) across diaphysis/metaphysis",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_F03_UI<-tagList(fixedPage(h3("ID:A_F03"),
                            h4("Description:New bone growth on outer cortex")))
A_F03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_F03",Type="Formation",Des="New bone growth on outer cortex",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_F04_UI<-tagList(fixedPage(h3("ID:A_F04"),
                            h4("Description:Smooth bone growth attached to outer cortex")))
A_F04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_F04",Type="Formation",Des="Smooth bone growth attached to outer cortex",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_C01_UI<-tagList(fixedPage(h3("ID:A_C01"),
                            h4("Description:Osteophytes/mushrooming/euburnation/pitting/distoration on distal humerus")))
A_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_C01",Type="Complex",Des="osteophytes/mushrooming/eburnation/pitting/distoration in distal humerus",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_C02_UI<-tagList(fixedPage(h3("ID:A_C02"),
                            h4("Description:Pitting and projection on radial tuberocity")))
A_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_C02",Type="Complex",Des="Pitting and projection on radail tuberocity",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_C03_UI<-tagList(fixedPage(h3("ID:A_C03"),
                            h4("Description:Osteophytes/eburnation/pitting on radial head")))
A_C03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_C03",Type="Complex",Des="Osteophytes/eburnation/pitting on radial head",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_C04_UI<-tagList(fixedPage(h3("ID:A_C04"),
                            h4("Description:Osteophytes/pitting/eburnation on proximal ulna")))
A_C04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_C04",Type="Complex",Des="Osteophytes/putting/eburnation on proximal ulna",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

A_C05_UI<-tagList(fixedPage(h3("ID:A_C05"),
                            h4("Description:Eburnation of superior humeral head")))
A_C05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="A_C05",Type="Complex",Des="Eburnation of superior humeral head",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

##################################################################################################
library(data.table)
Arm_DT<-data.table(ref=c("A_S01","A_L01","A_L02","A_L03","A_F01","A_F02","A_F03","A_F04","A_C01","A_C02","A_C03","A_C04","A_C05"),
                    uioptions=c(A_S01_UI,A_L01_UI,A_L02_UI,A_L03_UI,A_F01_UI,A_F02_UI,A_F03_UI,A_F04_UI,A_C01_UI,A_C02_UI,A_C03_UI,A_C04_UI,A_C05_UI),
                    RecordCreator=c(A_S01_RC,A_L01_RC,A_L02_RC,A_L03_RC,A_F01_RC,A_F02_RC,A_F03_RC,A_F04_RC,A_C01_RC,A_C02_RC,A_C03_RC,A_C04_RC,A_C05_RC))