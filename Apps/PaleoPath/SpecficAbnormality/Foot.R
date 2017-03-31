F_S01_UI<-tagList(fixedPage(h3("ID:F_S01"),
                    h4("Description:Supernummery bones"),
                    column(width=4,selectInput("F_S01_1","Left",multiple=TRUE,choice=c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid"))),
                    column(width=4,selectInput("F_S01_2","Right",multiple=TRUE,choice=c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid"))),
                    column(width=4,h4("Connection with other lesions"),
                         textInput("F_S01_Link1","ID of linked lesion(s)",value="None"),
                         selectizeInput("F_S01_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
F_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S01",Type="Shape",Des="Supernummery bones",Loc="Foot:Snum:see nature",Feat="NA:NA",Size="NA:NA",Shape="NA:NA",Nature=NA,Add=NA,Link=NA)
  left<-NA;right<-NA
  if(length(input$F_S01_1)>0){left<-paste(paste(input$F_S01_1,"l",sep="_"),collapse =",")}
  if(length(input$F_S01_2)>0){right<-paste(paste(input$F_S01_2,"r",sep="_"),collapse =",")}
  Table$Nature<-paste(left,right,sep=",")
  if(input$F_S01_Link1 != "None"){Table$Link<-paste(paste(input$F_S01_Link1,collapse=","),paste(input$F_S01_Link2,collapse=","),sep=":")}
  Table }

F_S02_UI<-tagList(fixedPage(h3("ID:F_S02"),
                            h4("Description:Rotation and malformation of talus and calcaneous")))
F_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S02",Type="Shape",Des="Rotation and malformation of talus and calcaneous",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_S03_UI<-tagList(fixedPage(h3("ID:F_S03"),
                            h4("Description:Fusion of talus and calcaneous")))
F_S03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S03",Type="Shape",Des="Fusion of talus and calcaneous",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_S04_UI<-tagList(fixedPage(h3("ID:F_S04"),
                            h4("Description:Fusion of Tibia and Talus")))
F_S04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S04",Type="Shape",Des="Fusion of tibia and talus",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_S05_UI<-tagList(fixedPage(h3("ID:F_S05"),
                            h4("Description:Fusion of interphalangeal joint(s)")))
F_S05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S05",Type="Shape",Des="Fusion of interphalangeal joints",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_S06_UI<-tagList(fixedPage(h3("ID:F_S06"),
                            h4("Description:Fusion of metatarsophalangeal joint(s)")))
F_S06_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S06",Type="Shape",Des="Fusion of metotarsophalangeal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_S07_UI<-tagList(fixedPage(h3("ID:F_S07"),
                            h4("Description:Fusion of intertarsal joint(s)")))
F_S07_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S07",Type="Shape",Des="Fusion of intertarsal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_L01_UI<-tagList(fixedPage(h3("ID:F_L01"),
                            h4("Description:Destruction of metatarsals/phalanges")))
F_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_L01",Type="Loss",Des="Destruction of metatarsals/phalanges",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_L02_UI<-tagList(fixedPage(h3("ID:F_L02"),
                            h4("Description:Enlarged nutrient foramen")))
F_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_l02",Type="Loss",Des="Enlarged nutrient foramen",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_L03_UI<-tagList(fixedPage(h3("ID:F_L03"),
                            h4("Description:Symetric erosion of metatarsophalangeal or interphalangeal joint(s)")))
F_L03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_L03",Type="Loss",Des="Symetric erosion of metatarsophalangeal or interphalangeal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_L04_UI<-tagList(fixedPage(h3("ID:F_L04"),
                            h4("Description:Asymetric erosion of metatarsophalangeal or interphalangeal joint(s)")))
F_L04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_L03",Type="Loss",Des="Asymetric erosion of metatarsophalageal or interphalangeal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_F01_UI<-tagList(fixedPage(h3("ID:F_F01"),
                            h4("Description:Spur(s) on superior calcaneal tuber")))
F_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_F01",Type="Formation",Des="Spur(s) on superior calcaneal tuber",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_F02_UI<-tagList(fixedPage(h3("ID:F_F02"),
                            h4("Description:Spur(s) on inferior calcaneal tuber")))
F_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_F02",Type="Formation",Des="Spur(s) on inferior calcaneal tuber",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_F03_UI<-tagList(fixedPage(h3("ID:F_F03"),
                            h4("Description:Spur on medial talar neck")))
F_F03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_F03",Type="Formation",Des="Spur on medial talar neck",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_C01_UI<-tagList(fixedPage(h3("ID:F_C01"),
                            h4("Description:Severe bone loss and deformity creating high arch")))
F_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_C01",Type="Complex",Des="Severe bone loss and deformity creating high arch",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

F_C02_UI<-tagList(fixedPage(h3("ID:F_C02"),
                            h4("Description:Tufting and resorption of distal phalanges")))
F_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_C02",Type="Complex",Des="Tufting and resorption of distal phalanges",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

##################################################################################################
library(data.table)
Foot_DT<-data.table(ref=c("F_S01","F_S02","F_S03","F_S04","F_S05","F_S06","F_S07","F_L01","F_L02","F_L03","F_L04","F_F01","F_F02","F_F03","F_C01","F_C02"),
                    uioptions=c(F_S01_UI,F_S02_UI,F_S03_UI,F_S04_UI,F_S05_UI,F_S06_UI,F_S07_UI,F_L01_UI,F_L02_UI,F_L03_UI,F_L04_UI,F_F01_UI,F_F02_UI,F_F03_UI,F_C01_UI,F_C02_UI),
                    RecordCreator=c(F_S01_RC,F_S02_RC,F_S03_RC,F_S04_RC,F_S05_UI,F_S06_RC,F_S07_RC,F_L01_RC,F_L02_RC,F_L03_RC,F_L04_RC,F_F01_RC,F_F02_RC,F_F03_RC,F_C01_RC,F_C02_RC))