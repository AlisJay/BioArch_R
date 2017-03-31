V_S01_UI<-tagList(fixedPage(h3("ID:V_S01"),
                             h4("Description:Anterior posterior curvature")))
V_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S01",Type="Shape",Des="Anterior posterior curvature",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S02_UI<-tagList(fixedPage(h3("ID:V_S02"),
                             h4("Description:Lateral curvature")))
V_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S02",Type="Shape",Des="Lateral curvature",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S03_UI<-tagList(fixedPage(h3("ID:V_S03"),
                             h4("Description:Cleft Centra")))
V_S03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S03",Type="Shape",Des="Cleft Centra",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S04_UI<-tagList(fixedPage(h3("ID:V_S04"),
                             h4("Description:Sacralisation")))
V_S04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S04",Type="Shape",Des="Sacralisation",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S05_UI<-tagList(fixedPage(h3("ID:V_S05"),
                             h4("Description:Lumbarisation")))
V_S05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S05",Type="Shape",Des="Lumbarisation",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S06_UI<-tagList(fixedPage(h3("ID:V_S06"),
                             h4("Description:Incomplete closure of sacral neural arch")))
V_S06_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S06",Type="Shape",Des="Incomplete closure of sacral neural arch",Loc="Vertebrae:Sacrum:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S07_UI<-tagList(fixedPage(h3("ID:V_S07"),
                             h4("Description:Incomplete closure of lumbar neural arch")))
V_S07_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S07",Type="Shape",Des="Incomplete closure of lumbar neural arch",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S08_UI<-tagList(fixedPage(h3("ID:V_S08"),
                             h4("Description:Incomplete closure of cervical/thorasic neural arch")))
V_S08_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S08",Type="Shape",Des="Incomplete closure of cervical/thoracic neural arch",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S09_UI<-tagList(fixedPage(h3("ID:V_S09"),
                             h4("Description:Separation of arch and body")))
V_S09_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S09",Type="Shape",Des="Separation of arch and body",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_S10_UI<-tagList(fixedPage(h3("ID:V_S10"),
                             h4("Description:Supernumery bones"),
                   column(width=6,selectInput("V_S10_1","Type",multiple=TRUE,choice=c("Cervical","Thorasic","Lumbar","Sacral","cervical/thorasic borderline"="Cervical_Thorasic","thorasic/lumbar borderline"="Thorasic_Lumbar","lumbar/sacral borderline"="Lumbar_Sacral","unknown")),
                          textInput("V_S10_2","Numbering/additional description")),
                   column(width=6,h4("Connection with other lesions"),
                          textInput("V_S10_Link1","ID of linked lesion(s)",value="None"),
                          selectizeInput("V_S10_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
V_S10_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_S10",Type="Shape",Des="Supernummery bones",Loc="Vert:Snum:NA",Feat="NA:NA",Size="NA:NA",Shape="NA:NA",Nature=NA,Add=NA,Link=NA)
  if(length(input$V_S10_1)>0){Table$Nature<-paste(input$V_S10_1,collapse =",")}
  Table$Add<-input$V_S10_2
  if(input$V_S10_Link1 != "None"){Table$Link<-paste(paste(input$V_S10_Link1,collapse=","),paste(input$V_S10_Link2,collapse=","),sep=":")}
  Table }

V_L01_UI<-tagList(fixedPage(h3("ID:V_L01"),
                             h4("Description:Cavitation of the vertebral body")))
V_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_L01",Type="Loss",Des="Cavitation of vertebral body",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_L02_UI<-tagList(fixedPage(h3("ID:V_L02"),
                             h4("Description:Loss of body height")))
V_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_L02",Type="Loss",Des="Loss of body height",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_L03_UI<-tagList(fixedPage(h3("ID:V_L03"),
                             h4("Description:Circular depression(s) in endplate")))
V_L03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_L03",Type="Loss",Des="Circular depression(s) in endplate",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_L04_UI<-tagList(fixedPage(h3("ID:V_L04"),
                             h4("Description:Anteriorolateral scooped out lesion(s)")))
V_L04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_L04",Type="Loss",Des="Anteriorlateral scooped out lesion(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_L05_UI<-tagList(fixedPage(h3("ID:V_L05"),
                             h4("Description:Erosion of odontoid")))
V_L05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_L05",Type="Loss",Des="Erosion of odontoid",Loc="Vertebrae:C2:NA",Feat="odontoid pro",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_F01_UI<-tagList(fixedPage(h3("ID:V_F01"),
                             h4("Description:Osteophytes on rim of body")))
V_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_F01",Type="Formation",Des="Osteophytes on rim of body",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_F02_UI<-tagList(fixedPage(h3("ID:V_F02"),
                             h4("Description:Fusion of rib and vertebra")))
V_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_f02",Type="Formation",Des="Fusion of rib and vertebra",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_F03_UI<-tagList(fixedPage(h3("ID:V_F03"),
                             h4("Description:Fusion of 2 vertebrae")))
V_F03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_F03",Type="Formation",Des="Fusion of 2 vertebrae",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_F04_UI<-tagList(fixedPage(h3("ID:V_F04"),
                             h4("Description:Fusion of 3+ vertebrae")))
V_F04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_F03",Type="Formation",Des="Fusion of 3+ vertebrae",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_C01_UI<-tagList(fixedPage(h3("ID:V_C01"),
                             h4("Description:Osteophytes/irregular margins/surface porosity/eburnation")))
V_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_C01",Type="Complex",Des="Osteophytes/irregular margins/surface porosity/eburnation",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_C02_UI<-tagList(fixedPage(h3("ID:V_C02"),
                             h4("Description:Pitting and osteophytes")))
V_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_C02",Type="Complex",Des="Pitting and osteophytes",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

V_C03_UI<-tagList(fixedPage(h3("ID:V_C03"),
                             h4("Description:L5S1 abnormality")))
V_C03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="V_C03",Type="Complex",Des="L5S1 abnormality",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
##################################################################################################
library(data.table)
Vert_DT<-data.table(ref=c("V_S01 ","V_S02","V_S03","V_S04","V_S05","V_S06","V_S07","V_S08","V_S09","V_S10","V_L01","V_L02","V_L03","V_L04","V_L05","V_F01","V_F02","V_F03","V_F04","V_C01","V_C02","V_C03"),
                     uioptions=c(V_S01_UI,V_S02_UI,V_S03_UI,V_S04_UI,V_S05_UI,V_S06_UI,V_S07_UI,V_S08_UI,V_S09_UI,V_S10_UI,V_L01_UI,V_L02_UI,V_L03_UI,V_L04_UI,V_L05_UI,V_F01_UI,V_F02_UI,V_F03_UI,V_F04_UI,V_C01_UI,V_C02_UI,V_C03_UI),
                     RecordCreator=c(V_S01_RC,V_S02_RC,V_S03_RC,V_S04_RC,V_S05_RC,V_S06_RC,V_S07_RC,V_S08_RC,V_S09_RC,V_S10_RC,V_L01_RC,V_L02_RC,V_L03_RC,V_L04_RC,V_L05_RC,V_F01_RC,V_F02_RC,V_F03_RC,V_F04_RC,V_C01_RC,V_C02_RC,V_C03_RC))