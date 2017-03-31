T_S01_UI<-tagList(fixedPage(h3("ID:T_S01"),
                            h4("Description:Shepard's Crook deformity")))
T_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="T_S01",Type="Shape",Des="Shepard's crook deformity",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

T_S02_UI<-tagList(fixedPage(h3("ID:T_S02"),
                            h4("Description:Bifid/bicipital rib(s)")))
T_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="T_S02",Type="Shape",Des="Bifid/bicipital rib(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

T_S03_UI<-tagList(fixedPage(h3("ID:T_S03"),
                            h4("Description:Supernummery rib(s)"),
                            column(width=4,selectInput("T_S03_1","Left",multiple=TRUE,choices=c("Cervical","Intrathoracic","Lumbar","Sacral","other/unknown")),
                            textInput("T_S03_2","Additional info")),
                            column(width=4,selectInput("T_S03_3","Right",multiple=TRUE,choices=c("Cervical","Intrathoracic","Lumbar","Sacral","other/unknown")),
                                   textInput("T_S03_4","Additional info")),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("T_S03_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("T_S03_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
T_S03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="T_S03",Type="Shape",Des="Supernummery rib(s)",Loc="Thorax:Snum:NA",Feat="NA:NA",Size="NA:NA",Shape="NA:NA",Nature=NA,Add=NA,Link=NA)
  left<-NA;right<-NA;Ladd<-"Left-NA";Radd<-"Right-NA"
  if(length(input$T_S03_1)>0){left<-paste(paste(input$T_S03_1,"l",sep="_"),collapse =",")
  Ladd<-paste0("Left-",input$T_S03_2)}
  if(length(input$T_S03_3)>0){right<-paste(paste(input$T_S03_3,"r",sep="_"),collapse =",")
  Radd<-paste0("Right-",input$T_S03_4)}
  Table$Nature<-paste(left,right,sep=",")
  Table$Add<-paste(Ladd,Radd,sep=":")
  if(input$T_S03_Link1 != "None"){Table$Link<-paste(paste(input$T_S03_Link1,collapse=","),paste(input$T_S03_Link2,collapse=","),sep=":")}
  Table }

T_L01_UI<-tagList(fixedPage(h3("ID:T_L01"),
                            h4("Description:Circular defect in sternum")))
T_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="T_L01",Type="Loss",Des="Circular defect in sternum",Loc="Thorax:Sternum:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

T_F01_UI<-tagList(fixedPage(h3("ID:T_F01"),
                            h4("Description:Irregular growth around vertebral articulation")))
T_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="T_F01",Type="Formation",Des="Irregular growth around vertebral articulation",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

T_F02_UI<-tagList(fixedPage(h3("ID:T_F02"),
                            h4("Description:Thickened periosteal bone")))
T_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="T_F02",Type="Thickened periosteal bone",Des=NA,Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

##################################################################################################
library(data.table)
Thorax_DT<-data.table(ref=c("T_S01","T_S02","T_S03","T_L01","T_F01","T_F02"),
                    uioptions=c(T_S01_UI,T_S02_UI,T_S03_UI,T_L01_UI,T_F01_UI,T_F02_UI),
                    RecordCreator=c(T_S01_RC,T_S02_RC,T_S03_RC,T_L01_RC,T_F01_RC,T_F02_RC))