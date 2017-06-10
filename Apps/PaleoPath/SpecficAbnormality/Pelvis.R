#Acetabular ossicle##############################################################################################
P_S01_UI<-tagList(fixedPage(h3("ID:P_S01"),
                    h4("Description:Acetabular ossicle")))
P_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_S01",Type="Shape",Des="Acetabular ossicle",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Wide pubic gap##############################################################################################
P_S02_UI<-tagList(fixedPage(h3("ID:P_S02"),
                            h4("Description:Wide pubic gap")))
P_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_S02",Type="Shape",Des="Wide pubic gap",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Enlarged nutrient foramina##############################################################################################
P_L01_UI<-tagList(fixedPage(h3("ID:P_L01"),
                            h4("Description:Enlarged nutrient foramina"),
                            column(width=4,selectInput("P_L01_1","Bone",multiple=TRUE,selected="OsCoxa_r",c("Right OS Coxa"="OsCoxa_r","Left Os Coxa"="OsCoxa_l","Sacrum")),
                            conditionalPanel("input.P_L01_1.indexOf('OsCoxa_r')>=0",selectInput("P_L01_1_OsCoxa_r","Right os Coxa  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                            conditionalPanel("input.P_L01_1.indexOf('OsCoxa_l')>=0",selectInput("P_L01_1_OsCoxa_l","Left os Coxa  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                            conditionalPanel("input.P_L01_1.indexOf('Sacrum')>=0",selectInput("P_L01_1_Sacrum","Sacrum relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("P_L01_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("P_L01_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
P_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_l01",Type="Loss",Des="Enlarged nutrient foramina",Loc=NA,Feat="nutrient_for:NA",Size=NA,Shape="NA:NA",Nature="NA:NA",Add=NA,Link=NA)
  Table$Loc<-paste0("Pelvis:",paste(input$P_L01_1,collapse=","),":NA")
  for(i in 1:length(input$P_L01_1)){
    if(i==1){Size<-input[[as.character(paste0("P_L01_1_",input$P_L01_1[i]))]]
    }else{Size<-c(Size,input[[as.character(paste0("P_L01_1_",input$P_L01_1[i]))]])}
  }
  Table$Size<-paste0("relativeSize:",paste(Size,collapse=","))
  if(input$P_L01_Link1 != "None"){Table$Link<-paste(paste(input$P_L01_Link1,collapse=","),paste(input$P_L01_Link2,collapse=","),sep=":")}
  Table }

#Concavity of illiac fossa##############################################################################################
P_L02_UI<-tagList(fixedPage(h3("ID:P_L02"),
                            h4("Description:Concavity/perforation of illiac fossa")))
P_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_L02",Type="Loss",Des="Concavity/perforation of illiac fossa",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Dorsal pubic depression##############################################################################################
P_L03_UI<-tagList(fixedPage(h3("ID:P_L03"),
                            h4("Description:Dorsal depression(s) on pubic symphyses"),
                            selectInput("P_L03_1","Side",multiple=TRUE,selected="_r",c("Left"="_l","Right"="_r")),
                            column(width=4,conditionalPanel("input.P_L03_1.indexOf('_r')>=0",h4("Right"),
                                             selectInput("P_L03_2_r","Shape (select most approprate)",c("Linear","Circular","Irregular")),
                                             numericInput("P_L03_3_r","Maximum superior inferior length(mm)",value=NA),
                                             numericInput("P_L03_4_r","Maximum Mediolateral length (mm)",value=NA),
                                             numericInput("P_L03_5_r","Minimum anterior posterior width within the depression(mm)",value=NA),
                                             numericInput("P_L03_6_r","Anterior posterior width outside the depression (mm)",value=NA),
                                             selectInput("P_L03_7_r","Edges (select all applicable)",multiple=TRUE,selected="Sharp",c("Sharp","Rounded","Irregular","Pitted")),
                                             checkboxInput("P_L03_8_r","Pitting within depression?",value=FALSE))),
                            column(width=4,conditionalPanel("input.P_L03_1.indexOf('_l')>=0",h4("Left"),
                                             selectInput("P_L03_2_l","Shape (select most approprate)",c("Linear","Circular","Irregular")),
                                             numericInput("P_L03_3_l","Maximum superior inferior length(mm)",value=NA),
                                             numericInput("P_L03_4_l","Maximum Mediolateral length (mm)",value=NA),
                                             numericInput("P_L03_5_l","Minimum anterior posterior width within the depression(mm)",value=NA),
                                             numericInput("P_L03_6_l","Anterior posterior width outside the depression (mm)",value=NA),
                                             selectInput("P_L03_7_l","Edges (select all applicable)",multiple=TRUE,selected="Sharp",c("Sharp","Rounded","Irregular","Pitted")),
                                             checkboxInput("P_L03_8_l","Pitting within depression?",value=FALSE))),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("P_L03_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("P_L03_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
P_L03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_L03",Type="Loss",Des="Dorsal depression(s) on pubic symphyses",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Loc<-paste0("Pelvis:",paste(paste0("OsCoxa",input$P_L03_1),collapse=","),":NA")
  if(length(input$P_L03_1)==2){
    Table$Size<-paste0("SupInf,MedLat,AntPos(in),AntPos(out):",paste(paste(input$P_L03_3_r,input$P_L03_3_l,sep="/"),paste(input$P_L03_4_r,input$P_L03_4_l,sep="/"),paste(input$P_L03_5_r,input$P_L03_5_l,sep="/"),paste(input$P_L03_6_r,input$P_L03_6_l,sep="/"),sep=","))
    Table$Shape<-paste0("General:",paste(input$P_L03_2_r,input$P_L03_2_l,sep="/"))
    Table$Nature<-paste0("Edges,Pitting:",paste(paste(paste(input$P_L03_7_r,collapse="_"),paste(input$P_L03_7_l,collapse="_"),sep="/"),paste(input$P_L03_8_r,input$P_L03_8_l,sep="/"),sep=","))
  }else{
    Table$Size<-paste0("SupInf,MedLat,AntPos(in),AntPos(out):",paste(input[[as.character(paste0("P_L03_3",input$P_L03_1))]],input[[as.character(paste0("P_L03_4",input$P_L03_1))]],input[[as.character(paste0("P_L03_5",input$P_L03_1))]],input[[as.character(paste0("P_L03_6",input$P_L03_1))]],sep=","))
    Table$Shape<-paste0("General:",input[[as.character(paste0("P_L03_2",input$P_L03_1))]])
    Table$Nature<-paste0("Edges,Pitting:",paste(paste(input[[as.character(paste0("P_L03_7",input$P_L03_1))]],collapse="_"),input[[as.character(paste0("P_L03_8",input$P_L03_1))]],sep=","))
  }
  if(input$P_L03_Link1 != "None"){Table$Link<-paste(paste(input$P_L03_Link1,collapse=","),paste(input$P_L03_Link2,collapse=","),sep=":")}
  Table }

#Acessory sacral facets##############################################################################################
P_F01_UI<-tagList(fixedPage(h3("ID:P_F01"),
                            h4("Description:Acessory sacral facets")))
P_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F01",Type="Formation",Des="Acessory sacral facets",Loc="Pelvis:Sacrum:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Fusion of sacroilliac##############################################################################################
P_F02_UI<-tagList(fixedPage(h3("ID:P_F02"),
                            h4("Description:Fusion of sacroilliac joint")))
P_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F02",Type="Formation",Des="Fusion of sacroilliac joint",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Projections##############################################################################################
P_F03_UI<-tagList(fixedPage(h3("ID:P_F03"),
                            h4("Description:Projections on illiac crest/ishial crest,spine or tuberocity/obturator foramen"),
                            selectInput("P_F03_1","Location",multiple=TRUE,c("Right Illiac crest","Right Ishial Crest","Right Ishial Spine","Right Ishial Tuberocity","Right Obturator foramen","Left Illiac crest","Left Ishial Crest","Leftt Ishial Spine","Left Ishial Tuberocity","Left Obturator foramen"))))
P_F03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F03",Type="Formation",Des="Projections on illiac crest/ishial crest,spine or tuberocity/obturator foramen",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Bridging of cotyloid notch##############################################################################################
P_F04_UI<-tagList(fixedPage(h3("ID:P_F04"),
                            h4("Description:Bridging of cotyloid notch")))
P_F04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F04",Type="Formation",Des="Bridging of cotyloid notch",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#long ishial tuberocity projection##############################################################################################
P_F05_UI<-tagList(fixedPage(h3("ID:P_F05"),
                            h4("Description:long projection from ishial tuberocity")))
P_F05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F05",Type="Formation",Des="Long projection from ishial tuberocity",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Flatterned acetabular rim##############################################################################################
P_C01_UI<-tagList(fixedPage(h3("ID:P_C01"),
                            h4("Description:Flatterned area of acetabular rim")))
P_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_C01",Type="Complex",Des="Flatterned area of acetabular rim",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Destruction of acetabulum##############################################################################################
P_C02_UI<-tagList(fixedPage(h3("ID:P_C02"),
                            h4("Description:Destruction of acetabulum with reactive bone growth")))
P_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_C02",Type="Complex",Des="Destruction of acetabulum with reactive bone growth",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#data table#################################################################################################
library(data.table)
Pelvis_DT<-data.table(ref=c("P_S01","P_S02","P_L01","P_L02","P_L03","P_F01","P_F03","P_F04","P_F05","P_C01","P_C02"),
                    uioptions=c(P_S01_UI,P_S02_UI,P_L01_UI,P_L02_UI,P_L03_UI,P_F01_UI,P_F03_UI,P_F04_UI,P_F05_UI,P_C01_UI,P_C02_UI),
                    RecordCreator=c(P_S01_RC,P_S02_RC,P_L01_RC,P_L02_RC,P_L03_RC,P_F01_RC,P_F03_RC,P_F04_RC,P_F05_RC,P_C01_RC,P_C02_RC))