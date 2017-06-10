#Swan-neck deformity##############################################################################################
H_S01_UI<-tagList(fixedPage(h3("ID:H_S01"),
                    h4("Description:Swan-neck deformaity")))
H_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_S01",Type="Shape",Des="Swan-neck deformity",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Supernummery bones##############################################################################################
<-tagList(fixedPage(h3("ID:H_S02"),
                            h4("Description:Supernumery bones"),
                            column(width=4,selectInput("H_S02_1","Left",multiple=TRUE,choice=c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                                               "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium"))),
                            column(width=4,selectInput("H_S02_2","Right",multiple=TRUE,choice=c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                                                "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium"))),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("H_S02_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("H_S02_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
H_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_S02",Type="Shape",Des="Supernumery bones",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  left<-NA;right<-NA
  if(length(input$H_S02_1)>0){left<-paste(paste(input$H_S02_1,"l",sep="_"),collapse =",")}
  if(length(input$H_S02_2)>0){right<-paste(paste(input$H_S02_2,"r",sep="_"),collapse =",")}
  Table$Loc<-paste0("Hand:Snum:",paste(left,right,sep=","))
  if(input$H_S02_Link1 != "None"){Table$Link<-paste(paste(input$H_S02_Link1,collapse=","),paste(input$H_S02_Link2,collapse=","),sep=":")}
  Table }

#enlarged nutrient foramen##############################################################################################
H_L01_UI<-tagList(fixedPage(h3("ID:H_L01"),
                            h4("Description:Enlarged nutrient foramen")))
H_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_L01",Type="Loss",Des="Enlarged nutrient formen",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#symetric joint erosion##############################################################################################
H_L02_UI<-tagList(fixedPage(h3("ID:H_L02"),
                            h4("Description:Symetric erosion of metcarpophalangeal/interphalangeal joint(s)")))
H_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_L02",Type="Loss",Des="Symetric erosion of metacarpophalangeal/interphalangeal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Tufting##############################################################################################
H_C01_UI<-tagList(fixedPage(h3("ID:H_C01"),
                            h4("Description:Tufting and resorption of distal phalanges"),
                            selectInput("H_C01_1","Bone(s)",multiple = TRUE,selected="Phalanx_d1_r",c("1 right"="Phalanx_d1_r","1 left"="Phalanx_d1_l","2 right"="Phalanx_d2_r","2 left"="Phalanx_d2_l","3 right"="Phalanx_d3_r","3 left"="Phalanx_d3_l","4 right"="Phalanx_d4_r","4 left"="Phalanx_d4_l","5 right"="Phalanx_d5_r","5 left"="Phalanx_d5_l")),
                            column(width=4,
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d1_r')>=0",h4("1st Right Phalanx"), 
                                                    numericInput("H_C01_1_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_1_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_1_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_1_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d2_r')>=0",h4("2nd Right Phalanx"), 
                                                    numericInput("H_C01_2_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_2_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_2_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_2_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d3_r')>=0",h4("3rd Right Phalanx"), 
                                                    numericInput("H_C01_3_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_3_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_3_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_3_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d4_r')>=0",h4("4th Right Phalanx"), 
                                                    numericInput("H_C01_4_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_4_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_4_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_4_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d5_r')>=0",h4("5th Right Phalanx"), 
                                                    numericInput("H_C01_5_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_5_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_5_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_5_r_4","Shape description",value="Spade-shaped"))),
                            column(width=4,
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d1_l')>=0",h4("1st Left Phalanx"), 
                                                    numericInput("H_C01_1_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_1_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_1_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_1_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d2_l')>=0",h4("2nd Left Phalanx"), 
                                                    numericInput("H_C01_2_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_2_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_2_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_2_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d3_l')>=0",h4("3rd Left Phalanx"), 
                                                    numericInput("H_C01_3_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_3_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_3_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_3_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d4_l')>=0",h4("4th Leftt Phalanx"), 
                                                    numericInput("H_C01_4_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_4_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_4_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_4_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d5_l')>=0",h4("5th Left Phalanx"), 
                                                    numericInput("H_C01_5_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_5_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_5_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_5_l_4","Shape description",value="Spade-shaped"))),
                            column(width=4,h4("Connection with other lesions"),
                                  textInput("H_C01_Link1","ID of linked lesion(s)",value="None"),
                                  selectizeInput("H_C01_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))
                            ))
H_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_C01",Type="Complex",Des="Tufting and resorption of distal phalanges",Loc=NA,Feat="NA:NA",Size=NA,Shape=NA,Nature="NA:NA",Add=NA,Link=NA)
  Table$Loc<-paste("Hand",paste(input$H_C01_1,collapse=","),"NA",sep=":")
  x<-data.frame("Phalanx"=substr(input$H_C01_1, nchar(input$H_C01_1)-3+1, nchar(input$H_C01_1)),DW=NA,PW=NA,MW=NA,Shape=NA)
  for(i in 1:length(input$H_C01_1)){
    x$Shape[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_4"))]]
    x$DW[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_1"))]]
    x$PW[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_4"))]]
    x$MW[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_4"))]]
  }
  Table$Shape<-paste(paste(x$Phalanx,collapse=","),paste(x$Shape,collapse=","),sep=":")
  Table$Size<-paste0("Distal,Proximal,Mid:",paste(paste(x$DW,collapse="/"),paste(x$PW,collapse="/"),paste(x$MW,collapse="/"),sep=","))
  if(input$H_C01_Link1 != "None"){Table$Link<-paste(paste(input$H_C01_Link1,collapse=","),paste(input$H_C01_Link2,collapse=","),sep=":")}
  Table }

#Eburnation##############################################################################################
H_C02_UI<-tagList(fixedPage(h3("ID:H_C02"),
                            h4("Description:Eburnation of joint")))
H_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_C02",Type="Complex",Des="Eburnation of joint",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#data table#################################################################################################
library(data.table)
Hand_DT<-data.table(ref=c("H_S01","H_S02","H_L01","H_L02","H_C01","H_C02"),
                    uioptions=c(H_S01_UI,H_S02_UI,H_L01_UI,H_L02_UI,H_C01_UI,H_C02_UI),
                    RecordCreator=c(H_S01_RC,H_S02_RC,H_L01_RC,H_L02_RC,H_C01_RC,H_C02_RC))