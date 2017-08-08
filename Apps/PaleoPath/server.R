F_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_C02",Type="Complex",Des="Tufting and resorption of distal phalanges",Loc=NA,Feat="Head,Shaft,Base:NA",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Loc<-paste("Foot",paste(input$F_C02_1,collapse=","),"NA",sep=":")
  x<-data.frame("Phalanx"=substr(input$F_C02_1, nchar(input$F_C02_1)-3+1, nchar(input$F_C02_1)),DW=NA,PW=NA,MW=NA,Shape=NA)
  for(i in 1:length(input$F_C02_1)){
    x$Shape[i]<-NoComma(input[[as.character(paste0("F_C02_",x$Phalanx[i],"_4"))]])
    x$DW[i]<-input[[as.character(paste0("F_C02_",x$Phalanx[i],"_1"))]]
    x$PW[i]<-input[[as.character(paste0("F_C02_",x$Phalanx[i],"_2"))]]
    x$MW[i]<-input[[as.character(paste0("F_C02_",x$Phalanx[i],"_3"))]]
  }
  Table$Shape<-paste(paste(x$Phalanx,collapse=","),paste(x$Shape,collapse=","),sep=":")
  Table$Size<-paste0("Distal,Proximal,Mid:",paste(paste(x$DW,collapse="/"),paste(x$PW,collapse="/"),paste(x$MW,collapse="/"),sep=","))
  if(input$F_C02_Link1 != "None"){Table$Link<-paste(paste(input$F_C02_Link1,collapse=","),paste(input$F_C02_Link2,collapse=","),sep=":")}
  Table }

source("Custom.R",local=TRUE);source("functions.R",local=TRUE)
shinyServer(function(input,output){
  #custom UI creation####
  observeEvent(input$Add,{
    insertUI(selector = "#Add",
             where="afterEnd",
             ui=make(input$Add,input))
  })
  #Specfic abnormality Ui creation####
  Skull_UI<-eventReactive(input$SkullUI,{uioptions(input=c(input$Loss_Skull,input$Formation_Skull,input$Shape_Skull,input$Complex_Skull),region="Skull")})
  output$Skull_UI<-renderUI({Skull_UI()})
  
  Vert_UI<-eventReactive(input$VertUI,{uioptions(input=c(input$Loss_Vert,input$Formation_Vert,input$Shape_Vert,input$Complex_Vert),region="Vert")})
  output$Vert_UI<-renderUI({Vert_UI()})
  
  Pelvis_UI<-eventReactive(input$PelvisUI,{uioptions(input=c(input$Shape_Pelvis,input$Loss_Pelvis,input$Formation_Pelvis,input$Complex_Pelvis),region="Pelvis")})
  output$Pelvis_UI<-renderUI({Pelvis_UI()})
  
  Shoulder_UI<-eventReactive(input$ShoulderUI,{uioptions(input=c(input$Shape_shoulder,input$Loss_Shoulder,input$Formation_Shoulder,input$Complex_Shoulder),region="Shoulder")})
  output$Shoulder_UI<-renderUI({Shoulder_UI()})
  
  Thorax_UI<-eventReactive(input$ThoraxUI,{uioptions(input=c(input$Shape_Thorax,input$Loss_Thorax,input$Formation_Thorax,input$Complex_Thorax),region="Thorax")})
  output$Thorax_UI<-renderUI({Thorax_UI()})
  
  Arm_UI<-eventReactive(input$ArmUI,{uioptions(input=c(input$Shape_Arm,input$Loss_Arm,input$Formation_Arm,input$Complex_Arm),region="Arm")})
  output$Arm_UI<-renderUI({Arm_UI()})
  
  Hand_UI<-eventReactive(input$HandUI,{uioptions(input=c(input$Shape_Hand,input$Loss_Hand,input$Formation_Hand,input$Complex_Hand),region="Hand")})
  output$Hand_UI<-renderUI({Hand_UI()})
  
  Leg_UI<-eventReactive(input$LegUI,{uioptions(input=c(input$Shape_Leg,input$Loss_Leg,input$Formation_Leg,input$Complex_Leg),region="Leg")})
  output$Leg_UI<-renderUI({Leg_UI()})
  
  Foot_UI<-eventReactive(input$FootUI,{uioptions(input=c(input$Shape_Foot,input$Loss_Foot,input$Formation_Foot,input$Complex_Foot),region="Foot")})
  output$Foot_UI<-renderUI({Foot_UI()})
  
  Systemic_UI<-eventReactive(input$SystemicUI,{uioptions(input=input$Systemic,region="Systemic")})
  output$Systemic_UI<-renderUI({Systemic_UI()})
  
  #photo reference####
  output$Photo<-renderUI({Photoref(input)[[1]]})
  output$Phototable<-renderTable({photocollect(Photoref(input)[[2]],input)})
  #file creation####
  PMessage<-eventReactive(input$Create,{
    CreatePP(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$PMessage<-renderPrint({PMessage()})
  output$ResultsTable<-renderTable({merge(RecordCreator(input),photocollect(Photoref(input)[[2]],input))})
  #append records####
  IMessage<-eventReactive(input$Append,{
    AppendPP(input$POPID,input$ID,input$Person2,input$dir,RecordCreator(input),photocollect(Photoref(input)[[2]],input))
  })
  output$IMessage<-renderPrint({IMessage()})
  
  #Compatability Check####
  ComMessage<-eventReactive(input$Compat,{
    CompCheck(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$ComMessage<-renderPrint({ComMessage()})
  
  #Record creation checks####
  #output$customcheck<-renderTable({})
  #output$SKTable<-renderTable({NA})
  #output$VTable<-renderTable({NA})
  #output$SHTable<-renderTable({NA})
  #output$PTable<-renderTable({NA})
  #output$TTable<-renderTable({NA})
  #output$ATable<-renderTable({NA})
  #output$HTable<-renderTable({NA})
  #output$LTable<-renderTable({NA})
  output$FTable<-renderTable({F_C02_RC(input)})
  
  
})

