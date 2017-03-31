source("Custom.R",local=TRUE);source("functions.R",local=TRUE)
shinyServer(function(input,output){
  #custom UI creation####
  observeEvent(input$Add,{
    insertUI(selector = "#Add",
             where="afterEnd",
             ui=make(input$Add,input))
  })
  #Specfic abnormality Ui creation####
  output$Skull_UI<-renderUI({uioptions(input=c(input$Loss_Skull,input$Formation_Skull,input$Shape_Skull,input$Complex_Skull),region="Skull")})
  output$Vert_UI<-renderUI({uioptions(input=c(input$Loss_Vert,input$Formation_Vert,input$Shape_Vert,input$Complex_Vert),region="Vert")})
  output$Pelvis_UI<-renderUI({uioptions(input=c(input$Shape_Pelvis,input$Loss_Pelvis,input$Formation_Pelvis,input$Complex_Pelvis),region="Pelvis")})
  output$Shoulder_UI<-renderUI({uioptions(input=c(input$Shape_shoulder,input$Loss_Shoulder,input$Formation_Shoulder,input$Complex_Shoulder),region="Shoulder")})
  output$Thorax_UI<-renderUI({uioptions(input=c(input$Shape_Thorax,input$Loss_Thorax,input$Formation_Thorax,input$Complex_Thorax),region="Thorax")})
  output$Arm_UI<-renderUI({uioptions(input=c(input$Shape_Arm,input$Loss_Arm,input$Formation_Arm,input$Complex_Arm),region="Arm")})
  output$Hand_UI<-renderUI({uioptions(input=c(input$Shape_Hand,input$Loss_Hand,input$Formation_Hand,input$Complex_Hand),region="Hand")})
  output$Leg_UI<-renderUI({uioptions(input=c(input$Shape_Leg,input$Loss_Leg,input$Formation_Leg,input$Complex_Leg),region="Leg")})
  output$Foot_UI<-renderUI({uioptions(input=c(input$Shape_Foot,input$Loss_Foot,input$Formation_Foot,input$Complex_Foot),region="Foot")})
  output$Systemic_UI<-renderUI({uioptions(input=input$Systemic,region="Systemic")})
  
  #photo reference####
  output$Photo<-renderUI({Photoref(input)[[1]]})
  
  #file creation####
  PMessage<-eventReactive(input$Create,{
    CreatePP(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$PMessage<-renderPrint({PMessage()})
  
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
  #output$SKTable<-renderTable({RecordCreator(input)})
  #output$VTable<-renderTable({NA})
  #output$SHTable<-renderTable({NA})
  #output$PTable<-renderTable({NA})
  #output$TTable<-renderTable({NA})
  #output$ATable<-renderTable({NA})
  #output$HTable<-renderTable({NA})
  #output$LTable<-renderTable({NA})
  #output$FTable<-renderTable({NA})
  #output$SYTable<-renderTable({NA})
})

