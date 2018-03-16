source("functions.R",local=TRUE)
shinyServer(function(input, output) {
  #UI rendering####
  #Location tab
  output$Bone<-renderUI({Location1(input$Region)})
  LocationOptions<-eventReactive(input$LocationGo,{
    Location2(input$Bone)})
  output$LocationOptions<-renderUI({LocationOptions()})
  
  #Trauma 
  output$FractOptions<-renderUI({Fracture(input$Trauma_Type,input$Region)})
  TraumaOptions<-eventReactive(input$TraumaGo,{
                               TraumaUI(input)})
  output$TraumaOptions<-renderUI({TraumaOptions()})
  
  #Patholoy
  output$Specific<-renderUI({Path1(input$Path_Type,input$Region)})
  
  PathologyOptions<-eventReactive(input$PathGo,{
    Path2(input$Path_Type,input$Region,input$Bone,input$Path_Specific)})
  output$PathologyOptions<-renderUI({PathologyOptions()})
  
  #Taphonomy
  TaphOptions<-eventReactive(input$TaphGo,{
    TaphUI(input)})
  output$TaphOptions<-renderUI({TaphOptions()})
  
  #Link tab
  Link<-eventReactive(input$show, {
    Link1(input$directory,input$File,input$Individual)
  })
  output$Link<-renderUI({Link()[[1]]})
  output$PopTable<-renderTable({Link()[[2]]})
  CM<-eventReactive(input$AddCM,{
    CustomMeasure(input$CustomMeasure)
  })
  output$CM<-renderUI({CM()})
  
  #File####
  PMessage<-eventReactive(input$Create,{
    CreateA(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$PMessage<-renderPrint({PMessage()})
  
  ComMessage<-eventReactive(input$Compat,{
    CompCheck(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$ComMessage<-renderPrint({ComMessage()})
  
  #TestTables####
  output$LocationTable<-renderTable({LocationTable(input)})
  output$TraumaTable<-renderTable({TraumaTable(input)})
  output$PathTable<-renderTable({PathTable(input)})
  output$TaphTable<-renderTable({TaphTable(input)})
  output$LinkTable<-renderTable({LinkTable(input)})
})