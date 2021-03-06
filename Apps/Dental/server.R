source("functions.R",local=TRUE)
source("write.R",local=TRUE)
shinyServer(function(input,output){
  #Ui creation####
  output$CC_UI<-renderUI({CreateDentalUI(input,tab="CC")})
  output$ED_UI<-renderUI({CreateDentalUI(input,tab="ED")})
  output$PM_UI<-renderUI({CreateDentalUI(input,tab="PM")})
  output$MW_UI<-renderUI({CreateDentalUI(input,tab="MW")})
  output$OW_UI<-renderUI({CreateDentalUI(input,tab="OW")})
  output$M_UI<-renderUI({CreateDentalUI(input,tab="M")})
  
  #Create fields####
  present<-reactive({Present(input)})
  output$pScore<-pScore<-reactive({InventoryScore(input)$Pscore})
  output$dScore<-dScore<-reactive({InventoryScore(input)$Dscore})
  Car<-reactive({Caries(c(present()[[1]],present()[[2]]),input)})
  Cal<-reactive({Calculus(c(present()[[1]],present()[[2]]),input)})
  Hypo<-reactive({Hypoplasia(c(present()[[1]],present()[[2]]),input)})
  Hyper<-reactive({Hypercalcification(c(present()[[1]],present()[[2]]),input)})
  Mod<-reactive({Moderfication(c(present()[[1]],present()[[2]]),input)})
  Ab<-reactive({Ablation(input)})
  W<-reactive({Wear(c(present()[[1]],present()[[2]]),input)})
  Measure<-reactive({Measurments(c(present()[[1]],present()[[2]]),input)})
  G<-reactive({Growth(c(present()[[1]],present()[[2]]),input)})
  Morph<-reactive({Mtraits(c(present()[[1]],present()[[2]]),input)})
  
  #File/Writeout####
  PMessage<-eventReactive(input$Create,{
    CreateDEN(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$PMessage<-renderText({PMessage()})
  
  IMessage<-eventReactive(input$Append,{
    AppendDEN(input$POPID,input$ID,input$Person2,input$dir,pScore(),dScore(),Car(),Cal(),Hypo(),Hyper(),W(),Measure(),G(),Morph(),Mod(),Ab())
  })
  output$IMessage<-renderText({IMessage()})
})