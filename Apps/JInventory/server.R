source("functions.R",local=TRUE)
source("tables.R",local = TRUE)

shinyServer(function(input,output){
  output$SKFScore<-SKFScore<-renderText({FusionScore(SKFusion,input$SKOblit,input$SKFused,input$SKFusing,input$SKOpen)})
  output$SKIScore<-SKIScore<-renderText({InventoryScore(SKInvent,input$SKPresent,input$SKComplete)})
  
  output$UScore<-UScore<-renderText({paste(input$USkull,input$ULimb,input$UVert,input$UHandFoot,input$UClavScap,input$UOsCoxae,input$UOther,sep=":")})
  
  output$VIScore<-VISCore<-renderText({InventoryScore(VInvent,input$VPresent,input$VComplete)})
})