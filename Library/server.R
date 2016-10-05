source("Helpers.R")

references<-c("Reference1","Reference2","Reference3","Reference4")

shinyServer(function(input,output){
  output$Ref<-renderText({BuildRef(input)})
  num<-reactive({as.numeric(input$resultNum)})
  output$SRef<-SRef<-renderPrint({Results()[num(),"Reference"]})
  output$Tags<-output$Tags2<-renderText({Tags(input)})
  
  WriteBib<-eventReactive(input$Write_B,{WriteBibliography(input,SRef())})
  WriteLib<-eventReactive(input$Write_L,{WriteLibrary(input)})
  output$WB<-renderPrint({WriteBib()})
  output$WL<-renderPrint({WriteLib()})
  Results<-reactive({SearchLibrary(input)})
  output$Results<-renderTable({Results()})
  
})