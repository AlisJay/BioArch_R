shinyServer(function(input, output) {
  output$SOC3<-renderUI({sliderInput("SOC3","Individuals position within society",min=1,max=input$SOC2,value=1,round=TRUE,step=1)})
  appendHI<-function(input){
    filepath<-paste0(input$File,input$Type)
    df<-data.frame("Site"=input$Site,"ID"=input$ID,"Sex"=input$Sex,"Age"=input$Age1,"DentalAge"=input$Age2,"MinAge"=input$Age3[1],"MaxAge"=input$Age3[2],"DOB"=input$DOB,"Ancestry"=input$Ancestry,"SOC"=NA,
                   "FDIAP"=input$FDIAP,"FLEN"=input$FLEN,"HEIGT"=input$HEIGT,"FMIDA"=input$FMIDA,"FMIDM"=input$FMIDM,"HLEN"=input$HLEN,"HCIR"=input$HCIR,"LDI"=input$LDI,"LDC"=input$LDC,"LPI"=input$LPI,"LPC"=input$LPC,
                   "SUMTET"=input$SUMTET,"SUMPRE"=input$SUMPRE,"SUMCAV"=input$SUMCAV,"SUMSOK"=input$SUMSOK,"SUMABS"=input$SUMABS,"CROB"=input$CROB,"PORHY"=input$PORHY,"AUDEX"=input$AUDEX,"TIBINF"=input$TIBINF,
                   "SKELINF"=input$SKELINF,"DJSH"=input$DJSH,"DJHK"=input$DJHK,"DJCER"=input$DJCER,"DJTHO"=input$DJTHO,"DJLUM"=input$DJLUM,"DJTMJ"=input$DJTMJ,"DJWR"=input$DJWR,"DJHAN"=input$DJHAN,"TRARM"=input$TRARM,
                   "TRLEG"=input$TRLEG,"TRNAS"=input$TRNAS,"TRFAC"=input$TRFAC,"TRSKUL"=input$TRSKUL,"TRHAN"=input$TRHAN,"TRWEAP"=input$TRWEAP)
    if(df$Age!=5){df[,c("DentalAge","FDIAP")]<-NA}
    if(input$SOC1==111){df$SOC<-input$SOC1}else{df$SOC<-paste0(input$SOC1,input$SOC2,input$SOC3)}
    
    if(!(file.exists(filepath))){
      if(input$Type==".txt"){write.table(df,filepath,row.names=FALSE)
        message<-paste("File",filepath,"created.Individual",input$ID,"added.")}
      if(input$Type==".xls"){library(xlsx)
        write.xlsx(df,filepath,sheetName="Sheet1",row.names=FALSE)
        message<-paste("File",filepath,"created.Individual",input$ID,"added.")}
    }else{
      if(input$Type==".txt"){write.table(df,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
        message<-paste("Individual",input$ID,"added to",filepath)}
      if(input$Type==".xls"){library(xlsx)
        write.xlsx(rbind(read.xlsx(filepath,sheetName="Sheet1"),df),filepath,sheetName="Sheet1",row.names=FALSE)}
    }
    message
  }
  add<-eventReactive(input$Append,{appendHI(input)})
  output$Add<-renderPrint({add()})
  
  })





