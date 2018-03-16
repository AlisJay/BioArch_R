#Custom#####
T_CS_UI<-tagList(h3("ID:T_CS"),h4("Description:Custom Shape Abnormality"))
T_CS_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
Table
}
#Shepard's crook#########
T_S01_UI<-tagList(fixedPage(h3("ID:T_S01"),
                            h4("Description:Shepard's Crook deformity")))
T_S01_RC<-function(input=input){   
  Table<-data.frame(Des1="Shepard's crook deformity",Size=NA,Nature=NA,Heal=NA)
  Table }

#Bifid rib#######
T_S02_UI<-tagList(fixedPage(h3("ID:T_S02"),
                            h4("Description:Bifid/bicipital rib(s)")))
T_S02_RC<-function(input=input){   
  Table<-data.frame(Des1="Bifid/bicipital rib(s)",Size=NA,Nature=NA,Heal=NA)
  Table }

#Supernummery bones#######
T_S03_UI<-tagList(h3("ID:T_S03"),h4("Description:Supernummery rib(s)"),
                            column(width=4,selectInput("T_S03_1","Left",multiple=TRUE,choices=c("Cervical","Intrathoracic","Lumbar","Sacral","other/unknown")),
                            textInput("T_S03_2","Additional info")),
                            column(width=4,selectInput("T_S03_3","Right",multiple=TRUE,choices=c("Cervical","Intrathoracic","Lumbar","Sacral","other/unknown")),
                                   textInput("T_S03_4","Additional info"))
                            )
T_S03_RC<-function(input=input){   
  Table<-data.frame(Des1="Supernummery rib(s)",Size=NA,Nature=NA,Heal=NA)
  left<-NA;right<-NA;Ladd<-NA;Radd<-NA
  if(length(input$T_S03_1)>0){
  left<-paste(paste(input$T_S03_1,"l",sep="_"),collapse =",")
  Ladd<-paste0("Left-",input$T_S03_2)}
  if(length(input$T_S03_3)>0){
  right<-paste(paste(input$T_S03_3,"r",sep="_"),collapse =",")
  Radd<-paste0("Right-",input$T_S03_4)}
  
  Table$Nature<-paste("RightType,LeftType,RightAdditional,LeftAdditional",paste(right,left,Radd,Ladd,sep=","),sep=":")
  Table }

