#Custom#####
L_CL_UI<-tagList(h3("ID:L_CL"),h4("Description:Custom Loss Abnormality"),
                 column(width=4,
                        selectInput("L_CL_Percent","% of bone affected",c("<1/3","1/3-2/3",">2/3")),
                        selectizeInput("L_CL_Involve","Part(s) of bone involved",multiple=TRUE,choice=c("Cortical","Trabeculae","Periosteal","Endosteal","Subchondral")),
                        textInput("L_CL_Shape","Overall shape description"),
                        selectInput("L_CL_Organisation","Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                        checkboxInput("L_CL_Collapse","Associated structural collapse?",value=FALSE)),
                 column(width=4,
                        conditionalPanel("input.L_CL_organisation == 'Focal'",
                                         selectInput("L_CL_Foci","Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                         selectInput("L_CL_FociSize","Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                         selectInput("L_CL_Response","Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined"))),
                        conditionalPanel("input.L_CL_organisation == 'Diffuse'",
                                         checkboxInput("L_CL_Thining","Cortical thinning?",value=FALSE),
                                         selectInput("L_CL_Sites","Number of separate sites",c("1","2","3-5","6-10","10+")),
                                         h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                         selectInput("L_CL_Overlapping","Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed")))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
L_CL_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  if(input$L_CL_organisation=="Focal"){
    Table$Des1<-"Custom Focal Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,Foci,FociSize,Response:",paste(input$L_CL_Percent,paste(input$L_CL_Involve,collapse="/"),FreeFix(input$L_CL_Shape),"Focal",input$L_CL_Collapse,input$L_CL_Foci,paste(input$L_CL_FociSize,collapse="/"),input$L_CL_Response,sep=","))}
  if(input$L_CL_organisation=="Diffuse"){
    Table$Des1<-"Custom Diffuse Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,CorticalThining,Sites,Overlapping:",paste(input$L_CL_Percent,paste(input$L_CL_Involve,collapse="/"),FreeFix(input$L_CL_Shape),"Focal",input$L_CL_Collapse,input$L_CL_Thining,input$L_CL_Sites,input$L_CL_Overlapping,sep=","))}
  Table
}
#Pit superior to fovea capitis#######
L_L01_UI<-tagList(h3("ID:L_L01"),h4("Description:Large pit superior to fovea capitis"))
L_L01_RC<-function(input=input){   
  Table<-data.frame(Des1="Large pit superior to fovea capitis",Size=NA,Nature=NA,Heal=NA)
  Table }

#Scooped lesion medial condyle#######
L_L02_UI<-tagList(h3("ID:L_L02"),h4("Description:Scooped lesion in femoral medial condyle"))
L_L02_RC<-function(input=input){   
  Table<-data.frame(Des1="Scooped lesion in femoral medial condyle",Size=NA,Nature=NA,Heal=NA)
  Table }

#Depression on distal articular margin#######
L_L03_UI<-tagList(h3("ID:L_L03"),h4("Description:Depression on margin of distal articular surface"))
L_L03_RC<-function(input=input){   
  Table<-data.frame(Des1="Depression on margin of distal articular surface",Size=NA,Nature=NA,Heal=NA)
  Table }

#depression on femoral neck#######
L_L04_UI<-tagList(h3("ID:L_L04"),h4("Description:Porous depression in femoral neck"))
L_L04_RC<-function(input=input){   
  Table<-data.frame(Des1="Porous depression in femoral neck",Size=NA,Nature=NA,Heal=NA)
  Table }

#Defect superior to medial condyle#######
L_L05_UI<-tagList(h3("ID:L_L05"),h4("Description:>2cm defect superior to medial condyle"))
L_L05_RC<-function(input=input){   
  Table<-data.frame(Des1=">2cm defect superior to medial condyle",Size=NA,Nature=NA,Heal=NA)
  Table }

#Groove inferior to lesser trochanter#######
L_L06_UI<-tagList(h3("ID:L_L06"),h4("Description:Groove inferior to lesser trochanter"))
L_L06_RC<-function(input=input){   
  Table<-data.frame(Des1="Groove inferior to lesser trochanter",Size=NA,Nature=NA,Heal=NA)
  Table }

#Continuity break of anterior distal tibial margin#######
L_L07_UI<-tagList(h3("ID:L_L07"),h4("Description:Break in continuity of anterior margin of distal tibia"))
L_L07_RC<-function(input=input){   
  Table<-data.frame(Des1="Break in continuity of anterior margin of distal tibia",Size=NA,Nature=NA,Heal=NA)
  Table }

