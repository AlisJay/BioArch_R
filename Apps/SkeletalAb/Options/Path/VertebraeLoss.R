#Custom#####
V_CL_UI<-tagList(h3("ID:V_CL"),h4("Description:Custom Loss Abnormality"),
                 column(width=4,
                        selectInput("V_CL_Percent","% of bone affected",c("<1/3","1/3-2/3",">2/3")),
                        selectizeInput("V_CL_Involve","Part(s) of bone involved",multiple=TRUE,choice=c("Cortical","Trabeculae","Periosteal","Endosteal","Subchondral")),
                        textInput("V_CL_Shape","Overall shape description"),
                        selectInput("V_CL_Organisation","Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                        checkboxInput("V_CL_Collapse","Associated structural collapse?",value=FALSE)),
                 column(width=4,
                        conditionalPanel("input.V_CL_organisation == 'Focal'",
                                         selectInput("V_CL_Foci","Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                         selectInput("V_CL_FociSize","Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                         selectInput("V_CL_Response","Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined"))),
                        conditionalPanel("input.V_CL_organisation == 'Diffuse'",
                                         checkboxInput("V_CL_Thining","Cortical thinning?",value=FALSE),
                                         selectInput("V_CL_Sites","Number of separate sites",c("1","2","3-5","6-10","10+")),
                                         h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                         selectInput("V_CL_Overlapping","Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed")))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
V_CL_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  if(input$V_CL_organisation=="Focal"){
    Table$Des1<-"Custom Focal Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,Foci,FociSize,Response:",paste(input$V_CL_Percent,paste(input$V_CL_Involve,collapse="/"),FreeFix(input$V_CL_Shape),"Focal",input$V_CL_Collapse,input$V_CL_Foci,paste(input$V_CL_FociSize,collapse="/"),input$V_CL_Response,sep=","))}
  if(input$V_CL_organisation=="Diffuse"){
    Table$Des1<-"Custom Diffuse Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,CorticalThining,Sites,Overlapping:",paste(input$V_CL_Percent,paste(input$V_CL_Involve,collapse="/"),FreeFix(input$V_CL_Shape),"Focal",input$V_CL_Collapse,input$V_CL_Thining,input$V_CL_Sites,input$V_CL_Overlapping,sep=","))}
  Table
}
#Cavitation of body######
V_L01_UI<-tagList(h3("ID:V_L01"),h4("Description:Cavitation of the vertebral body"))
V_L01_RC<-function(input=input){   
  Table<-data.frame(Des1="Cavitation of vertebral body",Size=NA,Nature=NA,Heal=NA)
  Table }

#Loss of body height######
V_L02_UI<-tagList(h3("ID:V_L02"),h4("Description:Loss of body height"))
V_L02_RC<-function(input=input){   
  Table<-data.frame(Des1="Loss of body height",Size=NA,Nature=NA,Heal=NA)
  Table }

#Circular endplate depressions######
V_L03_UI<-tagList(h3("ID:V_L03"),h4("Description:Circular depression(s) in endplate"))
V_L03_RC<-function(input=input){   
  Table<-data.frame(Des1="Circular depression(s) in endplate",Size=NA,Nature=NA,Heal=NA)
  Table }

#Scooped out lesions######
V_L04_UI<-tagList(h3("ID:V_L04"),h4("Description:Anteriorolateral scooped out lesion(s)"))
V_L04_RC<-function(input=input){   
  Table<-data.frame(Des1="Anteriorlateral scooped out lesion(s)",Size=NA,Nature=NA,Heal=NA)
  Table }

#Odontoid erosion######
V_L05_UI<-tagList(h3("ID:V_L05"),h4("Description:Erosion of odontoid"))
V_L05_RC<-function(input=input){   
  Table<-data.frame(Des1="Erosion of odontoid",Loc="Vertebrae:C2:NA",Feat="odontoid pro",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

