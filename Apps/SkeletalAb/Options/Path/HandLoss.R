#Custom#####
H_CL_UI<-tagList(h3("ID:H_CL"),h4("Description:Custom Loss Abnormality"),
                 column(width=4,
                        selectInput("H_CL_Percent","% of bone affected",c("<1/3","1/3-2/3",">2/3")),
                        selectizeInput("H_CL_Involve","Part(s) of bone involved",multiple=TRUE,choice=c("Cortical","Trabeculae","Periosteal","Endosteal","Subchondral")),
                        textInput("H_CL_Shape","Overall shape description"),
                        selectInput("H_CL_Organisation","Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                        checkboxInput("H_CL_Collapse","Associated structural collapse?",value=FALSE)),
                 column(width=4,
                        conditionalPanel("input.H_CL_organisation == 'Focal'",
                                         selectInput("H_CL_Foci","Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                         selectInput("H_CL_FociSize","Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                         selectInput("H_CL_Response","Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined"))),
                        conditionalPanel("input.H_CL_organisation == 'Diffuse'",
                                         checkboxInput("H_CL_Thining","Cortical thinning?",value=FALSE),
                                         selectInput("H_CL_Sites","Number of separate sites",c("1","2","3-5","6-10","10+")),
                                         h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                         selectInput("H_CL_Overlapping","Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed")))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
H_CL_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  if(input$H_CL_organisation=="Focal"){
    Table$Des1<-"Custom Focal Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,Foci,FociSize,Response:",paste(input$H_CL_Percent,paste(input$H_CL_Involve,collapse="/"),FreeFix(input$H_CL_Shape),"Focal",input$H_CL_Collapse,input$H_CL_Foci,paste(input$H_CL_FociSize,collapse="/"),input$H_CL_Response,sep=","))}
  if(input$H_CL_organisation=="Diffuse"){
    Table$Des1<-"Custom Diffuse Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,CorticalThining,Sites,Overlapping:",paste(input$H_CL_Percent,paste(input$H_CL_Involve,collapse="/"),FreeFix(input$H_CL_Shape),"Focal",input$H_CL_Collapse,input$H_CL_Thining,input$H_CL_Sites,input$H_CL_Overlapping,sep=","))}
  Table
}
#enlarged nutrient foramen#########
H_L01_UI<-tagList(h3("ID:H_L01"),h4("Description:Enlarged nutrient foramen"))
H_L01_RC<-function(input=input){   
  Table<-data.frame(Des1="Enlarged nutrient formen",Size=NA,Nature=NA,Heal=NA)
  Table }

#symetric joint erosion############
H_L02_UI<-tagList(h3("ID:H_L02"),h4("Description:Symetric erosion of metcarpophalangeal/interphalangeal joint(s)"))
H_L02_RC<-function(input=input){   
  Table<-data.frame(Des1="Symetric erosion of metacarpophalangeal/interphalangeal joint(s)",Size=NA,Nature=NA,Heal=NA)
  Table }

