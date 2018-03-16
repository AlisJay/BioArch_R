#Custom#####
P_CL_UI<-tagList(h3("ID:P_CL"),h4("Description:Custom Loss Abnormality"),
                 column(width=4,
                        selectInput("P_CL_Percent","% of bone affected",c("<1/3","1/3-2/3",">2/3")),
                        selectizeInput("P_CL_Involve","Part(s) of bone involved",multiple=TRUE,choice=c("Cortical","Trabeculae","Periosteal","Endosteal","Subchondral")),
                        textInput("P_CL_Shape","Overall shape description"),
                        selectInput("P_CL_Organisation","Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                        checkboxInput("P_CL_Collapse","Associated structural collapse?",value=FALSE)),
                 column(width=4,
                        conditionalPanel("input.P_CL_organisation == 'Focal'",
                                         selectInput("P_CL_Foci","Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                         selectInput("P_CL_FociSize","Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                         selectInput("P_CL_Response","Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined"))),
                        conditionalPanel("input.P_CL_organisation == 'Diffuse'",
                                         checkboxInput("P_CL_Thining","Cortical thinning?",value=FALSE),
                                         selectInput("P_CL_Sites","Number of separate sites",c("1","2","3-5","6-10","10+")),
                                         h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                         selectInput("P_CL_Overlapping","Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed")))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
P_CL_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
  TableTable<-data.frame(Des1=NA,Size=NA,Nature=NA)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  if(input$P_CL_organisation=="Focal"){
    Table$Des1<-"Custom Focal Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,Foci,FociSize,Response:",paste(input$P_CL_Percent,paste(input$P_CL_Involve,collapse="/"),FreeFix(input$P_CL_Shape),"Focal",input$P_CL_Collapse,input$P_CL_Foci,paste(input$P_CL_FociSize,collapse="/"),input$P_CL_Response,sep=","))}
  if(input$P_CL_organisation=="Diffuse"){
    Table$Des1<-"Custom Diffuse Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,CorticalThining,Sites,Overlapping:",paste(input$P_CL_Percent,paste(input$P_CL_Involve,collapse="/"),FreeFix(input$P_CL_Shape),"Focal",input$P_CL_Collapse,input$P_CL_Thining,input$P_CL_Sites,input$P_CL_Overlapping,sep=","))}
  Table
}
#Enlarged nutrient foramina######
P_L01_UI<-tagList(h3("ID:P_L01"),
                  h4("Description:Enlarged nutrient foramina"),
                  column(width=4,
                         conditionalPanel("input.Bone.indexOf('OsCoxa')>=0",
                                          selectInput("P_L01_Bilateral","Bilateral?",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                          conditionalPanel("input.P_L01_Bilateral=='Bilateral'",
                                                           selectInput("P_L01_right","Right os Coxa  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           selectInput("P_L01_left","Left os Coxa  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                          conditionalPanel("input.P_L01_Bilateral=='Unilateral_r'",selectInput("P_L01_right","Right os Coxa  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                          conditionalPanel("input.P_L01_Bilateral=='Unilateral_l'",selectInput("P_L01_left","Left os Coxa  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                         conditionalPanel("input.Bone.indexOf('Sacrum')>=0",selectInput("P_L01_Sacrum","Sacrum relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))))
)
P_L01_RC<-function(input=input){   
  Table<-data.frame(Des1="Enlarged nutrient foramina",Size=NA,Nature="NA:NA",Heal=NA)
  right<-NA;left<-NA;Sacrum<-NA
  if(Contains(input$Bone,"OsCoxa")){
    Table$Nature<-paste("Bilateral",input$P_L01_Bilateral,sep=":")
    if(input$P_L01_Bilateral=="Bilateral"){right<-input$P_L01_right;left<-input$P_L01_left}
    if(input$P_L01_Bilateral=="Unilateral_l"){left<-input$P_L01_left}
    if(input$P_L01_Bilateral=="Unilateral_r"){right<-input$P_L01_right}}
  if(Contains(input$Bone,"Sacrum")){Sacrum<-input$P_L01_Sacrum}
  Table$Size<-paste0("relativeSize(right,left,Sacrum):",paste(right,left,Sacrum,sep=","))
  Table }

#Concavity of illiac fossa######
P_L02_UI<-tagList(h3("ID:P_L02"),h4("Description:Concavity/perforation of illiac fossa"))
P_L02_RC<-function(input=input){   
  Table<-data.frame(Des1="Concavity/perforation of illiac fossa",Size=NA,Nature=NA,Heal=NA)
  Table }

#Dorsal pubic depression######
P_L03_UI<-tagList(h3("ID:P_L03"),
                  h4("Description:Dorsal depression(s) on pubic symphyses"),
                  selectInput("P_L03_1","Side",multiple=TRUE,selected="_r",c("Left"="_l","Right"="_r")),
                  column(width=4,conditionalPanel("input.P_L03_1.indexOf('_r')>=0",h4("Right"),
                                                  selectInput("P_L03_2_r","Shape (select most approprate)",c("Linear","Circular","Irregular")),
                                                  numericInput("P_L03_3_r","Maximum superior inferior length(mm)",value=NA),
                                                  numericInput("P_L03_4_r","Maximum Mediolateral length (mm)",value=NA),
                                                  numericInput("P_L03_5_r","Minimum anterior posterior width within the depression(mm)",value=NA),
                                                  numericInput("P_L03_6_r","Anterior posterior width outside the depression (mm)",value=NA),
                                                  selectInput("P_L03_7_r","Edges (select all applicable)",multiple=TRUE,selected="Sharp",c("Sharp","Rounded","Irregular","Pitted")),
                                                  checkboxInput("P_L03_8_r","Pitting within depression?",value=FALSE))),
                  column(width=4,conditionalPanel("input.P_L03_1.indexOf('_l')>=0",h4("Left"),
                                                  selectInput("P_L03_2_l","Shape (select most approprate)",c("Linear","Circular","Irregular")),
                                                  numericInput("P_L03_3_l","Maximum superior inferior length(mm)",value=NA),
                                                  numericInput("P_L03_4_l","Maximum Mediolateral length (mm)",value=NA),
                                                  numericInput("P_L03_5_l","Minimum anterior posterior width within the depression(mm)",value=NA),
                                                  numericInput("P_L03_6_l","Anterior posterior width outside the depression (mm)",value=NA),
                                                  selectInput("P_L03_7_l","Edges (select all applicable)",multiple=TRUE,selected="Sharp",c("Sharp","Rounded","Irregular","Pitted")),
                                                  checkboxInput("P_L03_8_l","Pitting within depression?",value=FALSE)))
)
P_L03_RC<-function(input=input){   
  Table<-data.frame(Des1="Dorsal depression(s) on pubic symphyses",Size=NA,Nature=NA,Heal=NA)
  if(length(input$P_L03_1)==2){
    Table$Size<-paste0("SupInf,MedLat,AntPos(in),AntPos(out):",paste(paste(input$P_L03_3_r,input$P_L03_3_l,sep="/"),paste(input$P_L03_4_r,input$P_L03_4_l,sep="/"),paste(input$P_L03_5_r,input$P_L03_5_l,sep="/"),paste(input$P_L03_6_r,input$P_L03_6_l,sep="/"),sep=","))
    Table$Nature<-paste0("Shape,Edges,Internal Pitting:",paste(paste(input$P_L03_2_r,input$P_L03_2_l,sep="/"),paste(paste(input$P_L03_7_r,collapse="_"),paste(input$P_L03_7_l,collapse="_"),sep="/"),paste(input$P_L03_8_r,input$P_L03_8_l,sep="/"),sep=","))
  }else{
    Table$Size<-paste0("SupInf,MedLat,AntPos(in),AntPos(out):",paste(input[[as.character(paste0("P_L03_3",input$P_L03_1))]],input[[as.character(paste0("P_L03_4",input$P_L03_1))]],input[[as.character(paste0("P_L03_5",input$P_L03_1))]],input[[as.character(paste0("P_L03_6",input$P_L03_1))]],sep=","))
    Table$Nature<-paste0("Shape,Edges,Internal Pitting:",paste(input[[as.character(paste0("P_L03_2",input$P_L03_1))]],paste(input[[as.character(paste0("P_L03_7",input$P_L03_1))]],collapse="_"),input[[as.character(paste0("P_L03_8",input$P_L03_1))]],sep=","))
  }
  Table }

