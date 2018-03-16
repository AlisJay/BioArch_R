#Custom#####
V_CS_UI<-tagList(h3("ID:V_CS"),h4("Description:Custom Shape Abnormality"),
                 column(width=4,
                        selectInput("V_CS_Degree","Degree of abnormality",c("Barely discernable","Clearly discernable")),
                        selectInput("V_CS_Type","Type of abnormality",c("Reduced size","Increased size","Bowing","Abnormal Width","Distortion or Absence of normal Feature","Failure of normal fusion","Other"))),
                 column(width=4,
                        conditionalPanel("input.V_CS_Type=='Reduced size'||input.V_CS_Type=='Increased size'",
                                         selectInput("V_CS_sLocal","Localised?",c("Yes,this is the only bone affected"="TRUE-only","yes,only a small number of articulated bones are effected"="TRUE-regional","no,multiple bones acrosss the skeleton are of reduced size"="FALSE")),
                                         selectInput("V_CS_sProportional","Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                         textInput("V_CS_sShape","further description of relative shape",value="none")),
                        conditionalPanel("input.V_CS_Type=='Bowing'",
                                         selectInput("V_CS_Bowing1","Type",c("Lordosis","Kyphosis","Scoliosis,left","Scoliosis,right","Other")),
                                         conditionalPanel("input.V_CS_Bowing1=='Other'",textInput("V_CS_Bowing1b","Define Other",value=NA)),
                                         selectInput("V_CS_Bowing2","Form",c("Reduced Anterior spinal body height","Anterior and posterior spinal body collapse","Missing spinal element","Other Malformation")),
                                         checkboxinput("V_CS_Bowing3","Ankylosis present",value=FALSE)),
                        conditionalPanel("input.V_CS_Type=='Distortion or Absence of normal Feature'",
                                         textInput("V_CS_daDes","Description",value="eg. Absent"),
                                         selectInput("V_CS_daSurface","Bone Surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                         conditionalPanel("input.V_CS_daSurface.indexOf('Other')>=0",textInput("V_CS_daSurface2","Define Other",value=NA))),
                        conditionalPanel("input.V_CS_Type=='Failure of normal fusion'",
                                         selectInput("V_CS_fFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.V_CS_fFusion=='Partial'",numericInput("V_CS_fFusion2","Approximate Percentage of suture fused",value=50))),
                        conditionalPanel("input.V_CS_Type=='Other'",textInput("V_CS_Other","Define other",value=NA))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM"))
)
V_CS_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
Table
}
#Anterior posterior curvature######
V_S01_UI<-tagList(h3("ID:V_S01"),h4("Description:Anterior posterior curvature"))
V_S01_RC<-function(input=input){   
  Table<-data.frame(Des1="Anterior posterior curvature",Size=NA,Nature=NA,Heal=NA)
  Table }

#Lateral curvature######
V_S02_UI<-tagList(h3("ID:V_S02"),h4("Description:Lateral curvature"))
V_S02_RC<-function(input=input){   
  Table<-data.frame(Des1="Lateral curvature",Size=NA,Nature=NA,Heal=NA)
  Table }

#Cleft centra######
V_S03_UI<-tagList(h3("ID:V_S03"),h4("Description:Cleft Centra"))
V_S03_RC<-function(input=input){   
  Table<-data.frame(Des1="Cleft Centra",Size=NA,Nature=NA,Heal=NA)
  Table }

#Sacralisation######
V_S04_UI<-tagList(h3("ID:V_S04"),h4("Description:Sacralisation"))
V_S04_RC<-function(input=input){   
  Table<-data.frame(Des1="Sacralisation",Size=NA,Nature=NA,Heal=NA)
  Table }

#Lumbarisation######
V_S05_UI<-tagList(h3("ID:V_S05"),h4("Description:Lumbarisation"))
V_S05_RC<-function(input=input){   
  Table<-data.frame(Des1="Lumbarisation",Size=NA,Nature=NA,Heal=NA)
  Table }

#Incomplete sacral arch######
V_S06_UI<-tagList(h3("ID:V_S06"),h4("Description:Incomplete closure of sacral neural arch"))
V_S06_RC<-function(input=input){   
  Table<-data.frame(Des1="Incomplete closure of sacral neural arch",Loc="Vertebrae:Sacrum:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Incomplete lumbar arch######
V_S07_UI<-tagList(h3("ID:V_S07"),h4("Description:Incomplete closure of lumbar neural arch"))
V_S07_RC<-function(input=input){   
  Table<-data.frame(Des1="Incomplete closure of lumbar neural arch",Size=NA,Nature=NA,Heal=NA)
  Table }

#Incomplete cervical/thorasic arch######
V_S08_UI<-tagList(h3("ID:V_S08"),h4("Description:Incomplete closure of cervical/thorasic neural arch"))
V_S08_RC<-function(input=input){   
  Table<-data.frame(Des1="Incomplete closure of cervical/thoracic neural arch",Size=NA,Nature=NA,Heal=NA)
  Table }

#Separation of arch and body######
V_S09_UI<-tagList(h3("ID:V_S09"),h4("Description:Separation of arch and body"))
V_S09_RC<-function(input=input){   
  Table<-data.frame(Des1="Separation of arch and body",Size=NA,Nature=NA,Heal=NA)
  Table }

#Supernummery bones######
V_S10_UI<-tagList(h3("ID:V_S10"),h4("Description:Supernumery bones"),
                  selectInput("V_S10_1","Type",multiple=TRUE,choice=c("Cervical","Thorasic","Lumbar","Sacral","cervical/thorasic borderline"="Cervical_Thorasic","thorasic/lumbar borderline"="Thorasic_Lumbar","lumbar/sacral borderline"="Lumbar_Sacral","unknown")),
                  column(width=6,
                         conditionalPanel("input.V_S10_1.indexOf('Cervical')>=0",strong("Cervical"),textInput("V_S10_Cervical","Numbering or additional postional information",value="Unknown")),
                         conditionalPanel("input.V_S10_1.indexOf('Thorasic')>=0",strong("Thorasic"),textInput("V_S10_Thorasic","Numbering or additional postional information",value="Unknown")),
                         conditionalPanel("input.V_S10_1.indexOf('Lumbar')>=0",strong("Lumbar"),textInput("V_S10_Lumbar","Numbering or additional postional information",value="Unknown")),
                         conditionalPanel("input.V_S10_1.indexOf('Sacral')>=0",strong("Sacral"),textInput("V_S10_Sacral","Numbering or additional postional information",value="Unknown"))
                  ))
V_S10_RC<-function(input=input){   
  Table<-data.frame(Des1="Supernummery bones",Size="NA:NA",Nature=NA,Heal=NA)
  if(length(input$V_S10_1)>0){
    Type<-paste(input$V_S10_1,collapse ="/")
    Postion<-NULL
    for(i in 1:length(input$V_S10_1)){
      if(input$V_S10_1[i] %in% c("Cervical","Thorasic","Lumbar","Sacral")){Position<-c(Position,input[[paste0("V_S10_",input$V_S10_1)]])
      }else{Position<-c(Position,"NA")}}
    Position<-paste(Position,collapse="/")
    Table$Nature<-paste0("Type,Position/Number:",paste(Type,Position,sep=","))}
  Table }

