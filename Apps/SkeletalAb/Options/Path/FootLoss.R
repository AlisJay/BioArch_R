#Custom#####
F_CL_UI<-tagList(h3("ID:F_CL"),h4("Description:Custom Loss Abnormality"),
                 column(width=4,
                        selectInput("F_CL_Percent","% of bone affected",c("<1/3","1/3-2/3",">2/3")),
                        selectizeInput("F_CL_Involve","Part(s) of bone involved",multiple=TRUE,choice=c("Cortical","Trabeculae","Periosteal","Endosteal","Subchondral")),
                        textInput("F_CL_Shape","Overall shape description"),
                        selectInput("F_CL_Organisation","Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                        checkboxInput("F_CL_Collapse","Associated structural collapse?",value=FALSE)),
                 column(width=4,
                        conditionalPanel("input.F_CL_organisation == 'Focal'",
                                         selectInput("F_CL_Foci","Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                         selectInput("F_CL_FociSize","Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                         selectInput("F_CL_Response","Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined"))),
                        conditionalPanel("input.F_CL_organisation == 'Diffuse'",
                                         checkboxInput("F_CL_Thining","Cortical thinning?",value=FALSE),
                                         selectInput("F_CL_Sites","Number of separate sites",c("1","2","3-5","6-10","10+")),
                                         h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                         selectInput("F_CL_Overlapping","Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed")))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
F_CL_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  if(input$F_CL_organisation=="Focal"){
    Table$Des1<-"Custom Focal Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,Foci,FociSize,Response:",paste(input$F_CL_Percent,paste(input$F_CL_Involve,collapse="/"),FreeFix(input$F_CL_Shape),"Focal",input$F_CL_Collapse,input$F_CL_Foci,paste(input$F_CL_FociSize,collapse="/"),input$F_CL_Response,sep=","))}
  if(input$F_CL_organisation=="Diffuse"){
    Table$Des1<-"Custom Diffuse Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,CorticalThining,Sites,Overlapping:",paste(input$F_CL_Percent,paste(input$F_CL_Involve,collapse="/"),FreeFix(input$F_CL_Shape),"Focal",input$F_CL_Collapse,input$F_CL_Thining,input$F_CL_Sites,input$F_CL_Overlapping,sep=","))}
  Table
}
#Destruction of small bones##############################################################################################
F_L01_UI<-tagList(h3("ID:F_L01"),h4("Description:Destruction of metatarsals/phalanges"))
F_L01_RC<-function(input=input){   
  Table<-data.frame(Des1="Destruction of metatarsals/phalanges",Size=NA,Nature=NA,Heal=NA)
  Table }

#Enlarged Nutrient foramen##############################################################################################
F_L02_UI<-tagList(h3("ID:F_L02"),h4("Description:Enlarged nutrient foramen"),
                  column(width=4,
                         conditionalPanel("input.Bone.indexOf('Talus')>=0",strong("Talus"),
                                          selectInput("Talus_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                          selectInput("Talus_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                          conditionalPanel("input.Talus_Bilateral=='Bilateral'",selectInput("Talus_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                         conditionalPanel("input.Bone.indexOf('Calcaneous')>=0",strong("Calcaneous"),
                                          selectInput("Calcaneous_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                          selectInput("Calcaneous_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                          conditionalPanel("input.Calcaneous_Bilateral=='Bilateral'",selectInput("Calcaneous_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                         conditionalPanel("input.Bone.indexOf('Navicular')>=0",strong("Navicular"),
                                          selectInput("Navicular_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                          selectInput("Navicular_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                          conditionalPanel("input.Navicular_Bilateral=='Bilateral'",selectInput("Navicular_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                         conditionalPanel("input.Bone.indexOf('Cuboid')>=0",strong("Cuboid"),
                                          selectInput("Cuboid_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                          selectInput("Cuboid_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                          conditionalPanel("input.Cuboid_Bilateral=='Bilateral'",selectInput("Cuboid_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                         conditionalPanel("input.Bone.indexOf('Cuniform')>=0",
                                          conditionalPanel("input.Cuniform_number.indexOf('_med')>=0",strong("Medial Cuniform"),
                                                           selectInput("Cuniform_med_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Cuniform_med_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Cuniform_med_Bilateral=='Bilateral'",selectInput("Cuniform_med_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Cuniform_number.indexOf('_inter')>=0",strong("Intermediate Cuniform"),
                                                           selectInput("Cuniform_inter_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Cuniform_inter_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Cuniform_inter_Bilateral=='Bilateral'",selectInput("Cuniform_inter_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Cuniform_number.indexOf('_lat')>=0",strong("Lateral Cuniform"),
                                                           selectInput("Cuniform_lat_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Cuniform_lat_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Cuniform_lat_Bilateral=='Bilateral'",selectInput("Cuniform_lat_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))))
                         )),
                  column(width=4,
                         conditionalPanel("input.Bone.indexOf('Metatarsal')>=0",
                                          conditionalPanel("input.Metatarsal_number.indexof('_1')>=0",strong("Metatarsal 1"),
                                                           selectInput("Metatarsal_1_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Metatarsal_1_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Metatarsal_1_Bilateral=='Bilateral'",selectInput("Metatarsal_1_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Metatarsal_number.indexof('_2')>=0",strong("Metatarsal 2"),
                                                           selectInput("Metatarsal_2_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Metatarsal_2_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Metatarsal_2_Bilateral=='Bilateral'",selectInput("Metatarsal_2_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Metatarsal_number.indexof('_3')>=0",strong("Metatarsal 3"),
                                                           selectInput("Metatarsal_3_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Metatarsal_3_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Metatarsal_3_Bilateral=='Bilateral'",selectInput("Metatarsal_3_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Metatarsal_number.indexof('_4')>=0",strong("Metatarsal 4"),
                                                           selectInput("Metatarsal_4_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Metatarsal_4_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Metatarsal_4_Bilateral=='Bilateral'",selectInput("Metatarsal_4_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Metatarsal_number.indexof('_5')>=0",strong("Metatarsal 5"),
                                                           selectInput("Metatarsal_5_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Metatarsal_5_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Metatarsal_5_Bilateral=='Bilateral'",selectInput("Metatarsal_5_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))))
                         )),
                  column(width=4,
                         conditionalPanel("input.Bone.indexOf('Phalanx')>=0",
                                          conditionalPanel("input.Phalanx_number.indexOf('_p1')>=0",strong("Proximal Phalanx 1"),
                                                           selectInput("Phalanx_p1_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_p1_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_p1_Bilateral=='Bilateral'",selectInput("Phalanx_p1_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_p2')>=0",strong("Proximal Phalanx 2"),
                                                           selectInput("Phalanx_p2_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_p2_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_p2_Bilateral=='Bilateral'",selectInput("Phalanx_p2_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_p3')>=0",strong("Proximal Phalanx 3"),
                                                           selectInput("Phalanx_p3_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_p3_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_p3_Bilateral=='Bilateral'",selectInput("Phalanx_p3_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_p4')>=0",strong("Proximal Phalanx 4"),
                                                           selectInput("Phalanx_p4_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_p4_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_p4_Bilateral=='Bilateral'",selectInput("Phalanx_p4_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_p5')>=0",strong("Proximal Phalanx 5"),
                                                           selectInput("Phalanx_p5_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_p5_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_p5_Bilateral=='Bilateral'",selectInput("Phalanx_p5_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_d1')>=0",strong("Distal Phalanx 1"),
                                                           selectInput("Phalanx_d1_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_d1_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_d1_Bilateral=='Bilateral'",selectInput("Phalanx_d1_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_d2')>=0",strong("Distal Phalanx 2"),
                                                           selectInput("Phalanx_d2_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_d2_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_d2_Bilateral=='Bilateral'",selectInput("Phalanx_d2_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_d3')>=0",strong("Distal Phalanx 3"),
                                                           selectInput("Phalanx_d3_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_d3_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_d3_Bilateral=='Bilateral'",selectInput("Phalanx_d3_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_d4')>=0",strong("Distal Phalanx 4"),
                                                           selectInput("Phalanx_d4_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_d4_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_d4_Bilateral=='Bilateral'",selectInput("Phalanx_d4_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_d5')>=0",strong("Distal Phalanx 5"),
                                                           selectInput("Phalanx_d5_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_d5_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_d5_Bilateral=='Bilateral'",selectInput("Phalanx_d5_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_i2')>=0",strong("Intermediate Phalanx 2"),
                                                           selectInput("Phalanx_i2_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_i2_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_i2_Bilateral=='Bilateral'",selectInput("Phalanx_i2_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_i3')>=0",strong("Intermediate Phalanx 3"),
                                                           selectInput("Phalanx_i3_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_i3_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_i3_Bilateral=='Bilateral'",selectInput("Phalanx_i3_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_i4')>=0",strong("Intermediate Phalanx 4"),
                                                           selectInput("Phalanx_i4_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_i4_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_i4_Bilateral=='Bilateral'",selectInput("Phalanx_i4_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                                          conditionalPanel("input.Phalanx_number.indexOf('_i5')>=0",strong("Intermediate Phalanx 5"),
                                                           selectInput("Phalanx_i5_Bilateral","BiLateral",c("Yes"="Bilateral","No left only"="Unilateral_l","No right only"="Unilateral_r")),
                                                           selectInput("Phalanx_i5_Size","Relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")),
                                                           conditionalPanel("input.Phalanx_i5_Bilateral=='Bilateral'",selectInput("Phalanx_i5_Size2","Left relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))))
                                          ))
                  )
                         
                  
F_L02_RC<-function(input=input){   
  Table<-data.frame(Des1="Enlarged nutrient foramen",Size=NA,Nature=NA,Heal="NA:NA")
  bones<-input$Bone[input$Bone %in% c("Talus","Calcaneous","Navicular","Cuboid")]
  if(Contains(input$Bone,"Phalanx")){bones<-c(bones,paste("Phalanx",input$Phalanx_number))}
  if(Contains(input$Bone,"Metatarsal")){bones<-c(bones,paste("Metatarsal",input$Metatarsal_number))}
  if(Contains(input$Bone,"Cuniform")){bones<-c(bones,paste("Cuniform",input$Cuniform_number))}
  Bilateral<-NULL
  RelativeSize<-NULL
  for(i in 1:length(bones)){
    Bilateral<-c(Bilateral,input[[paste0(bones[i],"_Bilateral")]])
    if(input[[paste0(bones[i],"_Bilateral")]]=="Bilateral"){RelativeSize<-c(RelativeSize,paste(input[[paste0(bones[i],"_size")]],input[[paste0(bones[i],"_Size2")]],sep="/"))
    }else{RelativeSize<-c(RelativeSize,input[[paste0(bones[i],"_size")]])}
  }
  Table$Size<-paste("RealtiveSize",paste(RelativeSize,collapse=","),sep=":")
  Table$Nature<-paste("Bilateral",paste(Bilateral,collapse=","),sep=":")
  Table }

#Symetric joint erosion#########
F_L03_UI<-tagList(h3("ID:F_L03"),h4("Description:Symetric erosion of metatarsophalangeal or interphalangeal joint(s)"))
F_L03_RC<-function(input=input){   
  Table<-data.frame(Des1="Symetric erosion of metatarsophalangeal or interphalangeal joint(s)",Size=NA,Nature=NA,Heal=NA)
  Table }

#Asymetric joint erosion####
F_L04_UI<-tagList(h3("ID:F_L04"),h4("Description:Asymetric erosion of metatarsophalangeal or interphalangeal joint(s)"))
F_L04_RC<-function(input=input){   
  Table<-data.frame(Des1="Asymetric erosion of metatarsophalageal or interphalangeal joint(s)",Size=NA,Nature=NA,Heal=NA)
  Table }

