#Custom#####
L_CS_UI<-tagList(h3("ID:L_CS"),h4("Description:Custom Shape Abnormality"),
                 column(width=4,
                        selectInput("L_CS_Degree","Degree of abnormality",c("Barely discernable","Clearly discernable")),
                        selectInput("L_CS_Type","Type of abnormality",c("Reduced size","Increased size","Bowing","Angulation","Abnormal Width","Distortion or Absence of normal Feature","Depressed Area of Bone"="Depression","Outward projecting Area of Bone"="Outward projection","Premature/Abnormal fusion of elements"="Excess Fusion","Failure of normal fusion","Other"))),
                 column(width=4,
                        conditionalPanel("input.L_CS_Type=='Reduced size'||input.L_CS_Type=='Increased size'",
                                         selectInput("L_CS_sLocal","Localised?",c("Yes,this is the only bone affected"="TRUE-only","yes,only a small number of articulated bones are effected"="TRUE-regional","no,multiple bones acrosss the skeleton are of reduced size"="FALSE")),
                                         selectInput("L_CS_sProportional","Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                         textInput("L_CS_sShape","further description of relative shape",value="none")),
                        conditionalPanel("input.L_CS_Type=='Bowing'",
                                         selectInput("L_CS_Bowing1","Direction of bowing",c("Medial","Lateral","Anterior","Posterior","Unknown")),
                                         h4("Associated deformity"),
                                         checkboxInput("L_CS_Bowing2","Areas of increase cortical thickness",value=FALSE),
                                         checkboxInput("L_CS_Bowing3","Reduced midshaft circumference",value=FALSE),
                                         checkboxInput("L_CS_Bowing4","Areas of bone lysis",value=FALSE),
                                         checkboxInput("L_CS_Bowing5","Fracturing on convex surface",value=FALSE),
                                         checkboxInput("L_CS_Bowing6","Other pathological Fracturing",value=FALSE)),
                        conditionalPanel("input.L_CS_Type=='Angulation'",
                                         selectInput("L_CS_Angulation1","Direction of angulation",c("Medial","Lateral","anterior","posterior","superior","inferior")),
                                         selectInput("L_CS_Angulation2","Degree of angulation",c(">170","150-170","125-150","100-125","<100")),
                                         h5("nb the degree of angulation refers to the smallest angle between the long axis of the bone and the angulated portion. Therefore 180 would indicate no angulation (straight)"),
                                         selectInput("L_CS_Angulation3","relate to healed fracture?",c("Yes","No","Unknown"))),
                        conditionalPanel("input.L_CS_Type=='Abnormal Width'",
                                         selectInput("L_CS_Width1","Form of Abnormal Width",c("Flaring proximal metaphysis","Flaring distal metaphysis","Flaring of both metaphyses","Uniform increased width","Uniform reduced width","Spindle shaped","Other")),
                                         conditionalPanel("input.L_CS_Width1=='Other'",textInput("L_CS_Width2","Define Other",value=NA))),
                        conditionalPanel("input.L_CS_Type=='Distortion or Absence of normal Feature'",
                                         textInput("L_CS_daDes","Description",value="eg. Absent"),
                                         selectInput("L_CS_daBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         selectInput("L_CS_daSurface","Bone Surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                         conditionalPanel("input.L_CS_daSurface.indexOf('Other')>=0",textInput("L_CS_daSurface2","Define Other",value=NA))),
                        conditionalPanel("input.L_CS_Type=='Depression'",
                                         selectInput("L_CS_dDepth","Depth",c("Shallow-just decernable with the eye or palpertation","Mild-easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced-depression with a maximum depth of between 0.5mm and 1.5mm","Deep-depression in excess of 1.5mm")),
                                         textInput("L_CS_dShape","Description of shape",value="None"),
                                         selectInput("L_CS_dAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.L_CS_dAssociated.indexOf('Other')>=0",textInput("L_CS_dAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.L_CS_dAssociated",textInput("L_CS_dAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.L_CS_Type=='Outward projection'",
                                         selectInput("L_CS_bBilateral","Bilateral Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         textInput("L_CS_bShape","Description of shape",value="None"),
                                         selectInput("L_CS_bAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.L_CS_bAssociated.indexOf('Other')>=0",textInput("L_CS_bAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.L_CS_bAssociated",textInput("L_CS_bAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.L_CS_Type=='Failure of normal fusion'",
                                         selectInput("L_CS_fFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.L_CS_fFusion=='Partial'",numericInput("L_CS_fFusion2","Approximate Percentage of Epiphysis fused",value=50))),
                        conditionalPanel("input.L_CS_Type=='Other'",textInput("L_CS_Other","Define other",value=NA)),
                        conditionalPanel("input.L_CS_Type=='Excess Fusion'",
                                         selectInput("L_CS_pFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.L_CS_pFusion=='Partial'",numericInput("L_CS_pFusion2","Approximate Percentage of Epiphysis fused",value=50)),
                                         h5("Associated deformity"),
                                         h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                         checkboxInput("L_CS_pBulging1","Bulging of Epiphysis?",value=FALSE),
                                         checkboxInput("L_CS_pBulging2","Bulging of metaphysis?",value=FALSE),
                                         checkboxInput("L_CS_pKeel","Keel of bone along line of fusion?",value=FALSE),
                                         textInput("L_CS_pOther","Other Assocated Deformity",value="None"))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
L_CS_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
  Table$Des1<-paste("Custom shape abnormality-",input$L_CS_Type)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  NatureValues<-NULL;NatureName<-NULL
  if(input$L_CS_Type=="Reduced Size"|input$L_CS_Type =="Increased Size"){
    NatureValues<-paste(input$L_CS_Type,input$L_CS_Degree,input$L_CS_sLocal,input$L_CS_sProportional,FreeFix(input$L_CS_sShape),sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Localised,Proportional,Shape"}
  if(input$L_CS_Type=="Distortion or Absence of normal Feature"){
    surface<-IfIsOther(input$L_CS_daSurface,input$L_CS_daSurface2)
    NatureValues<-paste("Distortion or Absence of normal Feature",input$L_CS_daDes,input$L_CS_daBilateral,surface,sep=",")
    NatureName<-"ShapeType,Description,Bilateral,Surface"}
  if(input$L_CS_Type=="Depression"){
    if(length(input$L_CS_dAssociated)>0){
      RL<-FreeFix(input$L_CS_dAssociated3)
      AA<-IfHassOther(input$L_CS_dAssociated,input$L_CS_dAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Depression",input$L_CS_dDepth,paste(AA,collapse="/"),RL,sep=",")
    NatureName<-"ShapeType,Depth,AssociatedAbnormality,RelativeLocation"}
  if(input$L_CS_Type=="Outward projection"){
    if(length(input$L_CS_bAssociated)>0){
      RL<-FreeFix(input$L_CS_bAssociated3)
      AA<-IfHassOther(input$L_CS_bAssociated,input$L_CS_bAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Bulging/Outward projection",input$L_CS_bBilateral,input$L_CS_bShape,AA,RL,sep=",")
    NatureName<-"ShapeType,Bilateral,Shape,AssociatedAbnormality,RelativeLocation"}
  if(input$L_CS_Type=="Failure of normal fusion"){
    if(input$L_CS_fFusion=="Partial"){Fusion<-paste0("Partial-",input$L_CS_fFusion2)}else{Fusion<-input$L_CS_fFusion}
    NatureValues<-paste("Failure of normal Fusion",Fusion,sep=",")
    NatureName<-"ShapeType,Extent"}
  if(input$L_CS_Type=="Other"){
    NatureValues<-paste(FreeFix(input$L_CS_Other),input$L_CS_Degree,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree"}
  if(input$L_CS_Type=="Excess Fusion"){
    if(input$L_CS_pFusion=="Partial"){Fusion<-paste0("Partial-",input$L_CS_pFusion2)}else{Fusion<-input$L_CS_pFusion}
    NatureValues<-paste("Abnormal fusion",Fusion,input$L_CS_pBulging1,input$L_CS_pBulging2,input$L_CS_pKeel,FreeFix(input$L_CS_pOther),sep=",")
    NatureName<-"ShapeType,Extent,BulgingEpiphysis,BulgingMetaphysis,Keel,OtherAbnormality"}
  if(input$L_CS_Type=="Bowing"){
    if(input$L_CS_Bowing2){AA<-AddTo("AA","Areas of increase cortical thickness")}
    if(input$L_CS_Bowing3){AA<-AddTo("AA","Reduced Midshaft diameter")}
    if(input$L_CS_Bowing2){AA<-AddTo("AA","Areas of bone lysis")}
    if(input$L_CS_Bowing2){AA<-AddTo("AA","Fracture(s) on convex surface")}
    if(input$L_CS_Bowing2){AA<-AddTo("AA","Other pathological fractures")}
    if(exists("AA")){AA<-paste(AA,collapse="/")}else{AA<-"None"}
    NatureValues<-paste("Bowing",input$L_CS_Degree,input$L_CS_Bowing1,AA,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,AssociatedAbnormality"}
  if(input$L_CS_Type=="Abnormal Width"){
    WT<-IfIsOther(input$L_CS_Width1,input$L_CS_Width2)
    NatureValues<-paste("Abnormal width",input$L_CS_Degree,WT,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,WidthType"}
  if(input$L_CS_Type=="Angulation"){
    NatureValues<-paste("Angulation",input$L_CS_Degree,input$L_CS_Angulation1,input$L_CS_Angulation2,input$Angulation3,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,AngulationDegree,HealedFracture"}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  Table$Nature<-paste(NatureName,NatureValues,sep=":")
  Table
}
#elongated lateral condyle###########
L_S01_UI<-tagList(h3("ID:L_S01"),h4("Description:Narrow elongated lateral condyle"))
L_S01_RC<-function(input=input){   
  Table<-data.frame(Des1="Narrow elongated lateral condyle",Size=NA,Nature=NA)
  Table }

#Deep trochlear fossa##########
L_S02_UI<-tagList(h3("ID:L_S02"),h4("Description:Deep trochlear fossa"))
L_S02_RC<-function(input=input){   
  Table<-data.frame(Des1="Deep trochlear fossa",Size=NA,Nature=NA)
  Table }

#Wide intercondylar fossa######
L_S03_UI<-tagList(h3("ID:L_S03"),h4("Description:Wide intercondylar fossa"))
L_S03_RC<-function(input=input){   
  Table<-data.frame(Des1="Wide intercondylar fossa",Size=NA,Nature=NA)
  Table }

#wide lateral articular facet######
L_S04_UI<-tagList(h3("ID:L_S04"),h4("Description:Large wide lateral articular facet"))
L_S04_RC<-function(input=input){   
  Table<-data.frame(,Des1="Large wide lateral articular facet",Size=NA,Nature=NA)
  Table }

#Small wide medial facet######
L_S05_UI<-tagList(h3("ID:L_S05"),h4("Description:Small wide medial articular facet"))
L_S05_RC<-function(input=input){   
  Table<-data.frame(Des1="Small wide medial articular facet",Size=NA,Nature=NA)
  Table }

#Bowing femur######
L_S06_UI<-tagList(h3("ID:L_S06"),h4("Description:Bowing femur"))
L_S06_RC<-function(input=input){   
  Table<-data.frame(Des1="Bowing femur",Size=NA,Nature=NA)
  Table }

#Bowing Tibia######
L_S07_UI<-tagList(h3("ID:L_S07"),h4("Description:Bowing Tibia"))
L_S07_RC<-function(input=input){   
  Table<-data.frame(Des1="Bowing tibia",Size=NA,Nature=NA)
  Table }

#elongated tibial spines######
L_S08_UI<-tagList(h3("ID:L_S08"),h4("Description:Elongated tibial spines"))
L_S08_RC<-function(input=input){   
  Table<-data.frame(Des1="Elongated tibial spines",Size=NA,Nature=NA)
  Table }

#fusion of tibia and femur######
L_S09_UI<-tagList(h3("ID:L_S09"),h4("Description:Fusion of tibia and femur"))
L_S09_RC<-function(input=input){   
  Table<-data.frame(Des1="Fusion of tibia and femur",Size=NA,Nature=NA)
  Table }

#Large superolateral notch######
L_S10_UI<-tagList(h3("ID:L_S10"),h4("Description:Large superolateral notch in patella"))
L_S10_RC<-function(input=input){   
  Table<-data.frame(Des1="Large superolateral notch in patella",Size=NA,Nature=NA)
  Table }

#Small superomedial notch######
L_S11_UI<-tagList(h3("ID:L_S11"),h4("Description:Small superomedial notch in patella"))
L_S11_RC<-function(input=input){   
  Table<-data.frame(Des1="Small superomedial notch in patella",Size=NA,Nature=NA)
  Table }

