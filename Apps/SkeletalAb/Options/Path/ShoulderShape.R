#Custom#####
SH_CS_UI<-tagList(h3("ID:SH_CS"),h4("Description:Custom Shape Abnormality"),
                 column(width=4,
                        selectInput("SH_CS_Degree","Degree of abnormality",c("Barely discernable","Clearly discernable")),
                        selectInput("SH_CS_Type","Type of abnormality",c("Reduced size","Increased size","Abnormal curvature","Abnormal clavicular width"="Abnormal Width","Distortion or Absence of normal Feature","Depressed Area of Bone"="Depression","Outward projecting Area of Bone"="Outward projection","Premature/Abnormal fusion of elements"="Excess Fusion","Failure of normal fusion","Other"))),
                 column(width=4,
                        conditionalPanel("input.SH_CS_Type=='Reduced size'||input.SH_CS_Type=='Increased size'",
                                         selectInput("SH_CS_sLocal","Localised?",c("Yes,this is the only bone affected"="TRUE-only","yes,only a small number of articulated bones are effected"="TRUE-regional","no,multiple bones acrosss the skeleton are of reduced size"="FALSE")),
                                         selectInput("SH_CS_sProportional","Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                         textInput("SH_CS_sShape","further description of relative shape",value="none")),
                        conditionalPanel("input.SH_CS_Type=='Abnormal curvature'",
                                         selectInput("SH_CS_Bowing1","Direction of Curvature",c("Medial","Lateral","Anterior","Posterior","Unknown","Multiple"="Other")),
                                         conditionalPanel("input.SH_CS_Bowing1=='Other'",textInput("SH_CS_Bowing1b","Describe Multiple",value=NA)),
                                         selectInput("SH_CS_Bowing2","Type of Curvature(select best description)",c("Bowing of bone edges","Abnormal level of curvature in normal direction","Curvature in an abnormal direction","Abnormal level of outward flaring of whole pelvic girdle","Straighting or loss of normal curvature","Other")),
                                         conditionalPanel("input.SH_CS_Bowing2=='Other'",textInput("SH_CS_Bowing2b","Define other",value=NA)),
                                         h4("Associated deformity"),
                                         h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                         checkboxInput("SH_CS_Bowing3","Areas of increase cortical thickness",value=FALSE),
                                         checkboxInput("SH_CS_Bowing5","Areas of bone lysis",value=FALSE),
                                         checkboxInput("SH_CS_Bowing6","Fracturing on convex surface",value=FALSE),
                                         checkboxInput("SH_CS_Bowing7","Other pathological Fracturing",value=FALSE)),
                        conditionalPanel("input.SH_CS_Type=='Distortion or Absence of normal Feature'",
                                         textInput("SH_CS_daDes","Description",value="eg. Absent"),
                                         selectInput("SH_CS_daBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         selectInput("SH_CS_daSurface","Bone Surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                         conditionalPanel("input.SH_CS_daSurface.indexOf('Other')>=0",textInput("SH_CS_daSurface2","Define Other",value=NA))),
                        conditionalPanel("input.SH_CS_Type=='Abnormal Width'",
                                         selectInput("A_CS_Width1","Form of Abnormal Width",c("Flaring medial metaphysis","Flaring lateral metaphysis","Flaring of both metaphyses","Uniform increased width","Uniform reduced width","Other")),
                                         conditionalPanel("input.A_CS_Width1=='Other'",textInput("A_CS_Width2","Define Other",value=NA))),
                        conditionalPanel("input.SH_CS_Type=='Depression'",
                                         selectInput("SH_CS_dDepth","Depth",c("Shallow-just decernable with the eye or palpertation","Mild-easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced-depression with a maximum depth of between 0.5mm and 1.5mm","Deep-depression in excess of 1.5mm")),
                                         textInput("SH_CS_dShape","Description of shape",value="None"),
                                         selectInput("SH_CS_dAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.SH_CS_dAssociated.indexOf('Other')>=0",textInput("SH_CS_dAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.SH_CS_dAssociated",textInput("SH_CS_dAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.SH_CS_Type=='Outward projection'",
                                         selectInput("SH_CS_bBilateral","Bilateral Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         textInput("SH_CS_bShape","Description of shape",value="None"),
                                         selectInput("SH_CS_bAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.SH_CS_bAssociated.indexOf('Other')>=0",textInput("SH_CS_bAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.SH_CS_bAssociated",textInput("SH_CS_bAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.SH_CS_Type=='Failure of normal fusion'",
                                         selectInput("SH_CS_fFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.SH_CS_fFusion=='Partial'",numericInput("SH_CS_fFusion2","Approximate Percentage fused",value=50))),
                        conditionalPanel("input.SH_CS_Type=='Other'",textInput("SH_CS_Other","Define other",value=NA)),
                        conditionalPanel("input.SH_CS_Type=='Excess Fusion'",
                                         selectInput("SH_CS_pFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.SH_CS_pFusion=='Partial'",numericInput("SH_CS_pFusion2","Approximate Percentage fused",value=50)),
                                         h5("Associated deformity"),
                                         h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                         checkboxInput("SH_CS_pBulging1","Bulging of Epiphysis?",value=FALSE),
                                         checkboxInput("SH_CS_pBulging2","Bulging of metaphysis?",value=FALSE),
                                         checkboxInput("SH_CS_pKeel","Keel of bone along line of fusion?",value=FALSE),
                                         textInput("SH_CS_pOther","Other Assocated Deformity",value="None"))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
SH_CS_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
  Table$Des1<-paste("Custom shape abnormality-",input$SH_CS_Type)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  NatureValues<-NULL;NatureName<-NULL
  if(input$SH_CS_Type=="Reduced Size"|input$SH_CS_Type =="Increased Size"){
    NatureValues<-paste(input$SH_CS_Type,input$SH_CS_Degree,input$SH_CS_sLocal,input$SH_CS_sProportional,FreeFix(input$SH_CS_sShape),sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Localised,Proportional,Shape"}
  if(input$SH_CS_Type=="Distortion or Absence of normal Feature"){
    surface<-IfIsOther(input$SH_CS_daSurface,input$SH_CS_daSurface2)
    NatureValues<-paste("Distortion or Absence of normal Feature",input$SH_CS_daDes,input$SH_CS_daBilateral,surface,sep=",")
    NatureName<-"ShapeType,Description,Bilateral,Surface"}
  if(input$SH_CS_Type=="Depression"){
    if(length(input$SH_CS_dAssociated)>0){
      RL<-FreeFix(input$SH_CS_dAssociated3)
      AA<-IfHassOther(input$SH_CS_dAssociated,input$SH_CS_dAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Depression",input$SH_CS_dDepth,paste(AA,collapse="/"),RL,sep=",")
    NatureName<-"ShapeType,Depth,AssociatedAbnormality,RelativeLocation"}
  if(input$SH_CS_Type=="Outward projection"){
    if(length(input$SH_CS_bAssociated)>0){
      RL<-FreeFix(input$SH_CS_bAssociated3)
      AA<-IfHassOther(input$SH_CS_bAssociated,input$SH_CS_bAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Bulging/Outward projection",input$SH_CS_bBilateral,input$SH_CS_bShape,AA,RL,sep=",")
    NatureName<-"ShapeType,Bilateral,Shape,AssociatedAbnormality,RelativeLocation"}
  if(input$SH_CS_Type=="Failure of normal fusion"){
    if(input$SH_CS_fFusion=="Partial"){Fusion<-paste0("Partial-",input$SH_CS_fFusion2)}else{Fusion<-input$SH_CS_fFusion}
    NatureValues<-paste("Failure of normal Fusion",Fusion,sep=",")
    NatureName<-"ShapeType,Extent"}
  if(input$SH_CS_Type=="Other"){
    NatureValues<-paste(FreeFix(input$SH_CS_Other),input$SH_CS_Degree,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree"}
  if(input$A_CS_Type=="Abnormal Width"){
    WT<-IfIsOther(input$A_CS_Width1,input$A_CS_Width2)
    NatureValues<-paste("Abnormal width",input$A_CS_Degree,WT,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,WidthType"}
  if(input$SH_CS_Type=="Excess Fusion"){
    if(input$SH_CS_pFusion=="Partial"){Fusion<-paste0("Partial-",input$SH_CS_pFusion2)}else{Fusion<-input$SH_CS_pFusion}
    NatureValues<-paste("Abnormal fusion",Fusion,input$SH_CS_pBulging1,input$SH_CS_pBulging2,input$SH_CS_pKeel,FreeFix(input$SH_CS_pOther),sep=",")
    NatureName<-"ShapeType,Extent,BulgingEpiphysis,BulgingMetaphysis,Keel,OtherAbnormality"}
  if(input$SH_CS_Type=="Abnormal curvature"){
    if(input$SH_CS_Bowing3){AA<-AddTo("AA","Areas of increase cortical thickness")}
    if(input$SH_CS_Bowing5){AA<-AddTo("AA","Areas of bone lysis")}
    if(input$SH_CS_Bowing6){AA<-AddTo("AA","Fracture(s) on convex surface")}
    if(input$SH_CS_Bowing7){AA<-AddTo("AA","Other pathological fractures")}
    if(exists("AA")){AA<-paste(AA,collapse="/")}else{AA<-"None"}
    CT<-IfIsOther(input$SH_CS_Bowing2,input$SH_CS_Bowing2b)
    Direction<-IfIsOther(input$SH_CS_Bowing1,input$SH_CS_Bowing1b)
    NatureValues<-paste("Bowing",input$SH_CS_Degree,Direction,CT,AA,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,CurvatureType,AssociatedAbnormality"}
  if(input$SH_CS_Type=="Abnormal Width"){
    WT<-IfIsOther(input$SH_CS_Width1,input$SH_CS_Width2)
    NatureValues<-paste("Abnormal width",input$SH_CS_Degree,WT,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,WidthType"}
  if(input$SH_CS_Type=="Angulation"){
    NatureValues<-paste("Angulation",input$SH_CS_Degree,input$SH_CS_Angulation1,input$SH_CS_Angulation2,input$Angulation3,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,AngulationDegree,HealedFracture"}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  Table$Nature<-paste(NatureName,NatureValues,sep=":")
  Table
}
#Separated acromion########
SH_S01_UI<-tagList(h3("ID:SH_S01"),h4("Description:Separated acromion"))
SH_S01_RC<-function(input=input){   
  Table<-data.frame(Des1="Separated acromion",Size=NA,Nature=NA,Heal=NA)
  Table }

