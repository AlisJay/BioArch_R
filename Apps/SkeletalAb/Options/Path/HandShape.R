#Custom#####
H_CS_UI<-tagList(h3("ID:H_CS"),h4("Description:Custom Shape Abnormality"),
                 column(width=4,
                        selectInput("H_CS_Degree","Degree of abnormality",c("Barely discernable","Clearly discernable")),
                        selectInput("H_CS_Type","Type of abnormality",c("Reduced size","Increased size","Bowing","Angulation","Abnormal Width","Distortion or Absence of normal Feature","Depressed Area of Bone"="Depression","Outward projecting Area of Bone"="Outward projection","Premature/Abnormal fusion of elements"="Excess Fusion","Failure of normal fusion","Other"))),
                 column(width=4,
                        conditionalPanel("input.H_CS_Type=='Reduced size'||input.H_CS_Type=='Increased size'",
                                         selectInput("H_CS_sLocal","Localised?",c("Yes,this is the only bone affected"="TRUE-only","yes,only a small number of articulated bones are effected"="TRUE-regional","no,multiple bones acrosss the skeleton are of reduced size"="FALSE")),
                                         selectInput("H_CS_sProportional","Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                         textInput("H_CS_sShape","further description of relative shape",value="none")),
                        conditionalPanel("input.H_CS_Type=='Bowing'",
                                         selectInput("H_CS_Bowing1","Direction of bowing",c("Medial","Lateral","Anterior","Posterior","Unknown")),
                                         h4("Associated deformity"),
                                         checkboxInput("H_CS_Bowing2","Areas of increase cortical thickness",value=FALSE),
                                         checkboxInput("H_CS_Bowing3","Reduced midshaft circumference",value=FALSE),
                                         checkboxInput("H_CS_Bowing4","Areas of bone lysis",value=FALSE),
                                         checkboxInput("H_CS_Bowing5","Fracturing on convex surface",value=FALSE),
                                         checkboxInput("H_CS_Bowing6","Other pathological Fracturing",value=FALSE)),
                        conditionalPanel("input.H_CS_Type=='Angulation'",
                                         selectInput("H_CS_Angulation1","Direction of angulation",c("Medial","Lateral","anterior","posterior","superior","inferior")),
                                         selectInput("H_CS_Angulation2","Degree of angulation",c(">170","150-170","125-150","100-125","<100")),
                                         h5("nb the degree of angulation refers to the smallest angle between the long axis of the bone and the angulated portion. Therefore 180 would indicate no angulation (straight)"),
                                         selectInput("H_CS_Angulation3","relate to healed fracture?",c("Yes","No","Unknown"))),
                        conditionalPanel("input.H_CS_Type=='Abnormal Width'",
                                         selectInput("H_CS_Width1","Form of Abnormal Width",c("Flaring proximal metaphysis","Flaring distal metaphysis","Flaring of both metaphyses","Uniform increased width","Uniform reduced width","Spindle shaped","Other")),
                                         conditionalPanel("input.H_CS_Width1=='Other'",textInput("H_CS_Width2","Define Other",value=NA))),
                        conditionalPanel("input.H_CS_Type=='Distortion or Absence of normal Feature'",
                                         textInput("H_CS_daDes","Description",value="eg. Absent"),
                                         selectInput("H_CS_daBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         selectInput("H_CS_daSurface","Bone Surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                         conditionalPanel("input.H_CS_daSurface.indexOf('Other')>=0",textInput("H_CS_daSurface2","Define Other",value=NA))),
                        conditionalPanel("input.H_CS_Type=='Depression'",
                                         selectInput("H_CS_dDepth","Depth",c("Shallow-just decernable with the eye or palpertation","Mild-easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced-depression with a maximum depth of between 0.5mm and 1.5mm","Deep-depression in excess of 1.5mm")),
                                         textInput("H_CS_dShape","Description of shape",value="None"),
                                         selectInput("H_CS_dAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.H_CS_dAssociated.indexOf('Other')>=0",textInput("H_CS_dAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.H_CS_dAssociated",textInput("H_CS_dAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.H_CS_Type=='Outward projection'",
                                         selectInput("H_CS_bBilateral","Bilateral Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         textInput("H_CS_bShape","Description of shape",value="None"),
                                         selectInput("H_CS_bAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.H_CS_bAssociated.indexOf('Other')>=0",textInput("H_CS_bAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.H_CS_bAssociated",textInput("H_CS_bAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.H_CS_Type=='Failure of normal fusion'",
                                         selectInput("H_CS_fFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.H_CS_fFusion=='Partial'",numericInput("H_CS_fFusion2","Approximate Percentage of Epiphysis fused",value=50))),
                        conditionalPanel("input.H_CS_Type=='Other'",textInput("H_CS_Other","Define other",value=NA)),
                        conditionalPanel("input.H_CS_Type=='Excess Fusion'",
                                         selectInput("H_CS_pFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.H_CS_pFusion=='Partial'",numericInput("H_CS_pFusion2","Approximate Percentage of Epiphysis fused",value=50)),
                                         h5("Associated deformity"),
                                         h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                         checkboxInput("H_CS_pBulging1","Bulging of Epiphysis?",value=FALSE),
                                         checkboxInput("H_CS_pBulging2","Bulging of metaphysis?",value=FALSE),
                                         checkboxInput("H_CS_pKeel","Keel of bone along line of fusion?",value=FALSE),
                                         textInput("H_CS_pOther","Other Assocated Deformity",value="None"))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
H_CS_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
  Table$Des1<-paste("Custom shape abnormality-",input$H_CS_Type)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  NatureValues<-NULL;NatureName<-NULL
  if(input$H_CS_Type=="Reduced Size"|input$H_CS_Type =="Increased Size"){
    NatureValues<-paste(input$H_CS_Type,input$H_CS_Degree,input$H_CS_sLocal,input$H_CS_sProportional,FreeFix(input$H_CS_sShape),sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Localised,Proportional,Shape"}
  if(input$H_CS_Type=="Distortion or Absence of normal Feature"){
    surface<-IfIsOther(input$H_CS_daSurface,input$H_CS_daSurface2)
    NatureValues<-paste("Distortion or Absence of normal Feature",input$H_CS_daDes,input$H_CS_daBilateral,surface,sep=",")
    NatureName<-"ShapeType,Description,Bilateral,Surface"}
  if(input$H_CS_Type=="Depression"){
    if(length(input$H_CS_dAssociated)>0){
      RL<-FreeFix(input$H_CS_dAssociated3)
      AA<-IfHassOther(input$H_CS_dAssociated,input$H_CS_dAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Depression",input$H_CS_dDepth,paste(AA,collapse="/"),RL,sep=",")
    NatureName<-"ShapeType,Depth,AssociatedAbnormality,RelativeLocation"}
  if(input$H_CS_Type=="Outward projection"){
    if(length(input$H_CS_bAssociated)>0){
      RL<-FreeFix(input$H_CS_bAssociated3)
      AA<-IfHassOther(input$H_CS_bAssociated,input$H_CS_bAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Bulging/Outward projection",input$H_CS_bBilateral,input$H_CS_bShape,AA,RL,sep=",")
    NatureName<-"ShapeType,Bilateral,Shape,AssociatedAbnormality,RelativeLocation"}
  if(input$H_CS_Type=="Failure of normal fusion"){
    if(input$H_CS_fFusion=="Partial"){Fusion<-paste0("Partial-",input$H_CS_fFusion2)}else{Fusion<-input$H_CS_fFusion}
    NatureValues<-paste("Failure of normal Fusion",Fusion,sep=",")
    NatureName<-"ShapeType,Extent"}
  if(input$H_CS_Type=="Other"){
    NatureValues<-paste(FreeFix(input$H_CS_Other),input$H_CS_Degree,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree"}
  if(input$H_CS_Type=="Excess Fusion"){
    if(input$H_CS_pFusion=="Partial"){Fusion<-paste0("Partial-",input$H_CS_pFusion2)}else{Fusion<-input$H_CS_pFusion}
    NatureValues<-paste("Abnormal fusion",Fusion,input$H_CS_pBulging1,input$H_CS_pBulging2,input$H_CS_pKeel,FreeFix(input$H_CS_pOther),sep=",")
    NatureName<-"ShapeType,Extent,BulgingEpiphysis,BulgingMetaphysis,Keel,OtherAbnormality"}
  if(input$H_CS_Type=="Bowing"){
    if(input$H_CS_Bowing2){AA<-AddTo("AA","Areas of increase cortical thickness")}
    if(input$H_CS_Bowing3){AA<-AddTo("AA","Reduced Midshaft diameter")}
    if(input$H_CS_Bowing2){AA<-AddTo("AA","Areas of bone lysis")}
    if(input$H_CS_Bowing2){AA<-AddTo("AA","Fracture(s) on convex surface")}
    if(input$H_CS_Bowing2){AA<-AddTo("AA","Other pathological fractures")}
    if(exists("AA")){AA<-paste(AA,collapse="/")}else{AA<-"None"}
    NatureValues<-paste("Bowing",input$H_CS_Degree,input$H_CS_Bowing1,AA,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,AssociatedAbnormality"}
  if(input$H_CS_Type=="Abnormal Width"){
    WT<-IfIsOther(input$H_CS_Width1,input$H_CS_Width2)
    NatureValues<-paste("Abnormal width",input$H_CS_Degree,WT,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,WidthType"}
  if(input$H_CS_Type=="Angulation"){
    NatureValues<-paste("Angulation",input$H_CS_Degree,input$H_CS_Angulation1,input$H_CS_Angulation2,input$Angulation3,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,AngulationDegree,HealedFracture"}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  Table$Nature<-paste(NatureName,NatureValues,sep=":")
  Table
}
#Supernummery bones#########
H_S02_UI<-tagList(h3("ID:H_S02"),h4("Description:Supernumery bones"),
                            column(width=4,selectInput("H_S02_1","Left",multiple=TRUE,choice=c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                                               "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium"))),
                            column(width=4,selectInput("H_S02_2","Right",multiple=TRUE,choice=c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                                                "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium"))))
H_S02_RC<-function(input=input){   
  Table<-data.frame(Des1="Supernumery bones",Size="NA:NA",Nature=NA)
  left<-NA;right<-NA
  if(length(input$H_S02_1)>0){left<-paste(paste(input$H_S02_1,"l",sep="_"),collapse =",")}
  if(length(input$H_S02_2)>0){right<-paste(paste(input$H_S02_2,"r",sep="_"),collapse =",")}
  Table$Nature<-paste0("ShapeType,Right,Left:",paste("Supernummery",right,left,sep=","))
  Table }

