#Custom#####
F_CS_UI<-tagList(h3("ID:F_CS"),h4("Description:Custom Shape Abnormality"),
                 column(width=4,
                        selectInput("F_CS_Degree","Degree of abnormality",c("Barely discernable","Clearly discernable")),
                        selectInput("F_CS_Type","Type of abnormality",c("Reduced size","Increased size","Bowing","Angulation","Abnormal Width","Distortion or Absence of normal Feature","Depressed Area of Bone"="Depression","Outward projecting Area of Bone"="Outward projection","Premature/Abnormal fusion of elements"="Excess Fusion","Failure of normal fusion","Other"))),
                 column(width=4,
                        conditionalPanel("input.F_CS_Type=='Reduced size'||input.F_CS_Type=='Increased size'",
                                         selectInput("F_CS_sLocal","Localised?",c("Yes,this is the only bone affected"="TRUE-only","yes,only a small number of articulated bones are effected"="TRUE-regional","no,multiple bones acrosss the skeleton are of reduced size"="FALSE")),
                                         selectInput("F_CS_sProportional","Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                         textInput("F_CS_sShape","further description of relative shape",value="none")),
                        conditionalPanel("input.F_CS_Type=='Bowing'",
                                         selectInput("F_CS_Bowing1","Direction of bowing",c("Medial","Lateral","Anterior","Posterior","Unknown")),
                                         h4("Associated deformity"),
                                         checkboxInput("F_CS_Bowing2","Areas of increase cortical thickness",value=FALSE),
                                         checkboxInput("F_CS_Bowing3","Reduced midshaft circumference",value=FALSE),
                                         checkboxInput("F_CS_Bowing4","Areas of bone lysis",value=FALSE),
                                         checkboxInput("F_CS_Bowing5","Fracturing on convex surface",value=FALSE),
                                         checkboxInput("F_CS_Bowing6","Other pathological Fracturing",value=FALSE)),
                        conditionalPanel("input.F_CS_Type=='Angulation'",
                                         selectInput("F_CS_Angulation1","Direction of angulation",c("Medial","Lateral","anterior","posterior","superior","inferior")),
                                         selectInput("F_CS_Angulation2","Degree of angulation",c(">170","150-170","125-150","100-125","<100")),
                                         h5("nb the degree of angulation refers to the smallest angle between the long axis of the bone and the angulated portion. Therefore 180 would indicate no angulation (straight)"),
                                         selectInput("F_CS_Angulation3","relate to healed fracture?",c("Yes","No","Unknown"))),
                        conditionalPanel("input.F_CS_Type=='Abnormal Width'",
                                         selectInput("F_CS_Width1","Form of Abnormal Width",c("Flaring proximal metaphysis","Flaring distal metaphysis","Flaring of both metaphyses","Uniform increased width","Uniform reduced width","Spindle shaped","Other")),
                                         conditionalPanel("input.F_CS_Width1=='Other'",textInput("F_CS_Width2","Define Other",value=NA))),
                        conditionalPanel("input.F_CS_Type=='Distortion or Absence of normal Feature'",
                                         textInput("F_CS_daDes","Description",value="eg. Absent"),
                                         selectInput("F_CS_daBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         selectInput("F_CS_daSurface","Bone Surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                         conditionalPanel("input.F_CS_daSurface.indexOf('Other')>=0",textInput("F_CS_daSurface2","Define Other",value=NA))),
                        conditionalPanel("input.F_CS_Type=='Depression'",
                                         selectInput("F_CS_dDepth","Depth",c("Shallow-just decernable with the eye or palpertation","Mild-easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced-depression with a maximum depth of between 0.5mm and 1.5mm","Deep-depression in excess of 1.5mm")),
                                         textInput("F_CS_dShape","Description of shape",value="None"),
                                         selectInput("F_CS_dAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.F_CS_dAssociated.indexOf('Other')>=0",textInput("F_CS_dAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.F_CS_dAssociated",textInput("F_CS_dAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.F_CS_Type=='Outward projection'",
                                         selectInput("F_CS_bBilateral","Bilateral Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                         textInput("F_CS_bShape","Description of shape",value="None"),
                                         selectInput("F_CS_bAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Cortical Thickening","Expansion/thickening of Trabeculae","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                         conditionalPanel("input.F_CS_bAssociated.indexOf('Other')>=0",textInput("F_CS_bAssociated2","Define Other",value=NA)),
                                         conditionalPanel("input.F_CS_bAssociated",textInput("F_CS_bAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                        conditionalPanel("input.F_CS_Type=='Failure of normal fusion'",
                                         selectInput("F_CS_fFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.F_CS_fFusion=='Partial'",numericInput("F_CS_fFusion2","Approximate Percentage of Epiphysis fused",value=50))),
                        conditionalPanel("input.F_CS_Type=='Other'",textInput("F_CS_Other","Define other",value=NA)),
                        conditionalPanel("input.F_CS_Type=='Excess Fusion'",
                                         selectInput("F_CS_pFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                         conditionalPanel("input.F_CS_pFusion=='Partial'",numericInput("F_CS_pFusion2","Approximate Percentage of Epiphysis fused",value=50)),
                                         h5("Associated deformity"),
                                         h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                         checkboxInput("F_CS_pBulging1","Bulging of Epiphysis?",value=FALSE),
                                         checkboxInput("F_CS_pBulging2","Bulging of metaphysis?",value=FALSE),
                                         checkboxInput("F_CS_pKeel","Keel of bone along line of fusion?",value=FALSE),
                                         textInput("F_CS_pOther","Other Assocated Deformity",value="None"))),
                 column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                        numericInput("CustomMeasure","Number of measurments",value=NA),
                        actionButton("AddCM","Add fields"),
                        uiOutput("CM")))
F_CS_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
  Table$Des1<-paste("Custom shape abnormality-",input$F_CS_Type)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  NatureValues<-NULL;NatureName<-NULL
  if(input$F_CS_Type=="Reduced Size"|input$F_CS_Type =="Increased Size"){
    NatureValues<-paste(input$F_CS_Type,input$F_CS_Degree,input$F_CS_sLocal,input$F_CS_sProportional,FreeFix(input$F_CS_sShape),sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Localised,Proportional,Shape"}
  if(input$F_CS_Type=="Distortion or Absence of normal Feature"){
    surface<-IfIsOther(input$F_CS_daSurface,input$F_CS_daSurface2)
    NatureValues<-paste("Distortion or Absence of normal Feature",input$F_CS_daDes,input$F_CS_daBilateral,surface,sep=",")
    NatureName<-"ShapeType,Description,Bilateral,Surface"}
  if(input$F_CS_Type=="Depression"){
    if(length(input$F_CS_dAssociated)>0){
      RL<-FreeFix(input$F_CS_dAssociated3)
      AA<-IfHassOther(input$F_CS_dAssociated,input$F_CS_dAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Depression",input$F_CS_dDepth,paste(AA,collapse="/"),RL,sep=",")
    NatureName<-"ShapeType,Depth,AssociatedAbnormality,RelativeLocation"}
  if(input$F_CS_Type=="Outward projection"){
    if(length(input$F_CS_bAssociated)>0){
      RL<-FreeFix(input$F_CS_bAssociated3)
      AA<-IfHassOther(input$F_CS_bAssociated,input$F_CS_bAssociated2)
    }else{RL<-NA;AA<-"None"}
    NatureValues<-paste("Bulging/Outward projection",input$F_CS_bBilateral,input$F_CS_bShape,AA,RL,sep=",")
    NatureName<-"ShapeType,Bilateral,Shape,AssociatedAbnormality,RelativeLocation"}
  if(input$F_CS_Type=="Failure of normal fusion"){
    if(input$F_CS_fFusion=="Partial"){Fusion<-paste0("Partial-",input$F_CS_fFusion2)}else{Fusion<-input$F_CS_fFusion}
    NatureValues<-paste("Failure of normal Fusion",Fusion,sep=",")
    NatureName<-"ShapeType,Extent"}
  if(input$F_CS_Type=="Other"){
    NatureValues<-paste(FreeFix(input$F_CS_Other),input$F_CS_Degree,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree"}
  if(input$F_CS_Type=="Excess Fusion"){
    if(input$F_CS_pFusion=="Partial"){Fusion<-paste0("Partial-",input$F_CS_pFusion2)}else{Fusion<-input$F_CS_pFusion}
    NatureValues<-paste("Abnormal fusion",Fusion,input$F_CS_pBulging1,input$F_CS_pBulging2,input$F_CS_pKeel,FreeFix(input$F_CS_pOther),sep=",")
    NatureName<-"ShapeType,Extent,BulgingEpiphysis,BulgingMetaphysis,Keel,OtherAbnormality"}
  if(input$F_CS_Type=="Bowing"){
    if(input$F_CS_Bowing2){AA<-AddTo("AA","Areas of increase cortical thickness")}
    if(input$F_CS_Bowing3){AA<-AddTo("AA","Reduced Midshaft diameter")}
    if(input$F_CS_Bowing2){AA<-AddTo("AA","Areas of bone lysis")}
    if(input$F_CS_Bowing2){AA<-AddTo("AA","Fracture(s) on convex surface")}
    if(input$F_CS_Bowing2){AA<-AddTo("AA","Other pathological fractures")}
    if(exists("AA")){AA<-paste(AA,collapse="/")}else{AA<-"None"}
    NatureValues<-paste("Bowing",input$F_CS_Degree,input$F_CS_Bowing1,AA,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,AssociatedAbnormality"}
  if(input$F_CS_Type=="Abnormal Width"){
    WT<-IfIsOther(input$F_CS_Width1,input$F_CS_Width2)
    NatureValues<-paste("Abnormal width",input$F_CS_Degree,WT,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,WidthType"}
  if(input$F_CS_Type=="Angulation"){
    NatureValues<-paste("Angulation",input$F_CS_Degree,input$F_CS_Angulation1,input$F_CS_Angulation2,input$Angulation3,sep=",")
    NatureName<-"ShapeType,AbnormalityDegree,Direction,AngulationDegree,HealedFracture"}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  Table$Nature<-paste(NatureName,NatureValues,sep=":")
  Table
}
#Supernummery bones#####
F_S01_UI<-tagList(h3("ID:F_S01"),h4("Description:Supernummery bones"),
                    column(width=4,selectInput("F_S01_1","Left",multiple=TRUE,choice=c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid"))),
                    column(width=4,selectInput("F_S01_2","Right",multiple=TRUE,choice=c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid")))
                    )
F_S01_RC<-function(input=input){   
  Table<-data.frame(Des1="Supernummery bones",Size="NA:NA",Nature=NA)
  left<-NA;right<-NA
  if(length(input$F_S01_1)>0){left<-paste(paste(input$F_S01_1,"l",sep="_"),collapse =",")}
  if(length(input$F_S01_2)>0){right<-paste(paste(input$F_S01_2,"r",sep="_"),collapse =",")}
  Table$Nature<-paste0("ShapeType,Right,Left:",paste("Supernummery",right,left,sep=","))
  Table }
#Rotation of talus and calcaneous#######
F_S02_UI<-tagList(h3("ID:F_S02"),h4("Description:Rotation and malformation of talus and calcaneous"))
F_S02_RC<-function(input=input){   
  Table<-data.frame(Des1="Rotation and malformation of talus and calcaneous",Size=NA,Nature=NA)
  Table }
#Fusion of talus and calcaneous#######
F_S03_UI<-tagList(h3("ID:F_S03"),h4("Description:Fusion of talus and calcaneous"))
F_S03_RC<-function(input=input){   
  Table<-data.frame(Des1="Fusion of talus and calcaneous",Size=NA,Nature=NA)
  Table }
#Fusion of Tibia and Talus##########
F_S04_UI<-tagList(h3("ID:F_S04"),h4("Description:Fusion of Tibia and Talus"))
F_S04_RC<-function(input=input){   
  Table<-data.frame(Des1="Fusion of tibia and talus",Size=NA,Nature=NA)
  Table }

#Interphalangeal fusion##############################################################################################
F_S05_UI<-tagList(h3("ID:F_S05"),h4("Description:Fusion of interphalangeal joint(s)"),
                            selectInput("F_S05_1","Bone",multiple=TRUE,selected="Phalanx_p1_r,Phalanx_d1_r",
                                        c("Right 1st Proximal and Distal"="Phalanx_p1_r,Phalanx_d1_r","Right 2nd Proximal and Intermediate"="Phalanx_p2_r,Phalanx_i2_r","Right 2nd Intermediate and distal"="Phalanx_i2_r,Phalanx_d2_r","Right 3rd Proximal and Intermediate"="Phalanx_p3_r,Phalanx_i3_r","Right 3rd Intermediate and distal"="Phalanx_i3_r,Phalanx_d3_r","Right 4th Proximal and Intermediate"="Phalanx_p4_r,Phalanx_i4_r","Right 4th Intermediate and distal"="Phalanx_i4_r,Phalanx_d4_r","Right 5th Proximal and Intermediate"="Phalanx_p5_r,Phalanx_i5_r","Right 5th Intermediate and distal"="Phalanx_i5_r,Phalanx_d5_r",
                                          "Left 1st Proximal and Intermediate"="Phalanx_p1_l,Phalanx_i1_l","Left 1st Intermediate and distal"="Phalanx_i1_l,Phalanx_d1_l","RLeft 2nd Proximal and Intermediate"="Phalanx_p2_l,Phalanx_i2_l","Left 2nd Intermediate and distal"="Phalanx_i2_l,Phalanx_d2_l","Left 3rd Proximal and Intermediate"="Phalanx_p3_l,Phalanx_i3_l","Left 3rd Intermediate and distal"="Phalanx_i3_l,Phalanx_d3_l","Left 4th Proximal and Intermediate"="Phalanx_p4_l,Phalanx_i4_l","Left 4th Intermediate and distal"="Phalanx_i4_l,Phalanx_d4_l","Left 5th Proximal and Intermediate"="Phalanx_p5_l,Phalanx_i5_l","Left 5th Intermediate and distal"="Phalanx_i5_l,Phalanx_d5_l")),
                            column(width=4,h4("Right"),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p1_r,Phalanx_d1_r')>=0",h4("1st Proximal and Distal"),
                                                    selectInput("F_S05_d1_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d1_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d1_r_2!='180'",selectInput("F_S05_d1_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p2_r,Phalanx_i2_r')>=0",h4("2nd Proximal and Intermediate Distal"),
                                                    selectInput("F_S05_i2_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i2_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i2_r_2!='180'",selectInput("F_S05_i2_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i2_r,Phalanx_d2_r')>=0",h4("2nd intermediate and Distal"),
                                                    selectInput("F_S05_d2_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d2_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d2_r_2!='180'",selectInput("F_S05_d2_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p3_r,Phalanx_i3_r')>=0",h4("3rd Proximal and intermediate"),
                                                    selectInput("F_S05_i3_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i3_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i3_r_2!='180'",selectInput("F_S05_i3_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i3_r,Phalanx_d3_r')>=0",h4("3rd Intermediate and Distal"),
                                                    selectInput("F_S05_d3_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d3_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d3_r_2!='180'",selectInput("F_S05_d3_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p4_r,Phalanx_i4_r')>=0",h4("4th Proximal and Intermediate Distal"),
                                                    selectInput("F_S05_i4_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i4_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i4_r_2!='180'",selectInput("F_S05_i4_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i4_r,Phalanx_d4_r')>=0",h4("4th intermediate and Distal"),
                                                    selectInput("F_S05_d4_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d4_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d4_r_2!='180'",selectInput("F_S05_d4_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p5_r,Phalanx_i5_r')>=0",h4("5th Proximal and intermediate"),
                                                    selectInput("F_S05_i5_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i5_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i5_r_2!='180'",selectInput("F_S05_i5_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i5_r,Phalanx_d5_r')>=0",h4("5th Intermediate and Distal"),
                                                    selectInput("F_S05_d5_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d5_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d5_r_2!='180'",selectInput("F_S05_d5_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral"))))),
                            column(width=4,h4("Left"),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p1_l,Phalanx_d1_l')>=0",h4("1st Proximal and Distal"),
                                                    selectInput("F_S05_d1_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d1_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d1_l_2!='180'",selectInput("F_S05_d1_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p2_l,Phalanx_i2_l')>=0",h4("2nd Proximal and Intermediate Distal"),
                                                    selectInput("F_S05_i2_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i2_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i2_l_2!='180'",selectInput("F_S05_i2_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i2_l,Phalanx_d2_l')>=0",h4("2nd intermediate and Distal"),
                                                    selectInput("F_S05_d2_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d2_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d2_l_2!='180'",selectInput("F_S05_d2_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p3_l,Phalanx_i3_l')>=0",h4("3rd Proximal and intermediate"),
                                                    selectInput("F_S05_i3_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i3_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i3_l_2!='180'",selectInput("F_S05_i3_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i3_l,Phalanx_d3_l')>=0",h4("3rd Intermediate and Distal"),
                                                    selectInput("F_S05_d3_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d3_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d3_l_2!='180'",selectInput("F_S05_d3_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p4_l,Phalanx_i4_l')>=0",h4("4th Proximal and Intermediate Distal"),
                                                    selectInput("F_S05_i4_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i4_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i4_l_2!='180'",selectInput("F_S05_i4_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i4_l,Phalanx_d4_l')>=0",h4("4th intermediate and Distal"),
                                                    selectInput("F_S05_d4_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d4_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d4_l_2!='180'",selectInput("F_S05_d4_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_p5_l,Phalanx_i5_l')>=0",h4("5th Proximal and intermediate"),
                                                    selectInput("F_S05_i5_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_i5_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_i5_l_2!='180'",selectInput("F_S05_i5_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S05_1.indexOf('Phalanx_i5_l,Phalanx_d5_l')>=0",h4("5th Intermediate and Distal"),
                                                    selectInput("F_S05_d5_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S05_d5_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S05_d5_l_2!='180'",selectInput("F_S05_d5_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))))
                            )
F_S05_RC<-function(input=input){   
  Table<-data.frame(Des1="Fusion of interphalangeal joints",Size=NA,Nature=NA)
  Extent<-NULL
  Angulation<-NULL
  Angle<-NULL
  for(i in 1:length(input$F_S05_1)){
    x<-substr(input$F_S05_1[i], nchar(input$F_S05_1[i])-4+1, nchar(input$F_S05_1[i]))
    Extent<-c(Extent,input[[as.character(paste0("F_S05_",x,"_1"))]])
    Angulation<-c(Angulation,input[[as.character(paste0("F_S05_",x,"_2"))]])
    if(input[[as.character(paste0("F_S05_",x,"_2"))]]!="180"){Angle<-c(Angle,input[[as.character(paste0("F_S05_",x,"_3"))]])}else{Angle<-c(Angle,"NA")}
  }
  Table$Size<-paste0("Angulation:",paste(Angulation,collapse="/"))
  Table$Nature<-paste0("ShapeType,Joints,FusionExtent,AngulationDirection:",paste("Abnormal Fusion",paste(input$F_S01_1,collapse="/"),paste(Extent,collapse="/"),paste(Angle,collapse="/"),sep=","))
  Table }

#Metatarsalphangeal fusion##########
F_S06_UI<-tagList(h3("ID:F_S06"),h4("Description:Fusion of metatarsophalangeal joint(s)"),
                            selectInput("F_S06_1","Ray(s)",multiple=TRUE,selected="Metatarsal_1_r,Phalanx_p1_r",c("Right 1st ray"="Metatarsal_1_r,Phalanx_p1_r","Right 2nd Ray"="Metatarsal_2_r,Phalanx_p2_r","Right 3rd ray"="Metatarsal_3_r,Phalanx_p3_r","Right 4th ray"="Metatarsal_4_r,Phalanx_p4_r","Right 5th ray"="Metatarsal_5_r,Phalanx_p5_r",
                                                             "Leftt 1st ray"="Metatarsal_1_l,Phalanx_p1_l","Leftt 2nd Ray"="Metatarsal_2_l,Phalanx_p2_l","Left 3rd ray"="Metatarsal_3_l,Phalanx_p3_l","Left 4th ray"="Metatarsal_4_l,Phalanx_p4_l","Left 5th ray"="Metatarsal_5_l,Phalanx_p5_l")),
                            column(width=4,
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_1_r,Phalanx_p1_r')>=0",h4("Right 1st Ray"),
                                                    selectInput("F_S06_1_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_1_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_1_r_2!='180'",selectInput("F_S06_1_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_2_r,Phalanx_p2_r')>=0",h4("Right 2nd Ray"),
                                                    selectInput("F_S06_2_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_2_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_2_r_2!='180'",selectInput("F_S06_1_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_3_r,Phalanx_p3_r')>=0",h4("Right 3rd Ray"),
                                                    selectInput("F_S06_3_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_3_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_3_r_2!='180'",selectInput("F_S06_1_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_4_r,Phalanx_p4_r')>=0",h4("Right 4th Ray"),
                                                    selectInput("F_S06_4_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_4_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_4_r_2!='180'",selectInput("F_S06_4_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_5_r,Phalanx_p5_r')>=0",h4("Right 5th Ray"),
                                                    selectInput("F_S06_5_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_5_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_5_r_2!='180'",selectInput("F_S06_5_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral"))))),
                            column(width=4,
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_1_l,Phalanx_p1_l')>=0",h4("Left 1st Ray"),
                                                    selectInput("F_S06_1_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_1_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_1_l_2!='180'",selectInput("F_S06_1_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_2_l,Phalanx_p2_l')>=0",h4("Left 2nd Ray"),
                                                    selectInput("F_S06_2_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_2_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_2_l_2!='180'",selectInput("F_S06_1_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_3_l,Phalanx_p3_l')>=0",h4("Left 3rd Ray"),
                                                    selectInput("F_S06_3_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_3_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_3_l_2!='180'",selectInput("F_S06_1_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_4_l,Phalanx_p4_l')>=0",h4("Left 4th Ray"),
                                                    selectInput("F_S06_4_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_4_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_4_l_2!='180'",selectInput("F_S06_4_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_5_l,Phalanx_p5_l')>=0",h4("Left 5th Ray"),
                                                    selectInput("F_S06_5_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_5_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_5_l_2!='180'",selectInput("F_S06_5_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))))
                            )
F_S06_RC<-function(input=input){   
  Table<-data.frame(Des1="Fusion of metotarsophalangeal joint(s)",Size=NA,Nature=NA,Heal="NA:NA")
  Extent<-NULL
  Angulation<-NULL
  Angle<-NULL
  for(i in 1:length(input$F_S06_1)){
    x<-substr(input$F_S06_1[i], nchar(input$F_S06_1[i])-3+1, nchar(input$F_S06_1[i]))
    Extent<-c(Extent,input[[as.character(paste0("F_S06_",x,"_1"))]])
    Angulation<-c(Angulation,input[[as.character(paste0("F_S06_",x,"_2"))]])
    if(input[[as.character(paste0("F_S06_",x,"_2"))]]!="180"){Angle<-c(Angle,input[[as.character(paste0("F_S06_",x,"_3"))]])}else{Angle<-c(Angle,"NA")}
  }
  Table$Size<-paste0("Angulation:",paste(Angulation,collapse="/"))
  Table$Nature<-paste0("ShapeType,Joints,FusionExtent,AngulationDirection:",paste("Abnormal fusion",paste(input$F_S06_1),paste(Extent,collapse="/"),paste(Angle,collapse="/"),sep=","))
  Table }

#Intertarsal fusion##########
F_S07_UI<-tagList(h3("ID:F_S07"),h4("Description:Fusion of intertarsal joint(s)"))
F_S07_RC<-function(input=input){   
  Table<-data.frame(Des1="Fusion of intertarsal joint(s)",Size=NA,Nature=NA)
  Table }

