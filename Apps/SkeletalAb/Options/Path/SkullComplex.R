#Custom#####
SK_CC_UI<-tagList(fixedPage(h3("ID:SK_CC"),h4("Description:Custom Complex Abnormality"),
                  column(width=6,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                         numericInput("CustomMeasure","Number of measurments",value=NA),
                         actionButton("AddCM","Add fields"),
                         uiOutput("CM")),
                  column(width=6,h4("Overall appearance"),
                         selectInput("SK_CC_Percent","% of bone affected",c("<1/3","1/3-2/3",">2/3")),
                         selectizeInput("SK_CC_Involve","Part(s) of bone involved",multiple=TRUE,choice=c("outer table","diploe","inner table")),
                         textInput("SK_CC_Shape","Overall shape description"),
                         selectInput("SK_CC_Connection","Connection between lesion aspects(Loss,Formation,Shape)",multiple=TRUE,selected="unknown",c("Single stage of healing process","Multi-stage healing (Healing/Healed and active portions)","Unseparable overlapping lesions","Structural ingetregy compromised by bone loss","Forced shape change due to bone formation","Other","unknown")),
                         conditionalPanel("input.SK_CC_Connection.indexOf('Other')>=0",textInput("SK_CC_Connection2","Define Other",value=NA)))),
                  conditionalPanel("input.Path_Type.indexOf('Loss')>=0",
                                   fixedPage(h5("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"),
                                     h4("Loss"),
                                     column(width=6,
                                            selectInput("SK_CC_organisation","Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                                            checkboxInput("SK_CC_Collapse","Associated structural collapse?",value=FALSE)),
                                     column(width=6,
                                            conditionalPanel("input.SK_CC_organisation=='Focal'",
                                                             selectInput("SK_CC_Foci","Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                                             selectInput("SK_CC_FociSize","Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                                             selectInput("SK_CC_Response","Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined"))),
                                            conditionalPanel("input.SK_CC_organisation == 'Diffuse'",
                                                             checkboxInput("SK_CC_Thining","Cortical thinning?",value=FALSE),
                                                             selectInput("SK_CC_Sites","Number of separate sites",c("1","2","3-5","6-10","10+")),
                                                             h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                                             selectInput("SK_CC_Overlapping","Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed")))))),
                  conditionalPanel("input.Path_Type.indexOf('Formation')>=0",
                                   fixedPage(h5("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"),
                                     h4("Formation"),
                                     column(width=6,
                                            selectInput("SK_CC_FType","Type of formation",c("Periostal reaction"="Periosteal","Spicules formed perpendicular to surface (intact cortex)"="Spicules","Ossified connective tissue/overdeveloped attachment site"="Connective","Cortex perforation"="Cortex","Diploe increase"="Diploe"))),
                                     column(width=6,
                                            conditionalPanel("input.SK_CC_FType == 'Periosteal'",
                                                             selectInput("SK_CC_Periosteal","Type of reaction",c("reactive woven bone"="Woven","sclerotic reaction"="Sclerotic","Mixture of sclerotic and woven"="Mixed"))),
                                            conditionalPanel("input.SK_CC_FType == 'Spicules'",
                                                             selectInput("SK_CC_Spicules","Pattern",c("Sunburst","Cauliflower","Other"))),
                                            conditionalPanel("input.SK_CC_FType == 'Connective'",
                                                             textInput("SK_CC_Connective1","Site of Attachment"),
                                                             textInput("SK_CC_Connective2","Description of abnormal bone")),
                                            conditionalPanel("input.SK_CC_FType == 'Cortex'",
                                                             selectInput("SK_CC_Cortex1","Table",c("Inner","Outer","Both")),
                                                             selectInput("SK_CC_Cortex2","Cause of perforation",c("Expansion shell type reaction","Cloacae/sinus tracks","Other")),
                                                             conditionalPanel("input.SK_CC_Cortex2=='Other'",textInput("SK_CC_Cortex3","Define Other",value=NA)),
                                                             conditionalPanel("input.SK_CC_Cortex2=='Expansion shell type reaction'",selectizeInput("SK_CC_Shell","Type of shell(select all applicable)",multiple=TRUE,choice=c("continuous","interrupted","expanded cortex","lobulated","ridged/trabeculated soap bubble","single","lamellated","lamellated onion skin","butress","common angle","spiculated","parallel spiculated/hair on end","spiculated sunburst")))),
                                            conditionalPanel("input.SK_CC_FType == 'Diploe'",
                                                             selectInput("SK_CC_Diploe1","Type of Diploe abnormality",c("Expansion","Coarsening","Increased density","Other")),
                                                             conditionalPanel("input.SK_CC_Diploe1 =='Other'",textInput("SK_CC_Diploe2","Define other",value=NA)))))),
                  conditionalPanel("input.Path_Type.indexOf('Shape')>=0",
                                   fixedPage(h5("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"),
                                     h4("Shape"),
                                     column(width=6,
                                            selectInput("SK_CC_Degree","Degree of abnormality",c("Barely discernable","Clearly discernable")),
                                            selectInput("SK_CC_SType","Type of abnormality",c("Reduced size","Increased size","Distortion or Absence of normal Feature","Depression","Bulging or Outward projection","Premature fusion","Failure of normal fusion","Other"))),
                                     column(width=6,
                                            conditionalPanel("input.SK_CC_SType=='Reduced size'||input.SK_CC_Type=='Increased size'",
                                                             selectInput("SK_CC_sLocal","Localised?",c("Yes,this is the only bone affected"="TRUE-only","yes,only a small number of articulated bones are effected"="TRUE-regional","no,multiple bones acrosss the skeleton are of reduced size"="FALSE")),
                                                             selectInput("SK_CC_sProportional","Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                                             textInput("SK_CC_sShape","further description of relative shape",value="none")),
                                            conditionalPanel("input.SK_CC_SType=='Distortion or Absence of normal Feature'",
                                                             textInput("SK_CC_daDes","Description",value="eg. Absent"),
                                                             selectInput("SK_CC_daBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                                             selectInput("SK_CC_daSurface","Bone Surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                                             conditionalPanel("input.SK_CC_daSurface.indexOf('Other')>=0",textInput("SK_CC_daSurface2","Define Other",value=NA))),
                                            conditionalPanel("input.SK_CC_SType=='Depression'",
                                                             selectInput("SK_CC_dDepth","Depth",c("Shallow-just decernable with the eye or palpertation","Mild-easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced-depression with a maximum depth of between 0.5mm and 1.5mm","Deep-depression in excess of 1.5mm")),
                                                             selectInput("SK_CC_dAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Expansion/thickening of diploe","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                                             conditionalPanel("input.SK_CC_dAssociated.indexOf('Other')>=0",textInput("SK_CC_dAssociated2","Define Other",value=NA)),
                                                             conditionalPanel("input.SK_CC_dAssociated",textInput("SK_CC_dAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                                            conditionalPanel("input.SK_CC_SType=='Bulging or Outward projection'",
                                                             selectInput("SK_CC_bBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                                             selectInput("SK_CC_bAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Expansion/thickening of diploe","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                                             conditionalPanel("input.SK_CC_bAssociated.indexOf('Other')>=0",textInput("SK_CC_bAssociated2","Define Other",value=NA)),
                                                             conditionalPanel("input.SK_CC_bAssociated",textInput("SK_CC_bAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                                            conditionalPanel("input.SK_CC_SType=='Failure of normal fusion'",
                                                             selectInput("SK_CC_fFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                                             conditionalPanel("input.SK_CC_fFusion=='Partial'",numericInput("SK_CC_fFusion2","Approximate Percentage of suture fused",value=50))),
                                            conditionalPanel("input.SK_CC_SType=='Other'",textInput("SK_CC_Other","Define other",value=NA)),
                                            conditionalPanel("input.SK_CC_SType=='Premature fusion'",
                                                             selectInput("SK_CC_pFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                                             conditionalPanel("input.SK_CC_pFusion=='Partial'",numericInput("SK_CC_pFusion2","Approximate Percentage of suture fused",value=50)),
                                                             h5("Associated deformity"),
                                                             h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                                             checkboxInput("SK_CC_pBulging","Bulging of Adjacent bones?",value=FALSE),
                                                             checkboxInput("SK_CC_pKeel","Keel of bone along suture?",value=FALSE),
                                                             textInput("SK_CC_pOther","Other Assocated Deformity",value="None")))))
                  )
SK_CC_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
Des<-"Custom Complex Abnormality-"
SizeValues<-NULL;SizeName<-NULL
for(i in 1:input$CustomMeasure){
  SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
  SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
NatureValues<-c(input$SK_CC_Percent,input$SK_CC_Involve,input$SK_CC_Shape);NatureName<-"PercentAffected,Surfaces,Shape,Connection"
if(Contain("Other",input$SK_CC_Connection)){
  NatureValues<-c(NatureValues,paste(c(input$SK_CC_Connection[input$SK_CC_Connection!="Other"],FreeFix(input$SK_CC_Connection2)),collapse="/"))
  }else{NatureValues<-c(NatureValues,paste(input$SK_CC_Connection,collapse="/"))}
if(Contains("Loss",input$Path_Type)){
  if(input$SK_CC_organisation=="Focal"){
    NatureValues<-c(NatureValues,"Focal",input$SK_CC_Collapse,input$SK_CC_Foci,paste(input$SK_CC_FociSize,collapse="/"),input$SK_CC_Response)
    NatureName<-c(NatureName,"Loss-Organisation,Loss-Collapse,Loss-Foci,Loss-FociSize,Loss-Response")
    Des<-paste0(Des,"Focal Bone Loss ")}
  if(input$SK_CC_organisation=="Diffuse"){
    NatureValues<-c(NatureValues,"Diffuse",input$SK_CC_Collapse,input$SK_CC_Thining,input$SK_CC_Sites,input$SK_CC_Overlapping)
    NatureName<-c(NatureName,"Loss-Organisation,Loss-Collapse,Loss-CorticalThining,Loss-Sites,Loss-Overlapping")
    Des<-paste0(Des,"Diffuse Bone Loss ")}}
if(Contains("Formation",input$Path_Type)){
  if(input$SK_CC_FType=="Periosteal"){
    NatureValues<-c(NatureValues,"Periosteal Reaction",input$SK_CC_Periosteal)
    NatureName<-c(NatureName,"FormationType,Formation-ReactionType")
    Des<-paste0(Des,"Periosteal Bone Formation ")}
  
  if(input$SK_CC_FType=="Spicules"){
    if(input$SK_CC_Spicules=="Other"){pattern<-FreeFix(input$SK_CC_Spicules2)}else{pattern<-input$SK_CC_Spicules}
    NatureValues<-c(NatureValues,"Spicules formed perpendicular to surface",pattern)
    NatureName<-c(NatureName,"FormationType,Formation-Pattern")
    Des<-paste0(Des,"Spicule bone formation ")}
  
  if(input$SK_CC_FType=="Connective"){
    NatureValues<-c(NatureValues,"Ossified connective tissue/overdeveloped attachment site",FreeFix(input$SK_CC_Connective1),FreeFix(input$SK_CC_Connective2))
    NatureName<-c(NatureName,"FormationType,Formation-AttachmentSite,Formation-BoneDescription")
    Des<-paste0(Des,"Connective tissue ossification ")}
  
  if(input$SK_CC_FType=="Cortex"){
    if(input$SK_CC_Cortex2=="Other"){P<-FreeFix(input$SK_CC_Cortex3)
    }else{if(input$SK_CC_Cortex2=="Expansion shell type reaction"){P<-paste0("Expansion shell type reaction-",paste(input$SK_CC_Shell,collapse="/"))
    }else{P<-input$SK_CC_Cortex2}}
    NatureValues<-c(NatureValues,"Cortex Perforation",input$SK_CF_Cortex1,P)
    NatureName<-c(NatureName,"FormationType,Formation-Table,Formation-PerforationCause")
    Des<-paste0(Des,"Cortex Perforation ")}
  
  if(input$SK_CC_FType=="Diploe"){
    if(input$SK_CC_Diploe1=="Other"){Dip<-FreeFix(input$SK_CC_Diploe2)}else{Dip<-input$SK_CC_Diploe1}
    NatureValues<-c(NatureValues,"Diploe Increase",Dip)
    NatureName<-c(NatureName,"FormationType,Formation-DiploeAbnormality")
    Des<-paste0(Des,"Diploe Formation Abnormality")}}
if(Contains("Shape",input$Path_Type)){
  if(input$SK_CC_SType=="Increased Size"){
    NatureValues<-c(NatureValues,"Increased size",input$SK_CC_Degree,input$SK_CC_sLocal,input$SK_CC_sProportional,FreeFix(input$SK_CC_sShape))
    NatureName<-c(NatureName,"ShapeType,Shape-AbnormalityDegree,Shape-Localised,Shape-Proportional,Shape-RelativeShape")
    Des<-paste0(Des,"Increased size")}
  
  if(input$SK_CC_SType=="Reduced Size"){
    NatureValues<-c(NatureValues,"Reduced size",input$SK_CC_Degree,input$SK_CC_sLocal,input$SK_CC_sProportional,FreeFix(input$SK_CC_sShape))
    NatureName<-c(NatureName,"ShapeType,Shape-AbnormalityDegree,Shape-Localised,Shape-Proportional,Shape-RelativeShape")
    Des<-paste0(Des,"Reduced Size")}
  
  if(input$SK_CC_SType=="Distortion or Absence of normal Feature"){
    if(input$SK_CC_daSurface=="Other"){surface<-FreeFix(input$SK_CC_daSurface2)}else{surface<-input$SK_CC_daSurface}
    NatureValues<-c(NatureValues,"Distortion or Absence of normal Feature",input$SK_CC_daDes,input$SK__CC_daBilateral,surface)
    NatureName<-c(NatureName,"ShapeType,Shape-Description,Shape-Bilateral,Shape-Surface")
    Des<-paste0(Des,"Feature Distortion/Absence")}
  
  if(input$SK_CC_SType=="Depression"){
    if(length(input$SK_CC_dAssociated)>0){
      RL<-FreeFix(input$SK_CC_dAssociated3)
      if(Contains("Other",input$SK_CC_dAssociated)){AA<-paste(c(input$SK_CC_dAssociated[input$SK_CC_dAssociated!="Other"],FreeFix(input$SK_CC_dAssociated2)),collapse="/")
      }else{AA<-paste(input$SK_CC_dAssociated,collapse="/")}
    }else{RL<-NA;AA<-"None"}
    NatureValues<-c(NatureValues,"Depression",input$SK_CC_dDepth,AA,RL)
    NatureName<-c(NatureName,"ShapeType,Shape-Depth,Shape-AssociatedAbnormality,Shape-RelativeLocation")
    Des<-paste0(Des,"Depression")}
  
  if(input$SK_CC_SType=="Bulging or Outward projection"){
    if(length(input$SK_CC_bAssociated)>0){
      RL<-FreeFix(input$SK_CC_bAssociated3)
      if(Contains("Other",input$SK_CC_bAssociated)){AA<-paste(c(input$SK_CC_bAssociated[input$SK_CC_bAssociated!="Other"],FreeFix(input$SK_CC_bAssociated2)),collapse="/")
      }else{AA<-paste(input$SK_CC_bAssociated,collapse="/")}
    }else{RL<-NA;AA<-"None"}
    NatureValues<-c(NatureValues,input$SK_CC_bBilateral,AA,RL)
    NatureName<-c(NatureName,"ShapeType,Shape-Bilateral,Shape-AssociatedAbnormality,Shape-RelativeLocation")
    Des<-paste0(Des,"Bulging/Outward projection")}
  
  if(input$SK_CC_SType=="Premature Fusion"){
    if(input$SK_CC_pFusion=="Partial"){Fusion<-paste0("Partial-",input$SK_CC_pFusion2)}else{Fusion<-input$SK_CC_pFusion}
    NatureValues<-c(NatureValues,"Premature fusion",Fusion,input$SK_CC_pBulging,input$SK_CC_pKeel,FreeFix(input$SK_CC_pOther))
    NatureName<-c(NatureName,"ShapeType,Shape-Extent,Shape-Bulging,Shape-Keel,Shape-OtherAbnormality")
    Des<-paste0(Des,"Abnormal Fusion")}
  
  if(input$SK_CC_SType=="Failure of normal fusion"){
    if(input$SK_CC_fFusion=="Partial"){Fusion<-paste0("Partial-",input$SK_CC_fFusion2)}else{Fusion<-input$SK_CC_fFusion}
    NatureValues<-c(NatureValues,"Failure of normal fusion",Fusion)
    NatureName<-c(NatureName,"ShapeType,Shape-Extent")
    Des<-paste0(Des,"UnFused")}
  
  if(input$SK_CC_SType=="Other"){
    NatureValues<-c(NatureValues,FreeFix(input$SK_CC_Other))
    NatureName<-c(NatureName,"ShapeType")
    Des<-paste0(Des,"Other shape abnormality")}
}
Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
Table$Nature<-paste(paste(NatureName,collapse=","),paste(NatureValues,collapse=","),sep=":")
Table$Des1<-Des
Table
}
#TMJ irregularity#############################################################################################################################################
SK_C01_UI<-tagList(h3("ID:SK_C01"),h4("Description:TMJ irregularity"))
SK_C01_RC<-function(input=input){   
  Table<-data.frame(Des1="TMJ irregularity",Size=NA,Nature=NA,Heal=NA)
  Table }
#Destruction, grooves and nodules#############################################################################################################################################
SK_C02_UI<-tagList(h3("ID:SK_C02"),h4("Description:Extensive destruction,raditing grooves and raised nodules"))
SK_C02_RC<-function(input=input){   
  Table<-data.frame(Des1="Extensive destruction,raditing grooves and raised nodules",Size=NA,Nature=NA,Heal=NA)
  Table }
#Disorganised remodelling#############################################################################################################################################
SK_C03_UI<-tagList(h3("ID:SK_C03"),h4("Description:Disorganised bone remodelling"))
SK_C03_RC<-function(input=input){   
  Table<-data.frame(Des1="Disorganised bone remodelling",Size=NA,Nature=NA,Heal=NA)
  Table }
#erosion,remodeling and scaring#############################################################################################################################################
SK_C04_UI<-tagList(h3("ID:SK_C04"),h4("Description:Erosion,remodelling and scarring"))
SK_C04_RC<-function(input=input){   
  Table<-data.frame(Des="Erosion,remodelling and scarring",Size=NA,Nature=NA,Heal=NA)
  Table }
