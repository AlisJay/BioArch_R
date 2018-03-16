#Custom#####
SK_CS_UI<-tagList(h3("ID:SK_CS"),h4("Description:Custom Shape Abnormality"),
                  column(width=4,
                         selectInput("SK_CS_Degree","Degree of abnormality",c("Barely discernable","Clearly discernable")),
                         selectInput("SK_CS_Type","Type of abnormality",c("Reduced size","Increased size","Distortion or Absence of normal Feature","Depression","Bulging or Outward projection","Premature fusion","Failure of normal fusion","Other"))),
                  column(width=4,
                         conditionalPanel("input.SK_CS_Type=='Reduced size'||input.SK_CS_Type=='Increased size'",
                                          selectInput("SK_CS_sLocal","Localised?",c("Yes,this is the only bone affected"="TRUE-only","yes,only a small number of articulated bones are effected"="TRUE-regional","no,multiple bones acrosss the skeleton are of reduced size"="FALSE")),
                                          selectInput("SK_CS_sProportional","Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                          textInput("SK_CS_sShape","further description of relative shape",value="none")),
                         conditionalPanel("input.SK_CS_Type=='Distortion or Absence of normal Feature'",
                                          textInput("SK_CS_daDes","Description",value="eg. Absent"),
                                          selectInput("SK_CS_daBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                          selectInput("SK_CS_daSurface","Bone Surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                          conditionalPanel("input.SK_CS_daSurface.indexOf('Other')>=0",textInput("SK_CS_daSurface2","Define Other",value=NA))),
                         conditionalPanel("input.SK_CS_Type=='Depression'",
                                          selectInput("SK_CS_dDepth","Depth",c("Shallow-just decernable with the eye or palpertation","Mild-easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced-depression with a maximum depth of between 0.5mm and 1.5mm","Deep-depression in excess of 1.5mm")),
                                          textInput("SK_CS_dShape","Description of shape",value="None"),
                                          selectInput("SK_CS_dAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Expansion/thickening of diploe","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                          conditionalPanel("input.SK_CS_dAssociated.indexOf('Other')>=0",textInput("SK_CS_dAssociated2","Define Other",value=NA)),
                                          conditionalPanel("input.SK_CS_dAssociated",textInput("SK_CS_dAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                         conditionalPanel("input.SK_CS_Type=='Bulging or Outward projection'",
                                          selectInput("SK_CS_bBilateral","Bilateral Absence/Distortion",selected="NA",c("Yes"="Bilateral","No,left only"="Unilateral_l","No,right only"="Unilateral_r","Unknown","NA")),
                                          textInput("SK_CS_bShape","Description of shape",value="None"),
                                          selectInput("SK_CS_bAssociated","Associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Expansion/thickening of diploe","Premature suture Fusion","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other")),
                                          conditionalPanel("input.SK_CS_bAssociated.indexOf('Other')>=0",textInput("SK_CS_bAssociated2","Define Other",value=NA)),
                                          conditionalPanel("input.SK_CS_bAssociated",textInput("SK_CS_bAssociated3","Relative Location of associated abnormality",value="eg. Along superior border"))),
                         conditionalPanel("input.SK_CS_Type=='Failure of normal fusion'",
                                          selectInput("SK_CS_fFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                          conditionalPanel("input.SK_CS_fFusion=='Partial'",numericInput("SK_CS_fFusion2","Approximate Percentage of suture fused",value=50))),
                         conditionalPanel("input.SK_CS_Type=='Other'",textInput("SK_CS_Other","Define other",value=NA)),
                         conditionalPanel("input.SK_CS_Type=='Premature fusion'",
                                          selectInput("SK_CS_pFusion","Extent of Abnormal fusion",c("Complete","Partial","Unknown")),
                                          conditionalPanel("input.SK_CS_pFusion=='Partial'",numericInput("SK_CS_pFusion2","Approximate Percentage of suture fused",value=50)),
                                          h5("Associated deformity"),
                                          h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                          checkboxInput("SK_CS_pBulging","Bulging of Adjacent bones?",value=FALSE),
                                          checkboxInput("SK_CS_pKeel","Keel of bone along suture?",value=FALSE),
                                          textInput("SK_CS_pOther","Other Assocated Deformity",value="None"))),
                  column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                         numericInput("CustomMeasure","Number of measurments",value=NA),
                         actionButton("AddCM","Add fields"),
                         uiOutput("CM"))
)
SK_CS_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
Table$Des1<-paste("Custom shape abnormality-",input$SK_CS_Type)
SizeValues<-NULL;SizeName<-NULL
for(i in 1:input$CustomMeasure){
  SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
  SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
NatureValues<-NULL;NatureName<-NULL
if(input$SK_CS_Type=="Reduced Size"|input$SK_CS_Type =="Increased Size"){
  NatureValues<-paste(input$SK_CS_Type,input$SK_CS_Degree,input$SK_CS_sLocal,input$SK_CS_sProportional,FreeFix(input$SK_CS_sShape),sep=",")
  NatureName<-"ShapeType,AbnormalityDegree,Localised,Proportional,Shape"}
if(input$SK_CS_Type=="Distortion or Absence of normal Feature"){
  if(input$SK_CS_daSurface=="Other"){surface<-FreeFix(input$SK_CS_daSurface2)}else{surface<-input$SK_CS_daSurface}
  NatureValues<-paste("Distortion or Absence of normal Feature",input$SK_CS_daDes,input$SK_CS_daBilateral,surface,sep=",")
  NatureName<-"ShapeType,Description,Bilateral,Surface"}
if(input$SK_CS_Type=="Depression"){
  if(length(input$SK_CS_dAssociated)>0){
    RL<-FreeFix(input$SK_CS_dAssociated3)
    if(Contains("Other",input$SK_CS_dAssociated)){AA<-paste(c(input$SK_CS_dAssociated[input$SK_CS_dAssociated!="Other"],FreeFix(input$SK_CS_dAssociated2)),collapse="/")
    }else{AA<-paste(input$SK_CS_dAssociated,collapse="/")}
  }else{RL<-NA;AA<-"None"}
  NatureValues<-paste("Depression",input$SK_CS_dDepth,AA,RL,sep=",")
  NatureName<-"ShapeType,Depth,AssociatedAbnormality,RelativeLocation"}
if(input$SK_CS_Type=="Bulging or Outward projection"){
  if(length(input$SK_CS_bAssociated)>0){
    RL<-FreeFix(input$SK_CS_bAssociated3)
    if(Contains("Other",input$SK_CS_bAssociated)){AA<-paste(c(input$SK_CS_bAssociated[input$SK_CS_bAssociated!="Other"],FreeFix(input$SK_CS_bAssociated2)),collapse="/")
    }else{AA<-paste(input$SK_CS_bAssociated,collapse="/")}
  }else{RL<-NA;AA<-"None"}
  NatureValues<-paste("Bulging/Outward projection",input$SK_CS_bBilateral,input$SK_CS_bShape,AA,RL,sep=",")
  NatureName<-"ShapeType,Bilateral,Shape,AssociatedAbnormality,RelativeLocation"}
if(input$SK_CS_Type=="Failure of normal fusion"){
  if(input$SK_CS_fFusion=="Partial"){Fusion<-paste0("Partial-",input$SK_CS_fFusion2)}else{Fusion<-input$SK_CS_fFusion}
  NatureValues<-paste("Failure of normal Fusion",Fusion,sep=",")
  NatureName<-"ShapeType,Extent"}
if(input$SK_CS_Type=="Other"){
  NatureValues<-FreeFix(input$SK_CS_Other)
  NatureName<-"ShapeType"}
if(input$SK_CS_Type=="Premature fusion"){
  if(input$SK_CS_pFusion=="Partial"){Fusion<-paste0("Partial-",input$SK_CS_pFusion2)}else{Fusion<-input$SK_CS_pFusion}
  NatureValues<-paste("Premature fusion",Fusion,input$SK_CS_pBulging,input$SK_CS_pKeel,FreeFix(input$SK_CS_pOther),sep=",")
  NatureName<-"ShapeType,Extent,Bulging,Keel,OtherAbnormality"}
Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
Table$Nature<-paste(paste(NatureName,collapse=","),paste(NatureValues,collapse=","),sep=":")
Table
}
#Microcephly####
SK_S01_UI<-tagList(h3("ID:SK_S01"),h4("Description: Microcephly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S01_1","Circumference(mm)",value=NA),
                                    numericInput("SK_S01_2","Height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S01_3","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S01_4","Maximum cranial breadth in the midsagital plane(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
                                    h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                    checkboxInput("SK_S01_5","Proportional size reduction across entire cranium",value=TRUE),
                                    conditionalPanel("input.SK_S01_5==false",textInput("SK_S01_5b","Description of lack of proportionality")),
                                    selectInput("SK_S01_6","Premature/ unusal fusion of sutures",multiple=TRUE,selected="coronal",c("coronal","saggital","lambdoid","right squamosal"="squamosal_r","left squamosal"="squamosal_l","Other"="Other")),
                                    conditionalPanel("input.SK_S01_6",
                                                     conditionalPanel("input.SK_S01_6.indexOf('Other')>=0",selectInput("SK_S01_6b","Other Sutures",multiple=TRUE,c("ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","metopic","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                        "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                        "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                        "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                        "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                        "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S01_6c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    textInput("SK_S01_7","Other associated deformity",value="None")))
SK_S01_RC<-function(input=input){   
  Table<-data.frame(Des1="Microcephly:Abnormally small circumference",Size=NA,Nature=NA)
  Table$Size<-paste0("Circumference,ba_b,g_op,eu_eu:",paste(input$SK_S01_1,input$SK_S01_2,input$SK_S01_3,input$SK_S01_4,sep=","))
  if(input$SK_S01_5){p<-"Proportional size reduction across entire cranium"}else{p<-paste0("Not proportional-",FreeFix(input$SK_S01_5b))}
  if(input$SK_S01_7=="None"){o<-"No additional deformity"}else{o<-FreeFix(input$SK_S01_7)}
  if(length(input$SK_S01_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S01_6C))
    if(sum(input$SK_S01_6=="Other")==0){Sutures<-paste(input$SK_S01_6,collapse="/")}else{Sutures<-paste(c(input$SK_S01_6[input$SK_S01_6!="Other"],input$SK_S01_6b),collapse="/")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  Table$Nature<-paste("Proportionality,SutureFusion,Sutures,OtherDeformity",paste(p,SDes,Sutures,o,sep=","),sep=":")
  Table }
#Macrocephly#########
SK_S02_UI<-tagList(h3("ID:SK_S02"),h4("Description:Macrocephly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S02_1","Circumference(mm)",value=NA),
                                    numericInput("SK_S02_2","Height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S02_3","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S02_4","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S02_8","Bizygomatic breadth (mm)",value=NA),
                                    numericInput("SK_S02_9","Minimum frontal breadth(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
                                    h5("nb if there is substantial deformity this should also be recoreded separately and linked using the link tab"),
                                    checkboxInput("SK_S02_5","Proportional size increase across entire cranium",value=TRUE),
                                    conditionalPanel("input.SK_S02_5==false",textInput("SK_S01_5b","Description of lack of proportionality")),
                                    selectInput("SK_S02_6","Premature/ unusal fusion of sutures",multiple=TRUE,selected="coronal",c("coronal","saggital","lambdoid","right squamosal"="squamosal_r","left squamosal"="squamosal_l","Other"="Other")),
                                    conditionalPanel("input.SK_S02_6",
                                                     conditionalPanel("input.SK_S02_6.indexOf('Other')>=0",selectInput("SK_S02_6b","Other Sutures",multiple=TRUE,c("ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","metopic","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                   "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                   "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                   "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                   "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                   "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S02_6C","Description of unusal fusion",value="eg. premature complete fusion")),
                                    textInput("SK_S02_7","Other associated deformity",value="None")))
SK_S02_RC<-function(input=input){   
  Table<-data.frame(Des1="Macrocephly:Abnormally large/wide cranium",Size=NA,Nature=NA)
  Table$Size<-paste("Circumference,ba_b,g_op,eu_eu,zy_zy,ft_ft",paste(input$SK_S02_1,input$SK_S02_2,input$SK_S02_3,input$SK_S02_4,input$SK_S02_8,input$SK_SO2_9,sep=","),sep=":")
  if(input$SK_S02_5){p<-"Proportional size reduction across entire cranium"}else{p<-paste0("Not proportional-",FreeFix(input$SK_S02_5b))}
  if(input$SK_S02_7=="None"){o<-"No additional deformity"}else{o<-FreeFix(input$SK_S02_7)}
  if(length(input$SK_S02_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S02_6C))
    if(sum(input$SK_S02_6=="Other")==0){Sutures<-paste(input$SK_S02_6,collapse="/")}else{Sutures<-paste(c(input$SK_S02_6[input$SK_S02_6!="Other"],input$SK_S02_6b),collapse="/")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  Table$Nature<-paste("Proportionality,SutureFusion,Sutures,OtherDeformity",paste(p,SDes,Sutures,o,sep=","),sep=":")
  Table }
#Scaphocephaly########
SK_S03_UI<-tagList(h3("ID:SK_S03"),h4("Description:Scaphocephaly"),
                   column(width=4,h4("Measurments"),
                          numericInput("SK_S03_1","Cranial height measured from basion to bregma(mm)",value=NA),
                          numericInput("SK_S03_2","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                          numericInput("SK_S03_3","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                          numericInput("SK_S03_4","Length from bregma to lambda (mm)",value=NA)),
                   column(width=4,h4("Associated deformity"),
                          h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                          selectInput("SK_S03_6","Premature/ unusal fusion of sutures",multiple=TRUE,selected="saggital",c("saggital","Other"="Other")),
                          conditionalPanel("input.SK_S03_6",
                                           conditionalPanel("input.SK_S03_6.indexOf('Other')>=0",selectInput("SK_S03_6b","Other Sutures",multiple=TRUE,c("coronal","lambdoid","right squamosal"="squamosal_r","left squamosal"="squamosal_l","ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","metopic","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                         "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                         "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                         "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                         "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                         "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                           textInput("SK_S03_6c","Description of unusal fusion",value="eg. premature complete fusion")),
                          checkboxInput("SK_S03_7","Raised keel of bone along saggital suture?",value=FALSE),
                          conditionalPanel("input.SK_S03_7",textInput("SK_S03_7b","Nature of Keel",value="Raised ridge of bone along whole suture"),
                                           textInput("SK_S03_7c","Length of keel(mm)"),
                                           textInput("SK_S03_7d","Height of keel(mm)")),
                          checkboxInput("SK_S03_8","Low set orbits (in relation to frontal bone)",value=FALSE),
                          checkboxInput("SK_S03_9","Bulbous/projecting frontal bone?",value=FALSE),
                          textInput("SK_S03_10","Other associated deformity",value="None")))
SK_S03_RC<-function(input=input){   
  Table<-data.frame(Des1="Scaphocephaly:Anterior Posterior elongation",Size=NA,Nature=NA)
  if(input$SK_S03_7){Table$Size<-paste("ba_b,g_op,eu_eu,b_l,keel length,keel height",paste(input$SK_S03_1,input$SK_S03_2,input$SK_S03_3,input$SK_S03_4,input$SK_S03_7c,input$SK_S03_7d,sep=","),sep=":")
  }else{Table$Size<-paste("ba_b,g_op,eu_eu,b_l",paste(input$SK_S03_1,input$SK_S03_2,input$SK_S03_3,input$SK_S03_4,sep=","),sep=":")}
  if(length(input$SK_S03_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S03_6c))
    if(sum(input$SK_S03_6=="Other")==0){Sutures<-paste(input$SK_S03_6,collapse="/")}else{Sutures<-paste(c(input$SK_S03_6[input$SK_S03_6!="Other"],input$SK_S03_6b),collapse="/")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  if(input$SK_S03_7){k<-paste0("Sagittal Keel-",FreeFix(input$SK_S03_7b))}else{k<-FALSE}
  Table$Nature<-paste0("SutureFusion,Sutures,Keel,LowSetOrbits,ProjectingFrontal,OtherDeformity:",paste(SDes,Sutures,k,input$SK_S03_8,input$SK_S03_9,FreeFix(input$SK_S03_10),sep=","))
  Table }
#Oxycephaly#############################################################################################################################################
SK_S04_UI<-tagList(h3("ID:SK_S04"),h4("Description:Oxycephaly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S04_1","Cranial height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S04_2","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S04_3","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S04_4","Forehead height measured from nasion to bregma (mm)",value=NA),
                                    numericInput("SK_S04_5","Upper face height from nasion to prosthion(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
                                    h5("nb if there is substantial deformity this should also be recoreded separately and linked using the link tab"),
                                    selectInput("SK_S04_6","Premature/ unusal fusion of sutures",multiple=TRUE,selected=c("coronal_r","coronal_l"),c("right coronal"="coronal_r","Left cornal"="coronal_l","saggital","right lambdoid"="lambdoid_r","left lambdoid"="lambdoid_l","Other"="Other")),
                                    conditionalPanel("input.SK_S04_6",
                                                     conditionalPanel("input.SK_S04_6.indexOf('Other')>=0",selectInput("SK_S04_6b","Other Sutures",multiple=TRUE,c("right squamosal"="squamosal_r","left squamosal"="squamosal_l","ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","metopic","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                   "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                   "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                   "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                   "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                   "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S04_6c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    selectInput("SK_S04_7","Shape of top of head",c("Rounded","Conical","Pointed","Other/Irregular")),
                                    selectInput("SK_S04_8","Nature of increase in cranial height",selected="Straight",c("Straight increase in height effecting frontal,parietal and occipital"="Straight","Increase in height skewed anteriorly (mostly effecting frontal)"="Anteriorly skewed","Increase in height skewed centerally (mostly effecting parietal)"="Centerally skewed","Increase in height skewed posteriorly (mostly effecting occipital)"="Posteriorly Skewed","Increased in height laterally skewed (right side)"="Laterally Skewed right","Increase in height laterally skewed (left side)"="Laterally Skewed Left","Other")),
                                    conditionalPanel("input.SK_S04_8=='Other'",textInput("SK_S04_8b","Custom height increase description",value="NA")),
                                    textInput("SK_S04_9","Other associated deformity",value="None")))
SK_S04_RC<-function(input=input){   
  Table<-data.frame(Des1="Oxycephaly:Superior-Inferior elongation",Size=NA,Nature=NA)
  Table$Size<-paste0("ba_b,g_op,eu_eu,n_b,n_pr:",paste(input$SK_S04_1,input$SK_S04_2,input$SK_S04_3,input$SK_S04_4,input$SK_S04_5,sep=","))
  if(length(input$SK_S04_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S04_6c))
    if(sum(input$SK_S04_6=="Other")==0){Sutures<-paste(input$SK_S04_6,collapse="/")}else{Sutures<-paste(c(input$SK_S04_6[input$SK_S04_6!="Other"],input$SK_S04_6b),collapse="/")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  if(input$SK_S04_8=="Other"){IType<-paste0("Other-",FreeFix(input$SK_S04_8b))}else{IType<-input$SK_S04_8}
  Table$Nature<-paste0("SutureFusion,Sutures,TopOfHeadShape,HeightIncrease,OtherDeformity:",paste(SDes,Sutures,input$SK_S04_7,IType,FreeFix(input$SK_S04_9),sep=","))
  Table }
#Brachycephaly#############################################################################################################################################
SK_S05_UI<-tagList(h3("ID:SK_S05"),h4("Description:Brachycephaly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S05_1","Cranial height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S05_2","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S05_3","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S05_4","Forehead height measured from nasion to bregma (mm)",value=NA),
                                    numericInput("SK_S05_5","Minimum frontal breadth(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
                                    h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                    selectInput("SK_S05_6","Premature/ unusal fusion of sutures",multiple=TRUE,selected=c("coronal_r","coronal_l"),c("right coronal"="coronal_r","Left cornal"="coronal_l","Other"="Other")),
                                    conditionalPanel("input.SK_S05_6",
                                                     conditionalPanel("input.SK_S05_6.indexOf('Other')>=0",selectInput("SK_S05_6b","Other Sutures",multiple=TRUE,c("saggital","right lambdoid"="lambdoid_r","left lambdoid"="lambdoid_l","right squamosal"="squamosal_r","left squamosal"="squamosal_l","ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","metopic","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                   "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                   "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                   "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                   "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                   "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S05_6c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    selectInput("SK_S05_7","Bulbous/projecting parietal bone",c("No"="FALSE","Bilateral","Unilateral(right)"="Unilateral_r","Unilateral(left)"="Unilateral_l")),
                                    selectInput("SK_S05_8","Increased cranial height",selected="None",c("None","Straight increase in height effecting frontal,parietal and occipital","Increase in height skewed anteriorly (mostly effecting frontal)","Increase in height skewed centerally (mostly effecting parietal)","Increase in height skewed posteriorly (mostly effecting occipital)","Increased in height laterally skewed (right side)","Increase in height laterally skewed (left side)","Other")),
                                    conditionalPanel("input.SK_S05_8=='Other'",textInput("SK_S05_8b","Custom height increase description",value="NA")),
                                    textInput("SK_S05_9","Other associated deformity",value="None")))
SK_S05_RC<-function(input=input){   
  Table<-data.frame(Des1="Brachycephaly:Flattening of the posterior skull",Size=NA,Nature=NA)
  Table$Size<-paste("ba_b,g_op,eu_eu,n_b,ft_ft",paste(input$SK_S05_1,input$SK_S05_2,input$SK_S05_3,input$SK_S05_4,input$SK_S05_5,sep=","),sep=":")
  if(length(input$SK_S05_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S05_6c))
    if(sum(input$SK_S05_6=="Other")==0){Sutures<-paste(input$SK_S05_6,collapse="/")}else{Sutures<-paste(c(input$SK_S05_6[input$SK_S05_6!="Other"],input$SK_S05_6b),collapse="/")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  if(input$SK_S05_8=="Other"){Height<-paste0("Other-",FreeFix(input$SK_S05_8b))}else{Height<-input$SK_S05_8}
  Table$Nature<-paste0("SutureFusion,Sutures,ProjectingParietal,HeightIncrease,OtherDeformity:",paste(SDes,Sutures,input$SK_S05_7,Height,FreeFix(input$SK_S05_9),sep=","))
  Table }
#Plagiocephly#############################################################################################################################################
SK_S06_UI<-tagList(h3("ID:SK_S06"),h4("Description:Plagiocephly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S06_1","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S06_2","Maximum cranial length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S06_3","Distance from the Left frontozygomatic point to the occipital bone(mm)",value=NA),
                                    numericInput("SK_S06_4","Distance from the right frontozygomatic point to the occipital bone(mm)",value=NA),
                                    h5("nb frontozygomatic point= the most lateral point on the frontozygomatic suture.These two measurments should be taken at the same angle (a) from the line denoting maximum cranial length"),
                                    img(src="fmt_ocp.png",height=200)),
                             column(width=4,h4("Associated deformity"),
                                    h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                    selectInput("SK_S06_6","Premature/ unusal fusion of sutures",multiple=TRUE,selected=c("coronal_r"),c("right coronal"="coronal_r","Left cornal"="coronal_l","right lambdoid"="lambdoid_r","left lambdoid"="lambdoid_l","Other"="Other")),
                                    conditionalPanel("input.SK_S06_6",
                                                     conditionalPanel("input.SK_S06_6.indexOf('Other')>=0",selectInput("SK_S06_6b","Other Sutures",multiple=TRUE,c("saggital","right squamosal"="squamosal_r","left squamosal"="squamosal_l","ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","metopic","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                   "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                   "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                   "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                   "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                   "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S06_6c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    checkboxInput("SK_S06_7","Deviation of the sagittal suture"),
                                    conditionalPanel("input.SK_S06_7",img(src="sagittaldeviation.png",height=120),
                                                     selectInput("SK_S06_7b","Extent of deviation",c("<5degrees right anterior","5-10 degrees right anterior",">10 degree right anterior","<5degrees right posterior","5-10 degrees right posterior",">10 degree right posterior","<5 degrees left anterior","5-10 degrees left anterior",">10 degrees left anterior","<5degrees left posterior","5-10 degrees left posterior",">10 degree left posterior"))),
                                    checkboxInput("SK_S06_8","Nasal asymmetry"),
                                    conditionalPanel("input.SK_S06_8",selectInput("SK_S06_8b","Type of nasal asymetry",selected="Other",multiple=TRUE,c("Size difference","Not aligned","Other")),
                                                     conditionalPanel("input.SK_S06_8b.indexOf('Size difference')>=0",
                                                                      numericInput("SK_S06_8c","Maximum width of the right side of the nasal cavity(mm)",value=NA),
                                                                      numericInput("SK_S06_8d","Maximum height of the right side of the nasal cavity(mm)",value=NA),
                                                                      numericInput("SK_S06_8e","Maximum width of the left side of the nasal cavity(mm)",value=NA),
                                                                      numericInput("SK_S06_8f","Maximum height of the left side of the nasal cavity(mm)",value=NA)),
                                                     conditionalPanel("input.SK_S06_8b.indexOf('Not aligned')>=0",
                                                                      numericInput("SK_S06_8g","Height difference between the lowest points of the nasal cavity(mm)",value=NA),
                                                                      numericInput("SK_S06_8h","Height difference between the highest points of the nasal cavity(mm)",value=NA)),
                                                     conditionalPanel("input.SK_S06_8b.indexOf('Other')>=0",textInput("SK_S06_8i","Description of asymmetry"))),
                                    checkboxInput("SK_S06_9","Orbit asymmetry"),
                                    conditionalPanel("input.SK_S06_9",selectInput("SK_S06_9b","Type of orbital asymetry",multiple=TRUE,selected="Other",c("Size difference","Not aligned","Other")),
                                                     conditionalPanel("input.SK_S06_9b.indexOf('Size difference')>=0",
                                                                      numericInput("SK_S06_9c","Maximum width of the right orbit(mm)",value=NA),
                                                                      numericInput("SK_S06_9d","Maximum height of the right orbit(mm)",value=NA),
                                                                      numericInput("SK_S06_9e","Maximum width of the left orbit(mm)",value=NA),
                                                                      numericInput("SK_S06_9f","Maximum height of the left orbit(mm)",value=NA)),
                                                     conditionalPanel("input.SK_S06_9b.indexOf('Not aligned')>=0",
                                                                      numericInput("SK_S06_9g","Height difference between the lower borders(mm)",value=NA),
                                                                      numericInput("SK_S06_9h","Height difference between the supraorbital margins(mm)",value=NA)),
                                                     conditionalPanel("input.SK_S06_9b.indexOf('Other')>=0",textInput("SK_S06_9i","Description of asymmetry"))),
                                    checkboxInput("SK_S06_10","Ear shift (asymmetry in position of the external auditory meatus)"),
                                    conditionalPanel("input.SK_S06_10",numericInput("SK_S06_10b","Difference in the anterior posterior position of most anterior point on the external auditory meatus(mm)",value=NA),
                                                     numericInput("SK_S06_10c","Difference in the superior inferior position of the most anterior point on the external auditory meatus(mm)",value=NA)),
                                    textInput("SK_S06_11","Other Associated deformity",value="None")))
SK_S06_RC<-function(input=input){   
  Table<-data.frame(Des1="Plagiocephly:Asymmetric distortion",Size=NA,Nature=NA)
  Nasal1<-FALSE;Nasal2<-"Nasal";Nasal3<-NA
  if(input$SK_S06_8){
    if(input$SK_S06_8b=="Other"){Nasal1<-paste0("Other-",FreeFix(input$SK_S06_8i))}else{Nasal1<-input$SK_S06_8b}
    if(input$SK_S06_8b=="Size difference"){Nasal2<-"Nasal_MaxWidth(r/l),Nasal_MaxHeight(r/l)";Nasal3<-paste(paste(input$SK_S06_8c,input$SK_S06_8e,sep="/"),paste(input$SK_S06_8d,input$SK_S06_8f,sep="/"),sep=",")}
    if(input$SK_S06_8b=="Not aligned"){Nasal2<-"Nasal_InfHeightDiff,Nasal_SupHeightDiff";Nasal3<-paste(input$SK_S06_8g,input$SK_S06_8h,sep=",")}}
  Orbit1<-FALSE;Orbit2<-"Orbit";Orbit3<-NA
  if(input$SK_S06_9){
    if(input$SK_S06_9b=="Other"){Orbit1<-paste0("Other-",FreeFix(input$SK_S06_9i))}else{Orbit1<-input$SK_S06_9b}
    if(input$SK_S06_9b=="Size difference"){Orbit2<-"Orbit_MaxWidth(r/l),Orbit_MaxHeight(r/l)";Orbit3<-paste(paste(input$SK_S06_9c,input$SK_S06_9e,sep="/"),paste(input$SK_S06_9d,input$SK_S06_9f,sep="/"),sep=",")}
    if(input$SK_S06_9b=="Not aligned"){Orbit2<-"Orbit_InfHeightDiff,Orbit_SupHeightDiff";Orbit3<-paste(input$SK_S06_9g,input$SK_S06_9h,sep=",")}}
  Ear1<-input$SK_S06_10;Ear2<-"EAM";Ear3<-NA
  if(Ear1){Ear2<-"EAM_AntPosDiff,EAM_SupInfDiff";Ear3<-paste(input$SK_S06_10b,input$SK_S06_10c)}
  if(length(input$SK_S06_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S06_6c))
    if(sum(input$SK_S06_6=="Other")==0){Sutures<-paste(input$SK_S06_6,collapse="/")}else{Sutures<-paste(c(input$SK_S06_6[input$SK_S06_6!="Other"],input$SK_S06_6b),collapse="/")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  Table$Nature<-paste0("SutureFusion,Sutures,NasalAsymmetry,OrbitalAsymmetry,EarShift,OtherDeformity:",paste(SDes,Sutures,Nasal1,Orbit1,Ear1,FreeFix(input$SK_S06_11),sep=","))
  Table$Size<-paste(paste("eu_eu,g_op,fmt_op_r,fmt_op_l",Ear2,Orbit2,Nasal2,sep=","),paste(input$SK_S06_1,input$SK_S06_2,input$SK_S06_3,input$SK_S06_4,Ear3,Orbit3,Nasal3,sep=","),sep=":")
  Table }
#Trigonocephaly#########
SK_S07_UI<-tagList(h3("ID:SK_S07"),h4("Description:Trigonocephaly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S07_1","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S07_2","Maximum cranial length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S07_3","Head circumference(mm)",value=NA),
                                    numericInput("SK_S07_4","Biorbital Breadth(mm)",value=NA),
                                    numericInput("SK_S07_5","Interorbital Breadth",value=NA)),
                             column(width=4,h4("Associated deformity"),
                                    h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                    selectInput("SK_S07_6","Premature/ unusal fusion of sutures",multiple=TRUE,selected=c("metopic"),c("metopic","Other"="Other")),
                                    conditionalPanel("input.SK_S07_6",
                                                     conditionalPanel("input.SK_S07_6.indexOf('Other')>=0",selectInput("SK_S07_6b","Other Sutures",multiple=TRUE,c("right coronal"="coronal_r","Left cornal"="coronal_l","right lambdoid"="lambdoid_r","left lambdoid"="lambdoid_l","saggital","right squamosal"="squamosal_r","left squamosal"="squamosal_l","ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                   "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                   "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                   "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                   "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                   "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S07_6c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    checkboxInput("SK_S07_7","Raised keel of bone along metopic suture?",value=FALSE),
                                    conditionalPanel("input.SK_S07_7",textInput("SK_S07_7b","Nature of Keel",value="Continuous ridge of bone"),
                                                     textInput("SK_S07_7c","Length of keel(mm)"),
                                                     textInput("SK_S07_7d","Height of keel(mm)")),
                                    selectInput("SK_S07_Fossa","Reduced anterior cranial fossa",c("Yes,Bilateral"="Bilateral","Yes,left side only"="Unilateral_l","Yes,right side only"="Unilateral_r","No"="FALSE","Unkown/Undeterminable"="Unknown")),
                                    selectInput("SK_S07_Apex","Apex horizontal location",selected="Central",c("Central","Skewed left","Skewed right")),
                                    selectInput("SK_S07_Parietal","Bulbous/projecting parietal bone",selected="FALSE",c("No"="FALSE","Bilateral","Unilateral(right)"="Unilateral_r","Unilateral(left)"="Unilateral_l")),
                                    checkboxInput("SK_S07_EH","Ethmoidal hypoplasia?",value=FALSE),
                                    checkboxInput("SK_S07_OH","Orbital Hypotelorism?(decreased distance between eyes)",value=FALSE),
                                    textInput("SK_S07_Other","Other associated deformity",value="None")))
SK_S07_RC<-function(input=input){   
  Table<-data.frame(Des1="Trigonocephaly:Anteriorly pointed forehead",Size=NA,Nature=NA)
  if(input$SK_S07_7){
    Table$Size<-paste("eu_eu,g_op,circumference,ec_ec,d_d,keel length,keel height",paste(input$SK_S07_1,input$SK_S07_2,input$SK_S07_3,input$SK_S07_4,input$SK_S07_5,input$SK_S07_7c,input$SK_S07_7d,sep=","),sep=":")
    K<-paste0("Raised Keel on metopic-",FreeFix(input$SK_S07_7b))
  }else{
    Table$Size<-paste0("eu_eu,g_op,circumference,ec_ec,d_d:",paste(input$SK_S07_1,input$SK_S07_2,input$SK_S07_3,input$SK_S07_4,input$SK_S07_5,sep=","))
    K<-FALSE}
  if(length(input$SK_S07_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S07_6C))
    if(sum(input$SK_S07_6=="Other")==0){Sutures<-paste(input$SK_S07_6,collapse=",")}else{Sutures<-paste(c(input$SK_S07_6[input$SK_S07_6!="Other"],input$SK_S07_6b),collapse=",")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  Table$Nature<-paste0("SutureFusion,Sutures,Keel,AnteriorFossa,ApexLocation,ProjectingParietal,EthmoidalHypoplasia,OrbitalHypotelorism,OtherDeformity:",paste(SDes,Suture,K,input$SK_S07_Fossa,input$SK_S07_Apex,SK_S07_Parietal,input$SK_S07_EH,input$SK_S07_OH,input$SK_S07_Other,sep=","))
  Table }
#Cranial base asymmetry########
SK_S08_UI<-tagList(h3("ID:SK_S08"),h4("Description:Asymmetry of the cranial base"),
                             column(width=4,
                                    h4("Measurments"),
                                    h5("n.b. take all measurements from the most anterior point of the given features"),
                                    numericInput("SK_S08_1","Pharyngeal tubercle to right Foramen ovale",value=NA),
                                    numericInput("SK_S08_2","Pharyngeal tubercle to right Foramen spinosum",value=NA),
                                    numericInput("SK_S08_3","Pharyngeal tubercle to right carotid canal",value=NA),
                                    numericInput("SK_S08_4","Pharyngeal tubercle to right Stylomastoid foramen",value=NA),
                                    numericInput("SK_S08_5","Pharyngeal tubercle to Left Foramen ovale",value=NA),
                                    numericInput("SK_S08_6","Pharyngeal tubercle to Left Foramen spinosum",value=NA),
                                    numericInput("SK_S08_7","Pharyngeal tubercle to Left carotid canal",value=NA),
                                    numericInput("SK_S08_8","Pharyngeal tubercle to Left Stylomastoid foramen",value=NA)),
                             column(width=4,h4("Associated deformity"),
                                    h5("nb if there is substantial deformity this should alse be recoreded separately and linked using the link tab"),
                                    selectInput("SK_S08_9","Premature/ unusal fusion of sutures",multiple=TRUE,selected=c("coronal_r","coronal_l","lambdoid_r","lambdoid_l","saggital","squamosal_r","squamosal_l"),c("right coronal"="coronal_r","Left cornal"="coronal_l","right lambdoid"="lambdoid_r","left lambdoid"="lambdoid_l","saggital","right squamosal"="squamosal_r","left squamosal"="squamosal_l","Other"="Other")),
                                    conditionalPanel("input.SK_S08_9",
                                                     conditionalPanel("input.SK_S08_9.indexOf('Other')>=0",selectInput("SK_S08_9b","Other Sutures",multiple=TRUE,c("metopic","ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                   "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                   "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                   "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                   "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                   "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S08_9c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    selectInput("SK_S08_Condyle","Occipital condyle asymmetry (select all that apply)",selected="FALSE",multiple=TRUE,c("None"="FALSE","Size","Alignment","Other")),
                                    conditionalPanel("input.SK_S08_Condyle.indexOf('Other')>=0",textInput("SK_S08_Condyle2","Other Condylar Assymetry",value="Description")),
                                    checkboxInput("SK_S08_OCS","Occipitocervical synostosis",value=FALSE),
                                    checkboxInput("SK_S08_FM","Skewed/ Asymmetric Foramen Magnum",value=FALSE),
                                    textInput("SK_S08_Other","Other associated deformity",value="None")))
SK_S08_RC<-function(input=input){   
  Table<-data.frame(Des1="Asymmerty of the cranial base",Size=NA,Nature=NA)
  Table$Size<-paste("PT_FOr,PT_FSr,PT_CCr,PT_SMFr,PT_FOl,PT_FSl,PT_CCl,PT_SMFl",paste(input$SK_S08_1,input$SK_S08_2,input$SK_S08_3,input$SK_S08_4,input$SK_S08_5,input$SK_S08_6,input$SK_S08_7,input$SK_S08_8,sep=","),sep=":")
  if(length(input$SK_S07_6)>0){
    SDes<-paste0("Premature/incomplete fusion-",FreeFix(input$SK_S07_6C))
    if(sum(input$SK_S07_6=="Other")==0){Sutures<-paste(input$SK_S07_6,collapse="/")}else{Sutures<-paste(c(input$SK_S07_6[input$SK_S07_6!="Other"],input$SK_S07_6b),collapse="/")}
  }else{
    SDes<-"No unusual suture fusion"
    Sutures<-"NA"}
  if(Contains(input$SK_S08_Condyle,"Other")){C<-paste(c(SK_S08_Condyle[SK_S08_Condyle!="Other"],paste0("Other-",FreeFix(SK_S08_Condyle2))),collapse=",")}else{C<-paste(SK_S08_Condyle,collapse=",")}
  Table$Nature<-paste0("SutureFusion,Sutures,CondyleAsymmetry,OCSynostosis,FMAsymmetry,OtherDeformty:",paste(SDes,Sutures,C,input$SK_S08_OCS,input$SK_S08_FM,FreeFix(input$SK_S08_Other),sep=","))
  Table}
#Supernummery bones######
SK_S09_UI<-tagList(h3("ID:SK_S09"),h4("Description:Supernummery bones"),
                             column(width=6,selectInput("SK_S09_1","Type",multiple=TRUE,selected="Lambdoidal",c("Inca","Lambda","Lambdoidal","Right Pterion","Left Pterion","Bregma","Coronal","Sagittal","Right Asterion","Left Asterion","Right Parietal Notch","Left Parietal Notch")),
                                    h4("Description of ossicles:"),
                                    h5("Inca = Large bone at lambda resulting from failure of fusion the mendosal suture, extending from asterion to asterion)"),
                                    h5("Lamdba= Other ossicle at lambda(use if any condition not met for true inca bone)"),
                                    h5("Lambdoidal=any ossicle along the lambdoidal suture but not at lambda (wormian)"),
                                    h5("Pterion= ossicle at pterion"),
                                    h5("Bregma= ossicle at bregma"),
                                    h5("Coronal=any ossicle in the coronal suture not at bregma or pterion"),
                                    h5("Sagittal=any ossicle in the sagittal suture not at bregma or lambda"),
                                    h5("Asterion=ossicle at asterion")),
                             column(width=3,
                                    conditionalPanel("input.SK_S09_1.indexOf('Lambdoidal')>=0",numericInput("SK_S09_Lamb1","Number of right lambdoidal ossicles",value=0),
                                                     numericInput("SK_S09_Lamb2","Number of left lambdoidal ossicles",value=0)),
                                    conditionalPanel("input.SK_S09_1.indexOf('Coronal')>=0",numericInput("SK_S09_Cor1","Number of right coronal ossicles",value=0),
                                                     numericInput("SK_S09_Cor2","Number of left coronal ossicles",value=0)),
                                    conditionalPanel("input.SK_S09_1.indexOf('Sagittal')>=0",numericInput("SK_S09_Sag1","Number of right sagittal ossicles",value=0),
                                                     numericInput("SK_S09_Sag2","Number of left sagittal ossicles",value=0)),
                                    conditionalPanel("input.SK_S09_1.indexOf('Pterion')>=0",selectInput("SK_S09_Pter","Pterion side",c("Bilateral","Right","Left")))))
SK_S09_RC<-function(input=input){   
  Table<-data.frame(Des1="Supernummery bones",Size="NA:NA",Nature=NA)
  Lambdoidal<-"NA/NA";Coronal<-"NA/NA";Sagittal<-"NA/NA";Pterion<-NA
  if(Contains(input$SK_S09_1,"Lambdoidal")){Lambdoidal<-paste(input$SK_S09_Lamb1,input$SK_S09_Lamb2,sep="/")}
  if(Contains(input$SK_S09_1,"Coronal")){Coronal<-paste(input$SK_S09_Cor1,input$SK_S09_Cor2,sep="/")}
  if(Contains(input$SK_S09_1,"Sagittal")){Sagittal<-paste(input$SK_S09_Sag1,input$SK_S09_Sag2,sep="/")}
  if(Contains(input$SK_S09_1,"Pterion")){Pterion<-input$SK_S09_Pter}
  Table$Nature<-paste0("Type,LambdoidalNo(r/l),CoronalNo(r/l),SagittalNo(r/l),Pterion:",paste(paste(input$SK_S09_1,collapse="/"),Lambdoidal,Coronal,Sagittal,Pterion,sep=","))
  Table }
#Unfused metopic#########
SK_S10_UI<-tagList(h3("ID:SK_S10"),h4("Description:Unfused metopic suture"),
                             column(width=6,selectInput("SK_S10_1","Extent of fusion",c("Completely unfused from bregma to nasion","Small area of unfusion from extending 1-2 cm superior to nasion","Other")),
                             numericInput("SK_S10_2","Length from Nasion to bregma (mm)",value=0),
                             conditionalPanel("input.SK_S10_1=='Other'",numericInput("SK_S10_3","length of unfused suture (mm)",value=0)),
                             textInput("SK_S10_4","Additional description of suture",value="None")))
SK_S10_RC<-function(input=input){   
  Table<-data.frame(Des1="Unfused metopic suture",Size=NA,Nature=NA)
  Table$Nature<-paste0("Extent,Additional:",paste(input$SK_S10_1,input$SK_S10_4,sep=","))
  if(input$SK_S10_1=="Other"){Table$Size<-paste0("n_b,LengthUnfused,PercentUnfused:",paste(input$SK_S10_2,input$SK_S10_3,round((input$SK_S10_3/input$SK_S10_2)*100,3),sep=","))
  }else{if(input$SK_S10_1=="Completely unfused from bregma to nasion"){Table$Size<-paste0("n_b,LengthUnfused,PercentUnfused:",paste(input$SK_S10_2,input$SK_S10_2,100,sep=","))
  }else{Table$Size<-paste0("n_b,LengthUnfused,PercentUnfused:",paste(input$SK_S10_2,"1-2cm",(20/input$SK_S10_2)*100,sep=","))}}
  Table }
#Deviated septum######
SK_S11_UI<-tagList(h3("ID:SK_S11"),h4("Description:Deviated nasal septum"),
                             column(width=4,
                                    selectInput("SK_S11_Type","Deviation Type",c("C-shaped","S-Shaped","Dislocation","Spur","Thickening")),
                                    img(src="nasalseptum.png",height=200),
                                    checkboxInput("SK_S11_Turbinates","Enlarged/Pneumatized turbinates?",value=FALSE),
                                    conditionalPanel("input.SK_S11_Turbinates",selectInput("SK_S11_Turbinates2","Which Turbinates?",multiple=TRUE,c("Right inferior"="inf_r","Left inferior"="inf_l","Right Middle"="Mid_r","Left Middle"="Mid_l","Right superior"="Sup_r","Left Superior"="Sup_l"))),
                                    selectInput("SK_S11_Blocking","Blocking of the nasal cavity",selected="FALSE",c("Yes,Left side"="Unilateral_l","Yes,Right side"="Unilateral_r","Yes both sides"="Bilateral","No"="FALSE","Unknown/undeterminable"="Unknown"))),
                             column(width=4,h4("Measurments:"),
                                    h5("Distance from the nasal septum to the border of the nasal cavity at the level of:"),
                                    numericInput("SK_S11_1","The most lateral point on the nasal aperture/Alare (right side)",value=TRUE),
                                    numericInput("SK_S11_2","Alare (left side)",value=TRUE),
                                    numericInput("SK_S11_3","The Superior inferior mid point of the nasal aperture (right side)",value=TRUE),
                                    numericInput("SK_S11_4","Mid (left side)",value=TRUE)))
SK_S11_RC<-function(input=input){   
  Table<-data.frame(Des1="Deviated nasal septum",Size=NA,Nature=NA)
  if(input$SK_S11_Turbinates){Tur<-paste0("True(",paste(input$SK_11_Turbinates2,collapse="/"),")")}else{Tur<-"False"}
  Table$Nature<-paste0("DeviationType,EnlargedTurbinates,Blocking:",paste(input$SK_S11_Type,Tur,input$SK_S11_Blocking,sep=","))
  Table$Size<-paste0("Septum_AlareR,Septum_AlareL,Septum_MidR,Septum_MidL:",paste(input$SK_S11_1,input$SK_S11_2,input$SK_S11_3,input$SK_S11_4,sep=","))
  Table }
#Cleft palate######
SK_S12_UI<-tagList(h3("ID:SK_S12"),h4("Description:Cleft palate"),
                             column(width=4,checkboxInput("SK_S12_Tooth","Separation between tooth sockets?",value=FALSE),
                                    conditionalPanel("input.SK_S12_Tooth",
                                                     selectInput("SK_S12_Tooth2","Associated dental deformity(s)",multiple=TRUE,c("Hypodontia","Hyperdontia","Enamel Hypoplasia","Fused teeth","Microdontia","Macrodontia","Cross bite","Overlapping/crowding")),
                                                     selectInput("SK_S12_Socketr","Right Sockets",selected="None",c("None","I1_I2","I2_C1","Unknown")),
                                                     selectInput("SK_S12_Socketl","Left Sockets",selected="None",c("None","I1_I2","I2_C1","Unknown")),
                                                     conditionalPanel("input.SK_S12_Socketr != 'None'",numericInput("SK_S12_Widthr","width of right cleft between teeth",value=NA)),
                                                     conditionalPanel("input.SK_S12_Socketl != 'None'",numericInput("SK_S12_Widthl","width of left cleft between teeth",value=NA)))),
                                    column(width=4,checkboxInput("SK_S12_Palate","Separation of the hard palate?",value=FALSE),
                                           conditionalPanel("input.SK_S12_Palate",
                                                            selectInput("SK_S12_Bilateral","Side",c("Right"="Unilateral_r","Left"="Unilateral_l","Bilateral")),
                                                            conditionalPanel("input.SK_S12_Bilateral=='Unilateral_r'||input.SK_S12_Bilateral=='Bilateral'",
                                                                             h5("Right"),
                                                                             checkboxInput("SK_S12_Palatine_r","Palatine separation?",value=FALSE),
                                                                             conditionalPanel("input.SK_S12_Palatine_r",checkboxInput("SK_S12_Palatine2_r","Complete palatine separation?",value=FALSE)),
                                                                             checkboxInput("SK_S12_Maxilla_r","Maxilla palatine process separation?",value=FALSE),
                                                                             conditionalPanel("input.SK_S12_Maxilla_r",checkboxInput("SK_S12_Maxilla2_r","Complete Maxilla separation?",value=FALSE)),
                                                                             conditionalPanel("input.SK_S12_Palatine_r && input.SK_S12_Maxilla_r",checkboxInput("SK_S12_Continous1_r","Cleft continous/uninterupted from paltine to maxilla?",value=FALSE)),
                                                                             conditionalPanel("input.SK_S12_Tooth && input.SK_S12_Socketr != 'None'",checkboxInput("SK_S12_Continous2_r","Cleft continous/uninterupted from socket?",value=FALSE)),
                                                                             numericInput("SK_S12_Wr","Maximum width of cleft",value=NA),
                                                                             numericInput("SK_S12_Lr","Length of cleft across palate (i.e up to the alveolar process)",value=NA)),
                                                            conditionalPanel("input.SK_S12_Bilateral=='Unilateral_l'||input.SK_S12_Bilateral=='Bilateral'",
                                                                             h5("Left"),
                                                                             checkboxInput("SK_S12_Palatine_l","Palatine separation?",value=FALSE),
                                                                             conditionalPanel("input.SK_S12_Palatine_l",checkboxInput("SK_S12_Palatine2_l","Complete palatine separation?",value=FALSE)),
                                                                             checkboxInput("SK_S12_Maxilla_l","Maxilla palatine process separation?",value=FALSE),
                                                                             conditionalPanel("input.SK_S12_Maxilla_l",checkboxInput("SK_S12_Maxilla2_l","Complete Maxilla separation?",value=FALSE)),
                                                                             conditionalPanel("input.SK_S12_Palatine_l && input.SK_S12_Maxilla_l",checkboxInput("SK_S12_Continous1_l","Cleft continous/uninterupted from paltine to maxilla?",value=FALSE)),
                                                                             conditionalPanel("input.SK_S12_Tooth && input.SK_S12_Socketl != 'None'",checkboxInput("SK_S12_Continous2_l","Cleft continous/uninterupted from socket?",value=FALSE)),
                                                                             numericInput("SK_S12_Wl","Maximum width of cleft",value=NA),
                                                                             numericInput("SK_S12_Ll","Length of cleft across palate (i.e up to the alveolar process)",value=NA)))))
SK_S12_RC<-function(input=input){   
  Table<-data.frame(Des1="Cleft palate",Size=NA,Nature=NA)
  NatureHead<-NULL;NatureValue<-NULL
  SizeHead<-NULL;SizeValue<-NULL
  if(input$SK_S12_Tooth){
    NatureHead<-c(NatureHead,"DentalDeformity,Socket(r/l)")
    NatureValue<-c(NatureValue,input$SK_S12_Tooth2,paste(input$SK_S12_Socketr,input$SK_S12_Socketl,sep="/"))
    if(input$SK_S12_Socketr!="None" && input$SK_S12_Socketl!= "None"){
      SizeHead<-c(SizeHead,"ToothCleftWidth(r/l)");SizeValue<-c(SizeValue,paste(input$SK_S12_Widthr,input$SK_S12_Widthl,sep="/"))
    }else{
      if(input$SK_S12_Socketr!="None"){SizeHead<-c(SizeHead,"ToothCleftWidth");SizeValue<-c(SizeValue,input$SK_S12_Widthr)}
      if(input$SK_S12_Socketl!="None"){SizeHead<-c(SizeHead,"ToothCleftWidth");SizeValue<-c(SizeValue,input$SK_S12_Widthl)}}}
  if(input$SK_S12_Palate){
    if(input$SK_S12_Bilateral=="Bilateral"){
      NatureHead<-c(NatureHead,"PalatineSeparation(r/l),MaxillaSeparation(r/l),CompletePalatine(r/l),CompleteMaxilla(r/l),PalatinetoMaxilla(r/l),ToothtoPalate(r/l)")
      if(input$SK_S12_Palatine_r){r1<-input$SK_S12_Palatine2_r}else{r1<-NA}
      if(input$SK_S12_Palatine_l){l1<-input$SK_S12_Palatine2_l}else{l1<-NA}
      if(input$SK_S12_Maxilla_r){r2<-input$SK_S12_Maxilla2_r}else{r2<-NA}
      if(input$SK_S12_Maxilla_l){l2<-input$SK_S12_Maxilla2_l}else{l2<-NA}
      if(input$SK_S12_Palatine_r && input$SK_S12_Maxilla_r){r3<-input$SK_S12_Continous1_r}else{r3<-NA}
      if(input$SK_S12_Palatine_l && input$SK_S12_Maxilla_l){l3<-input$SK_S12_Continous1_l}else{l3<-NA}
      if(input$SK_S12_Tooth){
        if(input$SK_S12_Socketr != "None"){r4<-input$SK_S12_Continous2_r}else{r4<-NA}
        if(input$SK_S12_Socketl != "None"){l4<-input$SK_S12_Continous2_l}else{l4<-NA}
      }else{r4<-NA;l4<-NA}
      NatureValue<-c(NatureValue,paste(input$SK_S12_Palatine_r,input$SK_S12_Palatine_l,sep="/"),paste(input$SK_S12_Maxilla_r,input$SK_S12_Maxilla_l,sep="/"),paste(r1,l1,sep="/"),paste(r2,l2,sep="/"),paste(r3,l3,sep="/"),paste(r4,l4,sep="/"))
      SizeHead<-c(SizeHead,"MaxWidth(r/l),Length(r/l)")
      SizeValue<-c(SizeValue,paste(input$SK_S12_Wr,input$SK_S12_Wl,sep="/"),paste(input$SK_S12_Lr,input$SK_S12_Ll,sep="/"))}
    if(input$SK_S12_Bilateral=="Unilateral_r"){
      NatureHead<-c(NatureHead,"PalatineSeparation,MaxillaSeparation,CompletePalatine,CompleteMaxilla,PalatinetoMaxilla,ToothtoPalate")
      if(input$SK_S12_Palatine_r){r1<-input$SK_S12_Palatine2_r}else{r1<-NA}
      if(input$SK_S12_Maxilla_r){r2<-input$SK_S12_Maxilla2_r}else{r2<-NA}
      if(input$SK_S12_Palatine_r && input$SK_S12_Maxilla_r){r3<-input$SK_S12_Continous1_r}else{r3<-NA}
      if(input$SK_S12_Tooth){
        if(input$SK_S12_Socketr != "None"){r4<-input$SK_S12_Continous2_r}else{r4<-NA}
      }else{r4<-NA}
      NatureValue<-c(NatureValue,input$SK_S12_Palatine_r,input$SK_S12_Maxilla_r,r1,r2,r3,r4)
      SizeHead<-c(SizeHead,"MaxWidth,Length")
      SizeValue<-c(SizeValue,input$SK_S12_Wr,input$SK_S12_Lr)}
    if(input$SK_S12_Bilateral=="Unilateral_l"){
      NatureHead<-c(NatureHead,"PalatineSeparation,MaxillaSeparation,CompletePalatine,CompleteMaxilla,PalatinetoMaxilla,ToothtoPalate")
      if(input$SK_S12_Palatine_l){l1<-input$SK_S12_Palatine2_l}else{l1<-NA}
      if(input$SK_S12_Maxilla_l){l2<-input$SK_S12_Maxilla2_l}else{l2<-NA}
      if(input$SK_S12_Palatine_l && input$SK_S12_Maxilla_l){l3<-input$SK_S12_Continous1_l}else{l3<-NA}
      if(input$SK_S12_Tooth){
        if(input$SK_S12_Socketl != "None"){l4<-input$SK_S12_Continous2_l}else{l4<-NA}
      }else{l4<-NA}
      NatureValue<-c(NatureValue,input$SK_S12_Palatine_l,input$SK_S12_Maxilla_l,l1,l2,l3,l4)
      SizeHead<-c(SizeHead,"MaxWidth,Length")
      SizeValue<-c(SizeValue,input$SK_S12_Wl,input$SK_S12_Ll)}
  }
  Table$Nature<-paste(paste(NatureHead,collapse=","),paste(NatureValue,collapse=","),sep=":")
  Table$Size<-paste(paste(SizeHead,collape=","),paste(SizeValue,collapse=","),sep=":")
  Table }
#Suprainion depression########
SK_S13_UI<-tagList(h3("ID:SK_S13"),h4("Description:Suprainion depression"),
                             column(width=4,
                                    numericInput("SK_S13_1","Maximum width (left to right)",value=NA),
                                    numericInput("SK_S13_2","Maximum height (superior inferior)",value=NA),
                                    selectInput("SK_S13_Depth","Depth",c("Shallow-just decernable with the eye or palpertation","Mild-easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced-depression with a maximum depth of between 0.5mm and 1.5mm","Deep-depression in excess of 1.5mm")),
                                    textInput("SK_S13_Shape","Description of shape",value="None")),
                             column(width=4,
                                    h4("Assciated deformity/Abnormality"),
                                    h5("nb if there is substantial deformity this should also be recoreded separately and linked using the link tab"),
                                    checkboxInput("SK_S13_Occipital","Other occipital deformation?",value=FALSE),
                                    conditionalPanel("input.SK_S13_Occipital",textInput("SK_S13_Occipital2","Description of occipital deformation",value="None")),
                                    checkboxInput("SK_S13_Fronto","Fronto-occipital deformation?",value=FALSE),
                                    conditionalPanel("input.SK_S13_Fronto",textInput("SK_S13_Fronto2","Description of fronto-occipital deformation",value="None")),
                                    selectInput("SK_S13_Other","Other associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Expansion/thickening of diploe","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other fracture","Other abnormality")),
                                    conditionalPanel("input.SK_S13_Other",
                                                     textInput("SK_S13_Other2","Location of abnormality in realtion to the depression",value="e.g. along superior margin"),
                                                     textInput("SK_S13_Other3","Further description of associated abnormality",value="None"))))
SK_S13_RC<-function(input=input){   
  Table<-data.frame(Des1="Suprainion depression",Size=NA,Nature=NA)
  Table$Size<-paste0("MxWidth,MxHeight:",paste(input$SK_S13_1,input$SK_S13_2,sep=","))
  if(SK_S13_Occipital){o<-paste0("TRUE-",FreeFix(input$SK_S13_Occipital2))}else{o<-FALSE}
  if(SK_S13_Fronto){f<-paste0("TRUE-",FreeFix(input$SK_S13_Fronto2))}else{f<-FALSE}
  if(length(SK_S13_Other)>0){
    Table$Nature<-paste0("Depth,Shape,OccipitalDeformation,Fronto-occipitalDeformation,OtherDeformity,OtherLocation,OtherDescription:",paste(input$SK_S13_Depth,FreeFix(input$SK_S13_Shape),o,f,paste(SK_S13_Other,collapse="/"),FreeFix(input$SK_S13_Other2),FreeFix(input$SK_S13_Other3),sep=","))
  }else{Table$Nature<-paste0("Depth,Shape,OccipitalDeformation,Fronto-occipitalDeformation,OtherDeformity:",paste(input$SK_S13_Depth,FreeFix(input$SK_S13_Shape),o,f,"None",sep=","))}
  Table }
#Absence EAM####
SK_S14_UI<-tagList(h3("ID:SK_S14"),h4("Description:Absence of external auditory meatus"),
                             h5("nb this section should not be used the record blockage of the auditory meatus by abnormal bone growth for this see:Formation-'Bone growth in or around the external auditory meatus'"),
                             column(width=4,
                                    selectInput("SK_S14_1","Side",c("Right"="Unilateral_r","Left"="Unilateral_l","Bilateral")),
                                    conditionalPanel("input.SK_S14_1=='Unilateral_r'||input.SK_S14_1=='Bilateral'",
                                                     strong("Right"),
                                                     selectInput("SK_S14_Surface","external surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                                     conditionalPanel("input.SK_S14_Surface==('Other')",textInput("SK_S14_Surface2","Other surface description")),
                                                     selectInput("SK_S14_Ossicles","Abnormal Ossicles",selected="Unknown",c("Unknown","No"="FALSE","Yes"="TRUE")),
                                                     conditionalPanel("input.SK_S14_Ossicles==('TRUE')",
                                                                      selectInput("SK_S14_Ossicles2","Which Ossicle?",multiple=TRUE,c("Incus","Malleous","Stapes")),
                                                                      textInput("SK_S14_Ossicles3","Description  of ossicle abnormality",value="None")),
                                                     selectInput("SK_S14_IAM","Internal auditory meatus",c("Normal","Reduced/constricted","Closed/absent","Unknown")),
                                                     textInput("SK_S14_Other","Other Associated deformity",value="None"))),
                             column(width=4,
                                    conditionalPanel("input.SK_S14_1=='Unilateral_l'||input.SK_S14_1=='Bilateral'",
                                                     strong("Left"),
                                                     selectInput("SK_S14_Surface_l","External surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                                     conditionalPanel("input.SK_S14_Surface_l==('Other')",textInput("SK_S14_Surface2_l","Other surface description")),
                                                     selectInput("SK_S14_Ossicles_l","Abnormal Ossicles",selected="Unknown",c("Unknown","No"="FALSE","Yes"="TRUE")),
                                                     conditionalPanel("input.SK_S14_Ossicles_l==('TRUE')",
                                                                      selectInput("SK_S14_Ossicles2_l","Which Ossicle?",multiple=TRUE,c("Incus","Malleous","Stapes")),
                                                                      textInput("SK_S14_Ossicles3_l","Description  of ossicle abnormality",value="None")),
                                                     selectInput("SK_S14_IAM_l","Internal auditory meatus",c("Normal","Reduced/constricted","Closed/absent","Unknown")),
                                                     textInput("SK_S14_Other_l","Other Associated deformity",value="None"))))
SK_S14_RC<-function(input=input){   
  Table<-data.frame(Des="Absence external auditory meatus",Size="NA:NA",Nature=NA)
  if(input$SK_S14_1=="Bilateral"){
    if(input$SK_S14_Surface=="Other"){S1<-paste0("Other-",FreeFix(input$SK_S14_Surface2))}else{S1<-input$SK_S14_Surface}
    if(input$SK_S14_Surface_l=="Other"){S2<-paste0("Other-",FreeFix(input$SK_S14_Surface2_l))}else{S2<-input$SK_S14_Surface_l}
    if(input$SK_S14_Ossicles=="TRUE"){ODes1<-paste0("TRUE-",FreeFix(input$SK_S14_Ossicles3));O1<-paste(input$SK_S14_Ossicles2,sep="_")}else{ODes1<-input$SK_S14_Ossicles;O1<-NA}
    if(input$SK_S14_Ossicles_l=="TRUE"){ODes2<-paste0("TRUE-",FreeFix(input$SK_S14_Ossicles3_l));O2<-paste(input$SK_S14_Ossicles2_l,sep="_")}else{ODes2<-input$SK_S14_Ossicles_l;O2<-NA}
    Table$Nature<-paste0("Bilateral,ExternalSurface(r/l),AbnormalOssicles(r/l),Ossicles(r/l),IAM,Other(r/l):",paste("Bilateral",paste(S1,S2,sep="/"),paste(ODes1,ODes2,sep="/"),paste(O1,o2,sep="/"),paste(input$SK_S14_IAM,input$SK_S14_IAM_l,sep="/"),paste(FreeFix(input$SK_S14_Other),FreeFix(input$SK_S14_Other_l),sep="/"),sep=","))}
  if(input$SK_S14_1=="Unilateral_r"){
    if(input$SK_S14_Surface=="Other"){S1<-paste0("Other-",FreeFix(input$SK_S14_Surface2))}else{S1<-input$SK_S14_Surface}
    if(input$SK_S14_Ossicles=="TRUE"){ODes1<-paste0("TRUE-",FreeFix(input$SK_S14_Ossicles3));O1<-paste(input$SK_S14_Ossicles2,sep="_")}else{ODes1<-input$SK_S14_Ossicles;O1<-NA}
    Table$Nature<-paste0("Bilateral,ExternalSurface,AbnormalOssicles,Ossicles,IAM,Other:",paste("Unilasteral_r",S1,ODes1,O1,input$SK_S14_IAM,FreeFix(input$SK_S14_Other),sep=","))}
  if(input$SK_S14_1=="Unilateral_l"){
    if(input$SK_S14_Surface_l=="Other"){S2<-paste0("Other-",FreeFix(input$SK_S14_Surface2_l))}else{S2<-input$SK_S14_Surface_l}
    if(input$SK_S14_Ossicles_l=="TRUE"){ODes2<-paste0("TRUE-",FreeFix(input$SK_S14_Ossicles3_l));O2<-paste(input$SK_S14_Ossicles2_l,sep="_")}else{ODes2<-input$SK_S14_Ossicles_l;O2<-NA}
    Table$Nature<-paste0("Bilateral,ExternalSurface(r/l),AbnormalOssicles(r/l),Ossicles(r/l),IAM,Other(r/l):",paste("Unilateral_l",S2,ODes2,O2,input$SK_S14_IAM_l,FreeFix(input$SK_S14_Other_l),sep=","))}
  Table }
