#Microcephly##############################################################################################
SK_S01_UI<-tagList(fixedPage(h3("ID:SK_S01"),
                             h4("Description: Microcephly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S01_1","Circumference(mm)",value=NA),
                                    numericInput("SK_S01_2","Height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S01_3","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S01_4","Maximum cranial breadth in the midsagital plane(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
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
                                    textInput("SK_S01_7","Other associated deformity",value="None"),
                                    textInput("SK_S01_8","Any additional/supplement description",value="None")),
                             column(width=4,h4("Connection with other lesions"),
                                   textInput("SK_S01_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("SK_S01_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S01",Type="Shape",Des="Microcephly",Loc="Skull:All:NA",Feat=NA,Size=NA,Shape="General:Abnormally small circumference",Nature=NA,Add=NA,Link=NA)
  Table$Size<-paste("Circumference,ba_b,g_op,eu_eu",paste(input$SK_S01_1,input$SK_S01_2,input$SK_S01_3,input$SK_S01_4,sep=","),sep=":")
  if(input$SK_S01_5){p<-"Proportional size reduction across entire cranium"}else{p<-input$SK_S01_5b}
  if(input$SK_S01_7=="None"){o<-"No additional deformity"}else{o<-input$SK_S01_7}
  if(length(input$SK_S01_6)>0){
    if(sum(input$SK_S01_6=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S01_6,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S01_6[input$SK_S01_6!="Other"],input$SK_S01_6b),collapse=","),sep=":")}
    s<-input$SK_S01_6c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  Table$Nature<-paste("Proportionality,suture fusion,other",paste(p,s,o,sep=","),sep=":")
  Table$Add<-input$SK_S01_8
  if(input$SK_S01_Link1 != "None"){Table$Link<-paste(paste(input$SK_S01_Link1,collapse=","),paste(input$SK_S01_Link2,collapse=","),sep=":")}
  Table }
#Macrocephly#############################################################################################################################################
SK_S02_UI<-tagList(fixedPage(h3("ID:SK_S02"),
                             h4("Description:Macrocephly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S02_1","Circumference(mm)",value=NA),
                                    numericInput("SK_S02_2","Height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S02_3","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S02_4","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S02_8","Bizygomatic breadth (mm)",value=NA),
                                    numericInput("SK_S02_9","Minimum frontal breadth(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
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
                                                     textInput("SK_S02_6c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    textInput("SK_S02_7","Other associated deformity",value="None"),
                                    textInput("SK_S02_8","Any additional/supplement description",value="None")),
                             column(width=4,h4("Connection with other lesions"),
                                    textInput("SK_S02_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S02_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S02",Type="Shape",Des="Macrocephly",Loc="Skull:All:NA",Feat="NA:NA",Size=NA,Shape="General:Abnormally large/wide",Nature=NA,Add=NA,Link=NA)
  Table$Size<-paste("Circumference,ba_b,g_op,eu_eu,zy_zy,ft_ft",paste(input$SK_S02_1,input$SK_S02_2,input$SK_S02_3,input$SK_S02_4,input$SK_S02_8,input$SK_SO2_9,sep=","),sep=":")
  if(input$SK_S02_5){p<-"Proportional size increase across entire cranium"}else{p<-input$SK_S02_5b}
  if(input$SK_S02_7=="None"){o<-"No additional deformity"}else{o<-input$SK_S02_7}
  if(length(input$SK_S02_6)>0){
    if(sum(input$SK_S02_6=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S02_6,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S02_6[input$SK_S02_6!="Other"],input$SK_S02_6b),collapse=","),sep=":")}
    s<-input$SK_S02_6c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  Table$Nature<-paste("Proportionality,suture fusion,other",paste(p,s,o,sep=","),sep=":")
  Table$Add<-input$SK_S02_8
  if(input$SK_S02_Link1 != "None"){Table$Link<-paste(paste(input$SK_S02_Link1,collapse=","),paste(input$SK_S02_Link2,collapse=","),sep=":")}
  Table }
#Scaphocephaly#############################################################################################################################################
SK_S03_UI<-tagList(fixedPage(h3("ID:SK_S03"),
                             h4("Description:Scaphocephaly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S03_1","Cranial height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S03_2","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S03_3","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S03_4","Length from bregma to lambda (mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
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
                                    conditionalPanel("input.SK_S03_7",textInput("SK_S03_7b","Nature of Keel"),
                                                     textInput("SK_S03_7c","Length of keel(mm)"),
                                                     textInput("SK_S03_7d","Height of keel(mm)")),
                                    checkboxInput("SK_S03_8","Low set orbits (in relation to frontal bone)",value=FALSE),
                                    checkboxInput("SK_S03_9","Bulbous/projecting frontal bone?",value=FALSE),
                                    textInput("SK_S03_10","Other associated deformity",value="None"),
                                    textInput("SK_S03_11","Any additional/supplement description",value="None")),
                             column(width=4,h4("Connection with other lesions"),
                                    textInput("SK_S03_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S03_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S03",Type="Shape",Des="Scaphocephaly",Loc="Skull:All:NA",Feat=NA,Size=NA,Shape="General:Anterior posterior elongation",Nature=NA,Add=NA,Link=NA)
  if(input$SK_S03_7){Table$Size<-paste("ba_b,g_op,eu_eu,b_l,keel length,keel height",paste(input$SK_S03_1,input$SK_S03_2,input$SK_S03_3,input$SK_S03_4,input$SK_S03_7c,input$SK_S03_7d,sep=","),sep=":")
  }else{Table$Size<-paste("ba_b,g_op,eu_eu,b_l",paste(input$SK_S03_1,input$SK_S03_2,input$SK_S03_3,input$SK_S03_4,sep=","),sep=":")}
  if(length(input$SK_S03_6)>0){
    if(sum(input$SK_S03_6=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S03_6,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S03_6[input$SK_S03_6!="Other"],input$SK_S03_6b),collapse=","),sep=":")}
    s<-input$SK_S03_6c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  if(input$SK_S03_7){k<-paste("TRUE",input$SK_S03_7b,sep="/")}else{k<-"FALSE/NA"}
  Table$Nature<-paste("Suture fusion,sagittal Keel,low set orbits,projecting fontal,other",paste(s,k,input$SK_S03_8,input$SK_S03_9,input$SK_S03_10,sep=","),sep=":")
  Table$Add<-input$SK_S03_11
  if(input$SK_S03_Link1 != "None"){Table$Link<-paste(paste(input$SK_S03_Link1,collapse=","),paste(input$SK_S03_Link2,collapse=","),sep=":")}
  Table }
#Oxycephaly#############################################################################################################################################
SK_S04_UI<-tagList(fixedPage(h3("ID:SK_S04"),
                             h4("Description:Oxycephaly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S04_1","Cranial height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S04_2","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S04_3","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S04_4","Forehead height measured from nasion to bregma (mm)",value=NA),
                                    numericInput("SK_S04_5","Upper face height from nasion to prosthion(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
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
                                    selectInput("SK_S04_8","Nature of increase in cranial height",selected="Straight increase in height effecting frontal,parietal and occipital",c("Straight increase in height effecting frontal,parietal and occipital","Increase in height skewed anteriorly (mostly effecting frontal)","Increase in height skewed centerally (mostly effecting parietal)","Increase in height skewed posteriorly (mostly effecting occipital)","Increased in height laterally skewed (right side)","Increase in height laterally skewed (left side)","Other")),
                                    conditionalPanel("input.SK_S04_8=='Other'",textInput("SK_S04_8b","Custom height increase description",value="NA")),
                                    textInput("SK_S04_9","Other associated deformity",value="None"),
                                    textInput("SK_S04_10","Any additional/supplement description",value="None")),
                             column(width=4,h4("Connection with other lesions"),
                                    textInput("SK_S04_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S04_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))
                             ))
SK_S04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S04",Type="Shape",Des="Oxycephaly",Loc="Skull:All:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Size<-paste("ba_b,g_op,eu_eu,n_b,n_pr",paste(input$SK_S04_1,input$SK_S04_2,input$SK_S04_3,input$SK_S04_4,input$SK_S04_5,sep=","),sep=":")
  if(length(input$SK_S04_6)>0){
    if(sum(input$SK_S04_6=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S04_6,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S04_6[input$SK_S04_6!="Other"],input$SK_S04_6b),collapse=","),sep=":")}
    s<-input$SK_S04_6c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  Table$Nature<-paste("suture fusion,other:",s,input$SK_S04_9,sep=",")
  if(input$SK_S04_8=="Other"){Table$Shape<-paste("General,top of head,height increase type:Superior inferior elongation",input$SK_S04_7,input$SK_S04_8b,sep=",")
  }else{Table$Shape<-paste("General,top of head,height increase type:Superior inferior elongation",input$SK_S04_7,input$SK_S04_8,sep=",")}
  Table$Add<-input$SK_S04_10
  if(input$SK_S04_Link1 != "None"){Table$Link<-paste(paste(input$SK_S04_Link1,collapse=","),paste(input$SK_S04_Link2,collapse=","),sep=":")}
  Table }
#Brachycephaly#############################################################################################################################################
SK_S05_UI<-tagList(fixedPage(h3("ID:SK_S05"),
                             h4("Description:Brachycephaly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S05_1","Cranial height measured from basion to bregma(mm)",value=NA),
                                    numericInput("SK_S05_2","Maximum cranial Length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S05_3","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S05_4","Forehead height measured from nasion to bregma (mm)",value=NA),
                                    numericInput("SK_S05_5","Minimum frontal breadth(mm)",value=NA)),
                             column(width=4,h4("Associated deformity"),
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
                                    textInput("SK_S05_9","Other associated deformity",value="None"),
                                    textInput("SK_S05_10","Any additional/supplement description",value="None")),
                             column(width=4,h4("Connection with other lesions"),
                                    textInput("SK_S05_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S05_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))
))
SK_S05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S05",Type="Shape",Des="Brachycephaly",Loc="Skull:ALL:NA",Feat="NA:NA",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Size<-paste("ba_b,g_op,eu_eu,n_b,ft_ft",paste(input$SK_S05_1,input$SK_S05_2,input$SK_S05_3,input$SK_S05_4,input$SK_S05_5,sep=","),sep=":")
  if(length(input$SK_S05_6)>0){
    if(sum(input$SK_S05_6=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S05_6,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S05_6[input$SK_S05_6!="Other"],input$SK_S05_6b),collapse=","),sep=":")}
    s<-input$SK_S05_6c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  Table$Nature<-paste("Suture,Projectng parietal,other",paste(s,input$SK_S05_7,input$SK_S05_9,sep=","),sep=":")
  if(input$SK_S05_8=="Other"){Table$Shape<-paste("General,height increase:Flatterning of the posterior skull",input$SK_S05_8b,sep=",")
  }else{Table$Shape<-paste("General,height increase:Flatterning of the posterior skull",input$SK_S05_8,sep=",")}
  Table$Add<-input$SK_S05_10
  if(input$SK_S05_Link1 != "None"){Table$Link<-paste(paste(input$SK_S05_Link1,collapse=","),paste(input$SK_S05_Link2,collapse=","),sep=":")}
  Table }
#Plagiocephly#############################################################################################################################################
SK_S06_UI<-tagList(fixedPage(h3("ID:SK_S06"),
                             h4("Description:Plagiocephly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S06_1","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S06_2","Maximum cranial length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S06_3","Distance from the Left frontozygomatic point to the occipital bone(mm)",value=NA),
                                    numericInput("SK_S06_4","Distance from the right frontozygomatic point to the occipital bone(mm)",value=NA),
                                    h5("nb frontozygomatic point= the most lateral point on the frontozygomatic suture.These two measurments should be taken at the same angle (a) from the line denoting maximum cranial length"),
                                    img(src="fmt_ocp.png",height=200)),
                             column(width=4,h4("Associated deformity"),
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
                                    textInput("SK_S06_11","Other Associated deformity",value="None")),
                             column(width=4,textInput("SK_S06_12","Any additional information",value="None"),
                                    h4("Connection with other lesions"),
                                    textInput("SK_S06_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S06_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S06_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S06",Type="Shape",Des="Plagiocephly",Loc="Skull:All:NA",Feat="NA:NA",Size=NA,Shape="General:Asymmetric distortion",Nature="Asymmetrical distortion",Add=NA,Link=NA)
  Table$Add<-input$SK_S06_12
  
  if(length(input$SK_S06_6)>0){
    if(sum(input$SK_S06_6=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S06_6,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S06_6[input$SK_S06_6!="Other"],input$SK_S06_6b),collapse=","),sep=":")}
    s<-input$SK_S06_6c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  if(input$SK_S06_7){sd<-input$SK_S06_7b}else{sd<-"FALSE"}
  if(input$SK_S06_8){
    if(sum(input$SK_S06_8b=='Other')>0){n<-paste(c(input$SK_S06_8b[input$SK_S06_8b!='Other'],input$SK_S06_8i),collapse="/")
    }else{n<-paste(input$SK_S06_8b,collapse="/")}
    if(sum(input$SK_S06_8b=='Size difference')>0){ns<-paste(input$SK_S06_8c,input$SK_S06_8d,input$SK_S06_8e,input$SK_S06_8f,sep="/")}else{ns<-"NA"}
    if(sum(input$SK_S06_8b=='Not aligned')){na<-paste(input$SK_S06_8g,input$SK_S06_8h,sep="/")}else{na<-"NA"}
    sizen<-paste(ns,na,sep=",")
  }else{sizen<-"NA,NA";n<-"FALSE"}
  if(input$SK_S06_9){
    if(sum(input$SK_S06_9b=='Other')>0){o<-paste(c(input$SK_S06_9b[input$SK_S06_9b!='Other'],input$SK_S06_9i),collapse="/")
    }else{o<-paste(input$SK_S06_9b,collapse="/")}
    if(sum(input$SK_S06_9b=='Size difference')>0){os<-paste(input$SK_S06_9c,input$SK_S06_9d,input$SK_S06_9e,input$SK_S06_9f,sep="/")}else{os<-"NA"}
    if(sum(input$SK_S06_9b=='Not aligned')){oa<-paste(input$SK_S06_9g,input$SK_S06_9h,sep="/")}else{oa<-"NA"}
    sizeo<-paste(os,oa,sep=",")
  }else{sizeo<-"NA,NA";o<-"FALSE"}
  if(input$SK_S06_10){e<-"TRUE";sizee<-paste(input$SK_S06_10b,input$SK_S06_10c,sep="/")
  }else{e<-"False";sizee<-"NA"}
  Table$Nature<-paste("Suture,Sagittal deviation,Nasal asymmetry,orbital asymmetry,ear shift,other",paste(s,sd,n,o,e,input$SK_S06_11,sep=","),sep=":")
  Table$Size<-paste("eu_eu,g_op,fmtr_opl,fmtl_opr,NasalSize(rw/rh/lw/lh),NasalAlignment(inf/sup),OrbitSize(rw/rh/lw/lh),OrbitAlignment(inf/sup),EarShift(ant-pos/sup-inf)",
                    paste(input$SK_S06_1,input$SK_S06_2,input$SK_S06_3,input$SK_S06_4,sizen,sizeo,sizee,sep=","),sep=":")
  if(input$SK_S06_Link1 != "None"){Table$Link<-paste(paste(input$SK_S06_Link1,collapse=","),paste(input$SK_S06_Link2,collapse=","),sep=":")}
  Table }
#Trigonocephaly#############################################################################################################################################
SK_S07_UI<-tagList(fixedPage(h3("ID:SK_S07"),
                             h4("Description:Trigonocephaly"),
                             column(width=4,h4("Measurments"),
                                    numericInput("SK_S07_1","Maximum cranial breadth in the midsagital plane(mm)",value=NA),
                                    numericInput("SK_S07_2","Maximum cranial length from glabella to opisthocranion(mm)",value=NA),
                                    numericInput("SK_S07_3","Head circumference(mm)",value=NA),
                                    numericInput("SK_S07_4","Biorbital Breadth(mm)",value=NA),
                                    numericInput("SK_S07_5","Interorbital Breadth",value=NA)),
                             column(width=4,h4("Associated deformity"),
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
                                    selectInput("SK_S07_8","Reduced anterior cranial fossa",c("Yes,Bilateral"="Bilateral","Yes,left side only"="Unilateral_l","Yes,right side only"="Unilateral_r","No"="FALSE","Unkown/Undeterminable"="Unknown")),
                                    selectInput("SK_S07_13","Apex horizontal location",selected="Central",c("Central","Skewed left","Skewed right")),
                                    selectInput("SK_S07_14","Bulbous/projecting parietal bone",selected="FALSE",c("No"="FALSE","Bilateral","Unilateral(right)"="Unilateral_r","Unilateral(left)"="Unilateral_l")),
                                    checkboxInput("SK_S07_9","Ethmoidal hypoplasia?",value=FALSE),
                                    checkboxInput("SK_S07_10","Orbital Hypotelorism?(decreased distance between eyes)",value=FALSE),
                                    textInput("SK_S07_11","Other associated deformity",value="None"),
                                    textInput("SK_S07_12","Any additional/supplement description",value="None")),
                             column(width=4,h4("Connection with other lesions"),
                                    textInput("SK_S07_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S07_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S07_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S07",Type="Shape",Des="Trigonocephaly",Loc="Skull:All:NA",Feat="NA:NA",Size=NA,Shape="General:Anteriorly pointed forhead",Nature=NA,Add=NA,Link=NA)
  if(input$SK_S07_7){
    Table$Size<-paste("eu_eu,g_op,circumference,ec_ec,d_d,keel length,keel height",paste(input$SK_S07_1,input$SK_S07_2,input$SK_S07_3,input$SK_S07_4,input$SK_S07_5,input$SK_S07_7c,input$SK_S07_7d,sep=","),sep=":")
    k<-input$SK_S07_7b
  }else{
    Table$Size<-paste("eu_eu,g_op,circumference,ec_ec,d_d,keel length,keel height",paste(input$SK_S07_1,input$SK_S07_2,input$SK_S07_3,input$SK_S07_4,input$SK_S07_5,NA,NA,sep=","),sep=":")
    k<-"None"}
  if(length(input$SK_S07_6)>0){
    if(sum(input$SK_S07_6=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S07_6,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S07_6[input$SK_S07_6!="Other"],input$SK_S07_6b),collapse=","),sep=":")}
    s<-input$SK_S07_6c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  Table$Nature<-paste("Suture,Keel,Reduced fossa,Ethmoidal hypoplasia,Hypoterlorism,Apex,Projectng parietal,other",paste(s,k,input$SK_S07_8,input$SK_S07_9,input$SK_S07_10,input$SK_S07_13,input$SK_S07_14,input$SK_S07_11,sep=","),sep=":")
  Table$Add<-input$SK_S07_12
  if(input$SK_S07_Link1 != "None"){Table$Link<-paste(paste(input$SK_S07_Link1,collapse=","),paste(input$SK_S07_Link2,collapse=","),sep=":")}
  Table }
#Cranial base asymmetry#############################################################################################################################################
SK_S08_UI<-tagList(fixedPage(h3("ID:SK_S08"),
                             h4("Description:Asymmetry of the cranial base"),
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
                                    selectInput("SK_S08_9","Premature/ unusal fusion of sutures",multiple=TRUE,selected=c("coronal_r","coronal_l","lambdoid_r","lambdoid_l","saggital","squamosal_r","squamosal_l"),c("right coronal"="coronal_r","Left cornal"="coronal_l","right lambdoid"="lambdoid_r","left lambdoid"="lambdoid_l","saggital","right squamosal"="squamosal_r","left squamosal"="squamosal_l","Other"="Other")),
                                    conditionalPanel("input.SK_S08_9",
                                                     conditionalPanel("input.SK_S08_9.indexOf('Other')>=0",selectInput("SK_S08_9b","Other Sutures",multiple=TRUE,c("metopic","ethmoidovomerian","frontoethmoidal","frontosphenoid","intermaxillary","internasal","interpalatine","sphenoethmoidal","spheno-occipital synchondrosis"="synchondrosis","sphenovomerian",
                                                                                                                                                                   "right conchal-maxillary"="conchalmaxillary_r","left conchal-maxillary"="conchalmaxillary_l","right conchal-palatine"="conchalpalatine_r","left conchal-palatine"="conchalpalatine_l","right ethmoidoconchal"="ethmoidoconchal_r","left ethmoidoconchal"="ethmoidoconchal_l","right ethmoidolacrimal"="ethmoidolacrimal_r","left ethmoidolacrimal"="ethmoidolacrimal_l","right ethmoidomaxillary"="ethmoidomaxillary_r","left ethmoidomaxillary"="ethmoidomaxillary_l","right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l",
                                                                                                                                                                   "right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l","right lacrimoconchal"="lacrimoconchal_r","left lacrimoconchal"="lacrimoconchal_l","right lacromaxillary","lacromaxillary_r","left lacromaxillary","lacromaxillary_l","right nasoethmodial"="nasoethmodial_r","left nasoethmodial"="nasoethmodial_l",
                                                                                                                                                                   "right nasomaxillary"="nasomaxillary_r","left nasomaxillary"="nasomaxillary_l","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right palatoethmoidal"="palatoethmoidal_r","left palatoethmoidal"="palatoethmoidal_l","right palatomaxillary"="palatomaxillary_r","left palatomaxillary"="palatomaxillary_l","right parietomastoid"="parietomastoid_r","left parietomastoid","right petro-occipital"="petrooccipital_r","left petro-occipital"="petrooccipital_l",
                                                                                                                                                                   "right petrosquamosal"="petrosquamosal_r","left petrosquamosal"="petrosquamosal_l","right sphenomaxillary"="sphenomaxillary_r","left sphenomaxillary"="sphenomaxillary_l","right sphenopalatine"="sphenopalatine_r","left sphenopalatine"="sphenopalatine_l","right sphenoparietal"="sphenoparietal_r","left,sphenoparietal"="sphenoparietal_l","right sphenosquamosal"="sphenosquamosal_r","left sphenosquamosal"="sphenosquamosal_l","right sphenozygomatic"="sphenozygomatic_r","left sphenozygomatic"="sphenozygomatic_l",
                                                                                                                                                                   "right vomer-maxillary"="vomermaxillary_r","left vomer-maxillary"="vomermaxillary_l","right vomer-palatine"="vomerpalatine_r","left vomer-palatine"="vomerpalatine_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right zygomaticomaxillary"="zygomaticomaxillary_r","left zygomaticomaxillary"="zygomaticomaxillary_l","right zygomaticotemporal"="zygomaticotemporal_r","left zygomaticotemporal"="zygomaticotemporal_l"))),
                                                     textInput("SK_S08_9c","Description of unusal fusion",value="eg. premature complete fusion")),
                                    selectInput("SK_S08_10","Occipital condyle asymmetry (select all that apply)",selected="FALSE",multiple=TRUE,c("None"="FALSE","Size","Alignment","Other")),
                                    conditionalPanel("input.SK_S08_10.indexOf('Other')>=0",textInput("SK_S08_10b","Other Condylar Assymetry",value="Description")),
                                    checkboxInput("SK_S08_11","Occipitocervical synostosis",value=FALSE),
                                    checkboxInput("SK_S08_12","Skewed/ Asymetric Foramen Magnum",value=FALSE),
                                    textInput("SK_S08_13","Other associated deformity",value="None"),
                                    textInput("SK_S08_14","Any additional/supplement description",value="None")),
                             column(width=4,h4("Connection with other lesions"),
                                    textInput("SK_S08_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S08_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))
                             ))
SK_S08_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S08",Type="Shape",Des="Asymmerty of the cranial base",Loc="Skull:All:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Size<-paste("PT_FOr,PT_FSr,PT_CCr,PT_SMFr,PT_FOl,PT_FSl,PT_CCl,PT_SMFl",paste(input$SK_S08_1,input$SK_S08_2,input$SK_S08_3,input$SK_S08_4,input$SK_S08_5,input$SK_S08_6,input$SK_S08_7,input$SK_S08_8,sep=","),sep=":")
  if(length(input$SK_S08_9)>0){
    if(sum(input$SK_S08_9=="Other")==0){Table$Feat<-paste("NA",paste(input$SK_S08_9,collapse=","),sep=":")}
    else{Table$Feat<-paste("NA",paste(c(input$SK_S08_9[input$SK_S08_9!="Other"],input$SK_S08_9b),collapse=","),sep=":")}
    s<-input$SK_S08_9c
  }else{
    Table$Feat<-"NA:NA"
    s<-"No unusual suture fusion"
  }
  if(sum(input$SK_S08_10=="Other")>0){ca<-paste(c(input$SK_S08_10[input$SK_S08_10!="Other"],input$SK_S08_10b),collapse="/")}else{ca<-paste(input$SK_S08_10,collapse="/")}
  Table$Add<-input$SK_S08_14
  Table$Nature<-paste("Suture,Condyle,Occcipitocervical sysnostosis,Skewed FM,other",paste(s,ca,input$SK_S08_11,input$SK_S08_12,input$SK_S08_13,sep=","),sep=":")
  if(input$SK_S08_Link1 != "None"){Table$Link<-paste(paste(input$SK_S08_Link1,collapse=","),paste(input$SK_S08_Link2,collapse=","),sep=":")}
  Table}
#Supernummery bones#############################################################################################################################################
SK_S09_UI<-tagList(fixedPage(h3("ID:SK_S09"),
                             h4("Description:Supernummery bones"),
                             column(width=6,selectInput("SK_S09_1","Type",multiple=TRUE,selected="Lambdoidal",c("Inca","Lambda","Lambdoidal","Pterion","Bregma","Coronal","Sagittal")),
                                    h4("Description of ossicles:"),
                                    h5("Inca = Large bone at lambda resulting from failure of fusion the mendosal suture, extending from asterion to asterion)"),
                                    h5("Lamdba= Other ossicle at lambda(use if any condition not met for true inca bone)"),
                                    h5("Lambdoidal=any ossicle along the lambdoidal suture but not at lambda (wormian)"),
                                    h5("Pterion= ossicle at pterion"),
                                    h5("Bregma= ossicle at bregma"),
                                    h5("Coronal=any ossicle in the coronal suture not at bregma or pterion"),
                                    h5("Sagittal=any ossicle in the sagittal suture not at bregma or lambda")),
                             column(width=3,
                                    conditionalPanel("input.SK_S09_1.indexOf('Lambdoidal')>=0",numericInput("SK_S09_2a","Number of right lambdoidal ossicles",value=0),
                                                     numericInput("SK_S09_2b","Number of left lambdoidal ossicles",value=0)),
                                    conditionalPanel("input.SK_S09_1.indexOf('Coronal')>=0",numericInput("SK_S09_3a","Number of right coronal ossicles",value=0),
                                                     numericInput("SK_S09_3b","Number of left coronal ossicles",value=0)),
                                    conditionalPanel("input.SK_S09_1.indexOf('Sagittal')>=0",numericInput("SK_S09_4a","Number of right sagittal ossicles",value=0),
                                                     numericInput("SK_S09_4b","Number of left sagittal ossicles",value=0)),
                                    conditionalPanel("input.SK_S09_1.indexOf('Pterion')>=0",selectInput("SK_S09_5","Pterion side",c("Bilateral","Right","Left")))),
                             column(width=3,h4("Connection with other lesions"),
                                    textInput("SK_S09_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S09_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S09_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S09",Type="Shape",Des="Supernummery bones",Loc="Skull:Snum:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Loc<-paste("Skull:Snum",paste(input$SK_S09_1,collapse=","),sep=":")
  if(sum(input$SK_S09_1=='Lambdoidal')>0){L<-paste(input$SK_S09_2a,input$SK_S09_2b,sep="/")}else{L<-"NA"}
  if(sum(input$SK_S09_1=='Coronal')>0){C<-paste(input$SK_S09_3a,input$SK_S09_3b,sep="/")}else{C<-"NA"}
  if(sum(input$SK_S09_1=='Sagittal')>0){S<-paste(input$SK_S09_4a,input$SK_S09_4b,sep="/")}else{S<-"NA"}
  if(sum(input$SK_S09_1=='Pterion')>0){P<-input$SK_S09_5}else{P<-"NA"}
  Table$Nature<-paste("no.Lambdoidal,no.Cornal,no.Sagittal,Pterion side",paste(L,C,S,P,sep=","),sep=":")
  if(input$SK_S09_Link1 != "None"){Table$Link<-paste(paste(input$SK_S09_Link1,collapse=","),paste(input$SK_S09_Link2,collapse=","),sep=":")}
  Table }
#Unfused metopic#############################################################################################################################################
SK_S10_UI<-tagList(fixedPage(h3("ID:SK_S10"),
                             h4("Description:Unfused metopic suture"),
                             column(width=6,selectInput("SK_S10_1","Extent of fusion",c("Completely unfused from bregma to nasion","Small area of unfusion from extending 1-2 cm superior to nasion","Other")),
                             numericInput("SK_S10_2","Length from Nasion to bregma (mm)",value=0),
                             conditionalPanel("input.SK_S10_1=='Other'",numericInput("SK_S10_3","length of unfused suture (mm)",value=0)),
                             textInput("SK_S10_4","Additional description of suture",value="None")),
                             column(width=6,h4("Connection with other lesions"),
                                    textInput("SK_S10_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S10_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S10_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S10",Type="Shape",Des="Unfused metopic suture",Loc="Skull:Frontal:NA",Feat="NA:Metopic",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Add<-input$SK_S10_4
  Table$Nature<-paste("Extent",input$SK_S10_1,sep=":")
  if(input$SK_S10_1=="Other"){percent<-round((input$SK_S10_3/input$SK_S10_2)*100,3)}
  else{if(input$SK_S10_1=="Completely unfused from bregma to nasion"){percent<-100}else{percent<-(20/input$SK_S10_2)*100}}
  Table$Size<-paste("n_b,percent unfused",paste(input$SK_S10_2,percent,sep=","),sep=":")
  if(input$SK_S10_Link1 != "None"){Table$Link<-paste(paste(input$SK_S10_Link1,collapse=","),paste(input$SK_S10_Link2,collapse=","),sep=":")}
  Table }
#Deviated septum#############################################################################################################################################
SK_S11_UI<-tagList(fixedPage(h3("ID:SK_S11"),
                             h4("Description:Deviated nasal septum"),
                             column(width=4,
                                    selectInput("SK_S11_1","Deviation Type",c("C-shaped","S-Shaped","Dislocation","Spur","Thickening")),
                                    img(src="nasalseptum.png",height=200),
                                    checkboxInput("SK_S11_2","Enlarged/Pneumatized turbinates?",value=FALSE),
                                    conditionalPanel("input.SK_S11_2",selectInput("SK_S11_2b","Which Turbinates?",multiple=TRUE,c("Right inferior"="inf_r","Left inferior"="inf_l","Right Middle"="Mid_r","Left Middle"="Mid_l","Right superior"="Sup_r","Left Superior"="Sup_l"))),
                                    selectInput("SK_S11_3","Blocking of the nasal cavity",selected="FALSE",c("Yes,Left side"="Unilateral_l","Yes,Right side"="Unilateral_r","Yes both sides"="Bilateral","No"="FALSE","Unknown/undeterminable"="Unknown"))),
                             column(width=4,h4("Measurments:"),
                                    h5("Distance from the nasal septum to the border of the nasal cavity at the level of:"),
                                    numericInput("SK_S11_4","The most lateral point on the nasal aperture/Alare (right side)",value=TRUE),
                                    numericInput("SK_S11_5","Alare (left side)",value=TRUE),
                                    numericInput("SK_S11_6","The Superior inferior mid point of the nasal aperture (right side)",value=TRUE),
                                    numericInput("SK_S11_7","Mid (left side)",value=TRUE),
                                    numericInput("SK_S11_8","The most inferior projection of the nasal bone (right side)",value=TRUE),
                                    numericInput("SK_S11_9","inferior nasal projection(left side)",value=TRUE)),
                             column(width=3,
                                    textInput("SK_S11_10","Any additional/supplement description",value="None"),
                                    h4("Connection with other lesions"),
                                    textInput("SK_S11_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S11_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S11_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S11",Type="Shape",Des="Deviated nasal septum",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Cleft palate#############################################################################################################################################
SK_S12_UI<-tagList(fixedPage(h3("ID:SK_S12"),
                             h4("Description:Cleft palate"),
                             column(width=4,checkboxInput("SK_S12_1","Separation between tooth sockets?",value=FALSE),
                                    conditionalPanel("input.SK_S12_1",
                                                     selectInput("SK_S12_1b","Which Sockets",multiple=TRUE,selected="rI1_rI2",c("rI1_rI2","rI2_rC1","lI1_lI2","lI2_lC1")),
                                                     selectInput("SK_S12_1c","Associated dental deformity",multiple=TRUE,c("Hypodontia","Hyperdontia","Enamel Hypoplasia","Fused teeth","Microdontia","Macrodontia","Cross bite","Overlapping/crowding")),
                                                     textInput("SK_S12_1d","Description of cleft",value="None"),
                                                     numericInput("SK_S12_1e","Maximum width of cleft",value=NA),
                                                     numericInput("SK_S12_1f","Length of cleft",value=NA),
                                                     conditionalPanel("input.SK_S12_1b.length>1",
                                                                      numericInput("SK_S12_1g","Maximum width of cleft (Left side)",value=NA),
                                                                      numericInput("SK_S12_1h","Length of cleft (Left side)",value=NA)))),
                             column(width=4,checkboxInput("SK_S12_2","Separation of the hard palate?",value=FALSE),
                                    conditionalPanel("input.SK_S12_2",
                                                     checkboxInput("SK_S12_2b","Palatine separation?",value=FALSE),
                                                     conditionalPanel("input.SK_S12_2b",checkboxInput("SK_S12_2c","Complete palatine separation?",value=FALSE)),
                                                     checkboxInput("SK_S12_2d","Maxilla palatine process separation?",value=FALSE),
                                                     conditionalPanel("input.SK_S12_2d",checkboxInput("SK_S12_2e","Complete Maxilla separation?",value=FALSE),
                                                                      conditionalPanel("input.SK_S12_2b",checkboxInput("SK_S12_2f","Cleft continous/uninterupted from paltine to maxilla?",value=FALSE))),
                                                     numericInput("SK_S12_2g","Maximum width of cleft",value=NA),
                                                     numericInput("SK_S12_2h","Length of cleft across palate (i.e up to the alveolar process)",value=NA),
                                                     textInput("SK_S12_2i","Description of cleft",value="None"),
                                                     conditionalPanel("input.SK_S12_1",checkboxInput("SK_S12_3","Cleft continous/uninterupted from palate to socket?",value=FALSE)))),
                             column(width=3,
                                    textInput("SK_S12_4","Any additional/supplement description",value="None"),
                                    h4("Connection with other lesions"),
                                    textInput("SK_S12_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S12_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S12_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S12",Type="Shape",Des="Cleft palate",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Suprainion depression#############################################################################################################################################
SK_S13_UI<-tagList(fixedPage(h3("ID:SK_S13"),
                             h4("Description:Suprainion depression"),
                             column(width=4,
                                    numericInput("SK_S13_1","Maximum width (left to right)",value=NA),
                                    numericInput("SK_S13_2","Maximum height (superior inferior)",value=NA),
                                    selectInput("SK_S13_3","Depth",c("Shallow, just decernable with the eye or palpertation","Mild,easierly decernable depression with a depth not exceeding 0.5mm at any point","Pronounced depression with a maximum depth of between 0.5mm and 1.5mm","Deep depression in excess of 1.5mm")),
                                    textInput("SK_S13_4","Description of shape",value="None")),
                             column(width=4,
                                    h4("Assciated deformity/Abnormality"),
                                    checkboxInput("SK_S13_5","Other occipital deformation?",value=FALSE),
                                    conditionalPanel("input.SK_S13_5",
                                                     textInput("SK_S13_5b","Description of occipital deformation",value="None"),
                                                     h5("nb if there is substantial deformity this should be recoreded separately and the ID of the separate lession should be entered into the linked lesions field")),
                                    checkboxInput("SK_S13_6","Fronto-occipital deformation?",value=FALSE),
                                    conditionalPanel("input.SK_S13_6",
                                                     textInput("SK_S13_6b","Description of fronto-occipital deformation",value="None"),
                                                     h5("nb if there is substantial deformity this should be recoreded separately and the ID of the separate lession should be entered into the linked lesions field")),
                                    selectInput("SK_S13_7","Other associated abnormalities(select all that apply)",selected="Pitting",multiple=TRUE,c("Pitting","Cortical thining","Expansion/thickening of diploe","Woven bone","Scerlotic bone","Necrosis","Radiating fractures","Other fracture","Other abnormality")),
                                    conditionalPanel("input.SK_S13_7",textInput("SK_S13_7b","Location of abnormality in realtion to the depression",value="e.g. along superior margin"),
                                                     textInput("SK_S13_7c","Further description of abnormality",value="None"),
                                                     h5("nb if required use the custom lesion tab and linked lesion field to enter further decription"))),
                             column(width=4,
                                    textInput("SK_S13_8","Any additional/supplement description",value="None"),
                                    h4("Connection with other lesions"),
                                    textInput("SK_S13_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S13_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S13_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S13",Type="Shape",Des="Suprainion depression",Loc="Skull:Occipital:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Absence EAM#############################################################################################################################################
SK_S14_UI<-tagList(fixedPage(h3("ID:SK_S14"),
                             h4("Description:Absence of external auditory meatus"),
                             h5("nb this section should not be used the record blockage of the auditory meatus by abnormal bone growth for this see:'Bone growth in or around the external auditory meatus'"),
                             column(width=4,
                                    selectInput("SK_S14_1","Side",c("Right"="Unilateral_r","Left"="Unilateral_l","Bilateral")),
                                    conditionalPanel("input.SK_S14_1=='Unilateral_r'||input.SK_S14_1=='Bilateral'",
                                                     selectInput("SK_S14_2","right external surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                                     conditionalPanel("input.SK_S14_2==('Other')",textInput("SK_S14_2b","Other surface description")),
                                                     selectInput("SK_S14_3","Abnormal Ossicles",selected="Unknown",c("Unknown","No"="FALSE","Yes"="TRUE")),
                                                     conditionalPanel("input.SK_S14_3==('TRUE')",selectInput("SK_S14_3b","Which Ossicle?",multiple=TRUE,c("Incus","Malleous","Stapes")),
                                                                      textInput("SK_S14_3c","Description  of ossicle abnormality",value="None")),
                                                     selectInput("SK_S14_4","Right Internal auditory meatus",c("Normal","Reduced/constricted","Closed/absent","Unknown")),
                                                     textInput("SK_S14_5","Other Associated deformity",value="None"))),
                             column(width=4,
                                    conditionalPanel("input.SK_S14_1=='Unilateral_l'||input.SK_S14_1=='Bilateral'",
                                                     selectInput("SK_S14_6","left external surface",c("Smooth/dense","Pitted","Woven bone","Scerlotic","Ridged","Other")),
                                                     conditionalPanel("input.SK_S14_6==('Other')",textInput("SK_S14_6b","Other surface description")),
                                                     selectInput("SK_S14_7","Abnormal Ossicles",selected="Unknown",c("Unknown","No"="FALSE","Yes"="TRUE")),
                                                     conditionalPanel("input.SK_S14_7==('TRUE')",selectInput("SK_S14_7b","Which Ossicle?",multiple=TRUE,c("Incus","Malleous","Stapes")),
                                                                      textInput("SK_S14_7c","Description  of ossicle abnormality",value="None")),
                                                     selectInput("SK_S14_8","Left Internal auditory meatus",c("Normal","Reduced/constricted","Closed/absent","Unknown")),
                                                     textInput("SK_S14_9","Other Associated deformity",value="None"))),
                             column(width=4,
                                    textInput("SK_S14_10","Any additional/supplement description",value="None"),
                                    h4("Connection with other lesions"),
                                    textInput("SK_S14_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_S14_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_S14_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_S14",Type="Shape",Des="Absence external auditory meatus",Loc=NA,Feat="ex auditory mea:NA",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Ectocranial porosis#############################################################################################################################################
SK_L01_UI<-tagList(fixedPage(h3("ID:SK_L01"),
                             h4("Description:Ectocranial porosis"),
                             selectizeInput("SK_L01_1","Bone(s) involved",multiple=TRUE,selected=" ",choices=c(" ","Parietal","Occipital squama"="Occipital","Frontal Squama"="Frontal","Orbit","Zygomatic")),
                             conditionalPanel("input.SK_L01_1.indexOf('Parietal') >=0",
                                              fixedPage(h4("Parietal Porosis (SK_L01_P)"),
                                                        column(width=4, h4("Location"),
                                                               selectizeInput("SK_L01_2a","Right:Features(select all involved)",multiple=TRUE,choices=c("parietal eminence"="emin_l","superior temporal line"="sup_temporal_line_l","parietal striae"="parietal_striae_l","inferior temporal line"="inf_temporal_line_l")),
                                                               selectizeInput("SK_L01_3a","Left:Features(select all involved)",multiple=TRUE,choices=c("parietal eminence"="emin_r","superior temporal line"="sup_temporal_line_r","parietal striae"="parietal_striae_r","inferior temporal line"="inf_temporal_line_r")),
                                                               selectizeInput("SK_L01_4a","Sutures(select all affected)",multiple=TRUE,choice=c("right coronal"="coronal_r","left coronal"="coronal_l","saggital","right lambdoid"="lambdoid_r","left lambdoid"="lambdoid_l","right squamosal"="squamosal_r","left squamosal"="squamosal_l","right parietomastoid"="parietomastoid_r","left parietomastoid"="parietomastoid_l","right sphenoparietal"="sphenoparietal_r","left sphenoparietal"="sphenoparietal_l"))),
                                                        column(width=4,h4("Size and shape"),
                                                               h5("nb for the size of affect area fields give the size of the overall area affected using atleast 2 diminesion separated by /"),
                                                               textInput("SK_L01_6a","Right:Size of affected area",value="NA/NA"),
                                                               textInput("SK_L01_7a","Left:Size of affected area",value="NA/NA"),
                                                               selectInput("SK_L01_8a","Size of holes(select all aplicable)",multiple=TRUE,choices=c("<0.5mm","0.5-<1mm","1mm-<2mm",">2mm")),
                                                               selectInput("SK_L01_9a","Hole size variabilty(select the most appropreate)",c("The size is roughly consitant across the whole lesion","There is some variabilty that is randomly distrubuted across the lesion","There is some variablity with some geographic grouping by size","There is significant variablity that is randomly distrubuted across the lesion","There is significant variablity with some geographic grouping by size")),
                                                               checkboxInput("SK_L01_10a","Symmetry?"),
                                                               textInput("SK_L01_11a","Right:Shape",value="enter description here"),
                                                               textInput("SK_L01_12a","Left:Shape",value="enter description here")),
                                                        column(width=4,h4("Appearance and character"),
                                                               selectInput("SK_L01_13a","Surfaces(select all involved)",multiple=TRUE,choices=c("outer table","diploe","inner table")),
                                                               checkboxInput("SK_L01_14a","Accompaning thicknening of bone?"),
                                                               checkboxInput("SK_L01_15a","Coalescing of foramina?"),
                                                               selectInput("SK_L01_16a","Activity",choices=c("Active lesion","Healed","Mixed/Healing")),
                                                               textInput("SK_L01_17a","Additional description",value="None"),
                                                               h4("Connection to other lesions"),
                                                               textInput("SK_L01_Link1a","ID of linked lesion(s)",value="None"),
                                                               selectizeInput("SK_L01_Link2a","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description"))))),	
                             conditionalPanel("input.SK_L01_1.indexOf('Occipital') >=0",
                                              fixedPage(h4("Occipital Porosis (SK_L01_Oc)"),
                                                        column(width=4, h4("Location"),
                                                               selectizeInput("SK_L01_2b","Features(select all involved)",multiple=TRUE,choices=c("occipital planum"="occipital_planum","external occipital protuberance"="ex_protub","superior nuchal line"="sup_nuchal_line","inferior nuchal line"="inf__nuchal_line","nuchal planum"="nuchal_planum","external occipital crest"="ex_cr")),
                                                               selectizeInput("SK_L01_4b","Sutures(select all affected)",multiple=TRUE,choice=c("lambdoidal","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right petro-occipital"="petrooccipital_r","left petro-occipital","petrooccipital_l"))),
                                                        column(width=4,h4("Size and shape"),
                                                               h5("nb for the size of affect area fields give the size of the overall area affected using atleast 2 diminesion separated by /"),
                                                               textInput("SK_L01_6b","Size of affected area",value="NA/NA"),
                                                               selectInput("SK_L01_8b","Size of holes(select all aplicable)",multiple=TRUE,choices=c("<0.5mm","0.5-<1mm","1mm-<2mm",">2mm")),
                                                               selectInput("SK_L01_9b","Hole size variabilty(select the most appropreate)",c("The size is roughly consitant across the whole lesion","There is some variabilty that is randomly distrubuted across the lesion","There is some variablity with some geographic grouping by size","There is significant variablity that is randomly distrubuted across the lesion","There is significant variablity with some geographic grouping by size")),
                                                               textInput("SK_L01_11b","Shape",value="enter description here")),
                                                        column(width=4,h4("Appearance and character"),
                                                               selectInput("SK_L01_13b","Surfaces(select all involved)",multiple=TRUE,choices=c("outer table","diploe","inner table")),
                                                               checkboxInput("SK_L01_14b","Accompaning thicknening of bone?"),
                                                               checkboxInput("SK_L01_15b","Coalescing of foramina?"),
                                                               selectInput("SK_L01_16b","Activity",choices=c("Active lesion","Healed","Mixed/Healing")),
                                                               textInput("SK_L01_17b","Additional description",value="None"),
                                                               h4("Connection to other lesions"),
                                                               textInput("SK_L01_Link1b","ID of linked lesion(s)",value="None"),
                                                               selectizeInput("SK_L01_Link2b","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description"))))),
                             conditionalPanel("input.SK_L01_1.indexOf('Frontal') >=0",
                                              fixedPage(h4("Frontal Porosis (SK_L01_F)"),
                                                        column(width=4, h4("Location"),
                                                               selectizeInput("SK_L01_2c","Features(select all involved)",multiple=TRUE,choices=c("supraorbital margin left"="supraorbital_margin_l","supraorbital margin right"="supraorbital_margin_r","glabella","frontal eminence left"="emin_l","frontal eminence right"="emin_r","temporal line left"="temporal_line_l","temporal line right"="temporal_line_r","zygomatic process left"="zygomatic_pro_l","zygomatic process right"="zygomatic_pro_r")),
                                                               selectizeInput("SK_L01_4c","Sutures(select all affected)",multiple=TRUE,choice=c("coronal","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","metopic","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l"))),
                                                        column(width=4,h4("Size and shape"),
                                                               h5("nb for the size of affect area fields give the size of the overall area affected using atleast 2 diminesion separated by /"),
                                                               textInput("SK_L01_6c","Size of affected area",value="NA/NA"),
                                                               selectInput("SK_L01_8c","Size of holes(select all aplicable)",multiple=TRUE,choices=c("<0.5mm","0.5-<1mm","1mm-<2mm",">2mm")),
                                                               selectInput("SK_L01_9c","Hole size variabilty(select the most appropreate)",c("The size is roughly consitant across the whole lesion","There is some variabilty that is randomly distrubuted across the lesion","There is some variablity with some geographic grouping by size","There is significant variablity that is randomly distrubuted across the lesion","There is significant variablity with some geographic grouping by size")),
                                                               textInput("SK_L01_11c","Shape",value="enter description here")),
                                                        column(width=4,h4("Appearance and character"),
                                                               selectInput("SK_L01_13c","Surfaces(select all involved)",multiple=TRUE,choices=c("outer table","diploe","inner table")),
                                                               checkboxInput("SK_L01_14c","Accompaning thicknening of bone?"),
                                                               checkboxInput("SK_L01_15c","Coalescing of foramina?"),
                                                               selectInput("SK_L01_16c","Activity",choices=c("Active lesion","Healed","Mixed/Healing")),
                                                               textInput("SK_L01_17c","Additional description",value="None"),
                                                               h4("Connection to other lesions"),
                                                               textInput("SK_L01_Link1c","ID of linked lesion(s)",value="None"),
                                                               selectizeInput("SK_L01_Link2c","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description"))))),	
                             conditionalPanel("input.SK_L01_1.indexOf('Orbit') >=0",
                                              fixedPage(h4("Orbit Porosis (SK_L01_Ob)"),
                                                        column(width=4, h4("Location"),
                                                               selectizeInput("SK_L01_2d","Right:Features(select all involved)",multiple=TRUE,choices=c("pars orbitalia"="pars_orbitalia_r","lacrimal fossa"="lacrimal_fos_r","supraorbital margin"="supraorbital_margin_r","Lacrimal bone"="Lacrimal_r","sphenoid greater wing"="great_wing_r","sphenoid lesser wing"="less_wing_r","sphenoid body"="body_r","ethmoid orbital plate"="Ethmoid_r","Zygomatic orbital surface"="Zygomatic_r","Maxilla orbital surface"="Maxilla_r")),
                                                               selectizeInput("SK_L01_3d","Left:Features(select all involved)",multiple=TRUE,choices=c("pars orbitalia"="pars_orbitalia_l","lacrimal fossa"="lacrimal_fos_l","supraorbital margin"="supraorbital_margin_l","Lacrimal bone"="Lacrimal_l","sphenoid greater wing"="great_wing_l","sphenoid lesser wing"="less_wing_l","sphenoid body"="body_l","ethmoid orbital plate"="Ethmoid_l","Zygomatic orbital surface"="Zygomatic_l","Maxilla orbital surface"="Maxilla_l")),
                                                               selectizeInput("SK_L01_4d","Right:Sutures(select all affected)",multiple=TRUE,choice=c("zygomaticofrontal_r","frontolacrimal_r","frontoethmoidal_r","sphenofrontal_r","sphenoethmoidal_r","ethmoidolacrimal_r","lacromaxillary_r","frontomaxillary_r","sphenozygomatic_r")),
                                                               selectizeInput("SK_L01_5d","Left:Sutures(select all affected)",multiple=TRUE,choice=c("zygomaticofrontal_l","frontolacrimal_l","frontoethmoidal_l","sphenofrontal_l","sphenoethmoidal_l","ethmoidolacrimal_l","lacromaxillary_l","frontomaxillary_l","sphenozygomatic_l"))),
                                                        column(width=4,h4("Size and shape"),
                                                               h5("nb for the size of affect area fields give the size of the overall area affected using atleast 2 diminesion separated by /"),
                                                               textInput("SK_L01_6d","Right:Size of affected area",value="NA/NA"),
                                                               textInput("SK_L01_7d","Left:Size of affected area",value="NA/NA"),
                                                               selectInput("SK_L01_8d","Size of holes(select all aplicable)",multiple=TRUE,choices=c("<0.5mm","0.5-<1mm","1mm-<2mm",">2mm")),
                                                               selectInput("SK_L01_9d","Hole size variabilty(select the most appropreate)",c("The size is roughly consitant across the whole lesion","There is some variabilty that is randomly distrubuted across the lesion","There is some variablity with some geographic grouping by size","There is significant variablity that is randomly distrubuted across the lesion","There is significant variablity with some geographic grouping by size")),
                                                               checkboxInput("SK_L01_10d","Symmetry?"),
                                                               textInput("SK_L01_11d","Right:Shape",value="enter description here"),
                                                               textInput("SK_L01_12d","Left:Shape",value="enter description here")),
                                                        column(width=4,h4("Appearance and character"),
                                                               selectInput("SK_L01_13d","Surfaces(select all involved)",multiple=TRUE,choices=c("outer table","diploe","inner table")),
                                                               checkboxInput("SK_L01_14d","Accompaning thicknening of bone?"),
                                                               checkboxInput("SK_L01_15d","Coalescing of foramina?"),
                                                               selectInput("SK_L01_16d","Activity",choices=c("Active lesion","Healed","Mixed/Healing")),
                                                               textInput("SK_L01_17d","Additional description",value="None"),
                                                               h4("Connection to other lesions"),
                                                               textInput("SK_L01_Link1d","ID of linked lesion(s)",value="None"),
                                                               selectizeInput("SK_L01_Link2d","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description"))))),	
                             conditionalPanel("input.SK_L01_1.indexOf('Zygomatic') >=0",
                                              fixedPage(h4("Zygomatic Porosis (SK_L01_Z)"),
                                                        column(width=4, h4("Location"),
                                                               selectizeInput("SK_L01_2e","Right:Features(select all involved)",multiple=TRUE,choices=c("frontal process"="frontal_pro_r","temporal process"="temporal_pro_r","maxillary process"="maxillary_pro_r","zygomaticofacial foramen"="zygomaticofacial_for_r","masseteric origin"="masseteric_r")),
                                                               selectizeInput("SK_L01_3e","Left:Features(select all involved)",multiple=TRUE,choices=c("frontal process"="frontal_pro_l","temporal process"="temporal_pro_l","maxillary process"="maxillary_pro_l","zygomaticofacial foramen"="zygomaticofacial_for_l","masseteric origin"="masseteric_l")),
                                                               selectizeInput("SK_L01_4e","Right:Sutures(select all affected)",multiple=TRUE,choice=c("zygomaticofrontal_r","zygomaticotemporal_r","zygomaticomaxillary_r")),
                                                               selectizeInput("SK_L01_5e","Left:Sutures(select all affected)",multiple=TRUE,choice=c("zygomaticofrontal_l","zygomaticotemporal_l","zygomaticomaxillary_l"))),
                                                        column(width=4,h4("Size and shape"),
                                                               h5("nb for the size of affect area fields give the size of the overall area affected using atleast 2 diminesion separated by /"),
                                                               textInput("SK_L01_6e","Right:Size of affected area",value="NA/NA"),
                                                               textInput("SK_L01_7e","Left:Size of affected area",value="NA/NA"),
                                                               selectInput("SK_L01_8e","Size of holes(select all aplicable)",multiple=TRUE,choices=c("<0.5mm","0.5-<1mm","1mm-<2mm",">2mm")),
                                                               selectInput("SK_L01_9e","Hole size variabilty(select the most appropreate)",c("The size is roughly consitant across the whole lesion","There is some variabilty that is randomly distrubuted across the lesion","There is some variablity with some geographic grouping by size","There is significant variablity that is randomly distrubuted across the lesion","There is significant variablity with some geographic grouping by size")),
                                                               checkboxInput("SK_L01_10e","Symmetry?"),
                                                               textInput("SK_L01_11e","Right:Shape",value="enter description here"),
                                                               textInput("SK_L01_12e","Left:Shape",value="enter description here")),
                                                        column(width=4,h4("Appearance and character"),
                                                               selectInput("SK_L01_13e","Surfaces(select all involved)",multiple=TRUE,choices=c("outer table","diploe","inner table")),
                                                               checkboxInput("SK_L01_14e","Accompaning thicknening of bone?"),
                                                               checkboxInput("SK_L01_15e","Coalescing of foramina?"),
                                                               selectInput("SK_L01_16e","Activity",choices=c("Active lesion","Healed","Mixed/Healing")),
                                                               textInput("SK_L01_17e","Additional description",value="None"),
                                                               h4("Connection to other lesions"),
                                                               textInput("SK_L01_Link1e","ID of linked lesion(s)",value="None"),
                                                               selectizeInput("SK_L01_Link2e","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
))
                            
SK_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L01",Type="Loss",Des="Ectocranial porosis",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  if(sum(input$SK_L01_1=="Parietal")>0){
    PTable<-Table[1,];PTable$ID2<-"SK_L01_P"
    PTable$Loc<-"Skull:Parietal_r,Parietal_l:NA"
    PTable$Feat<-paste(paste(c(input$SK_L01_2a,input$SK_L01_3a),collapse=","),paste(input$SK_L01_4a,collapse=","),sep=":")
    PTable$Size<-paste("Right area,Left area,hole size,hole variability",paste(input$SK_L01_6a,input$SK_L01_7a,paste(input$SK_L01_8a,collapse="/"),input$SK_L01_9a,sep=","),sep=":")
    PTable$Shape<-paste("Symetric,Right,Left",paste(input$SK_L01_10a,input$SK_L01_11a,input$SK_L01_12a,sep=","),sep=":")
    PTable$Nature<-paste("Surface,Thickening,coalescing,activity",paste(paste(input$SK_L01_13a,collapse="/"),input$SK_L01_14a,input$SK_L01_15a,input$SK_L01_16a,sep=","),sep=":")
    PTable$Add<-input$SK_L01_17a
    if(input$SK_L01_Link1a != "None"){PTable$Link<-paste(paste(input$SK_L01_Link1a,collapse=","),paste(input$SK_L01_Link2a,collapse=","),sep=":")}
    Table<-rbind(Table,PTable)
  }
  if(sum(input$SK_L01_1=="Frontal")>0){
    PTable<-Table[1,];PTable$ID2<-"SK_L01_F"
    PTable$Loc<-"Skull:Frontal:NA"
    PTable$Feat<-paste(paste(c(input$SK_L01_2c,"frontal_squama"),collapse=","),paste(input$SK_L01_4c,collapse=","),sep=":")
    PTable$Size<-paste("area,hole size,hole variability",paste(input$SK_L01_6c,paste(input$SK_L01_8c,collapse="/"),input$SK_L01_9c,sep=","),sep=":")
    PTable$Shape<-paste("General",input$SK_L01_11c,sep=":")
    PTable$Nature<-paste("Surface,Thickening,coalescing,activity",paste(paste(input$SK_L01_13c,collapse="/"),input$SK_L01_14c,input$SK_L01_15c,input$SK_L01_16c,sep=","),sep=":")
    PTable$Add<-input$SK_L01_17b
    if(input$SK_L01_Link1c != "None"){PTable$Link<-paste(paste(input$SK_L01_Link1c,collapse=","),paste(input$SK_L01_Link2c,collapse=","),sep=":")}
    Table<-rbind(Table,PTable)
  }
  if(sum(input$SK_L01_1=="Occipital")>0){
    PTable<-Table[1,];PTable$ID2<-"SK_L01_Oc"
    PTable$Loc<-"Skull:Occipital:NA"
    PTable$Feat<-paste(paste(input$SK_L01_2b,collapse=","),paste(input$SK_L01_4b,collapse=","),sep=":")
    PTable$Size<-paste("area,hole size,hole variability",paste(input$SK_L01_6b,paste(input$SK_L01_8b,collapse="/"),input$SK_L01_9b,sep=","),sep=":")
    PTable$Shape<-paste("General",input$SK_L01_11b,sep=":")
    PTable$Nature<-paste("Surface,Thickening,coalescing,activity",paste(paste(input$SK_L01_13b,collapse="/"),input$SK_L01_14b,input$SK_L01_15b,input$SK_L01_16b,sep=","),sep=":")
    PTable$Add<-input$SK_L01_17b
    if(input$SK_L01_Link1b != "None"){PTable$Link<-paste(paste(input$SK_L01_Link1b,collapse=","),paste(input$SK_L01_Link2b,collapse=","),sep=":")}
    Table<-rbind(Table,PTable)
  }
  if(sum(input$SK_L01_1=="Orbit")>0){
    PTable<-Table[1,];PTable$ID2<-"SK_L01_Ob"
    
    feat<-c(input$SK_L01_2d,input$SK_L01_3d)
    bone<-feat[feat%in%c("Lacrimal_r","Ethmoid_r","Zygomatic_r","Maxilla_r","Lacrimal_l","Ethmoid_l","Zygomatic_l","Maxilla_l")]
    feat<-feat[feat%in%c("pars_orbitalia_r","lacrimal_fos_r","supraorbital_margin_r","pars_orbitalia_l","lacrimal_fos_l","supraorbital_margin_l","great_wing_r","less_wing_r","body_r","great_wing_l","less_wing_l","body_l")]
    if(length(feat[feat%in%c("pars_orbitalia_r","lacrimal_fos_r","supraorbital_margin_r","pars_orbitalia_l","lacrimal_fos_l","supraorbital_margin_l")])>0){bone<-c(bone,"Frontal")}
    if(length(feat[feat%in%c("great_wing_r","less_wing_r","body_r","great_wing_l","less_wing_l","body_l")])>0){bone<-c(bone,"Sphenoid")}
    
    PTable$Loc<-paste("Skull",paste(bone,collapse=","),"Orbit",sep=":")
    PTable$Feat<-paste(paste(feat,collapse=","),paste(c(input$SK_L01_4d,input$SK_L01_5d),collapse=","),sep=":")
    PTable$Size<-paste("Right area,Left area,hole size,hole variability",paste(input$SK_L01_6d,input$SK_L01_7d,paste(input$SK_L01_8d,collapse="/"),input$SK_L01_9d,sep=","),sep=":")
    PTable$Shape<-paste("Symetric,Right,Left",paste(input$SK_L01_10d,input$SK_L01_11d,input$SK_L01_12d,sep=","),sep=":")
    PTable$Nature<-paste("Surface,Thickening,coalescing,activity",paste(paste(input$SK_L01_13d,collapse="/"),input$SK_L01_14d,input$SK_L01_15d,input$SK_L01_16d,sep=","),sep=":")
    PTable$Add<-input$SK_L01_17e
    if(input$SK_L01_Link1d != "None"){PTable$Link<-paste(paste(input$SK_L01_Link1d,collapse=","),paste(input$SK_L01_Link2d,collapse=","),sep=":")}
    Table<-rbind(Table,PTable)
  }
  if(sum(input$SK_L01_1=="Zygomatic")>0){
    PTable<-Table[1,];PTable$ID2<-"SK_L01_Z"
    PTable$Loc<-"Skull:Zygomatic_r,Zygomatic_l:NA"
    PTable$Feat<-paste(paste(c(input$SK_L01_2e,input$SK_L01_3e),collapse=","),paste(c(input$SK_L01_4e,input$SK_L01_5e),collapse=","),sep=":")
    PTable$Size<-paste("Right area,Left area,hole size,hole variability",paste(input$SK_L01_6e,input$SK_L01_7e,paste(input$SK_L01_8e,collapse="/"),input$SK_L01_9e,sep=","),sep=":")
    PTable$Shape<-paste("Symetric,Right,Left",paste(input$SK_L01_10e,input$SK_L01_11e,input$SK_L01_12e,sep=","),sep=":")
    PTable$Nature<-paste("Surface,Thickening,coalescing,activity",paste(paste(input$SK_L01_13e,collapse="/"),input$SK_L01_14e,input$SK_L01_15e,input$SK_L01_16e,sep=","),sep=":")
    PTable$Add<-input$SK_L01_17e
    if(input$SK_L01_Link1e != "None"){PTable$Link<-paste(paste(input$SK_L01_Link1e,collapse=","),paste(input$SK_L01_Link2e,collapse=","),sep=":")}
    Table<-rbind(Table,PTable)
  }
  if(length(Table[,1])>1){Table<-Table[-1,]}
  Table }
#Supraorbital grooves#############################################################################################################################################
SK_L02_UI<-tagList(fixedPage(h3("ID:SK_L02"),
                             h4("Description:Supraorbital groove(s)"),
                             column(width=4,selectInput("SK_L02_1","Side",c("Right"="Unilateral_r","Left"="Unilateral_l","Bilateral")),
                                    conditionalPanel("input.SK_L02_1=='Unilateral_r'||input.SK_L02_1=='Bilateral'",
                                                     numericInput("SK_L02_2","Number of grooves on the right",value=NA),
                                                     numericInput("SK_L02_3","Maximum length of groove(s)/longest groove length",value=NA),
                                                     conditionalPanel("input.SK_L02_2>1",numericInput("SK_L02_4","Minimum groove length/shortest groove length",value=NA)),
                                                     numericInput("SK_L02_4","Distance from the right supraorbital notch to bregma",value=NA),
                                                     textInput("SK_L02_5","Shape description"))),
                             column(width=4,
                                    conditionalPanel("input.SK_L02_1=='Unilateral_l'||input.SK_L02_1=='Bilateral'",
                                                     numericInput("SK_L02_6","Number of grooves on the Left",value=NA),
                                                     numericInput("SK_L02_7","Maximum length of groove(s)/longest groove length",value=NA),
                                                     conditionalPanel("input.SK_L02_6>1",numericInput("SK_L02_8","Minimum groove length/shortest groove length",value=NA)),
                                                     numericInput("SK_L02_9","Distance from the left supraorbital notch to bregma",value=NA),
                                                     textInput("SK_L02_10","Shape description"))),
                             column(width=4,
                                    textInput("SK_L02_10","Any additional/supplement description",value="None"),
                                    h4("Connection with other lesions"),
                                    textInput("SK_L02_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_L02_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L02",Type="Loss",Des="Supraorbital groove(s)",Loc="Skull:Frontal:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#nasal spine erosion#############################################################################################################################################
SK_L03_UI<-tagList(fixedPage(h3("ID:SK_L03"),
                             h4("Description:Erosion of nasal spine")))
SK_L03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L03",Type="Loss",Des="Erosion of nasal spine",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#periapical perforations#############################################################################################################################################
SK_L04_UI<-tagList(fixedPage(h3("ID:SK_L04"),
                             h4("Description:Periapical perforation(s)")))
SK_L04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L04",Type="Loss",Des="Periapical perforation(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#endocranial depressions#############################################################################################################################################
SK_L05_UI<-tagList(fixedPage(h3("ID:SK_L05"),
                             h4("Description:Endocranial depressions")))
SK_L05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L05",Type="Loss",Des="Endocranial depressions",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Mastoid resorption#############################################################################################################################################
SK_L06_UI<-tagList(fixedPage(h3("ID:SK_L06"),
                             h4("Description:Mastoid resorption")))
SK_L06_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L06",Type="Loss",Des="Mastoid resorption",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#parietal thinning#############################################################################################################################################
SK_L07_UI<-tagList(fixedPage(h3("ID:SK_L07"),
                             h4("Description:Parietal thinning"),
                             column(width=4,selectInput("SK_L07_1","Side",c("Right"="Unilateral_r","Left"="Unilateral_l","Bilateral")))))
SK_L07_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L07",Type="Loss",Des="Parietal thinning",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Mandibular concavity#############################################################################################################################################
SK_L08_UI<-tagList(fixedPage(h3("ID:SK_L08"),
                             h4("Description:Circular mandibular concavity")))
SK_L08_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L09",Type="Loss",Des="Circular mandibular concavity",Loc="Skull:Mandible:Lingual surface inferior to the mylohyoid line",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Enlarged parietal foramina#############################################################################################################################################
SK_L09_UI<-tagList(fixedPage(h3("ID:SK_L09"),
                             h4("Description:Enlarged parietal foramina"),
                             column(width=4,
                                    selectInput("SK_L09_1","Side",multiple=TRUE,c("Right","Left"),selected=c("Right","Left")),
                                    conditionalPanel("input.SK_L09_1.indexOf('Left') >=0",
                                                     selectInput("SK_L09_2a","Left shape",c("Circular/Oval","Slit/linear","Other/irregular")),
                                                     textInput("SK_L09_3a","Left size(mm)",value="NA/NA")),
                                    conditionalPanel("input.SK_L09_1.indexOf('Right') >=0",
                                                     selectInput("SK_L09_2b","Right shape",c("Circular/Oval","Slit/linear","Other/irregular")),
                                                     textInput("SK_L09_3b","Right size(mm)",value="NA/NA"))),
                             column(width=4,
                                    h4("Associated abnormality"),
                                    selectInput("SK_L09_4","Suture Stenosis",multiple=TRUE,c("sagittal","right lamboid"="lambdoid_r","Left lamboid"="lambdoid_l")),
                                    textInput("SK_L09_5","Other associated abnormality",value="None"),
                                    textInput("SK_L09_6","Any additional description",value="None")),
                             column(width=4,
                                    h4("Connection to other lesions"),
                                    textInput("SK_L09_Link1","ID of linked lesion(s)",value="None"),
                                    selectizeInput("SK_L09_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
SK_L09_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L09",Type="Loss",Des="Enlarged parietal foramina",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  if(is.null(input$SK_L09_1)){
    feat<-"parietal_mina"
    Table$Loc<-"Skull:Parietal:Unknown side"
    Table$Shape<-"NA:NA"
    Table$Size<-"NA:NA"
    nature<-"NA"
  }else{if(sum(input$SK_L09_1==c("Right","Left"))==2){
    feat<-"parietal_mina_r,parietal_mina_l"
    Table$Loc<-"Skull:Parietal_r,Parietal_l:NA"
    Table$Shape<-paste("Right,Left",paste(input$SK_L09_2b,input$SK_L09_2a,sep=","),sep=":")
    Table$Size<-paste("Right,Left",paste(input$SK_L09_3b,input$SK_L09_3a,sep=","),sep=":")
    nature<-"TRUE"
  }else{if(input$SK_L09_1=="Left"){
    feat<-"parietal_mina_l"
    Table$Loc<-"Skull:Parietal_l:NA"
    Table$Shape<-paste("General",input$SK_L09_2a,sep=":")
    Table$Size<-paste("General",input$SK_L09_3a,sep=":")
    nature<-"FALSE"
  }else{if(input$SK_L09_1=="Right"){
    feat<-"parietal_mina_r"
    Table$Loc<-"Skull:Parietal_r:NA"
    Table$Shape<-paste("General",input$SK_L09_2b,sep=":")
    Table$Size<-paste("General",input$SK_L09_3b,sep=":")
    nature<-"FALSE"
  }}}}
  
  if(length(input$SK_L09_4)>0){
    Table$Feat<-paste(feat,paste(input$SK_L09_4,collapse=","),sep=":")
    Table$Nature<-paste("Bilateral,Suture senosis,other",nature,"TRUE",input$SK_L09_5)
  }
  else{Table$Feat<-paste(feat,"NA",sep=":")
  Table$Nature<-paste("Bilateral,Suture senosis,other",nature,"FALSE",input$SK_L09_5)}
  Table$Add<-input$SK_L09_6
  if(input$SK_L09_Link1 != "None"){Table$Link<-paste(paste(input$SK_L09_Link1,collapse=","),paste(input$SK_L09_Link2,collapse=","),sep=":")}
  Table }
#Tympanic perforations#############################################################################################################################################
SK_L10_UI<-tagList(fixedPage(h3("ID:SK_L10"),
                             h4("Description:Perforation of the tympanic plate")))
SK_L10_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_L10",Type="Loss",Des="Perforation of the tympanic plate",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Ectocranial growths#############################################################################################################################################
SK_F01_UI<-tagList(fixedPage(h3("ID:SK_F01"),
                             h4("Description:Circular ectocranial growths")))
SK_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_F01",Type="Formation",Des="Circular ectocranial growths",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#EOP growth#############################################################################################################################################
SK_F02_UI<-tagList(fixedPage(h3("ID:SK_F02"),
                             h4("Description:External occiptital protuberance growth")))
SK_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_F03",Type="Formation",Des="External occipital protuberance growth",Loc="Skull:Occipital:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#EAM growth#############################################################################################################################################
SK_F03_UI<-tagList(fixedPage(h3("ID:SK_F03"),
                             h4("Description:Auditory meatus growth")))
SK_F03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_F03",Type="Formation",Des="Auditory meatus growth",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#elongated styloid#############################################################################################################################################
SK_F04_UI<-tagList(fixedPage(h3("ID:SK_F04"),
                             h4("Description:Elongated styloid")))
SK_F04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_F04",Type="Formation",Des="Elongated styloid",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Lingual mandibular growth#############################################################################################################################################
SK_F05_UI<-tagList(fixedPage(h3("ID:SK_F05"),
                             h4("Description:Lingual Mandibular growth")))
SK_F05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_F05",Type="Formation",Des="Lingual mandibular growth",Loc="Skull:Mandible:along lingual border",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Thick endocranial growth#############################################################################################################################################
SK_F06_UI<-tagList(fixedPage(h3("ID:SK_F06"),
                             h4("Description:Irregular thick endocranial growths")))
SK_F06_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_F07",Type="Formation",Des="Irregular thick endocranial growths",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#TMJ irregularity#############################################################################################################################################
SK_C01_UI<-tagList(fixedPage(h3("ID:SK_C01"),
                             h4("Description:TMJ irregularity")))
SK_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_C01",Type="Complex",Des="TMJ irregularity",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Destruction, grooves and nodules#############################################################################################################################################
SK_C02_UI<-tagList(fixedPage(h3("ID:SK_C02"),
                             h4("Description:Extensive destruction,raditing grooves and raised nodules")))
SK_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_C02",Type="Complex",Des="Extensive destruction,raditing grooves and raised nodules",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Disorganised remodelling#############################################################################################################################################
SK_C03_UI<-tagList(fixedPage(h3("ID:SK_C03"),
                             h4("Description:Disorganised bone remodelling")))
SK_C03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_C03",Type="Complex",Des="Disorganised bone remodelling",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#erosion,remodeling and scaring#############################################################################################################################################
SK_C04_UI<-tagList(fixedPage(h3("ID:SK_C04"),
                             h4("Description:Erosion,remodelling and scarring")))
SK_C04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="SK_c04",Type="Complex",Des="Erosion,remodelling and scarring",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Data table#############################################################################################################################################
library(data.table)
Skull_DT<-data.table(ref=c("SK_S01","SK_S02","SK_S03","SK_S04","SK_S05","SK_S06","SK_S07","SK_S08","SK_S09","SK_S10","SK_S11","SK_S12", "SK_S13","SK_S14","SK_L01","SK_L02","SK_L03","SK_L04","SK_L05","SK_L06","SK_L07","SK_L08","SK_L09", "SK_L10","SK_F01","SK_F02","SK_F03","SK_F04","SK_F05","SK_F06","SK_C01","SK_C02","SK_C03","SK_C04"),
                     uioptions=c(SK_S01_UI,SK_S02_UI,SK_S03_UI,SK_S04_UI,SK_S05_UI,SK_S06_UI,SK_S07_UI,SK_S08_UI,SK_S09_UI,SK_S10_UI,SK_S11_UI,SK_S12_UI,SK_S13_UI,SK_S14_UI,SK_L01_UI,SK_L02_UI,SK_L03_UI,SK_L04_UI,SK_L05_UI,SK_L06_UI,SK_L07_UI,SK_L08_UI,SK_L09_UI,SK_L10_UI,SK_F01_UI,SK_F02_UI,SK_F03_UI,SK_F04_UI,SK_F05_UI,SK_F06_UI,SK_C01_UI,SK_C02_UI,SK_C03_UI,SK_C04_UI),
                     RecordCreator=c(SK_S01_RC,SK_S02_RC,SK_S03_RC,SK_S04_RC,SK_S05_RC,SK_S06_RC,SK_S07_RC,SK_S08_RC,SK_S09_RC,SK_S10_RC,SK_S11_RC,SK_S12_RC,SK_S13_RC,SK_S14_RC,SK_L01_RC,SK_L02_RC,SK_L03_RC,SK_L04_RC,SK_L05_RC,SK_L06_RC,SK_L07_RC,SK_L08_RC,SK_L09_RC,SK_L10_RC,SK_F01_RC,SK_F02_RC,SK_F03_RC,SK_F04_RC,SK_F05_RC,SK_F06_RC,SK_C01_RC,SK_C02_RC,SK_C03_RC,SK_C04_RC))