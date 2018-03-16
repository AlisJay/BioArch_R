#Custom#####
SK_CL_UI<-tagList(h3("ID:SK_CL"),h4("Description:Custom Loss Abnormality"),
                  column(width=4,
                         selectInput("SK_CL_Percent","% of bone affected",c("<1/3","1/3-2/3",">2/3")),
                         selectizeInput("SK_CL_Involve","Part(s) of bone involved",multiple=TRUE,choice=c("outer table","diploe","inner table")),
                         textInput("SK_CL_Shape","Overall shape description"),
                         selectInput("SK_CL_organisation","Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                         checkboxInput("SK_CL_Collapse","Associated structural collapse?",value=FALSE)),
                  column(width=4,
                         conditionalPanel("input.SK_CL_organisation=='Focal'",
                                          selectInput("SK_CL_Foci","Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                          selectInput("SK_CL_FociSize","Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                          selectInput("SK_CL_Response","Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined"))),
                         conditionalPanel("input.SK_CL_organisation == 'Diffuse'",
                                          checkboxInput("SK_CL_Thining","Cortical thinning?",value=FALSE),
                                          selectInput("SK_CL_Sites","Number of separate sites",c("1","2","3-5","6-10","10+")),
                                          h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                          selectInput("SK_CL_Overlapping","Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed")))),
                  column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                         numericInput("CustomMeasure","Number of measurments",value=NA),
                         actionButton("AddCM","Add fields"),
                         uiOutput("CM")))
SK_CL_RC<-function(input){
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  if(input$SK_CL_organisation=="Focal"){
    Table$Des1<-"Custom Focal Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,Foci,FociSize,Response:",paste(input$SK_CL_Percent,paste(input$SK_CL_Involve,collapse="/"),FreeFix(input$SK_CL_Shape),"Focal",input$SK_CL_Collapse,input$SK_CL_Foci,paste(input$SK_CL_FociSize,collapse="/"),input$SK_CL_Response,sep=","))}
  if(input$SK_CL_organisation=="Diffuse"){
    Table$Des1<-"Custom Diffuse Loss Abnormality"
    Table$Nature<-paste0("PercentAffected,Surfaces,Shape,Organisation,Collapse,CorticalThining,Sites,Overlapping:",paste(input$SK_CL_Percent,paste(input$SK_CL_Involve,collapse="/"),FreeFix(input$SK_CL_Shape),"Diffuse",input$SK_CL_Collapse,input$SK_CL_Thining,input$SK_CL_Sites,input$SK_CL_Overlapping,sep=","))}
  Table
}
#Ectocranial Porosis####
SK_L01a_UI<-tagList(h3("ID:SK_L01a"),h4("Description:Ectocranial porosis"),
                    fixedPage(column(width=4,h4("Size"),
                                     numericInput("SK_L01a_MxML","Right:Maximum Mediolateral width(mm)",value=NA),
                                     numericInput("SK_L01a_MnML","Right:Minimum Mediolateral width(mm)",value=NA),
                                     numericInput("Sk_L01a_MxSI","Right:Maximum superior-inferior/Anterior-Posterior width(mm)",value=NA),
                                     numericInput("SK_L01a_MnSI","Right:Minimum superior-inferior/Anterior-Posterior width(mm)",value=NA),
                                     h5("If the porosis is on the frontal or occipital squama use the boxes in this column to record the total area affected, and ignore the second column")),
                              column(width=4,h4(br()),
                                     numericInput("SK_L01a_MXML2","Left: Maximum Mediolateral width(mm)",value=NA),
                                     numericInput("SK_L01a_MnML2","Left: Minimum Mediolateral width(mm)",value=NA),
                                     numericInput("Sk_L01a_MxSI2","Left: Maximum superior-inferior/Anterior-Posterior width(mm)",value=NA),
                                     numericInput("SK_L01a_MnSI2","Left: Minimum superior-inferior/Anterior-Posterior width(mm)",value=NA)),
                              column(width=4,h4(br()),
                                     selectInput("SK_L01a_HSize","Size of holes(select all aplicable)",multiple=TRUE,choices=c("<0.5mm","0.5-<1mm","1mm-<2mm",">2mm")),
                                     selectInput("SK_L01a_HVariability","Hole size variabilty(select the most appropreate)",c("The size is roughly consitant across the whole lesion","There is some variabilty that is randomly distrubuted across the lesion","There is some variablity with some geographic grouping by size","There is significant variablity that is randomly distrubuted across the lesion","There is significant variablity with some geographic grouping by size")))),
                    fixedPage(column(width=6,h4("Appearance and character"),
                                     textInput("SK_L01a_Shape","Right:Shape",value="enter description here"),
                                     textInput("SK_L01a_Shape2","Left:Shape",value="enter description here"),
                                     h5("If porosis is on the frontal or occipital squama use the Right input box to describe the whole lesion")),
                              column(width=6,h4(br()),
                                     selectInput("SK_L01a_Surface","Surfaces(select all involved)",multiple=TRUE,choices=c("outer table","diploe","inner table")),
                                     checkboxInput("SK_L01a_Symmetry","Symmetrical?"),
                                     checkboxInput("SK_L01a_Thickening","Accompaning thicknening of bone?"),
                                     checkboxInput("SK_L01a_Coalescing","Coalescing of foramina?"))))
SK_L01a_RC<-function(input){
  Table<-data.frame(Des1="Ectocranial porosis",Size=NA,Nature=NA)
  if(Contains(Input$Bone,c("Zygomatic","Parietal"))){
    Table$Size<-paste0("Max_MedLat(r/l),Min_MedLat(r/l),Max_SupInf(r/l),Min_SupInf(r/l),HoleSize:",paste(paste(input$SK_L01a_MxML,input$SK_L01a_MXML2,sep="/"),paste(input$SK_L01a_MnML,input$SK_L01a_MnML2,sep="/"),paste(input$SK_L01a_MxSI,input$SK_L01a_MXSI2,sep="/"),paste(input$SK_L01a_MnSI,input$SK_L01a_MnSI2,sep="/"),paste(input$SK_L01a_HSize,collapse="/"),sep=","))
    Table$Nature<-paste0("Shape(r/l),Symmetrical,Surfaces,Thickening,Coalescing,HoleSize_Variability:",paste(paste(FreeFix(input$SK_L01a_Shape),FreeFix(input$SK_L01a_Shape2),sep="/"),input$SK_L01a_Symmetry,input$SK_L01a_Surface,input$SK_L01a_Thickening,input$SK_L01a_Coalescing,input$SK_L01a_HVariability,sep=","))
    if(Contains(Input$Bone,"Parietal")){Table$Size<-paste0("Max_MedLat(r/l),Min_MedLat(r/l),Max_AntPos(r/l),Min_AntPos(r/l),HoleSize:",paste(paste(input$SK_L01a_MxML,input$SK_L01a_MXML2,sep="/"),paste(input$SK_L01a_MnML,input$SK_L01a_MnML2,sep="/"),paste(input$SK_L01a_MxSI,input$SK_L01a_MXSI2,sep="/"),paste(input$SK_L01a_MnSI,input$SK_L01a_MnSI2,sep="/"),paste(input$SK_L01a_HSize,collapse="/"),sep=","))}
  }else{
    Table$Size<-paste0("Max_MedLat,Min_MedLat,Max_SupInf,Min_SupInf,HoleSize:",paste(input$SK_L01a_MxML,input$SK_L01a_MnML,input$SK_L01a_MxSI,input$SK_L01a_MnSI,paste(input$SK_L01a_HSize,collapse="/"),sep=","))
    Table$Nature<-paste0("Shape,Symmetrical,Surfaces,Thickening,Coalescing,HoleSize_Variability:",paste(FreeFix(input$SK_L01a_Shape),input$SK_L01a_Symmetry,input$SK_L01a_Surface,input$SK_L01a_Thickening,input$SK_L01a_Coalescing,input$SK_L01a_HVariability,sep=","))}
  Table
}
#Orbital Porosis####
SK_L01b_UI<-tagList(h3("ID:SK_L01b"),h4("Description:Orbital Ectocranial porosis"),
                    
                    fixedPage(column(width=4,h4("Size"),
                                     numericInput("SK_L01b_MxML","Right:Maximum Mediolateral width(mm)",value=NA),
                                     numericInput("SK_L01b_MnML","Right:Minimum Mediolateral width(mm)",value=NA),
                                     numericInput("SK_L01b_MxSI","Right:Maximum superior-inferior/Anterior-Posterior width(mm)",value=NA),
                                     numericInput("SK_L01b_MnSI","Right:Minimum superior-inferior/Anterior-Posterior width(mm)",value=NA)),
                              column(width=4,h4(br()),
                                     numericInput("SK_L01b_MXML2","Left: Maximum Mediolateral width(mm)",value=NA),
                                     numericInput("SK_L01b_MnML2","Left: Minimum Mediolateral width(mm)",value=NA),
                                     numericInput("SK_L01b_MxSI2","Left: Maximum superior-inferior/Anterior-Posterior width(mm)",value=NA),
                                     numericInput("SK_L01b_MnSI2","Left: Minimum superior-inferior/Anterior-Posterior width(mm)",value=NA)),
                              column(width=4,h4(br()),
                                     selectInput("SK_L01b_HSize","Size of holes(select all aplicable)",multiple=TRUE,choices=c("<0.5mm","0.5-<1mm","1mm-<2mm",">2mm")),
                                     selectInput("SK_L01b_HVariability","Hole size variabilty(select the most appropreate)",c("The size is roughly consitant across the whole lesion","There is some variabilty that is randomly distrubuted across the lesion","There is some variablity with some geographic grouping by size","There is significant variablity that is randomly distrubuted across the lesion","There is significant variablity with some geographic grouping by size")))),
                    fixedPage(column(width=6,h4("Appearance and character"),
                                     textInput("SK_L01b_Shape","Right:Shape",value="enter description here"),
                                     textInput("SK_L01b_Shape2","Left:Shape",value="enter description here")),
                              column(width=6,h4(br()),
                                     selectInput("SK_L01b_Surface","Surfaces(select all involved)",multiple=TRUE,choices=c("outer table","diploe","inner table")),
                                     conditionalPanel("input.Bone.indexOf('Lacrimal') >=0",selectInput("SK_L01b_Lacrimal","Lacrimal involvement",c("Primary site of porosis","Continuations of lesion","Separate area of Pitting"))),
                                     conditionalPanel("input.Bone.indexOf('Sphenoid') >=0",selectInput("SK_L01b_Sphenoid","Sphenoidal involvement",c("Primary site of porosis","Continuations of lesion","Separate area of Pitting"))),
                                     checkboxInput("SK_L01b_Symmetry","Symmetrical?"),
                                     checkboxInput("SK_L01b_Thickening","Accompaning thicknening of bone?"),
                                     checkboxInput("SK_L01b_Coalescing","Coalescing of foramina?"))))
SK_L01b_RC<-function(input){
  Table<-data.frame(Des1="Orbital Ectocranial porosis",Size=NA,Nature=NA)
  Lacrimal<-NA;Sphenoid<-NA
  if(Contains(input$Bone,"Sphenoid")){Sphenoid<-input$SK_LO1b_Sphenoid}
  if(Contains(input$Bone,"Lacrimal")){Sphenoid<-input$SK_LO1b_Lacrimal}
  Table$Size<-paste0("Max_MedLat(r/l),Min_medLat(r/l),Max_SupInf/AntPos(r/l),Min_SupInf/AntPos(r/l),HoleSize:",paste(paste(input$SK_L01b_MxML,input$SK_L01b_MXML2,sep="/"),paste(input$SK_L01b_MnML,input$SK_L01b_MnML2,sep="/"),paste(input$SK_L01b_MxSI,input$SK_L01b_MXSI2,sep="/"),paste(input$SK_L01b_MnSI,input$SK_L01b_MnSI2,sep="/"),paste(input$SK_L01b_HSize,collapse="/"),sep=","))
  Table$Nature<-paste0("Shape(r/l),Symmetrical,Surfaces,Thickening,Coalescing,HoleSize_Variability,Lacrimal_involment,Sphenoidal_Involment:",paste(paste(FreeFix(input$SK_L01b_Shape),FreeFix(input$SK_L01b_Shape2),sep="/"),input$SK_L01b_Symmetry,input$SK_L01b_Surface,input$SK_L01b_Thickening,input$SK_L01b_Coalescing,input$SK_L01b_HVariability,Lacrimal,Sphenoid,sep=","))
  Table
}
#Supraorbital grooves###########
SK_L02_UI<-tagList(h3("ID:SK_L02"),h4("Description:Supraorbital groove(s)"),
                   column(width=4,selectInput("SK_L02_1","Side",c("Right"="Unilateral_r","Left"="Unilateral_l","Bilateral")),
                          conditionalPanel("input.SK_L02_1=='Unilateral_r'||input.SK_L02_1=='Bilateral'",
                                           h4("Right"),
                                           numericInput("SK_L02_NGroove","Number of grooves",value=NA),
                                           numericInput("SK_L02_MxGroove","Maximum length of groove/longest groove length",value=NA),
                                           conditionalPanel("input.SK_L02_NGroove > 1",numericInput("SK_L02_MnGroove","Shortest groove length",value=NA)),
                                           numericInput("SK_L02_SON-B","Distance from the right supraorbital notch to bregma",value=NA),
                                           textInput("SK_L02_shape","Shape description"))),
                   column(width=4,
                          conditionalPanel("input.SK_L02_1=='Unilateral_l'||input.SK_L02_1=='Bilateral'",
                                           h4("Left"),
                                           numericInput("SK_L02_NGroove2","Number of grooves",value=NA),
                                           numericInput("SK_L02_MxGroove2","Maximum length of groove/longest groove length",value=NA),
                                           conditionalPanel("input.SK_L02_NGroove2 > 1",numericInput("SK_L02_MnGroove2","Shortest groove length",value=NA)),
                                           numericInput("SK_L02_SON-B2","Distance from the right supraorbital notch to bregma",value=NA),
                                           textInput("SK_L02_shape2","Shape description"))))
SK_L02_RC<-function(input=input){   
  Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
  if(input$SK_L02_1=="Bilateral"){
    Table$Des1<-"Bilateral superaorbital grooves"
    Table$Nature<-paste0("Bilateral,N.grooves(r/l),Shape(r/l):",paste("Bilateral",paste(input$SK_L02_NGroove,input$SK_L02_NGroove2,sep="/"),paste(FreeFix(input$SK_L02_Shape),FreeFix(input$SK_L02_Shape),sep="/"),sep=","))
    RightMin<-NA;LeftMin<-NA
    if(input$SK_L02_NGroove>1){RightMin<-input$SK_L02_MnGroove}
    if(input$SK_L02_NGroove2>1){LeftMin<-input$SK_L02_MnGroove2}
    Table$Size<-paste0("MaxLength(r/l),MinLength(r/l),SupraorbitalNotch-Bregma(r/l):",paste(paste(input$SK_L01_MxGroove,input$SK_L02_MxGroove2,sep="/"),paste(RightMin,LeftMin,sep="/"),paste(input$SK_L02_SON-B,input$SK_L02_SON-B,sep="/"),sep=","))}
  if(input$SK_L02_1=="Unilateral_r"){
    Table$Des<-"Unilateral supraorbital groove(s) (right)"
    Table$Nature<-paste0("Bilateral,N.grooves,Shape:",paste("Unilateral_r",input$SK_L02_NGroove,FreeFix(input$SK_L02_Shape),sep=","))
    min<-NA
    if(input$SK_L02_NGroove>1){min<-input$SK_L02_MnGroove}
    Table$Size<-paste0("MaxLength,MinLength,SupraorbitalNotch-Bregma:",paste(input$SK_L02_MxGroove,min,input$SK_L02_SON-B,sep=","))}
  if(input$SK_L02_1=="Unilateral_l"){
    Table$Des<-"Unilateral supraorbital groove(s) (Left)"
    Table$Nature<-paste0("Bilateral,N.grooves,Shape:",paste("Unilateral_l",input$SK_L02_NGroove2,FreeFix(input$SK_L02_Shape2),sep=","))
    min<-NA
    if(input$SK_L02_NGroove2>1){min<-input$SK_L02_MnGroove2}
    Table$Size<-paste0("MaxLength,MinLength,SupraorbitalNotch-Bregma:",paste(input$SK_L02_MxGroove2,min,input$SK_L02_SON-B2,sep=","))}
  Table }
#nasal spine erosion###########
SK_L03_UI<-tagList(h3("ID:SK_L03"),h4("Description:Erosion of nasal spine"))
SK_L03_RC<-function(input=input){   
  Table<-data.frame(Des1="Erosion of nasal spine",Size=NA,Nature=NA)
  Table }
#periapical perforations######
SK_L04_UI<-tagList(h3("ID:SK_L04"),h4("Description:Periapical perforation(s)"))
SK_L04_RC<-function(input=input){   
  Table<-data.frame(Des1="Periapical perforation(s)",Size=NA,Nature=NA)
  Table }
#endocranial depressions######
SK_L05_UI<-tagList(h3("ID:SK_L05"),h4("Description:Endocranial depressions"))
SK_L05_RC<-function(input=input){   
  Table<-data.frame(Des1="Endocranial depressions",Size=NA,Nature=NA)
  Table }
#Mastoid resorption######
SK_L06_UI<-tagList(h3("ID:SK_L06"),h4("Description:Mastoid resorption"))
SK_L06_RC<-function(input=input){   
  Table<-data.frame(Des1="Mastoid resorption",Size=NA,Nature=NA)
  Table }
#parietal thinning######
SK_L07_UI<-tagList(h3("ID:SK_L07"),h4("Description:Parietal thinning"),
                   column(width=4,selectInput("SK_L07_1","Side",c("Right"="Unilateral_r","Left"="Unilateral_l","Bilateral"))))
SK_L07_RC<-function(input=input){   
  Table<-data.frame(Des1="Parietal thinning",Size=NA,Nature=NA)
  Table }
#Mandibular concavity######
SK_L08_UI<-tagList(h3("ID:SK_L08"),h4("Description:Circular mandibular concavity"))
SK_L08_RC<-function(input=input){   
  Table<-data.frame(Des1="Circular mandibular concavity",Size=NA,Nature=NA)
  Table }
#Enlarged parietal foramina######
SK_L09_UI<-tagList(h3("ID:SK_L09"),h4("Description:Enlarged parietal foramina"),
                             column(width=4,
                                    selectInput("SK_L09_1","Bilateral?",c("Yes"="Bilateral","No right only"="Unilateral_r","No Left only"="Unilateral_l")),
                                    conditionalPanel("input.SK_L09_1=='Unilateral_r'||input.SK_L09_1=='Bilateral'",
                                                     selectInput("SK_L09_Shape","right shape",c("Circular/Oval","Slit/linear","Other/irregular")),
                                                     numericInput("SK_L09_MxAntPos","Right Maximum Anterior-Posterior width(mm)",value=NA),
                                                     numericInput("SK_L09_MxMedLat","Right Maximum MedioLateral width(mm)",value=NA)),
                                    conditionalPanel("input.SK_L09_1=='Unilateral_l'||input.SK_L09_1=='Bilateral'",
                                                     selectInput("SK_L09_Shape2","Left shape",c("Circular/Oval","Slit/linear","Other/irregular")),
                                                     numericInput("SK_L09_MxAntPos2","Left Maximum Anterior-Posterior width(mm)",value=NA),
                                                     numericInput("SK_L09_MxMedLat2","Left Maximum MedioLateral width(mm)",value=NA))),
                             column(width=4,
                                    h4("Associated abnormality"),
                                    selectInput("SK_L09_Stenosis","Suture Stenosis",multiple=TRUE,c("sagittal","right lamboid"="lambdoid_r","Left lamboid"="lambdoid_l")),
                                    textInput("SK_L09_otherAb","Other associated abnormality",value="None")))
SK_L09_RC<-function(input=input){   
  Table<-data.frame(Des1="Enlarged parietal foramina",Size=NA,Nature=NA)
  if(input$SK_L09_1=="Bilateral"){
    Table$Nature<-paste0("Bilateral,Shape(r/l),Stenosis,OtherAbnormality:",paste(input$SK_L09_1,paste(input$SK_L09_Shape,input$SK_L09_Shape2,sep="/"),paste(input$SK_L09_Stenosis,collapse="/"),FreeFix(input$SK_L09_otherAb),sep=","))
    Table$Size<-paste0("Max_AntPos(r/l),Max_MedLat(r/l):",paste(paste(input$SK_L09_MxAntPos,input$SK_L09_MxAntPos2,sep="/"),paste(input$SK_L09_MxMedLat,input$SK_L09_MxMedLat2,sep="/"),sep=","))}
  if(input$SK_L09_1=="Unilateral_r"){
    Table$Nature<-paste0("Bilateral,Shape,Stenosis,OtherAbnormality:",paste(input$SK_L09_1,input$SK_L09_Shape,paste(input$SK_L09_Stenosis,collapse="/"),FreeFix(input$SK_L09_otherAb),sep=","))
    Table$Size<-paste0("Max_AntPos,Max_MedLat:",paste(input$SK_L09_MxAntPos,input$SK_L09_MxMedLat,sep=","))}
  if(input$SK_L09_1=="Unilateral_l"){
    Table$Nature<-paste0("Bilateral,Shape,Stenosis,OtherAbnormality:",paste(input$SK_L09_1,input$SK_L09_Shape2,paste(input$SK_L09_Stenosis,collapse="/"),FreeFix(input$SK_L09_otherAb),sep=","))
    Table$Size<-paste0("Max_AntPos,Max_MedLat:",paste(input$SK_L09_MxAntPos2,input$SK_L09_MxMedLat2,sep=","))}
  Table}
  
  
#Tympanic perforations######
SK_L10_UI<-tagList(h3("ID:SK_L10"),h4("Description:Perforation of the tympanic plate"))
SK_L10_RC<-function(input=input){   
  Table<-data.frame(Des1="Perforation of the tympanic plate",Size=NA,Nature=NA)
  Table }
