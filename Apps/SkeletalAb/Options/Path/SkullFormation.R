#Custom#####
SK_CF_UI<-tagList(h3("ID:SK_CF"),h4("Description:Custom Formation Abnormality"),
                  column(width=4,
                         selectInput("SK_CF_Percent","% of bone effected",c("<1/3","1/3-2/3",">2/3")),
                         textInput("SK_CF_Shape","Overall shape description"),
                         selectInput("SK_CF_Type","Type of formation",c("Periostal reaction"="Periosteal","Spicules formed perpendicular to surface (intact cortex)"="Spicules","Ossified connective tissue/overdeveloped attachment site"="Connective","Cortex perforation"="Cortex","Diploe Increase"="Diploe"))),
                  column(width=4,
                         conditionalPanel("input.SK_CF_Type == 'Periosteal'",
                                          selectInput("SK_CF_Periosteal","Type of reaction",c("reactive woven bone"="Woven","sclerotic reaction"="Sclerotic","Mixture of sclerotic and woven"="Mixed"))),
                         conditionalPanel("input.SK_CF_Type == 'Spicules'",
                                          selectInput("SK_CF_Spicules","Pattern",c("Sunburst","Cauliflower","Other")),
                                          conditionalPanel("SK_CF_Spicules == 'Other'",textInput("SK_CF_Spicules2","Define other",value=NA))),
                         conditionalPanel("input.SK_CF_Type == 'Connective'",
                                          textInput("SK_CF_Connective1","Site of Attachment"),
                                          textInput("SK_CF_Connective2","Description of abnormal bone",value=NA)),
                         conditionalPanel("input.SK_CF_Type == 'Cortex'",
                                          selectInput("SK_CF_Cortex1","Table",c("Inner","Outer","Both")),
                                          selectInput("SK_CF_Cortex2","Cause of perforation",c("Expansion shell type reaction","Cloacae/sinus tracks","Other")),
                                          conditionalPanel("input.SK_CF_Cortex2=='Other'",textInput("SK_CF_Cortex3","Define Other",value=NA)),
                                          conditionalPanel("input.SK_CF_Cortex2=='Expansion shell type reaction'",selectizeInput("SK_CF_Shell","Type of shell(select all applicable)",multiple=TRUE,choice=c("continuous","interrupted","expanded cortex","lobulated","ridged/trabeculated soap bubble","single","lamellated","lamellated onion skin","butress","common angle","spiculated","parallel spiculated/hair on end","spiculated sunburst")))),
                         conditionalPanel("input.SK_CF_Type == 'Diploe'",
                                          selectInput("SK_CF_Diploe1","Type of Diploe abnormality",c("Expansion","COarsening","Increased density","Other")),
                                          conditionalPanel("input.SK_CF.Diploe1 =='Other'",textInput("SK_CF_Diploe2","Define other",value=NA)))),
                  column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                         numericInput("CustomMeasure","Number of measurments",value=NA),
                         actionButton("AddCM","Add fields"),
                         uiOutput("CM")))
SK_CF_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA)
Table$Des1<-paste("Custom",input$SK_CF_Type,"Formation abnormality")
SizeValues<-NULL;SizeName<-NULL
for(i in 1:input$CustomMeasure){
  SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
  SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
NatureValues<-NULL;NatureName<-NULL
if(input$SK_CF_Type=="Periosteal"){
  NatureValues<-paste("Periosteal Reaction",FreeFix(input$SK_CF_Shape),input$SK_CF_Percent,input$SK_CF_Periosteal,sep=",")
  NatureName<-"FormationType,Shape,Percent,ReactionType"}
if(input$SK_CF_Type=="Spicules"){
  if(input$SK_CF_Spicules=="Other"){pattern<-FreeFix(input$SK_CF_Spicules2)}else{pattern<-input$SK_CF_Spicules}
  NatureValues<-paste("Spicules formed perpendicular to surface",FreeFix(input$SK_CF_Shape),input$SK_CF_Percent,pattern,sep=",")
  NatureName<-"FormationType,Shape,Percent,Pattern"}
if(input$SK_CF_Type=="Connective"){
  NatureValues<-paste("Ossified connective tissue/overdeveloped attachment site",FreeFix(input$SK_CF_Shape),input$SK_CF_Percent,FreeFix(input$SK_CF_Connective1),FreeFix(input$SK_CF_Connective2))
  NatureName<-"FormationType,Shape,Percent,AttachmentSite,BoneDescription"}
if(input$SK_CF_Type=="Cortex"){
  if(input$SK_CF_Cortex2=="Other"){P<-FreeFix(input$SK_CF_Cortex3)
  }else{if(input$SK_CF_Cortex2=="Expansion shell type reaction"){P<-paste0("Expansion shell type reaction-",paste(input$SK_CF_Shell,collapse="/"))
  }else{P<-input$SK_CF_Cortex2}}
  NatureValues<-paste("Cortex Perforation",FreeFix(input$SK_CF_Shape),input$SK_CF_Percent,input$SK_CF_Cortex1,P,sep=",")
  NatureName<-"FormationType,Shape,Percent,Table,PerforationCause"}
if(input$SK_CF_Type=="Diploe"){
  if(input$SK_CF_Diploe1=="Other"){Dip<-FreeFix(input$SK_CF_Diploe2)}else{Dip<-input$SK_CF_Diploe1}
  NatureValues<-paste("Diploe Increase",FreeFix(input$SK_CF_Shape),input$SK_CF_Percent,Dip,sep=",")
  NatureName<-"FormationType,Shape,Percent,DiploeAbnormality"}
Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
Table$Nature<-paste(paste(NatureName,collapse=","),paste(NatureValues,collapse=","),sep=":")
Table
}
#Ectocranial growths########
SK_F01_UI<-tagList(h3("ID:SK_F01"),h4("Description:Circular ectocranial growths"))
SK_F01_RC<-function(input=input){   
  Table<-data.frame(Des1="Circular ectocranial growths",Size=NA,Nature=NA,Heal=NA)
  Table }
#EOP growth#######
SK_F02_UI<-tagList(h3("ID:SK_F02"),h4("Description:External occiptital protuberance growth"))
SK_F02_RC<-function(input=input){   
  Table<-data.frame(Des1="External occipital protuberance growth",Size=NA,Nature=NA,Heal=NA)
  Table }
#EAM growth########
SK_F03_UI<-tagList(h3("ID:SK_F03"),h4("Description:Auditory meatus growth"))
SK_F03_RC<-function(input=input){   
  Table<-data.frame(Des1="Auditory meatus growth",Size=NA,Nature=NA,Heal=NA)
  Table }
#elongated styloid#########
SK_F04_UI<-tagList(h3("ID:SK_F04"),h4("Description:Elongated styloid"))
SK_F04_RC<-function(input=input){   
  Table<-data.frame(Des1="Elongated styloid",Size=NA,Nature=NA,Heal=NA)
  Table }
#Lingual mandibular growth##########
SK_F05_UI<-tagList(h3("ID:SK_F05"),h4("Description:Lingual Mandibular growth"))
SK_F05_RC<-function(input=input){   
  Table<-data.frame(Des1="Lingual mandibular growth",Size=NA,Nature=NA,Heal=NA)
  Table }
#Thick endocranial growth###########
SK_F06_UI<-tagList(h3("ID:SK_F06"),h4("Description:Irregular thick endocranial growths"))
SK_F06_RC<-function(input=input){   
  Table<-data.frame(Des1="Irregular thick endocranial growths",Size=NA,Nature=NA,Heal=NA)
  Table }
