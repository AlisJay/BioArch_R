#Custom#####
P_CF_UI<-tagList(h3("ID:P_CF"),h4("Description:Custom Formation Abnormality"))
P_CF_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
Table
}
#Acessory sacral facets######
P_F01_UI<-tagList(h3("ID:P_F01"),h4("Description:Acessory sacral facets"))
P_F01_RC<-function(input=input){   
  Table<-data.frame(Des1="Acessory sacral facets",Loc="Pelvis:Sacrum:NA",Size=NA,Nature=NA,Heal=NA)
  Table }

#Fusion of sacroilliac######
P_F02_UI<-tagList(h3("ID:P_F02"),h4("Description:Fusion of sacroilliac joint"))
P_F02_RC<-function(input=input){   
  Table<-data.frame(Des1="Fusion of sacroilliac joint",Size=NA,Nature=NA,Heal=NA)
  Table }

#Projections######
P_F03_UI<-tagList(h3("ID:P_F03"),h4("Description:Projections on illiac crest/ishial crest,spine or tuberocity/obturator foramen"),
                            selectInput("P_F03_1","Location",multiple=TRUE,c("Right Illiac crest","Right Ishial Crest","Right Ishial Spine","Right Ishial Tuberocity","Right Obturator foramen","Left Illiac crest","Left Ishial Crest","Leftt Ishial Spine","Left Ishial Tuberocity","Left Obturator foramen")))
P_F03_RC<-function(input=input){   
  Table<-data.frame(Des1="Projections on illiac crest/ishial crest,spine or tuberocity/obturator foramen",Size=NA,Nature=NA,Heal=NA)
  Table }

#Bridging of cotyloid notch######
P_F04_UI<-tagList(h3("ID:P_F04"),h4("Description:Bridging of cotyloid notch"))
P_F04_RC<-function(input=input){   
  Table<-data.frame(Des1="Bridging of cotyloid notch",Size=NA,Nature=NA,Heal=NA)
  Table }

#long ishial tuberocity projection######
P_F05_UI<-tagList(h3("ID:P_F05"),h4("Description:long projection from ishial tuberocity"))
P_F05_RC<-function(input=input){   
  Table<-data.frame(Des1="Long projection from ishial tuberocity",Size=NA,Nature=NA,Heal=NA)
  Table }

