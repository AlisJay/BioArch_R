#Custom#####
H_CC_UI<-tagList(h3("ID:H_CC"),h4("Description:Custom Complex Abnormality"))
H_CC_RC<-function(input){
Table<-data.frame(Des1=NA,Size=NA,Nature=NA,Heal=NA)
Table
}
#Tufting###########
H_C01_UI<-tagList(h3("ID:H_C01"),h4("Description:Tufting and resorption of distal phalanges"),
                            selectInput("H_C01_1","Bone(s)",multiple = TRUE,selected="Phalanx_d1_r",c("1 right"="Phalanx_d1_r","1 left"="Phalanx_d1_l","2 right"="Phalanx_d2_r","2 left"="Phalanx_d2_l","3 right"="Phalanx_d3_r","3 left"="Phalanx_d3_l","4 right"="Phalanx_d4_r","4 left"="Phalanx_d4_l","5 right"="Phalanx_d5_r","5 left"="Phalanx_d5_l")),
                            column(width=4,
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d1_r')>=0",h4("1st Right Phalanx"), 
                                                    numericInput("H_C01_1_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_1_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_1_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_1_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d2_r')>=0",h4("2nd Right Phalanx"), 
                                                    numericInput("H_C01_2_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_2_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_2_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_2_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d3_r')>=0",h4("3rd Right Phalanx"), 
                                                    numericInput("H_C01_3_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_3_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_3_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_3_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d4_r')>=0",h4("4th Right Phalanx"), 
                                                    numericInput("H_C01_4_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_4_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_4_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_4_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d5_r')>=0",h4("5th Right Phalanx"), 
                                                    numericInput("H_C01_5_r_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_5_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_5_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_5_r_4","Shape description",value="Spade-shaped"))),
                            column(width=4,
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d1_l')>=0",h4("1st Left Phalanx"), 
                                                    numericInput("H_C01_1_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_1_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_1_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_1_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d2_l')>=0",h4("2nd Left Phalanx"), 
                                                    numericInput("H_C01_2_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_2_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_2_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_2_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d3_l')>=0",h4("3rd Left Phalanx"), 
                                                    numericInput("H_C01_3_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_3_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_3_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_3_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d4_l')>=0",h4("4th Leftt Phalanx"), 
                                                    numericInput("H_C01_4_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_4_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_4_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_4_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.H_C01_1.indexOf('Phalanx_d5_l')>=0",h4("5th Left Phalanx"), 
                                                    numericInput("H_C01_5_l_1","Maximum Distal width",value=NA),
                                                    numericInput("H_C01_5_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("H_C01_5_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("H_C01_5_l_4","Shape description",value="Spade-shaped"))),
                            column(width=4,h4("Connection with other lesions"),
                                  textInput("H_C01_Link1","ID of linked lesion(s)",value="None"),
                                  selectizeInput("H_C01_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))
                            )
H_C01_RC<-function(input=input){   
  Table<-data.frame(Des1="Tufting and resorption of distal phalanges",Size=NA,Nature=NA,Heal=NA)
  x<-data.frame("Phalanx"=substr(input$H_C01_1, nchar(input$H_C01_1)-3+1, nchar(input$H_C01_1)),DW=NA,PW=NA,MW=NA,Shape=NA)
  for(i in 1:length(input$H_C01_1)){
    x$Shape[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_4"))]]
    x$DW[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_1"))]]
    x$PW[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_4"))]]
    x$MW[i]<-input[[as.character(paste0("H_C01_",x$Phalanx[i],"_4"))]]
  }
  Table$Nature<-paste("Phalanges,Shape",paste(paste(x$Phalanx,collapse="/"),paste(x$Shape,collapse="/"),sep=","),sep=":")
  Table$Size<-paste0("Distal,Proximal,Mid:",paste(paste(x$DW,collapse="/"),paste(x$PW,collapse="/"),paste(x$MW,collapse="/"),sep=","))
  Table }

#Eburnation#########
H_C02_UI<-tagList(h3("ID:H_C02"),h4("Description:Eburnation of joint"))
H_C02_RC<-function(input=input){   
  Table<-data.frame(Des1="Eburnation of joint",Size=NA,Nature=NA,Heal=NA)
  Table }

