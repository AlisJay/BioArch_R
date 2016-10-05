source("functions.R")
source("tables.R")

shinyServer(function(input, output) {
                        
  #UFs
  output$UFs<-UFs<-renderText({paste(input$USkull,input$ULimb,input$UVert,input$UHandFoot,input$UClavScap,input$UOsCoxae,input$UOther,sep=":")})
  
  #Counts
  output$SkullRecorded<-renderPrint({paste("Elements Present:",length(input$SkullPresent),sep="")})
  output$SkullComplete<-renderPrint({paste("Elements Complete:",length(input$SkullComplete),sep="")})
  
  output$ThoraxRecorded<-renderPrint({paste("Elements Present:",length(input$ThoraxPresent),sep="")})
  output$ThoraxComplete<-renderPrint({paste("Elements Complete:",length(input$ThoraxComplete),sep="")})
  
  output$VertRecorded<-renderPrint({paste("Elements Present:",length(input$VertPresent),sep="")})
  output$VertComplete<-renderPrint({paste("Elements Complete:",length(input$VertComplete),sep="")})
  
  output$ShoulderRecorded<-renderPrint({paste("Shoulder Elements Present:",length(input$ShoulderPresent),sep="")})
  output$ShoulderComplete<-renderPrint({paste("Shoulder Elements Complete:",length(input$ShoulderComplete),sep="")})
  
  output$PelvisRecorded<-renderPrint({paste("Pelvis Elements Present:",length(input$PelvisPresent),sep="")})
  output$PelvisComplete<-renderPrint({paste("Pelvis Elements Complete:",length(input$PelvisComplete),sep="")})
  
  output$ArmRecorded<-renderPrint({paste("Arm Elements Present:",length(input$ArmPresent),sep="")})
  output$ArmComplete<-renderPrint({paste("Arm Elements Complete:",length(input$ArmComplete),sep="")})
  
  output$HandRecorded<-renderPrint({paste("Hand Elements Present:",length(input$HandPresent),sep="")})
  output$HandComplete<-renderPrint({paste("Hand Elements Complete:",length(input$HandComplete),sep="")})
  
  output$LegRecorded<-renderPrint({paste("Leg Elements Present:",length(input$LegPresent),sep="")})
  output$LegComplete<-renderPrint({paste("Leg Elements Complete:",length(input$LegComplete),sep="")})
  
  output$FootRecorded<-renderPrint({paste("Foot Elements Present:",length(input$FootPresent),sep="")})
  output$FootComplete<-renderPrint({paste("Foot Elements Complete:",length(input$FootComplete),sep="")})
  
  output$NMCRecorded<-renderPrint({paste("Cranial traits Present:",length(input$NMCPresent),sep="")})
  output$NMPCRecorded<-renderPrint({paste("Post-Cranial traits Present:",length(input$NMPCPresent),sep="")})
  
  output$CustomRecorded<-renderPrint({paste("Elements Present:",length(input$CustomPresent),sep="")})
  output$CustomComplete<-renderPrint({paste("Elements Complete:",length(input$CustomComplete),sep="")})
  
  #calculation of scores (standard)
  output$SkullScore<-SkullScore<-renderText({InventoryScore(input$SkullPresent,input$SkullComplete,Skull)})
  output$VertScore<-VertScore<-renderText({InventoryScore(input$VertPresent,input$VertComplete,Vert)})
  output$ThoraxScore<-ThoraxScore<-renderText({InventoryScore(input$ThoraxPresent,input$ThoraxComplete,Thorax)})
  output$ShoulderScore<-ShoulderScore<-renderText({InventoryScore(input$ShoulderPresent,input$ShoulderComplete,Shoulder)})
  output$PelvisScore<-PelvisScore<-renderText({InventoryScore(input$PelvisPresent,input$PelvisComplete,Pelvis)})
  output$ArmScore<-ArmScore<-renderText({InventoryScore(input$ArmPresent,input$ArmComplete,Arm)})
  output$HandScore<-HandScore<-renderText({InventoryScore(input$HandPresent,input$HandComplete,Hand)})
  output$LegScore<-LegScore<-renderText({InventoryScore(input$LegPresent,input$LegComplete,Leg)})
  output$FootScore<-FootScore<-renderText({InventoryScore(input$FootPresent,input$FootComplete,Foot)})
  
  
  #Custom trait score
  
  output$CustomScore<-CustomScore<-renderText({CScore(C=c(input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,input$C8),
                                                            present=input$CustomPresent,complete=input$CustomComplete)})
  
  #nonmetric trait score
  output$NMScore<-NMScore<-renderText({NonMetricScore(p1=input$NMCPresent,d1=NMCranial,p2=input$NMPCPresent,d2=NMPostCranial)})
  
  #Calculation of errors
  output$SkullErrors<-renderPrint({InventoryIssue(input$SkullPresent,input$SkullComplete)})
  output$ThoraxErrors<-renderPrint({InventoryIssue(input$ThoraxPresent,input$ThoraxComplete)})
  output$VertErrors<-renderPrint({InventoryIssue(input$VertPresent,input$VertComplete)})
  output$ShoulderErrors<-renderPrint({InventoryIssue(input$ShoulderPresent,input$ShoulderComplete)})
  output$PelvisErrors<-renderPrint({InventoryIssue(input$PelvisPresent,input$PelvisComplete)})
  output$ArmErrors<-renderPrint({InventoryIssue(input$ArmPresent,input$ArmComplete)})
  output$HandErrors<-renderPrint({InventoryIssue(input$HandPresent,input$HandComplete)})
  output$LegErrors<-renderPrint({InventoryIssue(input$LegPresent,input$LegComplete)})
  output$FootErrors<-renderPrint({InventoryIssue(input$FootPresent,input$FootComplete)})
  
  
  #Writing to file
  PMessage<-eventReactive(input$Create,{
    CreateSI(input$POPID,input$POPName,input$Person1,custom=c(input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,input$C8))
  })
  output$PMessage<-renderPrint({PMessage()})
  
  #add individual
  IMessage<-eventReactive(input$Append,{
    AppendSI(input$POPID,input$ID,input$Person2,SkullScore(),VertScore(),ThoraxScore(),ShoulderScore(),PelvisScore(),
             ArmScore(),HandScore(),LegScore(),FootScore(),UFs(),NMScore(),CustomScore())
  })
  output$IMessage<-renderPrint({IMessage()})
  
  #Compatability Check
  ComMessage<-eventReactive(input$Compat,{
    CompCheck(input$POPID,input$POPName,input$Person1,c(input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,input$C8))
  })
  output$ComMessage<-renderPrint({ComMessage()})
})

