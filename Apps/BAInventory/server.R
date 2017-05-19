source("functions.R",local=TRUE)
source("tables.R",local = TRUE)

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
    CreateSI(input$POPID,input$POPName,input$Person1,custom=c(input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,input$C8),input$dir)
  })
  output$PMessage<-renderPrint({PMessage()})
  
  #add individual
  Photo<-renderText({paste(input$Photo1,input$Photo2,input$Photo3,input$Photo4,input$Photo5,input$Photo6,input$Photo7,input$Photo8,input$Photo9,input$Photo10,input$Photo11,input$Photo12,input$Photo13,input$Photo14,input$Photo15,input$Photo16,sep=":")})
  OS<-function(x,x2){if(x=="Partial"){out<-x2}else{out<-x};out}
  output$O_S<-O_S<-renderText({paste(OS(input$CVOrder,input$CVOrder2),OS(input$TVOrder,input$TVOrder2),OS(input$LVOrder,input$LVOrder2),OS(input$ROrder,input$ROrder2),OS(input$RSide,input$RSide2),
                         OS(input$MCOrder,input$MCOrder2),OS(input$MCSide,input$MCSide2),OS(input$HPOrder,input$HPOrder2),OS(input$HPSide,input$HPSide2),OS(input$MTOrder,input$MTOrder2),OS(input$MTSide,input$MTSide2),OS(input$FPOrder,input$FPOrder2),OS(input$FPSide,input$FPSide2),sep=":")})
  IMessage<-eventReactive(input$Append,{
    AppendSI(input$POPID,input$ID,input$Person2,SkullScore(),VertScore(),ThoraxScore(),ShoulderScore(),PelvisScore(),
             ArmScore(),HandScore(),LegScore(),FootScore(),UFs(),NMScore(),CustomScore(),O_S(),Photo(),input$dir)
  })
  output$IMessage<-renderPrint({IMessage()})
  
  #Compatability Check
  ComMessage<-eventReactive(input$Compat,{
    CompCheck(input$POPID,input$POPName,input$Person1,c(input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,input$C8),input$dir)
  })
  output$ComMessage<-renderPrint({ComMessage()})
})

