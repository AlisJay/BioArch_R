#sourcing functions from the helper file
source("helpers.R",local=TRUE)
source("write.R",local=TRUE)

shinyServer(function(input, output) {
  
  # known value outputs 
  #ancestory
  output$Kan <- renderPrint({ input$kan })
  #age
  output$Kag<-renderPrint({input$kage})
  output$Kagr<-renderPrint({paste(as.character(input$krange[1])," - ", as.character(input$krange[2]))})
  #sex
  output$Ks<-renderPrint({input$ksex})
  
  
  #unknown values by function call 
  
  #Ancestory()
  #function call
  an<-reactive({
    Ancestory(c(input$orbit,input$nRoot,input$LNB,input$palate,input$profile,input$nwidth),input$BR,input$VS,input$PBD,input$SI,input$nBridge,input$LEB,input$nSpine,input$fShape,input$mMarks,input$Jaw)
  })
  #outputs 
  output$an<-renderPrint({
    an()$outcome
  })
  output$anT<-renderTable({
    an()$scores
  })
  output$anw<-renderPrint({an()$warnings})
  
  
  #subadult()
  #outputs 
  Sub<-reactive({subadult(input$epiphysis)})
  output$ag<-renderPrint({
    Sub()$age
  })
  output$agr4<-renderPrint({Sub()$range})
  output$agw4<-renderPrint({Sub()$Mesage})
  
  #sex()
  #creating the pelvis and cranial field from the input
  p<-reactive({list(nonmetric=c(input$va,input$spc,input$ipr,input$sn,input$pas,input$pShape,input$iShape,input$pInlet,input$pShape,input$spAngle,input$OF,input$sShape),
                    metric=c(input$pLength,input$iLength,input$pLength2,input$iLength2,input$awidth))})
  
  cr<-reactive({list(nonmetric=c(input$nc,input$m,input$som,input$g,input$me,input$Chin,input$cSize,input$flare,input$flex,input$gAngle),
                     metric=data.frame(trait=c("maximum length","maximum breadth","basion bregma","basion nasion","bizygomatic breadth","basion prosthion","nasion alveolar","palate breadth","mastoid length"), 
                                       measurement=c(input$maxLength,input$maxBreadth,input$BaBr,input$BaNa,input$BB,input$BaPr,input$NaAl,input$pBreadth,input$mLength)))})
  
  other<-reactive({c(input$sHeight,input$gHeight,input$hHead,input$rHead,input$fHead)})
  #(ancestory,subAdult,pelvis,cranial)
  #options for sex(), using known and unknown values
  s1<-reactive({sex(input$kan,subAdult=list(age=input$kage),pelvis=p(),cranial=cr(),other=other())})
  s2<-reactive({sex(ancestory=input$kan,subAdult=subadult(input$epiphysis),pelvis=p(),cranial=cr(),other=other())})
  s3<-reactive({sex(ancestory=an()$outcome,subAdult=list(age=input$kage),pelvis=p(),cranial=cr(),other=other())})
  s4<-reactive({sex(ancestory=an()$outcome,subAdult=subadult(input$epiphysis),pelvis=p(),cranial=cr(),other=other())})
  
  #output options for sex()
  output$sx1<-renderPrint({s1()$Sex})
  output$st1<-renderTable({s1()$Table})
  output$sw1<-renderPrint({s1()$Warnings})
  
  output$sx2<-renderPrint({s2()$Sex})
  output$st2<-renderTable({s2()$Table})
  output$sw2<-renderPrint({s2()$Warnings})
  
  output$sx3<-renderPrint({s3()$Sex})
  output$st3<-renderTable({s3()$Table})
  output$sw3<-renderPrint({s3()$Warnings})
  
  output$sx4<-renderPrint({s4()$Sex})
  output$st4<-renderTable({s4()$Table})
  output$sw4<-renderPrint({s4()$Warnings})
  
  
  #adult()
  #creating vault and LA from input
  vault<-reactive({ComScore(input$ML,input$L,input$O,input$AS,input$B)})
  LA<-reactive({ComScore(input$P,input$MC,input$SF,input$IST,input$SST)})
  
  #adult options known and unknown sex
  a1<-reactive({Adult(sex=input$ksex,todd=input$todd,SucheyBrookes=input$SucheyBrookes,lovejoy=input$lovejoy,vault=vault(),LA=LA(),Rib=c(input$rib1,input$rib2,input$rib3,input$rib4))})
  a2<-reactive({Adult(sex=s4()$Sex,todd=input$todd,SucheyBrookes=input$SucheyBrookes,lovejoy=input$lovejoy,vault=vault(),LA=LA(),Rib=c(input$rib1,input$rib2,input$rib3,input$rib4))})
  a3<-reactive({Adult(sex=s2()$Sex,todd=input$todd,SucheyBrookes=input$SucheyBrookes,lovejoy=input$lovejoy,vault=vault(),LA=LA(),Rib=c(input$rib1,input$rib2,input$rib3,input$rib4))})
  
  #age output options
  output$agr1<-renderPrint({paste(as.character(a1()$TotalRange)," (",as.character(a1()$AverageRange),")")})
  output$agt1<-renderTable({a1()$Table})
  output$agw1<-renderPrint({a1()$Warnings})
  
  output$agr2<-renderPrint({paste(as.character(a2()$TotalRange)," (",as.character(a2()$AverageRange),")")})
  output$agt2<-renderTable({a2()$Table})
  output$agw2<-renderPrint({a2()$Warnings})
  
  output$agr3<-renderPrint({paste(as.character(a3()$TotalRange)," (",as.character(a3()$AverageRange),")")})
  output$agt3<-renderTable({a3()$Table})
  output$agw3<-renderPrint({a3()$Warnings})
  
  
  #Writing out data
  
  #create files 
  PMessage<-eventReactive(input$Create,{
    createBP(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$PMessage<-renderPrint({PMessage()})
  
  #Compatability Check
  ComMessage<-eventReactive(input$Compat,{
    CompCheck(input$POPID,input$POPName,input$Person1,input$dir)
  })
  output$ComMessage<-renderPrint({ComMessage()})
  
  #append individual record:
  #Fields
  An3<-reactive({if(input$knownac=="known"){"NA"}else{paste(input$orbit,input$nRoot,input$LNB,input$palate,input$profile,input$nwidth,sep=":")}})
  An2<-reactive({if(input$knownac=="known"){"NA"}else{paste(input$BR,input$VS,input$PBD,input$SI,input$nBridge,input$LEB,input$nSpine,input$fShape,input$mMarks,input$Jaw,sep=":")}})
  
  EScore<-reactive({if(input$knownage=="known"){"NA"}else{ECal(input$epiphysis)}})
  Pscore<-reactive({if(input$knownage=="known"){"NA"}else{paste(input$todd,input$SucheyBrookes,input$lovejoy,sep=":")}})
  Su<-reactive({if(input$knownage=="known"){"NA"}else{paste(input$ML,input$L,input$O,input$AS,input$B,input$P,input$MC,input$SF,input$IST,input$SST,sep=":")}})
  
  Pelvis<-reactive({if(input$knownsex=="known"){list(nonmetric="NA",metric="NA")}else{p()}})
  cranial<-reactive({if(input$knownsex=="known"){list(nometric="NA",metric=data.frame(trait="None",measurment="NA"))}else{cr()}})
  o<-reactive({if(input$knownsex=="known"){"NA"}else{other()}})
  
  rib<-reactive({if(input$knownsex=="known"){"NA"}else{paste(input$rib1,input$rib2,input$rib3,input$rib4,sep=":")}})
  
  k<-reactive({KnownScore(input$knownage,input$knownac,input$knownsex)})
  
  An<-reactive({if(input$knownac=="known"){input$kan}else{an()$outcome }})
  
  AnS<-reactive({if(input$knownac=="known"){"NA"
  }else{paste(an()$scores[1,1],an()$scores[1,2],an()$scores[1,3],an()$scores[1,4],sep=":")}})
  
  Ag<-reactive({if(input$knownage=="known"){input$kage}else{Sub()$age}})  
  
  AgeScores<-reactive({if(input$knownage=="known"){list(paste(as.character(input$krange[1])," - ", as.character(input$krange[2]),sep=""),"NA","NA","NA","NA","NA","NA","NA")
  }else{
    if(Sub()$age=="SubAdult"){list(Sub()$range,"NA","NA","NA","NA","NA","NA","NA")
    }else{
      if(k()=="1:0:0"){aExtract(a3())
      }else{
        if(k()=="0:0:0"){aExtract(a2())
        }else{aExtract(a3())}}}}
  })
  
  Sex<-reactive({if(input$knownsex=="known"){list(input$ksex,rep("NA",12))
  }else{if(k()=="1:0:1"){list(s1()$Sex,s1()$Table$count)
  }else{if(k()=="1:0:0"){list(s1()$Sex,s2()$Table$count)
  }else{if(k()=="0:0:1"){list(s1()$Sex,s3()$Table$count)
  }else{list(s1()$Sex,s4()$Table$count)}}}}
  })
  
  #Append
  IMessage<-eventReactive(input$Append,{
    Append2(input$POPID,input$ID,input$Person2,k=k(),an=An(),s=Sex()[[1]],ag=Ag(),r=AgeScores()[[1]],ans=AnS(),ss=Sex()[[2]],
            t=AgeScores()[[2]],sb=AgeScores()[[3]],l=AgeScores()[[4]],v=AgeScores()[[5]],la=AgeScores()[[6]],r4=AgeScores()[[7]],
            an3=An3(),an2=An2(),escore=EScore(),pscore=Pscore(),rib=rib(),su=Su(),Pelvis=Pelvis(),Cranial=cranial(),Other=o(),input$dir)
  })
  output$IMessage<-renderPrint({IMessage()})
  
  
  
})
