#This File contains all the function used in the Bioprofile App
######################################################################################################################################################################################
Ancestory<-function(ThreeOps=c(orbit,nRoot,LNB,palate,profile,nWidth),BR="NA",VS="NA",PBD="NA",SI="NA",nBridge="NA",LEB="NA",nSpine="NA",fShape="NA",mMarks="NA",Jaw="NA"){
  Asian<-0;African<-0;European=0;unknown<-0
  Asian<-Asian+length(ThreeOps[ThreeOps=="As"]);African<-African+length(ThreeOps[ThreeOps=="Af"])
  European<-European+length(ThreeOps[ThreeOps=="Eu"]);unknown<-unknown+length(ThreeOps[ThreeOps=="U"])
    
  if(BR=="T"){European<-European+1}
  if(BR=="U"|BR=="NA"){unknown<-unknown+1}
   
  if(VS=="T"){Asian<-Asian+1}
  if(VS=="U"|VS=="NA"){unknown<-unknown+1}
 
  if(PBD=="T"){African<-African+1}
  if(PBD=="U"|PBD=="NA"){unknown<-unknown+1}
  
  if(SI=="T"){Asian<-Asian+1}
  if(SI=="U"|SI=="NA"){unknown<-unknown+1}
  
  if(nBridge=="T"){European<-European+1}
  if(nBridge=="U"|nBridge=="NA"){unknown<-unknown+1}
  
  if(LEB=="T"){Asian<-Asian+1}
  if(LEB=="U"|LEB=="NA"){unknown<-unknown+1}
  
  if(nSpine=="T"){European<-European+1}
  if(nSpine=="U"|LEB=="NA"){unknown<-unknown+1}
  
  if(fShape=="T"){Asian<-Asian+1}
  if(fShape=="U"|LEB=="NA"){unknown<-unknown+1}
  
  if(mMarks=="T"){European<-European+1}
  if(mMarks=="U"|LEB=="NA"){unknown<-unknown+1}
  
  if(Jaw=="T"){European<-European+1}
  if(Jaw=="U"|LEB=="NA"){unknown<-unknown+1}
  
  scores<-data.frame(asian=Asian,african=African,european=European)
  unknown<-data.frame(Unknown=unknown)
  
  other<-sum(scores)-max(scores)
  
  outcome<-if(unknown>8){as.character("unknown")
  }else{if(unknown>6 & max(scores)<other){as.character("unknown")
    }else{if(unknown>6){paste("possible",as.character(names(scores)[which.max(scores)]))
    }else{if(max(scores)>other){as.character(names(scores)[which.max(scores)])
      }else{paste("possible",as.character(names(scores)[which.max(scores)]))}}}} 
  
  w<-NA; w<-if(unknown>0){"Your input was incomplete. The most reliable results come from complete analysis."}
  list(scores=cbind(scores,unknown),outcome=outcome,warnings=w)
}
######################################################################################################################################################################################
subadult<-function(epiphysis){
  ossification<-data.frame(epiphysis=c("pRadius","dFibula","dTibia","dFemur","pFibula","acromion","iliac","hHead","fHead","lTrochanter","pTibia","gTrochanter","dradius","s3s5","s2s3","s1s2","clavicle","SOS"),
                           max=c(18.5,19.5,19.5,21.0,21.0,21.5,21.5,23.0,19.5,19.5,22.0,19.5,22.0,23.5,24.5,32.0,30.0,25.5),
                           min=c(14.5,14.5,14.5,14.5,14.5,14.5,14.5,14.5,15.5,15.5,15.5,17.0,17.0,17.0,17.5,17.5,18.5,19.5),
                           fullname=c("proximal radius","distal fibula", "distal Tibia","distal femur","proximal fibula", "acromion process","illiac crest","humeral head", "femoral head", "lesser trochanter",
                                      "proximal Tibia","greater trochnater","distal radius","sacrum s3-s5","sacrum s3-s2","sacrum s1-s2","sternal end of clavicle","spheno-occipital synchondrosis"))
  sacrum<-"s1s2"
  Message<-NULL
  if(is.null(epiphysis)){age<-"Adult";Range<-"NA";Message<-"no epiphyses open so remains are adult"
  }else{if(identical(epiphysis,sacrum)){
      Message<-"non fusion of s1 and s2 is very common if no other epiphyses are open the remains are likely adult"
      age<-"Adult"
    }else{
      age<-"SubAdult"
      max<-ossification$max[ossification$epiphysis %in% epiphysis]
      min<-ossification$min[!(ossification$epiphysis %in% epiphysis)]
      maxEp<-ossification$fullname[ossification$max==min(max)]
      Range<-paste(as.character(max(min))," - ",as.character(min(max)))
      Message<-if(min(max)<max(min)){paste("maximun is larger than minimum, this could be congential nonfusion please untick ", maxEp, " and try again")}
    }}
  output<-list(age=age,range=Range,Mesage=Message)
}
######################################################################################################################################################################################
ComScore<-function(A,B,C,D,E){
  V<-c(A,B,C,D,E)
  V<-as.numeric(V)
  na<-length(is.na(V)[is.na(V)==TRUE])
  composite<-sum(V,na.rm=TRUE)
  if(na==0){composite<-composite
  }else{composite<-round(composite+(mean(V,na.rm=TRUE)*na),0)}
  list(na=na,score=composite)
}
######################################################################################################################################################################################
Adult<-function(sex,todd, SucheyBrookes,lovejoy,vault,LA){
  results<-data.frame(trait=c("Todd","Suchey-Brookes","Lovejoy","Vault","Lateral-Anterior"),
                      phase=c(NA,NA,NA,NA,NA),
                      min=c(NA,NA,NA,NA,NA),
                      max=c(NA,NA,NA,NA,NA),
                      average=c(NA,NA,NA,NA,NA))
  cvault<-data.frame(composite=c(1:20,"NaN"),
                     s=c("s1","s1","s2","s2","s2","s2","s3","s3","s3","s3","s3","s4","s4","s4","s4","s5","s5","s5","s6","s6",NA),
                     min=c(19,19,23,23,23,23,28,28,28,28,28,31,31,31,31,35,35,35,34,34,NA),
                     max=c(44,44,45,45,45,45,44,44,44,44,44,61,61,61,61,60,60,60,61,61,NA),
                     average=c(31,31,34,34,34,34,38,38,38,38,38,44,44,44,44,49,49,49,52,52,NA))
  cLA<-data.frame(composite=c(1:14,"NaN"),
                  s=c("s1","s2","s3","s3","s3","s4","s5","s5","s6","s6","s7","s7","s7","s7",NA),
                  min=c(20,29,28,28,28,31,35,35,39,39,49,49,49,49,NA),
                  max=c(41,44,51,51,51,54,56,56,61,61,61,61,61,61,NA),
                  average=c(32,37,40,40,40,43,46,46,52,52,55,55,55,55,NA))
  Todd<-data.frame(phase=c("NA","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10"),
                   min=c(NA,18,20,22,25,27,30,35,40,45,50),
                   max=c(NA,19,21,24,26,30,35,39,45,49,61),
                   average=c(NA,18.5,20.5,23,25.5,28.5,32.5,37,42.5,47,61))
  SB<-data.frame(phase=c("NA","p1","p2","p3","p4","p5","p6"),
                 minF=c(NA,15,19.5,21,26,26.5,42),
                 maxF=c(NA,24,40,53,61,61,61),
                 averageF=c(NA,18.5,25.5,32,38,49,59),
                 minM=c(NA,15,19,21,23,27,34),
                 maxM=c(NA,28,34,44,58,61,61),
                 averageM=c(NA,18.5,24,28,35,45,60))
  Lovejoy<-data.frame(phase<-c("NA","p1","p2","p3","p4","p5","p6","p7","p8"),
                      min=c(NA,20,25,30,35,40,45,50,60),
                      max=c(NA,24,29,34,39,44,49,59,61),
                      average=c(NA,22,27,32,37,42,47,54.5,61))
  results[,-1][results$trait=="Todd",]<-Todd[Todd$phase==todd,]
                     results[1,2]<-todd
  results[,-1][results$trait=="Lovejoy",]<-Lovejoy[Lovejoy$phase==lovejoy,]
                     results[3,2]<-lovejoy
  
  if(is.na(vault$score)){
    results[,-1][results$trait=="Vault",]<-c(NA,NA,NA,NA)
    results[4,2]<-NA
  }else{
    results[,-1][results$trait=="Vault",]<-cvault[cvault$composite==vault$score,][-1]
    results[4,2]<-as.character(cvault[cvault$composite==vault$score,2])
  }
   
  if(is.na(LA$score)){
    results[,-1][results$trait=="Lateral-Anterior",]<-c(NA,NA,NA,NA)
    results[5,2]<-NA
  }else{
    results[,-1][results$trait=="Lateral-Anterior",]<-cLA[cLA$composite==LA$score,][-1]
    results[5,2]<-as.character(cLA[cLA$composite==LA$score,2])
  }
  
  if(sex=="female"|sex=="possible female"){
    results[,-1][results$trait=="Suchey-Brookes",]<-SB[SB$phase==SucheyBrookes,c(1,2,3,4)]
    results[2,2]<-SucheyBrookes
  }else{
    if(sex=="male"|sex=="possible male"){
      results[,-1][results$trait=="Suchey-Brookes",]<-SB[SB$phase==SucheyBrookes,c(1,5,6,7)]
      results[2,2]<-SucheyBrookes
    }else{
      results[,-1][results$trait=="Suchey-Brookes",]<-c(NA,NA,NA,NA)
      results[2,2]<-NA
    }}
  w1<-if(vault$na>0|LA$na>0){paste("you are missing ",as.character(sum(vault$na,LA$na))," suture closure scores. The composite score uses the average of recorded scores to estimate missing values. Therefore the ",if(LA$na>0)"Lateral-Anterior",if(vault$na>0 & LA$na>0)" and ",if(vault$na>0) "vault ","age range",if(vault$na>0 & LA$na>0)"s", "will be less accurate")}
  w2<-if(anyNA(results)){paste("best results come from complete analysis.You are missing: ",if(anyNA(results[results$trait=="Suchey-Brookes"]))"Suchey-Brookes ", if(anyNA(results[results$trait=="Todd"]))"Todd ",if(anyNA(results[results$trait=="Lovejoy"]))"Lovejoy ",if(anyNA(results[results$trait=="Vault"]))"Vault ",if(anyNA(results[results$trait=="Lateral-Anterior"]))"Lateral-Anterior ")}
  
  tRange<-paste(as.character(min(results$min,na.rm=TRUE))," - ",as.character(max(results$max,na.rm=TRUE)))
  aRange<-paste(as.character(min(results$average,na.rm=TRUE))," - ",as.character(max(results$average,na.rm=TRUE)))  
  if(tRange=="Inf  -  -Inf"){tRange<-"NA"}
  if(aRange=="Inf  -  -Inf"){aRange<-"NA"}
  
  output<-list(TotalRange=tRange,AverageRange=aRange,Table=results,Warnings=c(w1,w2))
}
######################################################################################################################################################################################
dfchoose<-function(x,y){
  df<-NA
  rows<-NA
  if(ncol(x)==3){a<-head(row.names(x)[!(is.na(x[,2]))==TRUE],-2)
                 b<-head(row.names(x)[!(is.na(x[,3]))==TRUE],-2)
                 if(anyNA(y[a,2])==FALSE){df<-"DF1"
                                          rows<-a
                 }else{if(anyNA(y[b,2])==FALSE){df<-"DF2"
                                                rows<-b
                 }else{df<-NA}}}
  if(ncol(x)==7){a<-head(row.names(x)[!(is.na(x[,2]))==TRUE],-2)
                 b<-head(row.names(x)[!(is.na(x[,3]))==TRUE],-2)
                 c<-head(row.names(x)[!(is.na(x[,4]))==TRUE],-2)
                 d<-head(row.names(x)[!(is.na(x[,5]))==TRUE],-2)
                 e<-head(row.names(x)[!(is.na(x[,6]))==TRUE],-2)
                 f<-head(row.names(x)[!(is.na(x[,7]))==TRUE],-2)
                 if(anyNA(y[a,2])==FALSE){df<-"DF1"
                                          rows<-a
                 }else{if(anyNA(y[b,2])==FALSE){df<-"DF2"
                                                rows<-b
                 }else{if(anyNA(y[c,2])==FALSE){df<-"DF3"
                                                rows<-c
                 }else{if(anyNA(y[d,2])==FALSE){df<-"DF4"
                                                rows<-d
                 }else{if(anyNA(y[e,2])==FALSE){df<-"DF5"
                                                rows<-e
                 }else{if(anyNA(y[f,2])==FALSE){df<-"DF6"
                                                rows<-f
                 }else{df<-NA}}}}}}}
  list(df=df,rows=rows)
}
######################################################################################################################################################################################
sex<-function(ancestory,subAdult,pelvis,cranial,other){
  results<-data.frame(trait=c("ventral arch","subpubic concavity","ishiopubic ramus","sciatic notch","prearicular sulcus","ishiopubic index","nuchal crest","mastoid process","supra-orbital margin","glabella","mental eminence","craniometrics","Scapula height","Glenoid Height","Humeral Head","Radial Head","Femoral Head"),
                      sex=NA)
  DFW<-data.frame(measurement=c("ML","MB","BABR","BaNA","BB","BaPr","NaAl","PB","LM","sectioning point","percent correct"),
                  DF1=c(3.107,-4.643,5.786,NA,14.821,1.000,2.714,-5.179,6.071,2672.39,86.6),
                  DF2=c(3.400,-3.833,5.433,-0.167,12.200,-0.100,2.200,NA,5.367,2592.32,86.4),
                  DF3=c(1.800,-1.783,2.767,-0.100,6.300,NA,NA,NA,2.833,1296.20,86.4),
                  DF4=c(NA,NA,NA,10.714,16.381,-1.00,4.333,-6.571,14.810,3348.27,84.5),
                  DF5=c(1.236,-1.000,NA,NA,3.291,NA,NA,NA,1.528,536.93,85.5),
                  DF6=c(9.875,NA,NA,7.062,19.062,-1.000,4.375,NA,NA,5066.69,84.9))
  DFB<-data.frame(measurement=c("ML","MB","BABR","BaNA","BB","BaPr","NaAl","PB","LM","sectioning point","percent correct"),
                  DF1=c(9.222,7.000,1.000,NA,31.111,5.889,20.222,-30.556,47.111,8171.53,87.6),
                  DF2=c(3.895,3.632,1.000,-2.053,12.947,1.368,8.158,NA,19.947,4079.12,86.6),
                  DF3=c(3.533,1.667,0.867,0.100,8.700,NA,NA,NA,14.367,2515.91,86.5),
                  DF4=c(NA,NA,NA,1.000,19.389,2.778,11.778,-14.333,23.667,3461.46,87.5),
                  DF5=c(2.111,1.000,NA,NA,4.936,NA,NA,NA,8.037,1387.72,85.3),
                  DF6=c(2.867,NA,NA,-0.100,12.367,-0.233,6.900,NA,NA,2568.97,85.0))
  DFA<-data.frame(measurement=c("ML","MB","BABR","BaNA","BB","BaPr","NaAl","PB","LM","sectioning point","percent correct"),
                  DF1=c(1.000,-0.062,1.865,NA,12.57,NA,NA,NA,NA,579.96,86.4),
                  DF2=c(1.000,0.221,NA,NA,1.095,NA,0.504,NA,NA,380.84,83.1))
  match5<-data.frame(no=c(0:5),sex=c(NA,"f","pf","u","pm","m"))
  match3<-data.frame(no=c(0:3),sex=c("NA","f","u","m"))
  IPI<-data.frame(ancestory=c("european","possible european","african","possible african","asian","possible asian","unknown"),
                  over=c(95,95,88,88,94,94,94),
                  under=c(91,91,83,83,84,84,84))
  w1<-NULL;w2<-NULL
  
  if(subAdult$age=="SubAdult"){
    sex<-"unknown"
    w1<-"We do not sex subadult"
  }else{
    
    results$sex[results$trait=="nuchal crest"]<-as.character(match5$sex[match5$no==cranial$nonmetric[1,2]])
    results$sex[results$trait=="mastoid process"]<-as.character(match5$sex[match5$no==cranial$nonmetric[2,2]])
    results$sex[results$trait=="supra-orbital margin"]<-as.character(match5$sex[match5$no==cranial$nonmetric[3,2]])
    results$sex[results$trait=="glabella"]<-as.character(match5$sex[match5$no==cranial$nonmetric[4,2]])
    results$sex[results$trait=="mental eminence"]<-as.character(match5$sex[match5$no==cranial$nonmetric[5,2]])
    results$sex[results$trait=="sciatic notch"]<-as.character(match5$sex[match5$no==pelvis$nonmetric[4,2]])
    results$sex[results$trait=="ventral arch"]<-as.character(match3$sex[match3$no==pelvis$nonmetric[1,2]])
    results$sex[results$trait=="subpubic concavity"]<-as.character(match3$sex[match3$no==pelvis$nonmetric[2,2]])
    results$sex[results$trait=="ishiopubic ramus"]<-as.character(match3$sex[match3$no==pelvis$nonmetric[3,2]])
    
    if(!(is.na(pelvis$nonmetric[5,2]))){
      if(pelvis$nonmetric[5,2]=="T"){results$sex[results$trait=="prearicular sulcus"]<-"f"
      }else{if(pelvis$nonmetric[5,2]=="F"){results$sex[results$trait=="prearicular sulcus"]<-"m"
      }else{results$sex[results$trait=="prearicular sulcus"]<-"u"}}}
    
    if(!(anyNA(pelvis$metric))){index<-(pelvis$metric[1,2]/pelvis$metric[2,2])*100
                                if(index>IPI[IPI$ancestory==ancestory,2]){results$sex[results$trait=="ishiopubic index"]<-"f"
                                }else{if(index<IPI[IPI$ancestory==ancestory,3]){results$sex[results$trait=="ishiopubic index"]<-"m"
                                }else{results$sex[results$trait=="ishiopubic index"]<-"u"}}}
    
    if(ancestory=="asian"|ancestory=="possible asian"){
      x<-dfchoose(DFA,cranial$metric)
      y<-DFA
    }else{if(ancestory=="african"|ancestory=="possible african"){
      x<-dfchoose(DFB,cranial$metric)
      y<-DFB
    }else{if(ancestory=="european"|ancestory=="possible european"){
      x<-dfchoose(DFW,cranial$metric)
      y<-DFW
    }else{x<-list(df=NA,rows=NA)}}}
    
    if(!(is.na(x$df))){sum<-sum(y[x$rows,names(y)==x$df]*cranial$metric[x$rows,2])
                       if(sum>y[10,names(y)==x$df]){results$sex[results$trait=="craniometrics"]<-"m"
                       }else{results$sex[results$trait=="craniometrics"]<-"f"}}
    
    if(!(is.na(other[1]))){if(other[1]<140){results$sex[results$trait=="Scapula height"]<-"f"
    }else{if(other[1]>170){results$sex[results$trait=="Scapula height"]<-"m"
    }else{results$sex[results$trait=="Scapula height"]<-"u"}}}
    
    if(!(is.na(other[2]))){if(other[2]<36){results$sex[results$trait=="Glenoid Height"]<-"pf"
    }else{if(other[2]>36){results$sex[results$trait=="Glenoid Height"]<-"pm"
    }else{results$sex[results$trait=="Glenoid Height"]<-"u"}}}
    
    if(!(is.na(other[4]))){if(other[4]<21){results$sex[results$trait=="Radial Head"]<-"f"
    }else{if(other[4]>=24){results$sex[results$trait=="Radial Head"]<-"m"
    }else{results$sex[results$trait=="Radial Head"]<-"u"}}}
    
    if(!(is.na(other[3]))){if(other[3]<43){results$sex[results$trait=="Humeral Head"]<-"f"
    }else{if(other[3]>47){results$sex[results$trait=="Humeral Head"]<-"m"
    }else{if(other[3]<44){results$sex[results$trait=="Humeral Head"]<-"pf"
    }else{if(other[3]>46){results$sex[results$trait=="Humeral Head"]<-"pm"
    }else{results$sex[results$trait=="Humeral Head"]<-"u"}}}}}
    
    if(!(is.na(other[5]))){if(other[5]<40){results$sex[results$trait=="Femoral Head"]<-"f"
    }else{if(other[5]>47.5){results$sex[results$trait=="Femoral Head"]<-"m"
    }else{if(other[5]<43){results$sex[results$trait=="Femoral Head"]<-"pf"
    }else{if(other[5]>46.5){results$sex[results$trait=="Femoral Head"]<-"pm"
    }else{results$sex[results$trait=="Femoral Head"]<-"u"}}}}}
    
    results<-rbind(results,data.frame(trait=c("m","f","pf","pm","u"),sex=c("m","f","pf","pm","u")))
    results$sex<-as.factor(results$sex)
    results<-results[1:17,]
    
    if(sum(summary(results$sex)["f"],summary(results$sex)["pf"],na.rm=TRUE)>9){
      if(all(is.na(results[c(1:6),2]))){
        sex<-"possible female"
        w2<-"without pelvis assessment sex determination can only give possible results"
      }else{if(summary(results$sex)["f"]>7){sex<-"female"
      }else{sex<-"possible female"}}
      
    }else{if(sum(summary(results$sex)["m"],summary(results$sex)["pm"],na.rm=TRUE)>9){
      if(all(is.na(results[c(1:6),2]))){
        sex<-"possible male"
        w2<-"without pelvis assessment sex determination can only give possible results"
      }else{if(summary(results$sex)["m"]>7){sex<-"male"
      }else{sex<-"possible male"}}
      
    }else{if((sum(summary(results$sex)["f"],summary(results$sex)["pf"],na.rm=TRUE)>sum(summary(results$sex)["m"],summary(results$sex)["pm"],na.rm=TRUE))& summary(results$sex)["NA's"]<10){
      if(sum(summary(results[c(1:6),2])["f"],summary(results[c(1:6),2])["pf"],na.rm=TRUE)<sum(summary(results[c(1:6),2])["m"],summary(results[c(1:6),2])["pm"],na.rm=TRUE)){sex<-"unkown"
      }else{if(summary(results[c(1:6),2])["f"]>5){sex<-"female"
      }else{sex<-"possible female"}}
      
    }else{if((sum(summary(results$sex)["m"],summary(results$sex)["pm"],na.rm=TRUE)>sum(summary(results$sex)["f"],summary(results$sex)["pf"],na.rm=TRUE))& summary(results$sex)["NA's"]<10){
      if(sum(summary(results[c(1:6),2])["m"],summary(results[c(1:6),2])["pm"],na.rm=TRUE)<sum(summary(results[c(1:6),2])["f"],summary(results[c(1:6),2])["pf"],na.rm=TRUE)){sex<-"unkown"
      }else{if(summary(results[c(1:6),2])["m"]>5){sex<-"male"
      }else{sex<-"possible male"}}
      
    }else{if(summary(results[c(1:6),2])["f"]==6){sex<-"female"
    }else{if(summary(results[c(1:6),2])["m"]==6){sex<-"male"
    }else{if(sum(summary(results[c(1:6),2])["f"],summary(results[c(1:6),2])["pf"],na.rm=TRUE)>5){sex<-"probable female"
    }else{if(sum(summary(results[c(1:6),2])["m"],summary(results[c(1:6),2])["pm"],na.rm=TRUE)>5){sex<-"probable male"
    }else{sex<-"unknown"}}}}}}}}
    }
	
	w3<-if(anyNA(results)){"Your input was incomplete please note that the most reliable results come from complete analysis"}
    list(Sex=sex,Table=results,Warnings=c(w1,w2,w3))
    }
    
    