DecodeSI1<-function(Table,HeadLine){#standard
  library("BMS")
  Heads<-strsplit(HeadLine,split=":")[[1]][2]
  Heads<-strsplit(Heads,split=",")[[1]]
  IDS<-Table$ID
  
  dfPresent<-data.frame(element=Heads,total=NA,percent=NA)
  dfComplete<-data.frame(element=Heads,total=NA,percent=NA)
  
  for(i in 1:length(IDS)){
    df<-data.frame(element=Heads,present=0,complete=0)
    code<-strsplit(Table[i,2],split=":")[[1]]
    present<-hex2bin(code[1]); complete<-hex2bin(code[2])
    present<-present[(length(present)-length(df$present)+1):length(present)]
    df$present<-present
    dfPresent[,as.character(IDS[i])]<-df$present
    if(sum(present>0)){
      complete<-complete[(length(complete)-sum(present)+1):length(complete)]
      df$complete[df$present==1]<-complete
    }
    dfComplete[,as.character(IDS[i])]<-df$complete
  }
  dfPresent$total<-rowSums(dfPresent[,-c(1:3)])
  dfPresent$percent<-round(dfPresent$total/length(IDS),3)
  dfComplete$total<-rowSums(dfComplete[,-c(1:3)])
  dfComplete$percent<-round(dfComplete$total/length(IDS),3)
  list(Present=dfPresent,Complete=dfComplete)
}
###########################################################################################
DecodeSI2<-function(Table,HeadLine){#custom field
  library("BMS")
  if(HeadLine=="#C:NA,NA,NA,NA,NA,NA,NA,NA"){output<-"NA"
  }else{
    Heads<-strsplit(HeadLine,split=":")[[1]][2]
    Heads<-strsplit(Heads,split=",")[[1]]
    Heads<-Heads[Heads!="NA"]
    dfPresent<-data.frame(element=Heads,total=NA,percent=NA)
    dfComplete<-data.frame(element=Heads,total=NA,percent=NA)
    
    IDS<-Table$ID
    for(i in 1:length(IDS)){
      df<-data.frame(element=Heads,present=0,complete=0)
      code<-strsplit(Table[i,2],split=":")[[1]]
      present<-hex2bin(code[1]); complete<-hex2bin(code[2])
      present<-present[(length(present)-length(df$present)+1):length(present)]
      df$present<-present
      dfPresent[,as.character(IDS[i])]<-df$present
      if(sum(present>0)){
        complete<-complete[(length(complete)-sum(present)+1):length(complete)]
        df$complete[df$present==1]<-complete
      }
      dfComplete[,as.character(IDS[i])]<-df$complete
    }
    dfPresent$total<-rowSums(dfPresent[,-c(1:3)])
    dfPresent$percent<-round(dfPresent$total/length(IDS),3)
    dfComplete$total<-rowSums(dfComplete[,-c(1:3)])
    dfComplete$percent<-round(dfComplete$total/length(IDS),3)
    output<-list(Present=dfPresent,Complete=dfComplete)
  }
  output
}
###########################################################################################
DecodeSI3<-function(Table,HeadLines){#NonMetric traits
  library("BMS")
  Heads1<-strsplit(HeadLines[1],split=":")[[1]][2]
  Heads1<-strsplit(Heads1,split=",")[[1]]
  
  Heads2<-strsplit(HeadLines[2],split=":")[[1]][2]
  Heads2<-strsplit(Heads2,split=",")[[1]]
  
  dfPresent<-data.frame(element=c(Heads1,Heads2),total=NA,percent=NA)
  
  IDS<-Table$ID
  for(i in 1:length(IDS)){
    df<-data.frame(element=c(Heads1,Heads2),present=0)
    code<-strsplit(Table[i,2],split=":")[[1]]
    Cranial<-hex2bin(code[1]); PostCranial<-hex2bin(code[2])
    Cranial<-Cranial[(length(Cranial)-length(Heads1)+1):length(Cranial)]
    PostCranial<-PostCranial[(length(PostCranial)-length(Heads2)+1):length(PostCranial)]
    df$present<-c(Cranial,PostCranial)
    dfPresent[,as.character(IDS[i])]<-df$present
    }
    
  
  dfPresent$total<-rowSums(dfPresent[,-c(1:3)])
  dfPresent$percent<-round(dfPresent$total/length(IDS),3)
  
  dfPresent
  
}
##########################################################
DecodeSI4<-function(Table,HeadLine){#unidentified fragments 
  library("BMS")
  Heads<-strsplit(HeadLine,split=":")[[1]][2]
  Heads<-strsplit(Heads,split=",")[[1]]
  dfnumber<-data.frame(element=Heads,total=NA,average=NA)
  IDS<-Table$ID
  for(i in 1:length(IDS)){
    df<-data.frame(element=Heads,number=0)
    code<-strsplit(Table[i,2],split=":")[[1]]
    df$number<-code
    dfnumber[,as.character(IDS[i])]<-as.numeric(df$number)
  }
  dfnumber$total<-rowSums(dfnumber[,-c(1:3)])
  dfnumber$average<-round(dfnumber$total/length(IDS),3)
  dfnumber
}
###########################################################

DecodeSI<-function(SI){
  Skull<-DecodeSI1(SI[[2]][,c(1,4)],grep("#Sk:",SI[[1]],value=TRUE))
  Vert<-DecodeSI1(SI[[2]][,c(1,5)],grep("#V:",SI[[1]],value=TRUE))
  Thorax<-DecodeSI1(SI[[2]][,c(1,6)],grep("#T:",SI[[1]],value=TRUE))
  Shoulder<-DecodeSI1(SI[[2]][,c(1,7)],grep("#Sh:",SI[[1]],value=TRUE))
  Pelvis<-DecodeSI1(SI[[2]][,c(1,8)],grep("#P:",SI[[1]],value=TRUE))
  Arm<-DecodeSI1(SI[[2]][,c(1,9)],grep("#A:",SI[[1]],value=TRUE))
  Hand<-DecodeSI1(SI[[2]][,c(1,10)],grep("#H:",SI[[1]],value=TRUE))
  Leg<-DecodeSI1(SI[[2]][,c(1,11)],grep("#L:",SI[[1]],value=TRUE))
  Foot<-DecodeSI1(SI[[2]][,c(1,12)],grep("#F:",SI[[1]],value=TRUE))
  Custom<-DecodeSI2(SI[[2]][,c(1,15)],grep("#C:",SI[[1]],value=TRUE))
  NonMetric<-DecodeSI3(SI[[2]][,c(1,14)],grep("#NMT",SI[[1]],value=TRUE))
  UF<-DecodeSI4(SI[[2]][,c(1,13)],grep("#UF",SI[[1]],value=TRUE))
  list(Skull=Skull,Vertebrae=Vert,Thorax=Thorax,Shoulder=Shoulder,Pelvis=Pelvis,Arm=Arm,Hand=Hand,Leg=Leg,Foot=Foot,Custom=Custom,NonMetric=NonMetric,UF=UF)
}

#######################################################################
DecodeAge<-function(Table){
  age<-data.frame(ID=Table$ID,Age=NA,min=NA,max=NA,minA=NA,maxA=NA,AAge=NA)
  for(i in 1:length(age$ID)){
    age$Age[i]<-Table$Ag[i]
    numbers<-as.numeric(strsplit(Table$R[i],split=" ")[[1]])
    numbers<-as.numeric(na.omit(numbers))
    if(length(numbers)==4){age[i,c(3:6)]<-numbers
                           age$AAge[i]<-(age[i,5]+age[i,6])/2
    }else{age[i,c(3:4)]<-numbers
          age$AAge[i]<-(age[i,3]+age[i,4])/2
    }
  }
  age
}