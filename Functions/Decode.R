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
##########################################################################
Abbrev_Library<-function(FileType="Inventory",Bone=NA,Abbreviation=NA,Region=NA,Field=NA,Profile=NA){
  Filetypes<-c("Inventory","Profile")
  if(!(FileType %in% Filetypes)){output<-"Unrecognized file type, valid FileTypes are Inventory and Profile"}
  if(FileType=="Inventory"){
    Library<-read.table("Help/InventoryAbbrevs",header=TRUE,sep=",")
    if(!(is.na(Bone))){
      if(Bone %in% Library$Bone){
        output<-Library[Library$Bone==Bone,]
      }else{if(Bone=="All"){
        output<-unique(Library$Bone)
      }else{
        output<-"Unrecognised Bone use Bone='All' to see valid bone names"}}
    }else{if(!(is.na(Abbreviation))){
      if(Abbreviation %in% Library$Abbreviation){
        output<-Library[Library$Abbreviation==Abbreviation,]
      }else{
        output<-"Unrecognised Abbreviation"}
    }else{if(!(is.na(Region))){
      if(Region %in% Library$Region){
        output<-Library[Library$Region==Region,]
      }else{if(Region=="All"){output<-unique(Library$Region)
      }else{output<-"Unrecognised Region use Region='All' to see valid bone names"}}
        }else{output<-"Please enter either Bone, Abbreviation or Region "}}}
  }
  if(FileType=="Profile"){
    Library<-read.table("Help/ProfileAbbrevs",header=TRUE,sep=",")
    if(!(is.na(Abbreviation))){
      if(Abbreviation %in% Library$Abbreviation){
        output<-Library[Library$Abbreviation==Abbreviation,]
      }else{output<-"Unrecognised Abbreviation"}
    }else{if(!(is.na(Field))){
      if(Field %in% Library$Field){
        output<-Library[Library$Field==Field,]
      }else{if(Field=="All"){output<-unique(Library$Field)
      }else{output<-"Unrecognised Field use Field='All' to see valid field names"}}
    }else{if(!(is.na(Profile))){
      if(Profile %in% Library$Profile){
        output<-Library[Library$Profile==Profile,]
      }else{if(Profile=="All"){output<-unique(Library$Profile)
      }else{output<-"Unrecognised Profile use Profile='All' to see valid profile names"}}
        }else{output<-"Please enter either Field, Profile or Abbreviation"}}}
  }
  output
}
##########################################################################
DecodeM<-function(Metrics,x){
  HeadLine<-Metrics$Head[grep(paste0("#",x),Metrics$Head)]
  Heads<-strsplit(HeadLine,split=":")[[1]][2]
  Heads<-strsplit(Heads,split=",")[[1]]
  IDs<-Metrics$Table$ID
  DF<-data.frame(Measurement=Heads)
  
  for(i in 1:length(IDs)){DF[,as.character(IDs[i])]<-strsplit(Metrics$Table[i,x],split=":")[[1]]}
  DF2<-DF[grepl("/",DF[,2])==TRUE,]
  DF<-DF[grepl("/",DF[,2])==FALSE,]
  M2<-NULL
  for(i in 1:length(DF2$Measurement)){M2<-c(M2,paste0(as.character(DF2$Measurement[i]),"_r"),paste0(as.character(DF2$Measurement[i]),"_l"))}
  DF3<-data.frame(Measurement=M2)
  
  for(i in 1:length(IDs)){
    DF3[,as.character(IDs[i])]<-as.numeric(unlist(strsplit(DF2[,i+1],split="/")))
    DF[,as.character(IDs[i])]<-as.numeric(DF[,as.character(IDs[i])])}
  
  DF<-rbind(DF,DF3)
  DF$Average<-round(rowMeans(as.data.frame(DF[,-1]),na.rm=TRUE),3)
  DF
}
##########################################################################
DecodeDen<-function(Dental){
  library(BMS)
  Teeth<-strsplit(Dental$Head[5],split=":")[[1]][2];Teeth<-strsplit(Teeth,split=",")[[1]]
  T2<-strsplit(Dental$Head[6],split=":")[[1]][2];T2<-strsplit(T2,split=",")[[1]]
  Teeth<-c(Teeth,T2)
  Ids<-Dental$Table$ID
  Out<-list(Population=data.frame(Tooth=Teeth,Present=0,Loose=0,Occulsion=0,Unerupted=0,Premortem=0,Postmortem=0))
  
  for(i in 1:length(Ids)){
    pScore<-strsplit(Dental$Table$pScore[i],split=":")[[1]]
    dScore<-strsplit(Dental$Table$dScore[i],split=":")[[1]]
    Present_P<-hex2bin(pScore[1]);Present_D<-hex2bin(dScore[1])
    Present<-c(Present_P,Present_D)
    Loose_P<-hex2bin(pScore[2]);Loose_P<-Loose_P[(length(Loose_P)-length(Present_P[Present_P==1])+1):length(Loose_P)]
    Loose_D<-hex2bin(dScore[2]);Loose_D<-Loose_D[(length(Loose_D)-length(Present_D[Present_D==1])+1):length(Loose_D)]
    Loose<-c(Loose_P,Loose_D)
    Occulsion_P<-hex2bin(pScore[3]);Occulsion_P<-Occulsion_P[(length(Occulsion_P)-length(Present_P[Present_P==1])+1):length(Occulsion_P)]
    Occulsion_D<-hex2bin(dScore[3]);Occulsion_D<-Occulsion_D[(length(Occulsion_D)-length(Present_D[Present_D==1])+1):length(Occulsion_D)]
    Occulsion<-c(Occulsion_P,Occulsion_D)
    Unerupted_P<-hex2bin(pScore[4]);Unerupted_P<-Unerupted_P[(length(Unerupted_P)-length(Present_P[Present_P==1])+1):length(Unerupted_P)]
    Unerupted_D<-hex2bin(dScore[4]);Unerupted_D<-Unerupted_D[(length(Unerupted_D)-length(Present_D[Present_D==1])+1):length(Unerupted_D)]
    Unerupted<-c(Unerupted_P,Unerupted_D)
    Premortem_P<-hex2bin(pScore[5]);Premortem_P<-Premortem_P[(length(Premortem_P)-length(Present_P[Present_P==0])+1):length(Premortem_P)]
    Premortem_D<-hex2bin(dScore[5]);Premortem_D<-Premortem_D[(length(Premortem_D)-length(Present_D[Present_D==0])+1):length(Premortem_D)]
    Premortem<-c(Premortem_P,Premortem_D)
    Postmortem_P<-hex2bin(pScore[6]);Postmortem_P<-Postmortem_P[(length(Postmortem_P)-length(Present_P[Present_P==0])+1):length(Postmortem_P)]
    Postmortem_D<-hex2bin(dScore[6]);Postmortem_D<-Postmortem_D[(length(Postmortem_D)-length(Present_D[Present_D==0])+1):length(Postmortem_D)]
    Postmortem<-c(Postmortem_P,Postmortem_D)
    
    x<-data.frame(Tooth=Teeth,Present=Present,Loose=0,Occulsion=0,Unerupted=0,Premortem=0,Postmortem=0)
    x$Loose[x$Present==0]<-NA;x$Occulsion[x$Present==0]<-NA;x$Unerupted[x$Present==0]<-NA
    x$Loose[x$Present==1]<-Loose[!(is.na(Loose))];x$Occulsion[x$Present==1]<-Occulsion[!(is.na(Occulsion))];x$Unerupted[x$Present==1]<-Unerupted[!(is.na(Unerupted))]
    x$Premortem[x$Present==0]<-Premortem[!(is.na(Premortem))];x$Premortem[x$Present==1]<-NA;x$Postmortem[x$Present==0]<-Postmortem[!(is.na(Postmortem))];x$Postmortem[x$Present==1]<-NA
    
    if(length(x$Tooth[x$Present==1])>0){
      y<-data.frame(Tooth=x$Tooth[x$Present==1],Caries=strsplit(Dental$Table$Car[i],split=":")[[1]],Calculus=strsplit(Dental$Table$Cal[i],split=":")[[1]],Hypoplasia=strsplit(Dental$Table$Hypo[i],split=":")[[1]],Hypercalcification=strsplit(Dental$Table$Hyper[i],split=":")[[1]],Wear=strsplit(Dental$Table$W[i],split=":")[[1]],Mesiodistal=NA,Interproxmal=NA,Bucolingual=NA,Crown=NA,modification=strsplit(Dental$Table$Mod[i],split=":")[[1]])
      measurements<-strsplit(Dental$Table$Measure[i],split=":")[[1]]
      for(j in 1:length(y$Tooth)){y[j,7:10]<-strsplit(measurements[j],split="/")[[1]]}
    }else{y<-NULL}
    
    Out[[length(Out)+1]]<-list("Score"=x,"Features"=y)
    Out$Population$Present<-Out$Population$Present+x$Present
    Out$Population$Loose<-rowSums(cbind(Out$Population$Loose,x$Loose),na.rm=TRUE)
    Out$Population$Unerupted<-rowSums(cbind(Out$Population$Unerupted,x$Unerupted),na.rm=TRUE)
    Out$Population$Occulsion<-rowSums(cbind(Out$Population$Occulsion,x$Occulsion),na.rm=TRUE)
    Out$Population$Premortem<-rowSums(cbind(Out$Population$Premortem,x$Premortem),na.rm=TRUE)
    Out$Population$Postmortem<-rowSums(cbind(Out$Population$Postmortem,x$Postmortem),na.rm=TRUE)
  }
  Out$Population[,-1]<-(Out$Population[,-1])/length(Ids)
  names(Out)<-c("Population",Ids)
  Out
}

