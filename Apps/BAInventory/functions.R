#function definations 
InventoryScore<-function(present,complete,table){
  #function to produce coded scores
  library(BMS)
  table$present[table$element %in% present]<-1
  table$complete[table$element %in% complete]<-1
  complete<-bin2hex(table$complete[table$present==1])
  if(complete==""){complete<-0}
  paste(bin2hex(table$present),":",complete,sep="")
}

CScore<-function(C,present,complete){
  #produces coded score for named custom traits
  Custom<-data.frame(names=C,element=c("C1","C2","C3","C4","C5","C6","C7","C8"),present=0,complete=0)
  Custom<-Custom[!(Custom$names=="NA"),]
  if(length(Custom[,1])==0){
    Score<-NA
  }else{Score<-InventoryScore(present,complete,Custom)}
  Score
}

NonMetricScore<-function(p1,d1,p2,d2){
  #produces coded score for non-metric traits
  cranial<-InventoryScore(p1,NA,d1)
  cranial<-strsplit(cranial,split=":")[[1]][1]
  Post<-InventoryScore(p2,NA,d2)
  Post<-strsplit(Post,split=":")[[1]][1]
  paste(cranial,Post,sep=":")
}
InventoryIssue<-function(present,complete){
  #tests if any element are tick as 100% complete but not tick as present
  CinP<-complete %in% present
  if(sum(CinP)!=length(complete)){
    paste("Please check:",complete[CinP==FALSE])
  }else{"no issues"}
}
CreateSI<-function(ID,name,Investigator,custom,dir){
  #creates the .SI.txt file
  filepath<-paste(dir,ID,".SI.txt",sep="")
  if(file.exists(filepath)){stop(filepath," already exists")}
  Head<-c("#Skeletal Inventory File",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",Investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=BAinventoryV1.0"),
          "#Fields:IndividualID,InvestigatorInitials,Date,Skull,Vertebrae,Thorax,Shoulder,Pelvis,Arm,Hand,Leg,Foot,UnidentifiedFragments,Non-metricTraits,Custom",
          "#Coding: Standard=Present:Complete, Non-Metric trait=NMTc:NMTp",
          "#Sk:rI,lI,rM,lM,rS,lS,V,rINC,lINC,rPl,lPl,E,rN,lN,rL,lL,rPr,lPr,rZ,lZ,FS,FPO,FSO,rTS,rTM,rTP,rTZ,lTS,lTM,lTP,lTZ,OS,OB,rOC,lOC,SB,rSP,lSP,rSW,lSW,MC,rMR,lMR,rMA,rMP,rMF,lMA,lMP,lMF:hex",
          "#V:H,C1,C2,C2O,C3b,C3a,C4b,C4a,C5b,C5a,C6b,C6a,C7b,C7a,T1b,T1a,T2b,T2a,T3b,T3a,T4b,T4a,T5b,T5a,T6b,T6a,T7b,T7a,T8b,T8a,T9b,T9a,T10b,T10a,T11b,T11a,T12b,T12a,L1b,L1a,L2b,L2a,L3b,L3a,L4b,L4a,L5b,L5a,CO,TO,LO:hex",
          "#T:M,CS,X,r1,l1,r2,l2,r12,l12,r11,l11,r3,r4,r5,r6,r7,r8,r9,r10,l3,l4,l5,l6,l7,l8,l9,l10,O,S:hex",
          "#Sh:rC,lC,rSB,rG,rSS,rCC,rA,lSB,lG,lSS,lCC,lA:hex",
          "#P:rASS,lASS,rAl,lAL,Sa,S1b,S2b,LSb,C,rIL,rIS,rp,rAC,rPS,rASOS,lIL,lIS,lp,lAC,lPS,lASOS:hex",
          "#A:rHH,rHP,rHS,rHD,lHH,lHP,lHS,lHD,rRH,rRP,rRS,rRD,lRH,lRP,lRS,lRD,rO,rC,rUP,rUS,rUH,rO,rC,rUP,rUS,rUH:hex",
          "#H:rS,rL,rTQ,rP,rTM,rTD,rC,rH,lS,lL,lTQ,lP,lTM,lTD,lC,lH,rM1,rM2,rM3,rM4,rM5,lM1,lM2,lM3,lM4,lM5,rPP1,rPP2,rPP3,rPP4,rPP5,rIP2,rIP3,rIP4,rIP5,rDP1,rDP2,rDP3,rDP4,rDP5,lPP1,lPP2,lPP3,lPP4,lPP5,lIP2,lIP3,lIP4,lIP5,lDP1,lDP2,lDP3,lDP4,lDP5,MO,PO,MS,PS:hex",
          "#L:rFH,rFN,rFT,rFS,rFD,rP,lFH,lFN,lFT,lFS,lFD,lP,rTP,rPT,rTS,rTD,lTP,lPT,lTS,lTD,rFIP,rFIS,rFID,lFIP,lFIS,lFID:hex",
          "#F:rT,rCL,rCB,rN,rLC,rIC,rMC,lT,lCL,lCB,lN,lLC,lIC,lMC,rM1,rM2,rM3,rM4,rM5,lM1,lM2,lM3,lM4,lM5,rPP1,rPP2,rPP3,rPP4,rPP5,rIP2,rIP3,rIP4,rIP5,rDP1,rDP2,rDP3,rDP4,rDP5,lPP1,lPP2,lPP3,lPP4,lPP5,lIP2,lIP3,lIP4,lIP5,lDP1,lDP2,lDP3,lDP4,lDP5,MO,PO,MS,PS:hex",
          "#UF:Skull,LongBone,Vert,HandFoot,ClavScap,Oscoxae,Other:count",
          "#NMTc:HNL,OL,OLS,PF,OB,MS,OCS,OP,FTA,OPN,OA,AT,FH,MFA,PCC,DCF,PCT,ACC,IFO,OFS,ALPF,PT,MT,AZF,BSON,ASOF,AEFE,PEFA,AIOF:hex",
          "#NMTp:AF,PF,P,HTF,ETF,3T,MTSF,LTSF,SCP,SA,AC,SF,ASF,AAF,BSSN,CS,VN,VF,EP,OT,MTF,LTE,DICF,DACF,AACF,PT,DAF,PAB,LAB,TFB:hex",
          paste("#C:",paste(custom,collapse=","), sep="")
  )
  Table<-data.frame(ID=NA,In=NA,D=NA,Sk=NA,V=NA,T=NA,Sh=NA,P=NA,A=NA,H=NA,L=NA,F=NA,UF=NA,NMT=NA,C=NA)
  write(Head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
  paste("File ",ID,".SI.txt created", sep="")
}
AppendSI<-function(PopID,ID,In,Sk,V,T,Sh,P,A,H,L,F,UF,NMT,C,dir){
  #adds row to existing file with individual data in
  Table<-data.frame(ID,In,D=gsub(" ","_",as.character(Sys.time())),Sk,V,T,Sh,P,A,H,L,F,UF,NMT,C)
  filepath<-paste(dir,PopID,".SI.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    paste("Added Individual",ID)
  }
}
CompCheck<-function(PopID,PopName,Investigator,custom,dir){
  #checks to see if details in an existing file match those current on the form
  filepath<-paste(dir,PopID,".SI.txt",sep="")
  #Step1: check file exists
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    #Extract data from header lines
    file<-readLines(filepath,n=18)
    line2<-file[2];line2<-strsplit(line2,split=":")
    Name<-strsplit(line2[[1]][2],split="=")[[1]][2]
    Invest<-strsplit(line2[[1]][3],split="=")[[1]][2]
    line3<-file[3];line3<-strsplit(line3,split=":")
    version<-tail(line3[[1]],1)
    CustomLine<-grep("#C:",file,value=TRUE)
    CustomLine<-strsplit(CustomLine,split=":")[[1]][2]
    
    #compatability warnings
    if(!(identical(Name,PopName))){print(paste("The Population names differ.In the existing file it is given as ",Name,sep=""))}
    if(!(identical(Invest,Investigator))){print(paste("The file was created by ",Invest,". Please make sure you have permission to write to this file",sep=""))}
    if(!(identical(version,"Program=BAinventoryV1.0"))){print(paste("The file was created by using a diffrent version of this software, Please check version update info for potential compatability issues",sep=""))}
    if(!(identical(CustomLine,paste(custom,collapse=",")))){print(paste("The Custom features do not match. The current file has the following: ",CustomLine,sep=""))}
  }
  print("Compatability check complete")
}
