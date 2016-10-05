#writing functions for Bio-Profiler
createBP<-function(ID,name="NA",investigator="NA"){
  # create .BP.txt file, that contains the results of bioprofile analysis
  head<-c("#Biological profile",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=BioProfileV2.0"),
          "#Fields:ID,Investigator,date,known,ancestry,sex,age,range,AncestryScore,SexScore,Todd,Suchey-Brooks,Lovejoy,Vault,lateral-anterior",
          "#K:Ancestry,Sex,Age:1,0",
          "#AnS:Asian,African,European,Undetermined:Count",
          "#SS:Male,Probable Male,Unknown,Female,probable Female,Unrecorded:Count",
          "#T,SB,L,V,LA:Phase,min,max,average:seeB&U94,numeric,numeric,numeric")
  Table<-data.frame(ID=NA,In=NA,D=NA,K=NA,An=NA,S=NA,Ag=NA,R=NA,AnS=NA,SS=NA,T=NA,SB=NA,L=NA,V=NA,LA=NA)
  filepath<-paste("../data/",ID,".BP.txt",sep="")
  write(head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
    
  #create a OA.BP.txt file which stores the data entered into bioprofile
  head<-c("#Osteological assement:Biological Profile",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=BioProfileV2.0"),
          "#Fields:ID,Investigator,date,Ancestry3, Ancestry2,Epiphysis Score,Phase Score,Sutures,Pelvic non-metric traits,Preauricular sulcus,Cranial non-metric traits,Pelvis metrics,Post-cranial metrics,Cranial metrics",
          "#AN3:orbit,nRoot,LNB,palate,profile,nwidth:Eu,Af,As,U",
          "AN2:BR,VS,PBD,SI,nBridge,LEB,nSpine,fShape,mMarks,Jaw:T,F,U",
          "#EScore:pRadius,dFibula,dTibia,dFemur,pFibula,acromion,iliac,hHead,fHead,lTrochanter,pTibia,gTrochanter,dradius,s3s5,s2s3,s1s2,clavicle,SOS:hex",
          "#PScore:Todd,Suchey-Brookes,Lovejoy:seeB$U94",
          "#S:ML,L,O,AS,B,P,MC,SF,IST,SST:NA,0,1,2,3",
          "#PNM:VA,SPC,IPR,SN:0-3,0-3,0-3,0-5",
          "#PAS:PAS:NA,T,F",
          "#CNM:NC,MP,SOM,G,ME:0-5",
          "#PM:pLength,pLength2,iLength,iLength2,aWidth:numeric",
          "#PCM:sHeight,gHeight,hHead,rHead,FHead:numeric",
          "#CM:mxLength,mxBreadth,BaBr,BaNa,zBreadth,BaPr,NaAl,pBreadth,mLength:numeric")
  Table<-data.frame(ID=NA,In=NA,D=NA,AN3=NA,AN2=NA,EScore=NA,PScore=NA,S=NA,PNM=NA,PAS=NA,CNM=NA,PM=NA,PCM=NA,CM=NA)
  filepath<-paste("../data/",ID,".OA.BP.txt",sep="")
  write(head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
  paste("Files ", ID ,".BP.txt and ",ID,".OA.BP.txt created", sep="")
}


AppendBP<-function(PopID,ID,In,k,an,s,ag,r,ans,ss,t,sb,l,v,la){
  #adds indivdual record to an existing .BP.txt
  filepath<-paste("../data/",PopID,".BP.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    Table<-data.frame(ID=ID,In=In,D=gsub(" ","_",as.character(Sys.time())),K=k,An=an,S=s,Ag=ag,R=r,AnS=ans,SS=ss,T=t,SB=sb,L=l,V=v,LA=la)
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    paste("Added Individual",ID,"to",filepath)
  }
}

AppendOABP<-function(PopID,ID,In,an3,an2,escore,pscore,s,pnm,pas,cnm,pm,pcm,cm){
  #adds indivdual record to an existing OA.BP.txt
  filepath<-paste("../data/",PopID,".OA.BP.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    Table<-data.frame(ID=ID,In=In,D=gsub(" ","_",as.character(Sys.time())),AN3=an3,AN2=an2,EScore=escore,PScore=pscore,S=s,PNM=pnm,PAS=pas,CNM=cnm,PM=pm,PCM=pcm,CM=cm)
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    paste("Added Individual",ID,"to",filepath)
  }
}

Append2<-function(PopID,ID,In,k,an,s,ag,r,ans,ss,t,sb,l,v,la,an3,an2,escore,pscore,su,pnm,pas,cnm,pm,pcm,cm){
  #uses appropreate append commands depending on wheter known or unknow data is entered
  X<-AppendBP(PopID,ID,In,k,an,s,ag,r,ans,ss,t,sb,l,v,la)
  if(k=="1:1:1"){y<-paste("All data known, no record added to ",PopID,".OA.BP.txt")}
  if(k=="1:1:0"){y<-AppendOABP(PopID,ID,In,"NA","NA",escore,pscore,su,"NA","NA","NA","NA","NA","NA")}
  if(k=="1:0:1"){y<-AppendOABP(PopID,ID,In,"NA","NA","NA","NA","NA",pnm,pas,cnm,pm,pcm,cm)}
  if(k=="1:0:0"){y<-AppendOABP(PopID,ID,In,"NA","NA",escore,pscore,su,pnm,pas,cnm,pm,pcm,cm)}
  if(k=="0:1:1"){y<-AppendOABP(PopID,ID,In,an3,an2,"NA","NA","NA","NA","NA","NA","NA","NA","NA")}
  if(k=="0:0:1"){y<-AppendOABP(PopID,ID,In,an3,an2,"NA","NA","NA",pnm,pas,cnm,pm,pcm,cm)}
  if(k=="0:1:0"){y<-AppendOABP(PopID,ID,In,an3,an2,escore,pscore,su,"NA","NA","NA","NA","NA","NA")}
  if(k=="0:0:0"){y<-AppendOABP(PopID,ID,In,an3,an2,escore,pscore,su,pnm,pas,cnm,pm,pcm,cm)}
  paste(X,y,sep=".")
}

CompCheck<-function(PopID,PopName,Investigator){
  #checks to see if details in an existing file match those current on the form
  filepath<-paste("../data/",PopID,".BP.txt",sep="")
  #Step1: check file exists
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    #Extract data from header lines
    file<-readLines(filepath,n=17)
    line2<-file[2];line2<-strsplit(line2,split=":")
    Name<-strsplit(line2[[1]][2],split="=")[[1]][2]
    Invest<-strsplit(line2[[1]][3],split="=")[[1]][2]
    line3<-file[3];line3<-strsplit(line3,split=":")
    version<-tail(line3[[1]],1)
        
    #compatability warnings
    if(!(identical(Name,PopName))){print(paste("The Population names differ.In the existing file it is given as ",Name,sep=""))}
    if(!(identical(Invest,Investigator))){print(paste("The file was created by ",Invest,". Please make sure you have permission to write to this file",sep=""))}
    if(!(identical(version,"Program=BioProfileV2.0"))){print(paste("The file was created by using a diffrent version of this software, Please check version update info for potential compatability issues",sep=""))}
    
  }
  print("Compatability check complete")
}

ECal<-function(Epiphysis){
   library(BMS)
   ETable<-data.frame(E=c("pRadius","dFibula","dTibia","dFemur","pFibula","acromion","iliac","hHead","fHead","lTrochanter","pTibia","gTrochanter","dradius","s3s5","s2s3","s1s2","clavicle","SOS"),open=0) 
   ETable$open[ETable$E %in% Epiphysis]<-1
   bin2hex(ETable$open)
 }
 
 KnownScore<-function(KnownAge,KnownAncestry,KnownSex){
  age<-0;sex<-0;ancestry<-0
  if(KnownAge=="known"){age<-1}
  if(KnownAncestry=="known"){ancestry<-1}
  if(KnownSex=="known"){sex<-1}
  paste(ancestry,sex,age,sep=":")
  }

aExtract<-function(a){
  Range<-paste(as.character(a$TotalRange)," (",as.character(a$AverageRange),")")
  table<-a$Table
  Todd<-paste(table[1,-1],collapse=":")
  SB<-paste(table[2,-1],collapse=":")
  Love<-paste(table[3,-1],collapse=":")
  Vault<-paste(table[4,-1],collapse=":")
  LA<-paste(table[5,-1],collapse=":")
  list(Range,Todd,SB,Love,Vault,LA)
}

sExtract<-function(s){
  sex<-s$Sex
  table<-s$Table
  sScore<-paste(summary(table$sex)["m"],summary(table$sex)["pm"],summary(table$sex)["u"],summary(table$sex)["pf"],summary(table$sex)["f"],summary(table$sex)["NA's"],sep=":")
  list(sex,sScore)
}