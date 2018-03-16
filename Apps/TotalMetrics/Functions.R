HelpMetrics<-function(metric,skip,nrows){
  Help<-read.csv("Help",header=TRUE,skip=skip,nrows=nrows)
  Help$Description[Help$Abbrev==metric]
}

CreateMetric<-function(ID,name,Investigator,dir,equipment){
  filepath<-paste(dir,ID,".M.OD.txt",sep="")
  if(file.exists(filepath)){stop(filepath," already exists")}
  Head<-c("#Osteological data: Metrics",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",Investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=TotalMetricsV.1.2"),
          paste("#Equipment:",paste(equipment,collapse=","),sep=""),
          "#Fields:IndividualID,InvestigatorInitials,Date,Cranial,Mandible,Shoulder,Pelvis,UpperLimb,LowerLimb,Fragmented long bone,Articulated,Vertebrae,Metatarsal,Other",
          "#C:g_op,eu_eu,zy_zy,ba_b,ba_n,ba_pr,ecm_ecm,pr_alv,au_au,n_pr,ft_ft,fmt_fmt,n_ns,al_al,d_ec,OH,ec_ec,d_d,n_b,b_l,l_o,ba_o,ML,po_b,mfb,mob,acb,nmfs,nzos,nas:numeric",
          "#M:id_gn,mbh,mbb,go_go,cdl_cdl,minrb,maxrb,maxrh,ML,MA:numeric",
          "#Sh:cl,csd,cvd,sh,sb,gh,gb,ssl,ssw,al,cpl:numeric",
          "#P:och,ib,pl,pl2,pl3,il,il2,il3,psh,midpw,minpw,iprw,pa,snw,snd,ad,sl,sb,s1b:numeric",
          "#UL:mhl,heb,vhhd,mxhd,minhd,mrl,srd,trd,rhd,mul,pul,sud,tud,muc:numeric",
          "#LL:mfl,bfl,efb,mfhd,sfsd,tfsd,sfmd,tfmd,mfc,lfcd,mfcd,lfcw,mfcw,icw,icd,stl,mtl,ptl,mptb,mdtb,std,ttd,tc,mfbl,mfbd:numeric",
          "#FLB:fhhl,fhsl,fhol,fhtl,frhl,frpl,frsl,frdl,ffpl,ffsl,ffscl,ffdl,ftpl,fttl,ftsl,ftdl,ftml:numeric",
          "#A:AH,bid,cv,tpd,n_gn:numeric",
          "#V:c1,c2,c3,c4,c5,c6,c7,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,l1,l2,l3,l4,l5:numeric",
          "#MT:m1,m2,m3,m4,m5:numeric",
          "#O:mcl,cb,mtl,manl,msl,s1w,s3w:numeric")
  Table<-data.frame(ID=NA,In=NA,D=NA,C=NA,M=NA,Sh=NA,P=NA,UL=NA,LL=NA,FLB=NA,A=NA,V=NA,MT=NA,O=NA)
  write(Head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
  paste(ID,".M.OD.txt created", sep="")
}

AppendMetric<-function(PopID,ID,In,dir,C,M,Sh,P,UL,LL,FLB,A,V,MT,O){
  #adds indivdual record to an existing .BP.OA.txt
  filepath<-paste(dir,PopID,".M.OD.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    Table<-data.frame(ID=rep(ID,3),In=rep(In,3),D=format(Sys.time(),"%d/%m/%y_%H:%M:%S"),C=C,M=M,Sh=Sh,P=P,UL=UL,LL=LL,FLB=FLB,A=A,V=V,MT=MT,O=O)
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    paste("Added Individual",ID,"to",filepath)
  }
}

CompCheck<-function(PopID,PopName,Investigator,dir,equipment){
  #checks to see if details in an existing file match those current on the form
  filepath<-paste(dir,PopID,".M.OD.txt",sep="")
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
    line4<-file[4]
    #compatability warnings
    if(!(identical(Name,PopName))){print(paste("The Population names differ.In the existing file it is given as ",Name,sep=""))}
    if(!(identical(Invest,Investigator))){print(paste("The file was created by ",Invest,". Please make sure you have permission to write to this file",sep=""))}
    if(!(identical(version,"Program=TotalMetricsV.1.2"))){print(paste("The file was created by using a diffrent version of this software, Please check version update info for potential compatability issues",sep=""))}
    if(!(identical(line4,paste("#Equipment:",paste(equipment,collapse=","),sep="")))){print(paste("The equipment available differs.In the existing file it is given as:",line4,sep=""))}
  }
  print("Compatability check complete")
}
