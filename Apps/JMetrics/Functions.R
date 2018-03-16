HelpMetrics<-function(metric,skip,nrows){
  Help<-read.csv("Help",header=TRUE,skip=skip,nrows=nrows)
  Help[,4][Help[,1]==metric]
}

CreateJMetric<-function(ID,name,Investigator,dir,equipment){
  filepath<-paste(dir,ID,".JM.OD.txt",sep="")
  if(file.exists(filepath)){stop(filepath," already exists")}
  Head<-c("#Osteological data:Juvenille Metrics",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",Investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=JMetricsV.1.0"),
          paste("#Equipment:",paste(equipment,collapse=","),sep=""),
          "#Fields:IndividualID,InvestigatorInitials,Date,Cranial,Cranial2,Shoulder,UpperLimb,Pelvis,LowerLimb,Other",
          "#C:O_MWB,O_SLB,O_MLB,O_MLL,O_MWL,T_SH,T_SW,T_SL,T_PPL,T_PPW,T_TRD,S_BL,S_BW,S_LWL,S_LWW,S_GWL,S_GWW,Z_L,Z_OH,Pl_OH,Mx_L,Mx_H,Mx_W,Mx_OL,Mn_BL,Mn_W,Mn_OL:numeric",
          "#C2:P_CH,P_CW,P_AH,P_AW,P_BLC,P_BLA,F_CH,F_CW,F_AH,F_AW,F_NBC,F_NBA,N_L,N_W,N_AH,N_SAW,N_MAW,N_INSL,N_B,INC_L,V_L:numeric",
          "#Sh:S_GSL,S_MGD,S_GML,S_SL,S_W,S_L,S_ISH,S_SSH,S_AW,C_L:numeric",
          "#UL:H_DL,H_DW,H_TL,U_DL,U_TL,R_DL,R_TL,MC1_L,MC2_L,MC2_W:numeric",
          "#P:Il_L,Il_W,Is_L,Is_W,P_L:numeric",
          "#LL:F_DL,F_DW,F_TL,T_DL,T_TL,Fb_DL,Fb_TL,MT1_DL,MT2_DL,MT3_DL,MT4_DL,MT5_DL:numeric",
          "#O:At_NAL,Ax_NAL,R1_L:numeric")
  Table<-data.frame(ID=NA,In=NA,D=NA,C=NA,C2=NA,Sh=NA,UL=NA,P=NA,LL=NA,O=NA)
  write(Head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
  paste(ID,".M.OD.txt created", sep="")
}

AppendJMetric<-function(PopID,ID,In,dir,C,C2,Sh,UL,P,LL,O){
  #adds indivdual record to an existing .BP.OA.txt
  filepath<-paste(dir,PopID,".JM.OD.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    Table<-data.frame(ID=rep(ID,3),In=rep(In,3),D=rep(format(Sys.time(),"%d/%m/%y_%H:%M:%S"),3),C=C,C2=C2,Sh=Sh,UL=UL,P=P,LL=LL,O=O)
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    paste("Added Individual",ID,"to",filepath)
  }
}

CompCheck<-function(PopID,PopName,Investigator,dir,equipment){
  #checks to see if details in an existing file match those current on the form
  filepath<-paste(dir,PopID,".JM.OD.txt",sep="")
  #Step1: check file exists
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    #Extract data from header lines
    file<-readLines(filepath,n=12)
    line2<-file[2];line2<-strsplit(line2,split=":")
    Name<-strsplit(line2[[1]][2],split="=")[[1]][2]
    Invest<-strsplit(line2[[1]][3],split="=")[[1]][2]
    line3<-file[3];line3<-strsplit(line3,split=":")
    version<-tail(line3[[1]],1)
    line4<-file[4]
    #compatability warnings
    if(!(identical(Name,PopName))){print(paste("The Population names differ.In the existing file it is given as ",Name,sep=""))}
    if(!(identical(Invest,Investigator))){print(paste("The file was created by ",Invest,". Please make sure you have permission to write to this file",sep=""))}
    if(!(identical(version,"Program=JMetricsV.1.0"))){print(paste("The file was created by using a diffrent version of this software, Please check version update info for potential compatability issues",sep=""))}
    if(!(identical(line4,paste("#Equipment:",paste(equipment,collapse=","),sep="")))){print(paste("The equipment available differs.In the existing file it is given as:",line4,sep=""))}
  }
  print("Compatability check complete")
}

