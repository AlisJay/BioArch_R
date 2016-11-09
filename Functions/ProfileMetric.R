#' Produces basic assessment of the metrics recorded for sex determination
#'
#' @param BPOA loaded biological profile file, created using ReadBioArch("file.BP.OA.txt")
#' @param BPOD loaded Osteological assement:Biological Profile file, created using ReadBioArch("file.BP.OD.txt)
#' @param Prob.as.certain logical, should probable female and probale male be treated as female and male, default =TRUE
#'
#' @return List of dataframes, Individual= The recorded data for each individual, Population= summary of the data on population level, Calculated= indices and discriminate functions calculated from raw data for each individual, assignment= series of three dataframe that exainine the accuracy of sex assignment using each measure
#' 
#' @export
#'
#' @examples
ProfileMetrics<-function(BPOA,BPOD,Prob.as.certain=TRUE){
  
  if(Prob.as.certain){
    SexConvert<-function(sex){
      out<-"u"
      if(sex=="female"|sex=="probable female"|sex=="f"|sex=="pf"){out<-"f"}
      if(sex=="male"|sex=="probable male"|sex=="m"|sex=="pm"){out<-"m"}
      if(is.na(sex)|sex=="NA"){out<-NA}
      out
    }
  }else{
    SexConvert<-function(sex){
      out<-"u"
      if(sex=="female"|sex=="f"){out<-"f"}
      if(sex=="male"|sex=="m"){out<-"m"}
      if(is.na(sex)|sex=="NA"){out<-NA}
      out
    }
  }
  
  IDs<-OABP$Table$ID
  Individual<-data.frame(ID=IDs,sex=NA,pLength=NA,iLength=NA,pLength2=NA,iLength2=NA,aWidth=NA,sHeight=NA,gHeight=NA,hHead=NA,rHead=NA,FHead=NA,
                         mxLength=NA,mxBreadth=NA,BaBr=NA,BaNa=NA,zBreadth=NA,BaPr=NA,NaAL=NA,pBreadth=NA,mLength=NA)
  
  for(i in 1:length(IDs)){
    Individual$sex[Individual$ID==IDs[i]]<-SexConvert(BP$Table$S[BP$Table$ID==IDs[i]])
    Individual[Individual$ID==IDs[i],3:7]<-as.numeric(strsplit(OABP$Table$PM[i],split=":")[[1]])
    Individual[Individual$ID==IDs[i],8:12]<-as.numeric(strsplit(OABP$Table$PCM[i],split=":")[[1]])
    Individual[Individual$ID==IDs[i],13:21]<-as.numeric(strsplit(OABP$Table$CM[i],split=":")[[1]])
  }
  Population<-data.frame(metrics=c("pLength","pLength2","iLength","iLength2","aWidth","sHeight","gHeight","hHead","rHead","FHead",
                                   "mxLength","mxBreadth","BaBr","BaNa","zBreadth","BaPr","NaAL","pBreadth","mLength"),
                         mean=NA,SD=NA,mean_m=NA,mean_f=NA,SD_m=NA,SD_f=NA,Absence=NA)
  for(i in 1:length(Population$metrics)){
    Population$mean[i]<-mean(Individual[,as.character(Population$metrics[i])],na.rm=TRUE)
    Population$SD[i]<-sd(Individual[,as.character(Population$metrics[i])],na.rm=TRUE)
    Population$mean_m[i]<-mean(Individual[Individual$sex=="m",as.character(Population$metrics[i])],na.rm=TRUE)
    Population$mean_f[i]<-mean(Individual[Individual$sex=="f",as.character(Population$metrics[i])],na.rm=TRUE)
    Population$SD_m[i]<-sd(Individual[Individual$sex=="m",as.character(Population$metrics[i])],na.rm=TRUE)
    Population$SD_f[i]<-sd(Individual[Individual$sex=="f",as.character(Population$metrics[i])],na.rm=TRUE)
    Population$Absence[i]<-sum(is.na(Individual[,as.character(Population$metrics[i])]))/length(IDs)
  }
  
  Calculated<-data.frame(ID=IDs,IPI_1=Individual$pLength/Individual$iLength*100,IPI_2=Individual$pLength2/Individual$iLength2*100,
                         IP_Sielder=(13.1563*Individual$iLength)-(8.2527*Individual$pLength),
                         API_1=Individual$pLength/Individual$aWidth,API_2=Individual$aWidth/Individual$pLength2*100,
                         CM1=(Individual$mxLength*3.107)+(Individual$mxBreadth*-4.643)+(Individual$BaBr*5.786)+(Individual$zBreadth*14.821)+(Individual$BaPr*1)+(Individual$NaAL*2.714)+(Individual$pBreadth*-5.179)+(Individual$mLength*6.071),
                         CM2=(Individual$mxLength*3.400)+(Individual$mxBreadth*-3.833)+(Individual$BaBr*5.433)+(Individual$BaNa*-0.167)+(Individual$zBreadth*12.2)+(Individual$BaPr*-0.1)+(Individual$NaAL*2.2)+(Individual$mLength*5.367),
                         CM3=(Individual$mxLength*1.800)+(Individual$mxBreadth*-1.783)+(Individual$BaBr*2.767)+(Individual$BaNa*-0.1)+(Individual$zBreadth*6.3)+(Individual$mLength*2.833),
                         CM4=(Individual$BaNa*10.714)+(Individual$zBreadth*16.381)+(Individual$BaPr*-1)+(Individual$NaAL*4.333)+(Individual$pBreadth*-6.571)+(Individual$mLength*14.810),
                         CM5=(Individual$mxLength*1.236)+(Individual$mxBreadth*-1)+(Individual$zBreadth*3.291)+(Individual$mLength*1.528),
                         CM6=(Individual$mxLength*9.875)+(Individual$BaNa*7.062)+(Individual$zBreadth*19.062)+(Individual$BaPr*-1)+(Individual$NaAL*4.375),
                         CM7=(Individual$mxLength*9.222)+(Individual$mxBreadth*7.000)+(Individual$BaBr*1)+(Individual$zBreadth*31.111)+(Individual$BaPr*5.889)+(Individual$NaAL*20.222)+(Individual$pBreadth*-30.556)+(Individual$mLength*47.111),
                         CM8=(Individual$mxLength*3.895)+(Individual$mxBreadth*3.632)+(Individual$BaBr*1)+(Individual$BaNa*-2.053)+(Individual$zBreadth*12.947)+(Individual$BaPr*1.368)+(Individual$NaAL*8.158)+(Individual$mLength*19.947),
                         CM9=(Individual$mxLength*3.533)+(Individual$mxBreadth*1.667)+(Individual$BaBr*0.867)+(Individual$BaNa*0.100)+(Individual$zBreadth*8.700)+(Individual$mLength*14.367),
                         CM10=(Individual$BaNa*1)+(Individual$zBreadth*19.389)+(Individual$BaPr*2.778)+(Individual$NaAL*11.778)+(Individual$pBreadth*-14.333)+(Individual$mLength*23.667),
                         CM11=(Individual$mxLength*2.111)+(Individual$mxBreadth*1)+(Individual$zBreadth*4.936)+(Individual$mLength*8.037),
                         CM12=(Individual$mxLength*2.867)+(Individual$BaNa*-0.1)+(Individual$zBreadth*12.367)+(Individual$BaPr*-0.233)+(Individual$NaAL*6.900),
                         CM13=(Individual$mxLength*1)+(Individual$mxBreadth*-0.062)+(Individual$BaBr*1.865)+(Individual$zBreadth*1.257),
                         CM14=(Individual$mxLength*1)+(Individual$mxBreadth*0.221)+(Individual$zBreadth*1.095)+(Individual$NaAL*0.504))
  
  Assignment<-data.frame(ID=IDs,sex=Individual$sex,IPI_1=NA,aWidth_SteynIscan=NA,sHeight=NA,gHeight=NA,hHead=NA,rHead=NA,FHead=NA,craniometrics=NA,
                         IP_Siedler=NA,API_1=NA,API_2=NA,aWidth_Murphy=NA,aWidth_Patriquin=NA,CM1=NA,CM2=NA,CM3=NA,CM4=NA,
                         CM5=NA,CM6=NA,CM7=NA,CM8=NA,CM9=NA,CM10=NA,CM11=NA,CM12=NA,CM13=NA,CM14=NA)
  for(i in 1:length(IDs)){
    BpAssign<-strsplit(BP$Table$SD[i],split=":")[[1]]
    Assignment[i,3]<-SexConvert(BpAssign[13]);Assignment[i,4]<-SexConvert(BpAssign[14])
    Assignment[i,5]<-SexConvert(BpAssign[26]);Assignment[i,6]<-SexConvert(BpAssign[27])
    Assignment[i,7]<-SexConvert(BpAssign[28]);Assignment[i,8]<-SexConvert(BpAssign[29])
    Assignment[i,9]<-SexConvert(BpAssign[30]);Assignment[i,10]<-SexConvert(BpAssign[25])
    Assignment$API_2[i]<-if(is.na(Calculated$API_2[i])){"u"}else{if(Calculated$API_2[i]<75){"f"}else{if(Calculated$API_2[i]>80){"m"}else{"u"}}}
    Assignment$API_1[i]<-if(is.na(Calculated$API_1[i])){"u"}else{as.character(Dixit_API(Calculated$API_1[i]))}
    Assignment$aWidth_Murphy[i]<-if(is.na(Individual$aWidth[i])){"u"}else{if(((Individual$aWidth[i]*0.674199)-35.17075)>1.30345){"m"}else{"f"}}
    Assignment$aWidth_Patriquin[i]<-if(is.na(Individual$aWidth[i])){"u"}else{if(Individual$aWidth[i]>53.29){"m"}else{"f"}}
    Assignment$IP_Siedler[i]<-if(is.na(Calculated$IP_Sielder[i])){"u"}else{if(Calculated$IP_Sielder[i]<395.7){"f"}else{if(Calculated$IP_Sielder[i]>412.97){"m"}else{"u"}}}
    Assignment$CM1[i]<-if(is.na(Calculated$CM1[i])){NA}else{if(Calculated$CM1[i]>2672.39){"m"}else{"f"}}
    Assignment$CM2[i]<-if(is.na(Calculated$CM2[i])){NA}else{if(Calculated$CM2[i]>2592.32){"m"}else{"f"}}
    Assignment$CM3[i]<-if(is.na(Calculated$CM3[i])){NA}else{if(Calculated$CM3[i]>1296.20){"m"}else{"f"}}
    Assignment$CM4[i]<-if(is.na(Calculated$CM4[i])){NA}else{if(Calculated$CM4[i]>3348.27){"m"}else{"f"}}
    Assignment$CM5[i]<-if(is.na(Calculated$CM5[i])){NA}else{if(Calculated$CM5[i]>536.93){"m"}else{"f"}}
    Assignment$CM6[i]<-if(is.na(Calculated$CM6[i])){NA}else{if(Calculated$CM6[i]>5066.69){"m"}else{"f"}}
    Assignment$CM7[i]<-if(is.na(Calculated$CM7[i])){NA}else{if(Calculated$CM7[i]>8171.53){"m"}else{"f"}}
    Assignment$CM8[i]<-if(is.na(Calculated$CM8[i])){NA}else{if(Calculated$CM8[i]>4079.12){"m"}else{"f"}}
    Assignment$CM9[i]<-if(is.na(Calculated$CM9[i])){NA}else{if(Calculated$CM9[i]>2515.91){"m"}else{"f"}}
    Assignment$CM10[i]<-if(is.na(Calculated$CM10[i])){NA}else{if(Calculated$CM10[i]>3461.46){"m"}else{"f"}}
    Assignment$CM11[i]<-if(is.na(Calculated$CM11[i])){NA}else{if(Calculated$CM11[i]>1387.72){"m"}else{"f"}}
    Assignment$CM12[i]<-if(is.na(Calculated$CM12[i])){NA}else{if(Calculated$CM12[i]>2568.97){"m"}else{"f"}}
    Assignment$CM13[i]<-if(is.na(Calculated$CM13[i])){NA}else{if(Calculated$CM13[i]>579.96){"m"}else{"f"}}
    Assignment$CM14[i]<-if(is.na(Calculated$CM14[i])){NA}else{if(Calculated$CM14[i]>380.84){"m"}else{"f"}}
  }
  Agreement<-data.frame(ID=IDs,IPI_1=NA,aWidth_SteynIscan=NA,sHeight=NA,gHeight=NA,hHead=NA,rHead=NA,FHead=NA,craniometrics=NA,
                         IP_Siedler=NA,API_1=NA,API_2=NA,aWidth_Murphy=NA,aWidth_Patriquin=NA,CM1=NA,CM2=NA,CM3=NA,CM4=NA,
                         CM5=NA,CM6=NA,CM7=NA,CM8=NA,CM9=NA,CM10=NA,CM11=NA,CM12=NA,CM13=NA,CM14=NA)
  for(i in 1:length(IDs)){
    for(j in 2:dim(Agreement)[2]){
      Agreement[i,j]<-if(is.na(Assignment[i,j+1])){NA}else{Assignment[i,j+1]==Assignment$sex[i]}
    }
  }
  Agree_summary<-data.frame(rates=c("Absence","Assignment"),IPI_1=NA,aWidth_SteynIscan=NA,sHeight=NA,gHeight=NA,hHead=NA,rHead=NA,FHead=NA,craniometrics=NA,
                   IP_Siedler=NA,API_1=NA,API_2=NA,aWidth_Murphy=NA,aWidth_Patriquin=NA,CM1=NA,CM2=NA,CM3=NA,CM4=NA,
                   CM5=NA,CM6=NA,CM7=NA,CM8=NA,CM9=NA,CM10=NA,CM11=NA,CM12=NA,CM13=NA,CM14=NA)
  
  for (i in 2:dim(Agreement)[2]){
    Agree_summary[1,i]<-round(sum(is.na(Agreement[,i]))/length(IDs),3)
    Agree_summary[2,i]<-round(sum(Agreement[,i],na.rm=TRUE)/(length(IDs)-sum(is.na(Agreement[,i]))),3)
  }
  
  list(Individual=Individual,Population=Population,Calculated=Calculated,Assignment=list(summary=Agree_summary,Assign=Assignment,Agree=Agreement))
}

#' Calculates sex using the acetabular pubic index formula as outlined in Dixit et al (2007)
#'
#' @param API Acetabular pubic Index calculated by: Length of pubic bone /maximum width of acetabulum
#'
#' @return sex in the form of f/m/u
#' @export
#'
#' @examples
Dixit_API<-function(API){
  Dixit<-data.frame(sex=c("f","u","m"),score=NA)
  Dixit$score[1]<-(-140.767)+(171.403*API)
  Dixit$score[2]<-(-114.892)+(154.648*API)
  Dixit$score[3]<-(-101.324)+(145.092*API)
  as.character(Dixit$sex[Dixit$score==max(Dixit$score)])
}