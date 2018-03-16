CreateDEN<-function(ID,name,Investigator,dir){
  filepath<-paste(dir,ID,".DEN.txt",sep="")
  if(file.exists(filepath)){stop(filepath," already exists")}
  Head<-c("#Dental",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",Investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=DentalV1.0"),
          "#Fields:IndividualID,InvestigatorInitials,Date,Perminant score,Decidous Score,Caries,Calculus,Hypoplasia,Hypercalcification,Wear,Measurments,growth,Morphological traits,modification,Ablation",
          "#Perminant:uRM3,uRM2,uRM1,uRP4,uRP3,uRC1,uRI2,uRI1,uLI1,uLI2,uLC1,uLP3,uLP4,uLM1,uLM2,uLM3,lRM3,lRM2,lRM1,lRP4,lRP3,lRC1,lRI2,lRI1,lLI1,lLI2,lLC1,lLP3,lLP4,lLM1,lLM2,lLM3",
          "#Decidous:uRdM2,uRdM1,uRdC1,uRdI2,uRdI1,uLdI1,uLdI2,uLdC1,uLdM1,uLdM2,lRdM2,lRdM1,lRdC1,lRdI2,lRdI1,lLdI1,lLdI2,lLdC1,lLdM1,lLdM2",
          "#Score:Present,Loose,Occulsion,Unerupted,Premortem Loss,Postmortem Loss:Hex",
          "#Car:surface:I/L/B/C/R/XL/NC",
          "#Cal:side_size:B/L_NA/1/2/3",
          "#Hypo:Type_location:1/2/3/4/5_num",
          "#Hyper:Type,Colour_location:6/7,1/2/3/4_num",
          "#Molar Wear:Quandrant_score:Q1/Q2/Q3/Q4_NA/1-10",
          "#Other Wear:score:NA/1-8",
          "#Measure:mmd/cmd/bl/ch:num",
          "#G:eruption,formation:1-21,1-14",
          "#Growth teeth:lRM3,lRM2,lRM1,lLM1,lLM2,lLM3,lRdM1,lRdM2,lRdC1,lLdC1,lLdM1,lLdM2,",
          "#Morph:see B&U94",
          "#Mod:Ids",
          "#Ab:of premortem loss:hex"
  )
  Table<-data.frame(ID=NA,In=NA,D=NA,pScore=NA,dScore=NA,Car=NA,Cal=NA,Hypo=NA,Hyper=NA,W=NA,Measure=NA,G=NA,Morph=NA,Mod=NA,Ab=NA)
  write(Head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
  
  filepath2<-paste(dir,ID,".Mod.DEN.txt",sep="")
  Head<-c("Premortem Dental modifications",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",Investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=DentalV1.0"),
          "#Fields:ID,Tooth,Type,Description",
          "#Types:Filing,Drilling,Inlay,Restoration,Artifact:F,D,I,R,A",
          "#Filing/Drilling:Classification:Classifciation=B$U,group,number/Custom,surface,description",
          "#Inlay:Classification,Inlay,Adhesive:Classifciation=B$U,group,number/Custom,surface,description",
          "#Restoration:Material,Location and spread,further description",
          "#Artifact:Surface,Foreign material inclusion,Description,Carious lesions/perdontal disease")
  Table<-data.frame(ID=NA,Tooth=NA,Type=NA,Description=NA)
  write(Head,filepath2)
  write.table(Table[-1,],filepath2,append=TRUE,row.names=FALSE)
  paste("Files ",filepath," and",filepath2," created", sep="")
}

AppendDEN<-function(PopID,ID,In,dir,pScore,dScore,Car,Cal,Hypo,Hyper,W,Measure,G,Morph,Mod,Ab){
  #adds indivdual record to an existing .BP.OA.txt
  filepath<-paste(dir,PopID,".DEN.txt",sep="")
  filepath2<-paste(dir,PopID,".Mod.DEN.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    Table<-data.frame(ID=ID,In=In,D=format(Sys.time(),"%d/%m/%y_%H:%M:%S"),pScore=pScore,dScore=dScore,Car=Car,Cal=Cal,Hypo=Hypo,Hyper=Hyper,W=W,Measure=Measure,G=G,Morph=Morph,Mod=Mod[[1]],Ab=Ab)
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    if(!(is.na(Mod[[2]]))){write.table(Mod[[2]],filepath2,append=TRUE,row.names=FALSE,col.names=FALSE)}
    paste("Added Individual",ID,sep=" ")
  }
}