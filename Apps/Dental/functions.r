InventoryScore<-function(input){
  #Perminant score
  Perminant<-data.frame("Teeth"=c("uRM3","uRM2","uRM1","uRP4","uRP3","uRC1","uRI2","uRI1","uLI1","uLI2","uLC1","uLP3","uLP4","uLM1","uLM2","uLM3",
                                  "lRM3","lRM2","lRM1","lRP4","lRP3","lRC1","lRI2","lRI1","lLI1","lLI2","lLC1","lLP3","lLP4","lLM1","lLM2","lLM3"),
                        "Present"=0,"Loose"=0,"Occulsion"=0,"Unerupted"=0,"Premortem"=0,"Postmortem"=0)
  Present<-c(paste("u",input$u_present,sep=""),paste("l",input$L_present,sep=""))
  Loose<-c(paste("u",input$u_loose,sep=""),paste("l",input$L_loose,sep=""))
  Occulsion<-c(paste("u",input$u_occlusion,sep=""),paste("l",input$L_occlusion,sep=""))
  Unerupted<-c(paste("u",input$u_unerupted,sep=""),paste("l",input$L_unerupted,sep=""))
  Premortem<-c(paste("u",input$u_premortem,sep=""),paste("l",input$L_premortem,sep=""))
  Postmortem<-c(paste("u",input$u_postmortem,sep=""),paste("l",input$L_postmortem,sep=""))
  Perminant$Present[Perminant$Teeth %in% Present]<-1;Perminant$Loose[Perminant$Teeth %in% Loose]<-1
  Perminant$Occulsion[Perminant$Teeth %in% Occulsion]<-1;Perminant$Unerupted[Perminant$Teeth %in% Unerupted]<-1
  Perminant$Premortem[Perminant$Teeth %in% Premortem]<-1;Perminant$Postmortem[Perminant$Teeth %in% Postmortem]<-1
  Perminant$Postmortem[Perminant$Present==0 & Perminant$Premortem ==0]<-1
  Perminant$Occulsion[Perminant$Loose==1 | Perminant$Unerupted==1]<-0
  library(BMS)
  PerminantScore<-paste(bin2hex(Perminant$Present),bin2hex(Perminant$Loose[Perminant$Present==1]),bin2hex(Perminant$Occulsion[Perminant$Present==1]),
                        bin2hex(Perminant$Unerupted[Perminant$Present==1]),bin2hex(Perminant$Premortem[Perminant$Present==0]),bin2hex(Perminant$Postmortem[Perminant$Present==0]),sep=":")
  #Decidous score
  Decidous<-data.frame("Teeth"=c("uRdM2","uRdM1","uRdC1","uRdI2","uRdI1","uLdI1","uLdI2","uLdC1","uLdM1","uLdM2",
                                 "lRdM2","lRdM1","lRdC1","lRdI2","lRdI1","lLdI1","lLdI2","lLdC1","lLdM1","lLdM2"),
                       "Present"=0,"Loose"=0,"Occulsion"=0,"Unerupted"=0,"Premortem"=0,"Postmortem"=0)
  Present<-c(paste("u",input$ud_present,sep=""),paste("l",input$Ld_present,sep=""))
  Loose<-c(paste("u",input$ud_loose,sep=""),paste("l",input$Ld_loose,sep=""))
  Occulsion<-c(paste("u",input$ud_occlusion,sep=""),paste("l",input$Ld_occlusion,sep=""))
  Unerupted<-c(paste("u",input$ud_unerupted,sep=""),paste("l",input$Ld_unerupted,sep=""))
  Premortem<-c(paste("u",input$ud_premortem,sep=""),paste("l",input$Ld_premortem,sep=""))
  Postmortem<-c(paste("u",input$ud_postmortem,sep=""),paste("l",input$Ld_postmortem,sep=""))
  Decidous$Present[Decidous$Teeth %in% Present]<-1;Decidous$Loose[Decidous$Teeth %in% Loose]<-1
  Decidous$Occulsion[Decidous$Teeth %in% Occulsion]<-1;Decidous$Unerupted[Decidous$Teeth %in% Unerupted]<-1
  Decidous$Premortem[Decidous$Teeth %in% Premortem]<-1;Decidous$Postmortem[Decidous$Teeth %in% Postmortem]<-1
  Decidous$Premortem[Decidous$Present==0 & Decidous$Postmortem ==0]<-1
  Decidous$Occulsion[Decidous$Loose==1 | Decidous$Unerupted==1]<-0
  DecidousScore<-paste(bin2hex(Decidous$Present),bin2hex(Decidous$Loose[Decidous$Present==1]),bin2hex(Decidous$Occulsion[Decidous$Present==1]),
                        bin2hex(Decidous$Unerupted[Decidous$Present==1]),bin2hex(Decidous$Premortem[Decidous$Present==0]),bin2hex(Decidous$Postmortem[Decidous$Present==0]),sep=":")
  list("Pscore"=PerminantScore,"Dscore"=DecidousScore)
}
######################################################################################################
#Present
Present<-function(input){
  present_p<-c(paste("u",input$u_present,sep=""),paste("l",input$L_present,sep=""))
  present_d<-c(paste("u",input$ud_present,sep=""),paste("l",input$Ld_present,sep=""))
  remove<-c("l","u")
  present_p<-present_p[!(present_p %in% remove)]
  present_d<-present_d[!(present_d %in% remove)]
  list(present_p,present_d)
}
######################################################################################################
Contains<-function(x,y){#returns true/false if x contains the value y
  sum(x==y)>0
}
######################################################################################################
Mtraits<-function(present,input){
  Teeth<-c("uRI1","uRI2","uLI2","uLI1","uRdI1","uRId2","uLId2","uLId1","uRP4","uRP3","uLP4","uLP3","lRP3","lLP3",
           "uRM3","uRM2","uRM1","uLM1","uLM2","uLM3","lRM3","lRM2","lRM1","lLM1","lLM2","lLM3")
  present<-present[present %in% Teeth]
  if(length(present)==0){out<-NA
  }else{
    out<-data.frame("Teeth"=present,x=NA)
    for(i in 1:length(out$Teeth)){
      if(out$Teeth[i]%in% c("uRI1","uLI1","uRdI1","uLdI1")){
        out$x[i]<-paste(input[[as.character(paste("shovel",out$Teeth[i],sep="_"))]],input[[as.character(paste("DShovel",out$Teeth[i],sep="_"))]],sep="/")}
      if(out$Teeth[i]%in% c("uRI2","uLI2","uRdI2","uLdI2")){
        out$x[i]<-paste(input[[as.character(paste("shovel",out$Teeth[i],sep="_"))]],input[[as.character(paste("DShovel",out$Teeth[i],sep="_"))]],input[[as.character(paste("Peg",out$Teeth[i],sep="_"))]],sep="/")}
      if(out$Teeth[i]%in% c("uRP3","uLP3","uRP4","uLP4")){
        out$x[i]<-input[[as.character(paste("root",out$Teeth[i],sep="_"))]]}
      if(out$Teeth[i]%in% c("lRP3","lLP3")){
        out$x[i]<-input[[as.character(paste("Tomes",out$Teeth[i],sep="_"))]]}
      if(out$Teeth[i]%in% c("uRM3","uRM2","uRM1","uLM3","uLM2","uLM1")){
        out$x[i]<-paste(input[[as.character(paste("Hypocone",out$Teeth[i],sep="_"))]],input[[as.character(paste("Metaconule",out$Teeth[i],sep="_"))]],input[[as.character(paste("Carabelli",out$Teeth[i],sep="_"))]],input[[as.character(paste("extensions",out$Teeth[i],sep="_"))]],sep="/")}
      if(out$Teeth[i]%in% c("lRM3","lRM2","lRM1","lLM3","lLM2","lLM1")){
        out$x[i]<-paste(input[[as.character(paste("Groove",out$Teeth[i],sep="_"))]],input[[as.character(paste("nCusp",out$Teeth[i],sep="_"))]],input[[as.character(paste("Protostylid",out$Teeth[i],sep="_"))]],
                        input[[as.character(paste("Hypoconulid",out$Teeth[i],sep="_"))]],input[[as.character(paste("Entoconulid",out$Teeth[i],sep="_"))]],input[[as.character(paste("Metaconulid",out$Teeth[i],sep="_"))]],input[[as.character(paste("nroot",out$Teeth[i],sep="_"))]],sep="/")}
    }
    w1<-NA;w2<-NA
    if(Contains(present,"uRI1") & Contains(present,"uLI1") & !(Contains(input$u_loose,"RI1")) & !(Contains(input$u_loose,"RI1"))){w1<-input$winging}
    if(Contains(present,"uRdI1") & Contains(present,"uLdI1") & !(Contains(input$u_loose,"uRdI1")) & !(Contains(input$u_loose,"uRdI1"))){w2<-input$winging_d}
    w<-paste(w1,w2,sep="/")
    out<-paste(w,paste(out$x,collapse=":"),sep=":")
  }
  out
}
######################################################################################################
Ablation<-function(input){
  Premortem<-c(paste("u",input$u_premortem,sep=""),paste("l",input$L_premortem,sep=""),paste("u",input$ud_premortem,sep=""),paste("l",input$Ld_premortem,sep=""))
  remove<-c("l","u");Premortem<-Premortem[!(Premortem %in% remove)]
  if(length(Premortem)>0){
  fields<-data.frame("Teeth"=Premortem,Ablation=paste("Ablation",Premortem,sep="_"),Score=0)
  for(i in 1:length(fields$Teeth)){
    if(input[[as.character(fields$Ablation[i])]]){fields$Score[i]<-1}
  }
  library(BMS);out<-bin2hex(fields$Score)
  }else{out<-NA}
  out
}
######################################################################################################
Caries<-function(present,input){
  if(length(present)==0){out<-NA
  }else{
    out<-data.frame("Teeth"= present,x=NA)
    fields<-data.frame("Teeth"=present,"Caries"=paste("Caries",present,sep="_"))
    for(i in 1:length(fields$Teeth)){
      if(length(input[[as.character(fields$Caries[i])]])>0){out$x[i]<-paste(input[[as.character(fields$Caries[i])]],collapse="")}
    }
    out<-paste(out$x,collapse=":")
  }
  out
}
#####################################################################################################
Calculus<-function(present,input){
  if(length(present)==0){out<-NA
  }else{
    out<-data.frame("Teeth"=present,x=NA)
    fields<-data.frame("Teeth"=present,"Calculus"=paste("Calculus",present,sep="_"),"Cal_side"=paste("Cal",present,"side",sep="_"))
    for(i in 1:length(fields$Teeth)){
      if(length(input[[as.character(fields$Calculus[i])]]>0)){out$x[i]<-paste(paste(input[[as.character(fields$Cal_side[i])]],collapse=""),input[[as.character(fields$Calculus[i])]],sep="")}
    }
    out<-paste(out$x,collapse=":")
  }
}
#####################################################################################################
Hypoplasia<-function(present,input){
  if(length(present)==0){out<-NA
  }else{
    out<-data.frame("Teeth"=present,x=NA)
    fields<-data.frame("Teeth"=present,Hypo=paste("Hypo",present,sep="_"),Hypo_lhg=paste("Hypo_lhg",present,sep="_"),Hypo_lhp=paste("Hypo_lhp",present,sep="_"),
                       Hypo_lvg=paste("Hypo_lvg",present,sep="_"),Hypo_nap=paste("Hypo_nap",present,sep="_"),Hypo_sp=paste("Hypo_sp",present,sep="_"))
    for(i in 1:length(fields$Teeth)){
      lhg<-NULL;lhp<-NULL;lvg<-NULL;nap<-NULL;sp<-NULL
      if(Contains(input[[as.character(fields$Hypo[i])]],"lhg")){lhg<-strsplit(input[[as.character(fields$Hypo_lhg[i])]],split="/")[[1]];lhg<-paste(1,lhg,sep="_")}
      if(Contains(input[[as.character(fields$Hypo[i])]],"lhp")){lhp<-strsplit(input[[as.character(fields$Hypo_lhp[i])]],split="/")[[1]];lhp<-paste(2,lhp,sep="_")}
      if(Contains(input[[as.character(fields$Hypo[i])]],"lvg")){lvg<-strsplit(input[[as.character(fields$Hypo_lvg[i])]],split="/")[[1]];lvg<-paste(3,lvg,sep="_")}
      if(Contains(input[[as.character(fields$Hypo[i])]],"nap")){nap<-strsplit(input[[as.character(fields$Hypo_nap[i])]],split="/")[[1]];nap<-paste(4,nap,sep="_")}
      if(Contains(input[[as.character(fields$Hypo[i])]],"sp")){sp<-strsplit(input[[as.character(fields$Hypo_sp[i])]],split="/")[[1]];sp<-paste(5,sp,sep="_")}
      Hypo<-c(lhg,lhp,nap,sp)
      if(!(is.null(Hypo))){out$x[i]<-paste(Hypo,collapse="/")}
    }
    out<-paste(out$x,collapse=":")
  }
  out
}
######################################################################################################
Hypercalcification<-function(present,input){
  if(length(present)==0){out<-NA
  }else{
    out<-data.frame("Teeth"=present,x=NA)
    fields<-data.frame("Teeth"=present,"Hyper_dis"=paste("Hyper_dis",present,sep="_"),"Hyper_ydis"=paste("Hyper_ydis",present,sep="_"),"Hyper_cdis"=paste("Hyper_cdis",present,sep="_"),"Hyper_odis"=paste("Hyper_odis",present,sep="_"),"Hyper_bdis"=paste("Hyper_bdis",present,sep="_"),
                       "Hyper_dif"=paste("Hyper_dif",present,sep="_"),"Hyper_ydif"=paste("Hyper_ydif",present,sep="_"),"Hyper_cdif"=paste("Hyper_cdif",present,sep="_"),"Hyper_odif"=paste("Hyper_odif",present,sep="_"),"Hyper_bdif"=paste("Hyper_bdif",present,sep="_"))
    for(i in 1:length(fields$Teeth)){
      ydis<-NULL;cdis<-NULL;odis<-NULL;bdis<-NULL;ydif<-NULL;cdif<-NULL;odif<-NULL;bdif<-NULL
      if(Contains(input[[as.character(fields$Hyper_dis[i])]],"Y")){ydis<-strsplit(input[[as.character(fields$Hyper_ydis[i])]],split="/")[[1]];ydis<-paste("61",ydis,sep="_")}
      if(Contains(input[[as.character(fields$Hyper_dis[i])]],"C")){cdis<-strsplit(input[[as.character(fields$Hyper_cdis[i])]],split="/")[[1]];cdis<-paste("62",cdis,sep="_")}
      if(Contains(input[[as.character(fields$Hyper_dis[i])]],"O")){odis<-strsplit(input[[as.character(fields$Hyper_odis[i])]],split="/")[[1]];odis<-paste("63",odis,sep="_")}
      if(Contains(input[[as.character(fields$Hyper_dis[i])]],"B")){bdis<-strsplit(input[[as.character(fields$Hyper_bdis[i])]],split="/")[[1]];bdis<-paste("64",bdis,sep="_")}
      if(Contains(input[[as.character(fields$Hyper_dif[i])]],"Y")){ydif<-strsplit(input[[as.character(fields$Hyper_ydif[i])]],split="/")[[1]];ydif<-paste("71",ydif,sep="_")}
      if(Contains(input[[as.character(fields$Hyper_dif[i])]],"C")){cdif<-strsplit(input[[as.character(fields$Hyper_cdif[i])]],split="/")[[1]];cdif<-paste("72",cdif,sep="_")}
      if(Contains(input[[as.character(fields$Hyper_dif[i])]],"O")){odif<-strsplit(input[[as.character(fields$Hyper_odif[i])]],split="/")[[1]];odif<-paste("73",odif,sep="_")}
      if(Contains(input[[as.character(fields$Hyper_dif[i])]],"B")){bdif<-strsplit(input[[as.character(fields$Hyper_bdif[i])]],split="/")[[1]];bdif<-paste("74",bdif,sep="_")}
      Hyper<-c(ydis,cdis,odis,bdis,ydif,cdif,odif,bdif)
      if(!(is.null(Hyper))){out$x[i]<-paste(Hyper,collapse="/")}
    }
    out<-paste(out$x,collapse=":")
  }
  out
}
#####################################################################################################
Wear<-function(present,input){
  if(length(present)==0){out<-NA
  }else{
    out<-data.frame("Teeth"=present,x=NA)
    fields<-data.frame("Teeth"=present,wear=paste("wear",present,sep="_"),Q1=paste("Q1",present,sep="_"),Q2=paste("Q2",present,sep="_"),Q3=paste("Q3",present,sep="_"),Q4=paste("Q4",present,sep="_"))
    molars<-c("uRM3","uRM2","uRM1","uLM1","uLM2","uLM3","lRM3","lRM2","lRM1","lLM1","lLM2","lLM3","uRdM2","uRdM1","uLdM1","uLdM2","lRdM2","lRdM1","lLdM1","lLdM2")
    for(i in 1:length(fields$Teeth)){
      if(fields$Teeth[i] %in% molars){
        Q1<-NA;Q2<-NA;Q3<-NA;Q4<-NA
        if(Contains(input[[as.character(fields$wear[i])]],"Q1")){Q1<-input[[as.character(fields$Q1[i])]]}
        if(Contains(input[[as.character(fields$wear[i])]],"Q2")){Q2<-input[[as.character(fields$Q2[i])]]}
        if(Contains(input[[as.character(fields$wear[i])]],"Q3")){Q3<-input[[as.character(fields$Q3[i])]]}
        if(Contains(input[[as.character(fields$wear[i])]],"Q4")){Q4<-input[[as.character(fields$Q4[i])]]}
        out$x[i]<-paste(Q1,Q2,Q3,Q4,sep="/")
      }else{out$x[i]<-input[[as.character(fields$wear[i])]]}
    }
    out<-paste(out$x,collapse=":")
  }
  out
}
#####################################################################################################
Measurments<-function(present,input){
  if(length(present)==0){out<-NA
  }else{
    out<-data.frame("Teeth"=present,x=NA)
    fields<-data.frame("Teeth"=present,mmd=paste("mmd",present,sep="_"),cmd=paste("cmd",present,sep="_"),bl=paste("bl",present,sep="_"),ch=paste("ch",present,sep="_"))
    loose<-c(paste("u",input$u_loose,sep=""),paste("l",input$L_loose,sep=""),paste("u",input$ud_loose,sep=""),paste("l",input$Ld_loose,sep=""))
    for(i in 1:length(fields$Teeth)){
      if(fields$Teeth[i] %in% loose){out$x[i]<-paste(input[[as.character(fields$mmd[i])]],NA,input[[as.character(fields$bl[i])]],input[[as.character(fields$ch[i])]],sep="/")
      }else{out$x[i]<-paste(input[[as.character(fields$mmd[i])]],input[[as.character(fields$cmd[i])]],input[[as.character(fields$bl[i])]],input[[as.character(fields$ch[i])]],sep="/")}
    }
    out<-paste(out$x,collapse=":")
  }
  out
}
#####################################################################################################
Growth<-function(present,input){
  if(length(present)==0){
    out<-NA
  }else{
    teeth<-c("lRdC1","lLdC1","lRdM1","lRdM2","lLdM1","lLdM2","lRM3","lRM2","lRM1","lLM1","lLM2","lLM3")
    present2<-present[present %in% teeth]
    if(length(present2)==0){out<-NA
    }else{
      out<-data.frame("Teeth"=present2,x=NA)
      fields<-data.frame("Teeth"=present2,"formation"=paste("formation",present2,sep="_"))
      for(i in 1:length(fields$Teeth)){out$x[i]<-input[[as.character(fields$formation[i])]]}
      out<-paste(out$x,collapse=":")
    }}
  out<-paste(input$eruption,out,sep=":")
}
#####################################################################################################
Moderfication<-function(present,input){
  if(length(present)==0){out<-list(NA,NA)
  }else{
    out1<-data.frame("Teeth"=present,x=NA)
    out2<-data.frame("ID"=NA,"Tooth"=NA,"Type"=NA,"Description"=NA)
    fields<-data.frame("Teeth"=present,Mods=paste("Mods",present,sep="_"),File=paste("File",present,sep="_"),gfig30=paste("gfig30",present,sep="_"),nfig30=paste("nfig30",present,sep="_"),cFile1=paste("cFile1",present,sep="_"),cFile2=paste("cFile2",present,sep="_"),
                       inlay=paste("inlay",present,sep="_"),adhesive=paste("adhesive",present,sep="_"),restoration1=paste("restoration1",present,sep="_"),restoration2=paste("restoration2",present,sep="_"),
                       restoration3=paste("restoration3",present,sep="_"),Artifact1=paste("Artifact1",present,sep="_"),Artifact2=paste("Artifact2",present,sep="_"),Artifact3=paste("Artifact3",present,sep="_"),Artifact4=paste("Artifact4",present,sep="_"))
    n<-0
    
    for(i in 1:length(fields$Teeth)){
      if(length(input[[as.character(fields$Mods[i])]])!=0){
        Tooth<-fields$Teeth[i];IDs<-NULL
        
        if(Contains(input[[as.character(fields$Mods[i])]],"F")|Contains(input[[as.character(fields$Mods[i])]],"D")|Contains(input[[as.character(fields$Mods[i])]],"I")){
          n<-n+1
          ID<-paste(input$ID,n,sep="_");IDs<-c(IDs,ID)
          Type<-input[[as.character(fields$Mods[i])]];Type<-paste(Type[Type %in% c("F","I","D")],collapse="")
          if(input[[as.character(fields$File[i])]]=="fig30"){Description<-paste("B&U94",input[[as.character(fields$gfig30[i])]],input[[as.character(fields$nfig30[i])]],sep=",")
          }else{Description<-paste("Custom",input[[as.character(fields$cFile1[i])]],input[[as.character(fields$cFile2[i])]],sep=",")}
          if(Contains(input[[as.character(fields$Mods[i])]],"I")){Description<-paste(Description,input[[as.character(fields$inlay[i])]],input[[as.character(fields$adhesive[i])]],sep=",")}
          df<-data.frame("ID"=ID,"Tooth"=Tooth,"Type"=Type,"Description"=Description)
          out2<-rbind(out2,df)
        }
        if(Contains(input[[as.character(fields$Mods[i])]],"R")){
          n<-n+1
          ID<-paste(input$ID,n,sep="_");IDs<-c(IDs,ID)
          Type<-"R"
          Description<-paste(input[[as.character(fields$restoration1[i])]],input[[as.character(fields$restoration2[i])]],input[[as.character(fields$restoration3[i])]],sep=",")
          df<-data.frame(ID,Tooth,Type,Description)
          out2<-rbind(out2,df)
        }
        if(Contains(input[[as.character(fields$Mods[i])]],"A")){
          n<-n+1
          ID<-paste(input$ID,n,sep="_");IDs<-c(IDs,ID)
          Type<-"A"
          Description<-paste(input[[as.character(fields$Artifact1[i])]],input[[as.character(fields$Artifact2[i])]],input[[as.character(fields$Artifact3[i])]],input[[as.character(fields$Artifact4[i])]],sep=",")
          df<-data.frame(ID,Tooth,Type,Description)
          out2<-rbind(out2,df)
        }
        if(!(is.null(IDs))){out1$x[i]<-paste(IDs,collapse="/")}
        }}
    out1<-paste(out1$x,collapse=":")
    if(length(out2$ID)==1){out2<-NA}else{out2<-out2[-1,]}
    out<-list(out1,out2)
  }
  out
}