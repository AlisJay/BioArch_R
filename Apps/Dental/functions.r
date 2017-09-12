#Scores####
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
  Perminant$Occulsion[Perminant$Loose==1 | Perminant$Unerupted==1]<-0
  
  Perminant$Premortem[Perminant$Teeth %in% Premortem]<-1;Perminant$Postmortem[Perminant$Teeth %in% Postmortem]<-1
  Perminant$Premortem[Perminant$Postmortem==1]<-0
  
  
  library(BMS)
  if(sum(Perminant$Present)==0){
    PerminantScore<-paste(bin2hex(Perminant$Present),0,0,
                          0,bin2hex(Perminant$Premortem[Perminant$Present==0]),bin2hex(Perminant$Postmortem[Perminant$Present==0]),sep=":")
  }else{if(sum(Perminant$Present)==length(Perminant$Present)){
    PerminantScore<-paste(bin2hex(Perminant$Present),bin2hex(Perminant$Loose[Perminant$Present==1]),bin2hex(Perminant$Occulsion[Perminant$Present==1]),
                          bin2hex(Perminant$Unerupted[Perminant$Present==1]),0,0,sep=":")
  }else{
    PerminantScore<-paste(bin2hex(Perminant$Present),bin2hex(Perminant$Loose[Perminant$Present==1]),bin2hex(Perminant$Occulsion[Perminant$Present==1]),
                          bin2hex(Perminant$Unerupted[Perminant$Present==1]),bin2hex(Perminant$Premortem[Perminant$Present==0]),bin2hex(Perminant$Postmortem[Perminant$Present==0]),sep=":")
  }}
  
  
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
  Decidous[Decidous$Present==0,c("Loose","Occulsion","Unerupted")]<-0
  Decidous$Occulsion[Decidous$Loose==1 | Decidous$Unerupted==1]<-0
  
  Decidous$Premortem[Decidous$Teeth %in% Premortem]<-1;Decidous$Postmortem[Decidous$Teeth %in% Postmortem]<-1
  Decidous[Decidous$Present==1,c("Premortem","Postmortem")]<-0
  Decidous$Postmortem[Decidous$Premortem==1]<-0
  
  if(sum(Decidous$Present)==0){
    DecidousScore<-paste(bin2hex(Decidous$Present),0,0,
                         0,bin2hex(Decidous$Premortem[Decidous$Present==0]),bin2hex(Decidous$Postmortem[Decidous$Present==0]),sep=":")
  }else{if(sum(Decidous$Present)==length(Decidous$Present)){
    DecidousScore<-paste(bin2hex(Decidous$Present),bin2hex(Decidous$Loose[Decidous$Present==1]),bin2hex(Decidous$Occulsion[Decidous$Present==1]),
                         bin2hex(Decidous$Unerupted[Decidous$Present==1]),0,0,sep=":")
  }else{
    DecidousScore<-paste(bin2hex(Decidous$Present),bin2hex(Decidous$Loose[Decidous$Present==1]),bin2hex(Decidous$Occulsion[Decidous$Present==1]),
                         bin2hex(Decidous$Unerupted[Decidous$Present==1]),bin2hex(Decidous$Premortem[Decidous$Present==0]),bin2hex(Decidous$Postmortem[Decidous$Present==0]),sep=":")
  }}
 
  list("Pscore"=PerminantScore,"Dscore"=DecidousScore)
}
#Field creation#############################################################
#Present
Present<-function(input){
  present_p<-c(paste("u",input$u_present,sep=""),paste("l",input$L_present,sep=""))
  present_d<-c(paste("u",input$ud_present,sep=""),paste("l",input$Ld_present,sep=""))
  remove<-c("l","u")
  present_p<-present_p[!(present_p %in% remove)]
  present_d<-present_d[!(present_d %in% remove)]
  list(present_p,present_d)
}

Contains<-function(x,y){#returns true/false if x contains the value y
  sum(x==y)>0
}

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
#Create UI#################
CreateDentalUI<-function(input,tab){
  out<-NA
  MaxPerm<-input$u_present
  ManPerm<-input$L_present
  MaxDec<-input$ud_present
  ManDec<-input$Ld_present
  if(tab=="CC"){#Caries and calculus
    Car<-function(Teeth,tList,ul){
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        tList<-tagList(tList,checkboxGroupInput(inputId=paste0("Caries",ul,Teeth[i]),label=Teeth[i],choices=c("Occulusal"="O","Interproximal"="I","Lingual"="L","Buccal"="B","Cevical"="C","Root"="R","Too large to assign"="XL","Noncarious pulp exposure"="NC")))}}
      tList}
    MaxPermCar<-Car(MaxPerm,tagList(h3("Caries"),h4("Maxillary")),"_u")
    ManPermCar<-Car(ManPerm,tagList(h3(br()),h4("Mandibular")),"_l")
    MaxDecCar<-Car(MaxDec,tagList(h3(br()),h4("Decidous"),strong("Maxillary")),"_u")
    ManDecCar<-Car(ManDec,strong("Mandibular"),"_l")
    
    Cal<-function(Teeth,tList,ul){
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        tList<-tagList(tList,selectInput(inputId=paste0("Calculus",ul,Teeth[i]),label=Teeth[i],c("None"=NA,"Small"=1,"Moderate"=2,"Large"=3)))
        tList<-tagList(tList,
                       conditionalPanel(condition=paste0("input.Calculus",ul,Teeth[i]," > 0"),
                                        checkboxGroupInput(paste0("Cal",ul,Teeth[i],"_side"),"Location",c("Buccal"="B","Lingual"="L"))))}}
      tList}
    MaxPermCal<-Cal(MaxPerm,tagList(h3("Calculus"),h4("Maxillary")),"_u")
    ManPermCal<-Cal(ManPerm,tagList(h3(br()),h4("Mandibular")),"_l")
    MaxDecCal<-Cal(MaxDec,tagList(h3(br()),h4("Decidous"),strong("Maxillary")),"_u")
    ManDecCal<-Cal(ManDec,strong("Mandibular"),"_l")
    
    out<-tagList(column(width=2,MaxPermCar,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,ManPermCar,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,MaxDecCar,ManDecCar,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,MaxPermCal,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,ManPermCal,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,MaxDecCal,ManDecCal,h1(br()),h1(br()),h1(br()),h1(br())))}
  if(tab=="ED"){
    Hypo<-function(Teeth,tList,ul){
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        tList<-tagList(tList,checkboxGroupInput(paste0("Hypo",ul,Teeth[i]),Teeth[i],c("Linear horizontal grooves"="lhg","Linear vertical grooves"="lvg","Linear horizontal pits"="lhp","Nonlinear arrays of pits"="nap","Single pits"="sp")))
        tList<-tagList(tList,
                         conditionalPanel(condition=paste0("input.Hypo",ul,Teeth[i],".indexOf('lhg') >=0"),
                                          textInput(paste0("Hypo_lhg",ul,Teeth[i]),"Location of linear horizontal groove(s)")),
                         conditionalPanel(condition=paste0("input.Hypo",ul,Teeth[i],".indexOf('lvg') >=0"),
                                          textInput(paste0("Hypo_lvg",ul,Teeth[i]),"Location of linear vertical groove(s)")),
                         conditionalPanel(condition=paste0("input.Hypo",ul,Teeth[i],".indexOf('lhp') >=0"),
                                          textInput(paste0("Hypo_lhp",ul,Teeth[i]),"Location of linear horizontal pits(s)")),
                         conditionalPanel(condition=paste0("input.Hypo",ul,Teeth[i],".indexOf('nap') >=0"),
                                          textInput(paste0("Hypo_nap",ul,Teeth[i]),"Location of pit arrays(s)")),
                         conditionalPanel(condition=paste0("input.Hypo",ul,Teeth[i],".indexOf('sp') >=0"),
                                          textInput(paste0("Hypo_sp",ul,Teeth[i]),"Location of single pit(s)")))}}
      tList}
    MaxPermHypo<-Hypo(MaxPerm,tagList(h3("Hypoplasia"),h4("Maxillary")),"_u")
    ManPermHypo<-Hypo(ManPerm,tagList(h3(br()),h4("Mandibular")),"_l")
    MaxDecHypo<-Hypo(MaxDec,tagList(h3(br()),h4("Decidous"),strong("Maxillary")),"_u")
    ManDecHypo<-Hypo(ManDec,strong("Mandibular"),"_l")
    
    Hyper<-function(Teeth,tList,ul){
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        tList<-tagList(tList,strong(Teeth[i]),checkboxGroupInput(paste0("Hyper_dis",ul,Teeth[i]),"Discrete boundary opacity",c("Yellow"="Y","Cream/white"="C","Orange"="O","Brown"="B")))
        tList<-tagList(tList,
                       conditionalPanel(condition=paste0("input.Hyper_dis",ul,Teeth[i],".indexOf('Y') >=0"),
                                        textInput(paste0("Hyper_ydis",ul,Teeth[i]),"Location of yellow hypercalcification")),
                       conditionalPanel(condition=paste0("input.Hyper_dis",ul,Teeth[i],".indexOf('C') >=0"),
                                        textInput(paste0("Hyper_cdis",ul,Teeth[i]),"Location of cream hypercalcification")),
                       conditionalPanel(condition=paste0("input.Hyper_dis",ul,Teeth[i],".indexOf('O') >=0"),
                                        textInput(paste0("Hyper_odis",ul,Teeth[i]),"Location of orange hypercalcification")),
                       conditionalPanel(condition=paste0("input.Hyper_dis",ul,Teeth[i],".indexOf('B') >=0"),
                                        textInput(paste0("Hyper_bdis",ul,Teeth[i]),"Location of Brown hypercalcification")))
        tList<-tagList(tList,checkboxGroupInput(paste0("Hyper_dif",ul,Teeth[i]),"Diffuse boundary opacity",c("Yellow"="Y","Cream/white"="C","Orange"="O","Brown"="B")))
        tList<-tagList(tList,
                       conditionalPanel(condition=paste0("input.Hyper_dif",ul,Teeth[i],".indexOf('Y') >=0"),
                                        textInput(paste0("Hyper_ydif",ul,Teeth[i]),"Location of yellow hypercalcification")),
                       conditionalPanel(condition=paste0("input.Hyper_dif",ul,Teeth[i],".indexOf('C') >=0"),
                                        textInput(paste0("Hyper_cdif",ul,Teeth[i]),"Location of cream hypercalcification")),
                       conditionalPanel(condition=paste0("input.Hyper_dif",ul,Teeth[i],".indexOf('O') >=0"),
                                        textInput(paste0("Hyper_odif",ul,Teeth[i]),"Location of orange hypercalcification")),
                       conditionalPanel(condition=paste0("input.Hyper_dif",ul,Teeth[i],".indexOf('B') >=0"),
                                        textInput(paste0("Hyper_bdif",ul,Teeth[i]),"Location of Brown hypercalcification")))}}
      tList}
    MaxPermHyper<-Hyper(MaxPerm,tagList(h3("Hypercalcification"),h4("Maxillary")),"_u")
    ManPermHyper<-Hyper(ManPerm,tagList(h3(br()),h4("Mandibular")),"_l")
    MaxDecHyper<-Hyper(MaxDec,tagList(h3(br()),h4("Decidous"),strong("Maxillary")),"_u")
    ManDecHyper<-Hyper(ManDec,strong("Mandibular"),"_u")
    
    out<-tagList(column(width=2,MaxPermHypo,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,ManPermHypo,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,MaxDecHypo,ManDecHypo,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,MaxPermHyper,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,ManPermHyper,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=2,MaxDecHyper,ManDecHyper,h1(br()),h1(br()),h1(br()),h1(br())))
  }
  if(tab=="PM"){
    Ab<-function(Teeth,tList,ul){
      if(length(Teeth)>0){for(i in 1:length(Teeth)){tList<-tagList(tList,checkboxInput(paste0("Ablation",ul,Teeth[i]),Teeth[i]))}}
      tList}
    AbMaxPerm<-Ab(input$u_premortem,tagList(h3("Ablation"),h4("Maxillary")),"_u")
    AbManPerm<-Ab(input$L_premortem,h4("Mandibular"),"_l")
    AbMaxDec<-Ab(input$ud_premortem,tagList(h4("Decidous"),strong("Maxillary")),"_u")
    AbManDec<-Ab(input$Ld_premortem,strong("Mandibular"),"_l")
    
    PMod<-function(Teeth,tList,ul){
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        tList<-tagList(tList,checkboxGroupInput(paste0("Mods",ul,Teeth[i]),Teeth[i],c("Filing"="F","Drilling with inlays"="I","Drilling without inlays"="D","Dental restoration"="R","Wear associated with artifact use"="A")))
        tList<-tagList(tList,
                       conditionalPanel(condition=paste0("input.Mods",ul,Teeth[i],".indexOf('F') >=0||input.Mods",ul,Teeth[i],".indexOf('I') >=0||input.Mods",ul,Teeth[i],".indexOf('D') >=0"),
                                        selectInput(paste0("File",ul,Teeth[i]),"Classification",c("Figure 30a B&U,94"="fig30","Other")),
                                        conditionalPanel(condition=paste0("input.File",ul,Teeth[i]," == 'fig30'"),
                                                         selectInput(paste0("gfig30",ul,Teeth[i]),"Group",c("I","II","III","IV","V","VI","VII")),
                                                         selectInput(paste0("nfig30",ul,Teeth[i]),"Number",c(1,2,3,4,5,6,7,8,9,10))),
                                        conditionalPanel(condition=paste0("input.File",ul,Teeth[i]," == 'Other'"),
                                                         checkboxGroupInput(paste0("cFile1",ul,Teeth[i]),"Surface",c("Edge/Occulusal"="O","Interproximal"="I","outer/Buccal"="B")),
                                                         textInput(paste0("cFile2",ul,Teeth[i]),"Description"))),
                       conditionalPanel(condition=paste0("input.Mods",ul,Teeth[i],".indexOf('I') >=0"),
                                        textInput(paste0("inlay",ul,Teeth[i]),"Inlay description"),
                                        textInput(paste0("adhesive",ul,Teeth[i]),"Adhesive description")),
                       conditionalPanel(condition=paste0("input.Mods",ul,Teeth[i],".indexOf('R') >=0"),
                                        h4("Dental restoration"),
                                        textInput(paste0("restoration1",ul,Teeth[i]),"Material"),
                                        textInput(paste0("restoration2",ul,Teeth[i]),"Location and spread"),
                                        textInput(paste0("restoration3",ul,Teeth[i]),"Further description")),
                       conditionalPanel(condition=paste0("input.Mods",ul,Teeth[i],".indexOf('A') >=0"),
                                        h4("Artifact use"),
                                        checkboxGroupInput(paste0("Artifact1",ul,Teeth[i]),"Surface",c("Occulusal"="O","Interproximal"="I","Lingual"="L","Buccal"="B")),
                                        textInput(paste0("Artifact2",ul,Teeth[i]),"Foreign material inclusions",value="None"),
                                        textInput(paste0("Artifact3",ul,Teeth[i]),"Description"),
                                        checkboxGroupInput(paste0("Artifact4",ul,Teeth[i]),"Evidence of--in surrounding tissue",c("Carious lesions","Perdontal disease"))))}}
      tList}
    MaxPermPM<-PMod(MaxPerm,h3("Maxillary"),"_u")
    ManPermPM<-PMod(ManPerm,h3("Mandibular"),"_l")
    MaxDecPM<-PMod(MaxDec,tagList(h3("Decidous"),h4("Maxillary")),"_u")
    ManDecPM<-PMod(ManDec,h4("Mandibular"),"_l")
    
    out<-tagList(column(width=1,AbMaxPerm,AbManPerm,AbMaxDec,AbManDec),
                column(width=4,MaxPermPM,h1(br()),h1(br()),h1(br()),h1(br())),
                column(width=4,ManPermPM,h1(br()),h1(br()),h1(br()),h1(br())),
                column(width=3,MaxDecPM,ManDecPM,h1(br()),h1(br()),h1(br()),h1(br())))
  }
  if(tab=="MW"){
    MWear<-function(Teeth,tList,ul){
      Teeth<-grep("M",Teeth,value=TRUE)
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        tList<-tagList(tList,checkboxGroupInput(paste0("wear",ul,Teeth[i]),Teeth[i],c("Quadrant1"="Q1","Quadrant2"="Q2","Quadrant3"="Q3","Quadrant4"="Q4")))
        tList<-tagList(tList,
                       conditionalPanel(condition=paste0("input.wear",ul,Teeth[i],".indexOf('Q1') >=0"),
                                        selectInput(paste0("Q1",ul,Teeth[i]),"Quadrant1",c("None"=NA,"Invisble or very small facets"=1,"Facets visible but cusps and surface features very evident"=2,"Some rounding/obliteration of cusps,but not flat"=3,"Quadrant flat but limited detine exposure"=4,"Flat quadrant 1/4 or less dentine exposure"=5,">1/4 dentine exposure,ring of enamel on three sides"=6,"Enamel only on 2 sides of quadrant"=7,"Enamel only on 1 sides of quadrant,thick/medium"=8,"Thin enamel only on 1 sides of quadrant"=9,"No enamel complete dentine exposure"=10))),
                       conditionalPanel(condition=paste0("input.wear",ul,Teeth[i],".indexOf('Q2') >=0"),
                                        selectInput(paste0("Q2",ul,Teeth[i]),"Quadrant2",c("None"=NA,"Invisble or very small facets"=1,"Facets visible but cusps and surface features very evident"=2,"Some rounding/obliteration of cusps,but not flat"=3,"Quadrant flat but limited detine exposure"=4,"Flat quadrant 1/4 or less dentine exposure"=5,">1/4 dentine exposure,ring of enamel on three sides"=6,"Enamel only on 2 sides of quadrant"=7,"Enamel only on 1 sides of quadrant,thick/medium"=8,"Thin enamel only on 1 sides of quadrant"=9,"No enamel complete dentine exposure"=10))),
                       conditionalPanel(condition=paste0("input.wear",ul,Teeth[i],".indexOf('Q3') >=0"),
                                        selectInput(paste0("Q3",ul,Teeth[i]),"Quadrant3",c("None"=NA,"Invisble or very small facets"=1,"Facets visible but cusps and surface features very evident"=2,"Some rounding/obliteration of cusps,but not flat"=3,"Quadrant flat but limited detine exposure"=4,"Flat quadrant 1/4 or less dentine exposure"=5,">1/4 dentine exposure,ring of enamel on three sides"=6,"Enamel only on 2 sides of quadrant"=7,"Enamel only on 1 sides of quadrant,thick/medium"=8,"Thin enamel only on 1 sides of quadrant"=9,"No enamel complete dentine exposure"=10))),
                       conditionalPanel(condition=paste0("input.wear",ul,Teeth[i],".indexOf('Q4') >=0"),
                                        selectInput(paste0("Q4",ul,Teeth[i]),"Quadrant4",c("None"=NA,"Invisble or very small facets"=1,"Facets visible but cusps and surface features very evident"=2,"Some rounding/obliteration of cusps,but not flat"=3,"Quadrant flat but limited detine exposure"=4,"Flat quadrant 1/4 or less dentine exposure"=5,">1/4 dentine exposure,ring of enamel on three sides"=6,"Enamel only on 2 sides of quadrant"=7,"Enamel only on 1 sides of quadrant,thick/medium"=8,"Thin enamel only on 1 sides of quadrant"=9,"No enamel complete dentine exposure"=10))))
      }}
      tList}
    MaxPermMW<-MWear(MaxPerm,h3("Maxillary"),"_u")
    ManPermMW<-MWear(ManPerm,h3("Mandibular"),"_l")
    MaxDecMW<-MWear(MaxDec,tagList(h3("Decidous"),h4("Maxillary")),"_u")
    ManDecMW<-MWear(ManDec,tagList(h3(br()),h4("Mandibular")),"_l")
    
    out<-tagList(column(width=3,MaxPermMW,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,ManPermMW,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,MaxDecMW,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,ManDecMW,h1(br()),h1(br()),h1(br())),h1(br()))
    
  }
  if(tab=="OW"){
    OWear<-function(Teeth,tList,ul){
      Teeth<-grep("M",Teeth,value=TRUE,invert=TRUE)
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        if(grepl("P",Teeth[i])){tList<-tagList(tList,selectInput(paste0("wear",ul,Teeth[i]),Teeth[i],c("None"=NA,"Unworn to polished or small facets"=1,"Moderate cusp removal"=2,"Full cusp removal and/or dentine patches"=3,"at least 1 large dentine exposure"=4,"Two large dentine area,only slight coalescence"=5,"Dentinal areas coalesced,enamel rim complete"=6,"Loss of enamel on atleast 1 side of rim"=7,"Severe loss of crown height"=8)))
        }else{tList<-tagList(tList,selectInput(paste0("wear",ul,Teeth[i]),Teeth[i],c("None"=NA,"Unworn to polished or small facets"=1,"Point or hairline dentine exposure"=2,"Dentine line of distinct thickness"=3,"Moderate dentine exposure no longer linear"=4,"large dentine area,enamel rim complete"=5,"Loss of enamel rim on one side or very thin"=6,"Enamel rim lost on 2 sides or samll remanants"=7,"Complete crown loss, no enamel"=8)))}
      }}
      tList}
    MaxPermOW<-OWear(MaxPerm,h3("Maxillary"),"_u")
    ManPermOW<-OWear(ManPerm,h3("Mandibular"),"_l")
    MaxDecOW<-OWear(MaxDec,tagList(h3("Decidous"),h4("Maxillary")),"_u")
    ManDecOW<-OWear(ManDec,tagList(h3(br()),h4("Mandibular")),"_l")
    out<-tagList(column(width=3,MaxPermOW,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,ManPermOW,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,MaxDecOW,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,ManDecOW,h1(br()),h1(br()),h1(br()),h1(br())))
  }
  if(tab=="M"){
    Measure<-function(Teeth,tList,ul,loose){
      if(length(Teeth)>0){for(i in 1:length(Teeth)){
        if(Contains(loose,Teeth[i])){
          tList<-tagList(tList,h4(Teeth[i]),
                         numericInput(paste0("mmd",ul,Teeth[i]),"Maximum mesiodistal diameter",value=NA),
                         numericInput(paste0("bl",ul,Teeth[i]),"Maximum buccolingual diameter",value=NA),
                         numericInput(paste0("ch",ul,Teeth[i]),"Crown Height",value=NA))
        }else{tList<-tagList(tList,h4(Teeth[i]),
                             numericInput(paste0("mmd",ul,Teeth[i]),"Maximum mesiodistal diameter",value=NA),
                             numericInput(paste0("cmd",ul,Teeth[i]),"Mesiodistal diameter between interproximal points",value=NA),
                             numericInput(paste0("bl",ul,Teeth[i]),"Maximum buccolingual diameter",value=NA),
                             numericInput(paste0("ch",ul,Teeth[i]),"Crown Height",value=NA))}
      }}
      tList}
    MaxPermMeas<-Measure(MaxPerm,h3("Maxillary"),"_u",input$u_loose)
    ManPermMeas<-Measure(ManPerm,h3("Mandibular"),"_l",input$L_loose)
    MaxDecMeas<-Measure(MaxDec,tagList(h3("Decidous"),h4("Maxillary")),"_u",input$ud_loose)
    ManDecMeas<-Measure(ManDec,tagList(h3(br()),h4("Mandibular")),"_l",input$Ld_loose)
    out<-tagList(column(width=3,MaxPermMeas,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,ManPermMeas,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,MaxDecMeas,h1(br()),h1(br()),h1(br()),h1(br())),
                 column(width=3,ManDecMeas,h1(br()),h1(br()),h1(br()),h1(br())))
  }
  out
}