library("igraph")
############################################################################################
Map<-function(Invent,Individual,region,PC="Present",margin=c(0,0,0,0),label="Default",colour="lightgreen"){
  
  files<-data.frame(region=c("Skull","Vertebrae","Thorax","Shoulder","Pelvis","Arm","Leg","Hand","Foot"),
                    AJM=c("igraphskeleton/SkullAJM.txt","igraphskeleton/VertAJM.txt","igraphskeleton/ThoraxAJM.txt","igraphskeleton/ShoulderAJM.txt","igraphskeleton/PelvisAJM.txt",
                          "igraphskeleton/ArmAJM.txt","igraphskeleton/LegAJM.txt","igraphskeleton/HandAJM.txt","igraphskeleton/FootAJM.txt"),
                    coords=c("igraphskeleton/skullcoords.txt","igraphskeleton/vertcoords.txt","igraphskeleton/thoraxcoords.txt","igraphskeleton/shouldercoords.txt","igraphskeleton/pelviscoords.txt",
                             "igraphskeleton/armcoords.txt","igraphskeleton/legcoords.txt","igraphskeleton/handcoords.txt","igraphskeleton/footcoords.txt"))
  
  if(!(region %in% files$region)){Stop("invalid region, valid regions are Skull,Vertebrae,Thorax,Shoulder,Pelvis,Arm,Leg,Hand,Foot")
  }else{coords<-as.character(files$coords[files$region==region]);AJM<-as.character(files$AJM[region==files$region])}
  
  library(igraph)
  AJM<-read.table(AJM,header=TRUE)
  coords<-read.table(coords,header=TRUE)
  ig<-graph_from_adjacency_matrix(as.matrix(AJM),mode="undirected")
  
 
  data<-Invent[[region]][[PC]][,Individual]
  data<-data[1:length(AJM[1,])]
  V(ig)$color<-data
  if(label=="Default"){label<-paste(Individual,region,PC,sep="_")}
  plot(ig,layout=as.matrix(coords),main=label,palette=c(colour,"white"),margin=margin,vertex.size=10)
}
#######################################################################################################################
Map2<-function(Invent1,Individual,region,PC=c("Present","Complete"),Invent2=NA,margin=c(0,0,0,0),label1="Default",label2="Default",colour="lightgreen",layout=c(1,2) ){
  
  
  files<-data.frame(region=c("Skull","Vertebrae","Thorax","Shoulder","Pelvis","Arm","Leg","Hand","Foot"),
                    AJM=c("igraphskeleton/SkullAJM.txt","igraphskeleton/VertAJM.txt","igraphskeleton/ThoraxAJM.txt","igraphskeleton/ShoulderAJM.txt","igraphskeleton/PelvisAJM.txt",
                          "igraphskeleton/ArmAJM.txt","igraphskeleton/LegAJM.txt","igraphskeleton/HandAJM.txt","igraphskeleton/FootAJM.txt"),
                    coords=c("igraphskeleton/skullcoords.txt","igraphskeleton/vertcoords.txt","igraphskeleton/thoraxcoords.txt","igraphskeleton/shouldercoords.txt","igraphskeleton/pelviscoords.txt",
                             "igraphskeleton/armcoords.txt","igraphskeleton/legcoords.txt","igraphskeleton/handcoords.txt","igraphskeleton/footcoords.txt"))
  
  if(!(region %in% files$region)){Stop("invalid region, valid regions are Skull,Vertebrae,Thorax,Shoulder,Pelvis,Arm,Leg,Hand,Foot")
  }else{coords<-as.character(files$coords[files$region==region]);AJM<-as.character(files$AJM[region==files$region])}
  
  library(igraph)
  AJM<-read.table(AJM,header=TRUE)
  coords<-read.table(coords,header=TRUE)
  ig<-graph_from_adjacency_matrix(as.matrix(AJM),mode="undirected")
  
  par(mfrow=layout)
  Individual1<-Invent1[[region]][[PC[1]]][,Individual[1]]
  Individual1<-Individual1[1:length(AJM[1,])]
  V(ig)$color<-Individual1
  if(label1=="Default"){label1<-paste(Individual[1],PC[1],sep="_")}
  plot(ig,layout=as.matrix(coords),main=label1,palette=c(colour,"white"),margin=margin,vertex.size=10)
  if(is.na(Invent2)){Individual2<-Invent1[[region]][[PC[2]]][,Individual[2]]
  }else{Individual2<-Invent2[[region]][[PC[2]]][,Individual[2]]}
  Individual2<-Individual2[1:length(AJM[1,])]
  V(ig)$color<-Individual2
  if(label2=="Default"){label2<-paste(Individual[2],PC[2],sep="_")}
  plot(ig,layout=as.matrix(coords),main=label2,palette=c(colour,"white"),margin=margin,vertex.size=10)
}
####################################################################################################################
MapPercent<-function(Invent,region,PC="Present",margin=c(0,0,0,0),label="iPlot",colours=c("turquoise","lightgreen","lightgoldenrod1","orange","coral") ){
  if(PC=="Present"){percents<-GetPercents(Inventory2)[[paste(region,"P",sep="_")]]
  }else{percents<-GetPercents(Inventory2)[[paste(region,"C",sep="_")]]}
  scores<-data.frame(percent=percents,score=0)
  scores$score[scores$percent>0]<-1;scores$score[scores$percent>0.25]<-2
  scores$score[scores$percent>0.50]<-3;scores$score[scores$percent>0.75]<-4
  scores$score[scores$percent==1]<-5
  
  files<-data.frame(region=c("Skull","Vert","Thorax","Shoulder","Pelvis","Arm","Leg","Hand","Foot","ULimb","LLimb","Axial"),
                    AJM=c("igraphskeleton/SkullAJM.txt","igraphskeleton/VertAJM.txt","igraphskeleton/ThoraxAJM.txt","igraphskeleton/ShoulderAJM.txt","igraphskeleton/PelvisAJM.txt",
                          "igraphskeleton/ArmAJM.txt","igraphskeleton/LegAJM.txt","igraphskeleton/HandAJM.txt","igraphskeleton/FootAJM.txt",
                          "igraphskeleton/ULimbAJM.txt","igraphskeleton/LLimbAJM.txt","igraphskeleton/AxialAJM.txt"),
                    coords=c("igraphskeleton/skullcoords.txt","igraphskeleton/vertcoords.txt","igraphskeleton/thoraxcoords.txt","igraphskeleton/shouldercoords.txt","igraphskeleton/pelviscoords.txt",
                             "igraphskeleton/armcoords.txt","igraphskeleton/legcoords.txt","igraphskeleton/handcoords.txt","igraphskeleton/footcoords.txt",
                             "igraphskeleton/ulimbcoords.txt","igraphskeleton/llimbcoords.txt","igraphskeleton/axialcoords.txt"))
  
  if(!(region %in% files$region)){Stop("invalid region, valid regions are Skull,Vert,Thorax,Shoulder,Pelvis,Arm,Leg,Hand,Foot,ULimb,LLimb,Axial")
  }else{coords<-as.character(files$coords[files$region==region]);AJM<-as.character(files$AJM[region==files$region])}
  
  library(igraph)
  AJM<-read.table(AJM,header=TRUE)
  coords<-read.table(coords,header=TRUE)
  ig<-graph_from_adjacency_matrix(as.matrix(AJM),mode="undirected")
  V(ig)$color<-scores$score
  
  par(mfrow=c(1,1))
  plot(ig,layout=as.matrix(coords),main=label,palette=colours,margin=margin,vertex.size=10)
  colours<-c("white",colours)
  legend(locator(1), legend=c("0%","0-25%","25-50%","50-75%","75-100%","100%"),fill=colours)
}

####################################################################################################################
Map2Percent<-function(Invent,region,PC=c("Present","Complete"),Invent2=NA,margin=c(0,0,0,0),label1="Pop1",label2="Pop2",colours=c("turquoise","lightgreen","lightgoldenrod1","orange","coral"),layout=c(1,3) ){
  
  if(PC[1]=="Present"){percents1<-GetPercents(Invent)[[paste(region,"P",sep="_")]]
  }else{percents1<-GetPercents(Invent)[[paste(region,"C",sep="_")]]}
  
  if(PC[2]=="Present"){
    if(is.na(Invent2)){percents2<-GetPercents(Invent)[[paste(region,"P",sep="_")]]
    }else{percents2<-GetPercents(Invent2)[[paste(region,"P",sep="_")]]}
  }else{
    if(is.na(Invent2)){percents2<-GetPercents(Invent)[[paste(region,"C",sep="_")]]
    }else{percents2<-GetPercents(Invent2)[[paste(region,"C",sep="_")]]}
  }
  scores<-data.frame(percent1=percents1,percent2=percents2,score1=0,score2=0)
  
  scores$score1[scores$percent1>0]<-1;scores$score1[scores$percent1>0.25]<-2
  scores$score1[scores$percent1>0.50]<-3;scores$score1[scores$percent1>0.75]<-4
  scores$score1[scores$percent1==1]<-5
  
  scores$score2[scores$percent2>0]<-1;scores$score2[scores$percent2>0.25]<-2
  scores$score2[scores$percent2>0.50]<-3;scores$score2[scores$percent2>0.75]<-4
  scores$score2[scores$percent2==1]<-5
  
  files<-data.frame(region=c("Skull","Vert","Thorax","Shoulder","Pelvis","Arm","Leg","Hand","Foot","ULimb","LLimb","Axial"),
                    AJM=c("igraphskeleton/SkullAJM.txt","igraphskeleton/VertAJM.txt","igraphskeleton/ThoraxAJM.txt","igraphskeleton/ShoulderAJM.txt","igraphskeleton/PelvisAJM.txt",
                          "igraphskeleton/ArmAJM.txt","igraphskeleton/LegAJM.txt","igraphskeleton/HandAJM.txt","igraphskeleton/FootAJM.txt",
                          "igraphskeleton/ULimbAJM.txt","igraphskeleton/LLimbAJM.txt","igraphskeleton/AxialAJM.txt"),
                    coords=c("igraphskeleton/skullcoords.txt","igraphskeleton/vertcoords.txt","igraphskeleton/thoraxcoords.txt","igraphskeleton/shouldercoords.txt","igraphskeleton/pelviscoords.txt",
                             "igraphskeleton/armcoords.txt","igraphskeleton/legcoords.txt","igraphskeleton/handcoords.txt","igraphskeleton/footcoords.txt",
                             "igraphskeleton/ulimbcoords.txt","igraphskeleton/llimbcoords.txt","igraphskeleton/axialcoords.txt"))
  
  if(!(region %in% files$region)){Stop("invalid region, valid regions are Skull,Vert,Thorax,Shoulder,Pelvis,Arm,Leg,Hand,Foot,ULimb,LLimb,Axial")
  }else{coords<-as.character(files$coords[files$region==region]);AJM<-as.character(files$AJM[region==files$region])}
  
  library(igraph)
  AJM<-read.table(AJM,header=TRUE)
  coords<-read.table(coords,header=TRUE)
  ig<-graph_from_adjacency_matrix(as.matrix(AJM),mode="undirected")
  par(mfrow=layout)
  V(ig)$color<-scores$score1
  plot(ig,layout=as.matrix(coords),palette=colours,margin=margin,vertex.size=10)
  title(label1, line = -4)
  V(ig)$color<-scores$score2
  plot(ig,layout=as.matrix(coords),palette=colours,margin=margin,vertex.size=10)
  title(label2, line = -4)
  plot(1, type="n", axes=F, xlab="", ylab="")
  colours<-c("white",colours)
  legend("left", legend=c("0%",">0-25%",">25-50%",">50-75%",">75-<100%","100%"),fill=colours)
}
####################################################################################################################

GetPercents<-function(decoded){
  Skull_P<-decoded$Skull$Present$percent
  Skull_C<-decoded$Skull$Complete$percent
  
  Vert_P<-decoded$Vertebrae$Present$percent;Vert_P<-Vert_P[1:(length(Vert_P)-3)]
  Vert_C<-decoded$Vertebrae$Complete$percent;Vert_C<-Vert_C[1:(length(Vert_C)-3)]
  
  Thorax_P<-decoded$Thorax$Present$percent;Thorax_P<-Thorax_P[1:(length(Thorax_P)-2)]
  Thorax_C<-decoded$Thorax$Complete$percent;Thorax_C<-Thorax_C[1:(length(Thorax_C)-2)]
  
  Shoulder_P<-decoded$Shoulder$Present$percent
  Shoulder_C<-decoded$Shoulder$Complete$percent
  
  Pelvis_P<-decoded$Pelvis$Present$percent
  Pelvis_C<-decoded$Pelvis$Complete$percent
  
  Arm_P<-decoded$Arm$Present$percent
  Arm_C<-decoded$Arm$Complete$percent
  
  Hand_P<-decoded$Hand$Present$percent;Hand_P<-Hand_P[1:(length(Hand_P)-4)]
  Hand_C<-decoded$Hand$Complete$percent;Hand_C<-Hand_C[1:(length(Hand_C)-4)]
  
  Leg_P<-decoded$Leg$Present$percent
  Leg_C<-decoded$Leg$Complete$percent
  
  Foot_P<-decoded$Foot$Present$percent;Foot_P<-Foot_P[1:(length(Foot_P)-4)]
  Foot_C<-decoded$Foot$Complete$percent;Foot_C<-Foot_C[1:(length(Foot_C)-4)]
  
  LLimb_P<-c(Pelvis_P,Leg_P,Foot_P)
  LLimb_C<-c(Pelvis_C,Leg_C,Foot_C)
  
  ULimb_P<-c(Shoulder_P,Arm_P,Hand_P)
  ULimb_C<-c(Shoulder_C,Arm_C,Hand_C)
  
  Axial_P<-c(Shoulder_P,Thorax_P,Vert_P,Pelvis_P)
  Axial_C<-c(Shoulder_C,Thorax_C,Vert_C,Pelvis_C)
  
  list(Skull_P=Skull_P,Skull_C=Skull_C,Vert_P=Vert_P,Vert_C=Vert_C,
       Thorax_P=Thorax_P,Thorax_C=Thorax_C,Shoulder_P=Shoulder_P,Shoulder_C=Shoulder_C,
       Pelvis_P=Pelvis_P,Pelvis_C=Pelvis_C,Arm_P=Arm_P,Arm_C=Arm_C,
       Hand_P=Hand_P,Hand_C=Hand_C,Leg_P=Leg_P,Leg_C=Leg_C,Foot_P=Foot_P,Foot_C=Foot_C,
       Axial_P=Axial_P,Axial_C=Axial_C,LLimb_P=LLimb_P,LLimb_C=LLimb_C,ULimb_P=ULimb_P,ULimb_C=ULimb_C)
}
####################################################################################################################
MapDen<-function(Den,ID,Type="Inventory",label="Default",margin=c(0,0,0,0),colours=c("turquoise","lightgreen","lightblue","cornflowerblue","lightgoldenrod1","orange","coral")){
  library(igraph)
  AJM<-read.table("IgraphSkeleton/teethAJM.txt",header=TRUE)
  coords<-read.table("IgraphSkeleton/teethcoords.txt",header=TRUE)
  ig<-graph_from_adjacency_matrix(as.matrix(AJM),mode="undirected")
  if(Type=="Inventory"){
    data<-Den[[ID]][["Score"]]
    data$colour<-0
    for(i in 1:52){
      if(data$Present[i]==1){
        if(data$Occulsion[i]==1){data$colour[i]<-1}
        if(data$Loose[i]==1){data$colour[i]<-2}
        if(data$Unerupted[i]==1){data$colour[i]<-3}
        if(sum(data$Occulsion[i],data$Loose[i],data$Unerupted[i])==0){data$colour[i]<-4}
      }else{
        if(data$Premortem[i]==1){data$colour[i]<-5}
        if(data$Postmortem[i]==1){data$colour[i]<-6}
        if(sum(data$Premortem[i],data$Postmortem[i])==0){data$colour[i]<-7}
      }
    }
    V(ig)$color<-data$colour
  }
  if(label=="Default"){label<-paste(ID,Type,sep="_")}
  plot(ig,layout=as.matrix(coords),main=label,palette=colours,margin=margin,vertex.size=10)
  legend(locator(1), legend=c("Present(Occulsion)","Present(Loose)","Present(unerupted)","Present(unknown)","Absent(Premortem)","Absent(Postmortem)","Absent(unknown)"),fill=colours)
}