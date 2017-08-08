CreatePP<-function(ID,name,Investigator,dir){
  #creates new blank file with header
  filepath<-paste(dir,ID,".PP.OD.txt",sep="")
  if(file.exists(filepath)){stop(filepath," already exists")}
  Head<-c("#Osteological data:Pathology",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",Investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=PaleoPathV1.0"),
          "#Fields:Individual ID,InvestigatorInitials,Date,Lesion ID,Type,Description,Location,Features,Size,Shape,Nature,Additional,Linked lesions",
          "#Type:Type of abnormality:Formation/Loss/Shape/Complex",
          "#Des:headline description:prescribed",
          "#Loc:Region,Bone(s),Additional:prescribed,prescribed,free",
          "#Feat:features affected, sutures (skull only):prescribed",
          "#Size:names,values:free/prescribed,num(mm)",
          "#Shape:names,values:prescribed, free/prescribed",
          "#Nature:names,values:prescribed,free/prescribed",
          "#Add:Addtional information:free",
          "#Link:IDS,connection:alphanumeric,prescribed")
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2=NA,Type=NA,Des=NA,Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA,photo=NA)
  write(Head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
  paste("File ",ID,".pp.OD.txt created", sep="")
}

NoComma<-function(txt){
  txt<-gsub(",","_",txt)
  txt
}

AppendPP<-function(PopID,ID,In,dir,Table,Photo){
  #adds record to an existing .PP.OD.txt
  filepath<-paste(dir,PopID,".PP.OD.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    Table$ID<-ID
    Table$In<-In
    Table$D<-gsub(" ","_",as.character(Sys.time()))
    Table<-merge(Table,Photo)
    Table<-Table[,c(2,3,4,1,5:14)]
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    paste("Added Individual",ID,"to",filepath)
  }
}

uioptions<-function(input,region){
  #takes checkboxgroup input from side bar and creates mainpanel ui using _UI objects
  filename<-paste0("SpecficAbnormality/",region,".R")
  source(filename,local=TRUE)
  DT<-get(paste0(region,"_DT"))
  uiopts<-DT$uioptions[DT$ref %in% input]
  tagList(uiopts)
}

RecordCreator<-function(input){
  #uses _RC functions to tabilize input
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2=NA,Type=NA,Des=NA,Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  
  inputs<-list(
    Skull=c(input$Shape_Skull,input$Loss_Skull,input$Formation_Skull,input$Complex_Skull),
    Vert=c(input$Shape_Vert,input$Loss_Vert,input$Formation_Vert,input$Complex_Vert),
    Pelvis=c(input$Shape_Pelvis,input$Loss_Pelvis,input$Formation_Pelvis,input$Complex_Pelvis),
    Shoulder=c(input$Shape_Shoulder,input$Loss_Shoulder,input$Formation_Shoulder,input$Complex_Shoulder),
    Thorax=c(input$Shape_Thorax,input$Loss_Thorax,input$Formation_Thorax,input$Complex_Thorax),
    Arm=c(input$Shape_Arm,input$Loss_Arm,input$Formation_Arm,input$Complex_Arm),
    Hand=c(input$Shape_Hand,input$Loss_Hand,input$Formation_Hand,input$Complex_Hand),
    Leg=c(input$Shape_Leg,input$Loss_Leg,input$Formation_Leg,input$Complex_Leg),
    Foot=c(input$Shape_Foot,input$Loss_Foot,input$Formation_Foot,input$Complex_Foot),
    Systemic=input$Systemic)
  
  for(i in 1:length(inputs)){
    if(length(inputs[[i]])>0){
      filename<-paste0("SpecficAbnormality/",names(inputs)[i],".R")
      source(filename,local=TRUE)
      DT<-get(paste0(names(inputs)[i],"_DT"))
      Functions<-DT$RecordCreator[DT$ref %in% inputs[[i]]]
      records<-data.frame(ID=NA,In=NA,D=NA,ID2=NA,Type=NA,Des=NA,Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
      for(j in 1:length(Functions)){
        record<-Functions[[j]](input)
        records<-rbind(records,record)
      }
      records<-records[-1,]
      Table<-rbind(Table,records)
    }
  }
  if(input$Add>0){
    for(i in 1:input$Add){
      x<-Custom_RC(input,i)
      Table<-rbind(Table,x)
    }
  }
  Table[-1,]
}

Photoref<-function(input){
  #creates input for photoreference numbers for each record created by the record creator
  x<-h4("Photographic reference number(s)/filepath(s)")
  records<-c(input$Shape_Skull,input$Loss_Skull,input$Formation_Skull,input$Complex_Skull,
             input$Shape_Vert,input$Loss_Vert,input$Formation_Vert,input$Complex_Vert,
             input$Shape_Pelvis,input$Loss_Pelvis,input$Formation_Pelvis,input$Complex_Pelvis,
             input$Shape_shoulder,input$Loss_Shoulder,input$Formation_Shoulder,input$Complex_Shoulder,
             input$Shape_Thorax,input$Loss_Thorax,input$Formation_Thorax,input$Complex_Thorax,
             input$Shape_Arm,input$Loss_Arm,input$Formation_Arm,input$Complex_Arm,
             input$Shape_Hand,input$Loss_Hand,input$Formation_Hand,input$Complex_Hand,
             input$Shape_Leg,input$Loss_Leg,input$Formation_Leg,input$Complex_Leg,
             input$Shape_Foot,input$Loss_Foot,input$Formation_Foot,input$Complex_Foot,input$Systemic)
  if(input$Add>0){for(i in 1:input$Add){records<-c(records,paste0("C_",i))}}
  if(sum(records=="SK_L01")>0){
    y<-"SK_L01"
    if(sum(input$SK_L01_1=="Parietal")>0){y<-c(y,"SK_L01_P")}
    if(sum(input$SK_L01_1=="Frontal")>0){y<-c(y,"SK_L01_F")}
    if(sum(input$SK_L01_1=="Orbit")>0){y<-c(y,"SK_L01_Ob")}
    if(sum(input$SK_L01_1=="Occipital")>0){y<-c(y,"SK_L01_Oc")}
    if(sum(input$SK_L01_1=="Zygomatic")>0){y<-c(y,"SK_L01_Z")}
    if(length(y)>1){records<-c(records[records!="SK_L01"],y[-1])}
    }
  if(length(records)>0){
  for(i in 1:length(records)){
    x<-tagList(x,textInput(records[i],label=paste0("Lesion ID:",records[i]),value="NA"))
  }}
  
  list(x,records)
}

photocollect<-function(records,input){
  #produces a table photo reference numbers and IDs uses output of Photoref
  if(length(records)>0){
  photodata<-data.frame(ID2=records,photo=NA)
  for(i in 1:length(records)){
    photodata$photo[photodata$ID2==records[i]]<-input[[as.character(records[i])]]
  }
  }else{photodata<-NA}
  photodata
}

CompCheck<-function(PopID,PopName,Investigator,dir){
  #checks to see if details in an existing file match those current on the form
  filepath<-paste(dir,PopID,".PP.OD.txt",sep="")
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
    if(!(identical(version,"Program=PaleoPathV1.0"))){print(paste("The file was created by using a diffrent version of this software, Please check version update info for potential compatability issues.The current version is PaleoPathV1.0",sep=""))}
    
  }
  print("Compatability check complete")
}