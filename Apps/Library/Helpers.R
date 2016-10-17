Tags<-function(input){
  #creates list of tags
  primary<-input$tags_1
  tags<-primary
  for(i in 1:length(primary)){
    tag<-paste("Tags_",primary[i],sep="")
    tags<-c(tags,input[[tag]])
  }
  tags<-paste(unique(tags),collapse=":")
  tags
}

AuthorsList<-function(Author,Authors){
  #combines author and auhors field to create start of reference
  if(Authors=="None"){output<-Author
  }else{
    Authors<-strsplit(Authors,split="[:;]")[[1]]
    if(length(Authors)==1){output<-paste(Author,"and",Authors)
    }else{if(length(Authors)==2){
      Authors<-paste(Authors[1],"and",Authors[2])
      output<-paste(Author,Authors,sep="; ")
    }else{end<-tail(Authors,2);Authors<-head(Authors,-2)
          end<-paste(end[1],"and",end[2])
          if(length(Authors)==1){output<-paste(Author,Authors,end,sep="; ")
          }else{Authors<-paste(Authors,collapse="; ")
                output<-paste(Author,Authors,end,sep="; ")
          }
    }}}
  output
}

BuildRef<-function(input){
  library(toOrdinal)
  #Book
  if(input$Type=="B"){
    A<-AuthorsList(input$Author,input$Authors_B)
    if(input$Edition_B=="1"){
      Ref<-paste(A,".(",input$Date,").",input$Title,".",input$Place_B,":",input$Publisher_B,".",input$Volume_B,sep="")
    }else{
      Ref<-paste(A,".(",input$Date,").",input$Title,".",toOrdinal(as.numeric(input$Edition_B))," edn.",input$Place_B,":",input$Publisher_B,".",input$Volume_B,".",sep="")
    }
  }
  #Journal
  if(input$Type=="J"){
    A<-AuthorsList(input$Author,input$Authors_J)
    Ref<-paste(A,".(",input$Date,").",input$Title,".",input$Title_J,",",input$Issue_J,".pp",input$sPage_J,"-",input$ePage_J,".",sep="")
  }
  #online Journal  
  if(input$Type=="OJ"){
    A<-AuthorsList(input$Author,input$Authors_OJ)
    if(is.na(input$DOI)){
      Ref<-paste(A,".(",input$Date,").",input$Title,".",input$Title_OJ,",",input$Issue_OJ,".pp",input$sPage_OJ,"-",input$ePage_OJ,".",input$Collection,"[Online].Available at:",input$URL_OJ,"(Acessed:",input$Acessed_OJ,").",sep="")
    }else{Ref<-paste(A,".(",input$Date,").",input$Title,".",input$Title_OJ,",",input$Issue_OJ,".pp",input$sPage_OJ,"-",input$ePage_OJ,".",input$Collection,"[Online].DOI:",input$DOI,"(Acessed:",input$Acessed_OJ,").",sep="")}
  }
  #Book Chapter
  if(input$Type=="BC"){
    A<-AuthorsList(input$Author,input$Authors_B)
    BA<-strsplit(input$Editor_BC,split="[:;]")[[1]]
    if(length(BA)<2){BA<-BA
    }else{BA<-AuthorsList(BA[1],paste(BA[-1],collapse=";"))}
    if(input$Edition_BC=="1"){
      Ref<-paste(A,".(",input$Date,").'",input$Title,"',in ",BA,".(ed.).",input$Title_BC,".",input$Place_BC,":",input$Publisher_BC,".",input$Volume_BC,sep="")
    }else{
      Ref<-paste(A,".(",input$Date,").",input$Title,"',in ",BA,".(ed.).",input$Title_BC,".",toOrdinal(as.numeric(input$Edition_BC))," edn.",input$Place_BC,":",input$Publisher_BC,".",input$Volume_BC,sep="")
    }
  }
  #Thesis
  if(input$Type=="T"){
    if(input$Published=="FALSE"){
      Ref<-paste(input$Author,".(",input$Date,").",input$Title,".Unpublished ",input$Degree," thesis.",input$Uni,".",sep="")
    }else{
      Ref<-paste(input$Author,".(",input$Date,").",input$Title,".",input$Degree," thesis.",input$Uni,"[Online].Avaliable at:",input$URL_T,"(Accessed:",input$Acessed_T,").",sep="")
    }
  }
  #Conference proceedings
  if(input$Type=="C"){
    A<-AuthorsList(input$Author,input$Authors_C)
    Ref<-paste(A,".(",input$Date,").",input$Title_C,".",input$Location_C,",",input$Date_C,".",input$Place_C,":",input$Publisher_C,sep="")
  }
  #Conference Paper
  if(input$Type=="CP"){
    A<-AuthorsList(input$Author,input$Authors_CP)
    Ref<-paste(A,".(",input$Date,").",input$Title,",",input$Title_CP,".",input$Location_CP,",",input$Date_CP,".",input$Place_CP,":",input$Publisher_CP,",pp",input$sPage_CP,"-",input$ePage_CP,sep="")
  }
  #Online Conference Paper
  if(input$Type=="OC"){
    A<-AuthorsList(input$Author,input$Authors_OC)
    Ref<-paste(A,".(",input$Date,").",input$Title,",",input$Title_OC,".",input$Location_OC,",",input$Date_OC,".",input$Publisher_OC,"[Online].Available at:",input$URL_OC,"(Accessed:",input$Acessed_OC,").",sep="")
  }
  #Report
  if(input$Type=="R"){
    A<-AuthorsList(input$Author,input$Authors_R)
    Ref<-paste(A,".(",input$Date,").",input$Title,".",input$Place_R,":",input$Publisher_R,".",sep="")
  }
  #Online Report
  if(input$Type=="OR"){
    A<-AuthorsList(input$Author,input$Authors_OR)
    Ref<-paste(A,".(",input$Date,").",input$Title,".","[Online].Available at:",input$URL_OR,"(Accessed:",input$Acessed_OR,").",sep="")
  }
  #Government Publication
  if(input$Type=="GP"){
    if(is.na(input$URL_IOP)){Ref<-paste(input$Author,".(",input$Date,").",input$Title,".",input$Place_GP,":",input$Publisher_GP,".",sep="")
    }else{Ref<-paste(input$Authors_GP,".",input$Author,".(",input$Date,").",input$Title,".",input$Publisher_GP,"[Online].Available at:",input$URL_GP,"(Acessed:",input$Acessed_GP,").",sep="")}
  }
  #International organisation publication
  if(input$Type=="IOP"){
    if(is.na(input$URL_IOP)){Ref<-paste(input$Author,".(",input$Date,").",input$Title,".",input$Place_IOP,":",input$Publisher_IOP,".",sep="")
    }else{Ref<-paste(input$Author,".(",input$Date,").",input$Title,".","[Online].Available at:",input$URL_IOP,"(Accessed:",input$Acessed_IOP,").",sep="")}
  }
  #Standards
  if(input$Type=="S"){
    if(is.na(input$Number)){Ref<-paste(input$Author,".(",input$Date,").",input$Title,".",input$Place_S,":",input$Publisher_S,".",sep="")
    }else{Ref<-paste(input$Author,".(",input$Date,").",input$Number,":",input$Title,".",input$Place_S,":",input$Publisher_S,".",sep="")}
    
  }
  
  if(input$Type=="O"){
    A<-AuthorsList(input$Author,input$Authors_O)
    Ref<-paste(A,".(",input$Date,").",input$Title,".",input$Ref_O,sep="")
  }
  Ref
}

WriteLibrary<-function(input){
  file<-paste(input$FWrite,".LIB.txt",sep="")
  if(input$Type!="C"){x<-data.frame(Author=tolower(input$Author),Date=input$Date,Title=tolower(input$Title),Reference=BuildRef(input),Tags=Tags(input),Location=input$Location)
  }else{x<-data.frame(Author=tolower(input$Author),Date=input$Date,Title=tolower(input$Title_C),Reference=BuildRef(input),Tags=Tags(input),Location=input$Location)}
  if(!(file.exists(file))){
    write.table(x,file,row.names=FALSE)
    m<-"New Library file created"
  }else{
    write.table(x,file,row.names=FALSE,col.names=FALSE,append=TRUE)
    m<-paste("Data appended to file ",file,sep="")
  }
  m
}

WriteBibliography<-function(input,SRef){
  if(input$NewSearch=="New"){x<-BuildRef(input)
  }else{x<-SRef}
  file<-paste(input$FWrite,".BIB.txt",sep="")
  if(!(file.exists(file))){
    write(x,file)
    m<-"New Bibliography file created"
  }else{
    write(x,file,append=TRUE)
    m<-paste("Data appended to file ",file,sep="")
  }
  m
}

SearchLibrary<-function(input){
  file<-paste(input$FSearch,".LIB.txt",sep="")
  if(!(file.exists(file))){stop("File ",file," does not exist")}
  paste("Reading in ",file)
  Lib<-read.table(file,header=TRUE,stringsAsFactors=FALSE)
  if(input$SearchBy=="Title"){
    title<-tolower(input$STitle)
    Results<-Lib[Lib$Title==title,c("Reference","Location")]
    R2<-grep(title,tolower(Lib$Title))
    R2<-Lib[R2,c("Reference","Location")]
    Results<-rbind(Results,R2)
    Results<-unique(Results)
  }
  if(input$SearchBy=="Author"){
    author<-tolower(input$SAuthor)
    Results<-Lib[Lib$Author==author,c("Reference","Location")]
    R2<-grep(author,tolower(Lib$Reference))
    R2<-Lib[R2,c("Reference","Location")]
    Results<-rbind(Results,R2)
    Results<-unique(Results)
  }
  if(input$SearchBy=="Date"){
    date<-tolower(input$SDate)
    Results<-Lib[Lib$Date==date,c("Reference","Location")]
  }
  if(input$SearchBy=="Author and date"){
    author<-tolower(input$SAuthor)
    date<-tolower(input$SDate)
    Results<-Lib[(Lib$Author==author) & (Lib$Date==date),c("Reference","Location")]
  }
  if(input$SearchBy=="Tags"){
    tags<-Tags(input)
    tags<-strsplit(tags,split=":")[[1]]
    check<-NULL
    for(i in 1:length(tags)){
      check<-c(check,grep(tags[i],Lib$Tags))
    }
    check<-table(check)
    X<-as.numeric(names(check[check==length(tags)]))
    Results<-Lib[X,c("Reference","Location")]
  }
  if(length(Results[,1])<1){Results<-data.frame(Reference="No results found",Location=NA)}
  Results
}