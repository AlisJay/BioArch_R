DecodePP<-function(PP){
  Shape<-PPTablize(Paleopath$Table$Shape,Paleopath$Table$ID2)
  Size<-PPTablize(Paleopath$Table$Size,Paleopath$Table$ID2)
  Nature<-PPTablize(Paleopath$Table$Nature,Paleopath$Table$ID2)
  Individual<-as.data.frame(table(Paleopath$Table$ID))
  names(Individual)<-c("ID","N")
  Types<-unique(Paleopath$Table$Type);Individual[,Types]<-NA
  Des<-unique(Paleopath$Table$Des);Individual[,Des]<-NA
  for(i in 1:length(Individual$ID)){
    for(j in 1:length(Types)){
      Individual[i,Types[j]]<-sum(Paleopath$Table$Type[Paleopath$Table$ID==Individual$ID[i]]==Types[j])
    }
    for(j in 1:length(Des)){
      Individual[i,Des[j]]<-sum(Paleopath$Table$Des[Paleopath$Table$ID==Individual$ID[i]]==Des[j])
    }
  }
  Location<-data.frame("ID"=Paleopath$Table$ID,"ID2"=Paleopath$Table$ID2,"Region"=NA,"Bones"=NA,"Other"=NA,"Features"=NA,"Suture"=NA)
  
}

count<-function(x,y){
  for(i in 1:length(x)){
    y[,as.character(x[i])]<-NULL
    for(j in 1:length(y$ID)){
      y[j,as.character(x[i])]<-
    }
  }
}