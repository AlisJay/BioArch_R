FusionScore<-function(Names,Oblit,Fused,Fusing,Open){
  library(BMS)
  Table<-data.frame("Name"=Names,"Score"=0)
  Table$Score[Table$Name %in% Oblit]<-1
  Score<-bin2hex(Table$Score)
  Table<-Table[Table$Score==0,]
  Table$Score[Table$Name %in% Fused]<-1
  Score<-paste(Score,bin2hex(Table$Score),sep=":")
  Table<-Table[Table$Score==0,]
  Table$Score[Table$Name %in% Fusing]<-1
  Score<-paste(Score,bin2hex(Table$Score),sep=":")
  Table<-Table[Table$Score==0,]
  Table$Score[Table$Name %in% Open]<-1
  Score<-paste(Score,bin2hex(Table$Score),sep=":")
  Score
}
InventoryScore<-function(Names,Present,Complete){
  library(BMS)
  Table<-data.frame("Name"=Names,"Score"=0)
  Table$Score[Table$Name %in% Present]<-1
  Score<-bin2hex(Table$Score)
  if(sum(Table$Score)==0){Score<-paste(Score,0,sep=":")
  }else{
    Table<-Table[Table$Score==1,]
    Table$Score<-0
    Table$Score[Table$Name %in% Complete]<-1
    Score<-paste(Score,bin2hex(Table$Score),sep=":")}
  Score
}