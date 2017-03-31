inputBioArch("Inventory")
inputBioArch("Profile")
inputBioArch("Metrics")
inputBioArch("Dental")
inputBioArch("Paleopathology")

Inventory<-ReadBioArch("data/001.SI.txt")
Profile1<-ReadBioArch("data/EG001.BP.OD.txt")
Profile2<-ReadBioArch("data/EG001.BP.OA.txt")
Dental<-ReadBioArch("data/Pop01.DEN.txt")
Dental2<-ReadBioArch("data/Pop01.Mod.DEN.txt")
Metrics<-ReadBioArch("data/EG001.M.OD.txt")
Paleopath<-ReadBioArch("data/001.PP.OD.txt")

summary(Inventory)
Inventory$Head
View(Inventory$Table)
Inventory2<-DecodeSI(Inventory)
summary(Inventory2)
View(Inventory2$Skull$Complete)

Map(Inventory2$Skull$Present[,5],region="Skull",label="Skull element presence, ID 002")
Map2(Inventory2$Pelvis$Present[,5],Inventory2$Pelvis$Present[,6],region="Pelvis",label1="002",label2="003")
MapPercent(GetPercents(Inventory2)$Vert_P,region="Vert",label="Percentage vertebrae element presence")
MapPercent(GetPercents(Inventory2)$Axial_P,region="Axial",label="Percentage Presence")
MapPercent(GetPercents(Inventory2)$LLimb_C,region="LLimb",label="Percentage Completness")
Map2Percent(GetPercents(Inventory2)$Arm_P,GetPercents(Inventory2)$Arm_C,region="Arm",label1=" Presence",label2="Completeness")

PM<-ProfileMetrics(Profile1,Profile2)
summary(PM)
View(PM$Individual)
View(PM$Population)
View(PM$Calculated)
summary(PM$Assignment)
View(PM$Assignment$Assign)
View(PM$Assignment$Agree)
View(PM$Assignment$summary)


View(DecodeM(Metrics,"C"))

View(DecodeAge(Profile2$Table))

table(Paleopath$Table$ID)
table(Paleopath$Table$Type)
table(Paleopath$Table$Des)
View(PPTablize(Paleopath$Table$Nature[c(4,5)],Paleopath$Table$ID2[c(4,5)]))

D<-DecodeDen(Dental)
summary(D)
summary(D$Sk01)
View(D$Sk01$Score)
View(D$Sk01$Fetaures)
View(D$Population)