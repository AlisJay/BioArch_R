P_S01_UI<-tagList(fixedPage(h3("ID:P_S01"),
                    h4("Description:Acetabular ossicle")))
P_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_S01",Type="Shape",Des="Acetabular ossicle",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_S02_UI<-tagList(fixedPage(h3("ID:P_S02"),
                            h4("Description:Wide pubic gap")))
P_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_S02",Type="Shape",Des="Wide pubic gap",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_L01_UI<-tagList(fixedPage(h3("ID:P_L01"),
                            h4("Description:Enlarged nutrient foramina")))
P_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_l01",Type="Loss",Des="Enlarged nutrient foramina",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_L02_UI<-tagList(fixedPage(h3("ID:P_L02"),
                            h4("Description:Concavity/perforation of illiac fossa")))
P_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_L02",Type="Loss",Des="Concavity/perforation of illiac fossa",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_L03_UI<-tagList(fixedPage(h3("ID:P_L03"),
                            h4("Description:Depression(s) on pubic symphyses")))
P_L03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_L03",Type="Loss",Des="Depression(s) on pubic symphyses",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_F01_UI<-tagList(fixedPage(h3("ID:P_F01"),
                            h4("Description:Acessory sacral facets")))
P_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F01",Type="Formation",Des="Acessory sacral facets",Loc="Pelvis:Sacrum:NA",Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_F02_UI<-tagList(fixedPage(h3("ID:P_F02"),
                            h4("Description:Fusion of sacroilliac joint")))
P_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F02",Type="Formation",Des="Fusion of sacroilliac joint",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_F03_UI<-tagList(fixedPage(h3("ID:P_F03"),
                            h4("Description:Projections on illiac crest/ishial crest,spine or tuberocity/obturator foramen")))
P_F03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F03",Type="Formation",Des="Projections on illiac crest/ishial crest,spine or tuberocity/obturator foramen",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_F04_UI<-tagList(fixedPage(h3("ID:P_F04"),
                            h4("Description:Bridging of cotyloid notch")))
P_F04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F04",Type="Formation",Des="Bridging of cotyloid notch",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_F05_UI<-tagList(fixedPage(h3("ID:P_F05"),
                            h4("Description:long projection from ishial tuberocity")))
P_F05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_F05",Type="Formation",Des="Long projection from ishial tuberocity",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_C01_UI<-tagList(fixedPage(h3("ID:P_C01"),
                            h4("Description:Flatterned area of acetabular rim")))
P_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_C01",Type="Complex",Des="Flatterned area of acetabular rim",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

P_C02_UI<-tagList(fixedPage(h3("ID:P_C02"),
                            h4("Description:Destruction of acetabulum with reactive bone growth")))
P_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="P_C02",Type="Complex",Des="Destruction of acetabulum with reactive bone growth",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
##################################################################################################
library(data.table)
Pelvis_DT<-data.table(ref=c("P_S01","P_S02","P_L01","P_L02","P_L03","P_F01","P_F03","P_F04","P_F05","P_C01","P_C02"),
                    uioptions=c(P_S01_UI,P_S02_UI,P_L01_UI,P_L02_UI,P_L03_UI,P_F01_UI,P_F03_UI,P_F04_UI,P_F05_UI,P_C01_UI,P_C02_UI),
                    RecordCreator=c(P_S01_RC,P_S02_RC,P_L01_RC,P_L02_RC,P_L03_RC,P_F01_RC,P_F03_RC,P_F04_RC,P_F05_RC,P_C01_RC,P_C02_RC))