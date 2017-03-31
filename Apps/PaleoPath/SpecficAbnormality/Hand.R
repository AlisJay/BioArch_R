H_S01_UI<-tagList(fixedPage(h3("ID:H_S01"),
                    h4("Description:Swan-neck deformaity")))
H_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_S01",Type="Shape",Des="Swan-neck deformity",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

H_S02_UI<-tagList(fixedPage(h3("ID:H_S02"),
                            h4("Description:Supernumery bones"),
                            column(width=4,selectInput("H_S02_1","Left",multiple=TRUE,choice=c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                                               "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium"))),
                            column(width=4,selectInput("H_S02_2","Right",multiple=TRUE,choice=c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                                                "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium"))),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("H_S02_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("H_S02_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
H_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_S02",Type="Shape",Des="Supernumery bones",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

H_L01_UI<-tagList(fixedPage(h3("ID:H_L01"),
                            h4("Description:Enlarged nutrient foramen")))
H_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_L01",Type="Loss",Des="Enlarged nutrient formen",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

H_L02_UI<-tagList(fixedPage(h3("ID:H_L02"),
                            h4("Description:Symetric erosion of metcarpophalangeal/interphalangeal joint(s)")))
H_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_L02",Type="Loss",Des="Symetric erosion of metacarpophalangeal/interphalangeal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

H_C01_UI<-tagList(fixedPage(h3("ID:H_C01"),
                            h4("Description:Tufting and resorption of distal phalanges"),
                            selectInput("H_C01_1","Bone(s)",multiple = TRUE,c("1 right"="Phalanx_d1_r","1 left"="Phalanx_d1_l","2 right"="Phalanx_d2_r","2 left"="Phalanx_d2_l","3 right"="Phalanx_d3_r","3 left"="Phalanx_d3_l","4 right"="Phalanx_d4_r","4 left"="Phalanx_d4_l","5 right"="Phalanx_d5_r","5 left"="Phalanx_d5_l"))
                            ))
H_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_C01",Type="Complex",Des="Tufting and resorption of distal phalanges",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

H_C02_UI<-tagList(fixedPage(h3("ID:H_C02"),
                            h4("Description:Eburnation of joint")))
H_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="H_C02",Type="Complex",Des="Eburnation of joint",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

##################################################################################################
library(data.table)
Hand_DT<-data.table(ref=c("H_S01","H_S02","H_L01","H_L02","H_C01","H_C02"),
                    uioptions=c(H_S01_UI,H_S02_UI,H_L01_UI,H_L02_UI,H_C01_UI,H_C02_UI),
                    RecordCreator=c(H_S01_RC,H_S02_RC,H_L01_RC,H_L02_RC,H_C01_RC,H_C02_RC))