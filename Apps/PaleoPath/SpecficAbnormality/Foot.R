#Supernummery bones##############################################################################################
F_S01_UI<-tagList(fixedPage(h3("ID:F_S01"),
                    h4("Description:Supernummery bones"),
                    column(width=4,selectInput("F_S01_1","Left",multiple=TRUE,choice=c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid"))),
                    column(width=4,selectInput("F_S01_2","Right",multiple=TRUE,choice=c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid"))),
                    column(width=4,h4("Connection with other lesions"),
                         textInput("F_S01_Link1","ID of linked lesion(s)",value="None"),
                         selectizeInput("F_S01_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
F_S01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S01",Type="Shape",Des="Supernummery bones",Loc=NA,Feat="NA:NA",Size="NA:NA",Shape="NA:NA",Nature="NA:NA",Add=NA,Link=NA)
  left<-NA;right<-NA
  if(length(input$F_S01_1)>0){left<-paste(paste(input$F_S01_1,"l",sep="_"),collapse =",")}
  if(length(input$F_S01_2)>0){right<-paste(paste(input$F_S01_2,"r",sep="_"),collapse =",")}
  Table$Loc<-paste0("Foot:Snum:",paste(left,right,sep=","))
  if(input$F_S01_Link1 != "None"){Table$Link<-paste(paste(input$F_S01_Link1,collapse=","),paste(input$F_S01_Link2,collapse=","),sep=":")}
  Table }
#Rotation of talus and calcaneous##############################################################################################

F_S02_UI<-tagList(fixedPage(h3("ID:F_S02"),
                            h4("Description:Rotation and malformation of talus and calcaneous")))
F_S02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S02",Type="Shape",Des="Rotation and malformation of talus and calcaneous",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Fusion of talus and calcaneous##############################################################################################

F_S03_UI<-tagList(fixedPage(h3("ID:F_S03"),
                            h4("Description:Fusion of talus and calcaneous")))
F_S03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S03",Type="Shape",Des="Fusion of talus and calcaneous",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }
#Fusion of Tibia and Talus##############################################################################################

F_S04_UI<-tagList(fixedPage(h3("ID:F_S04"),
                            h4("Description:Fusion of Tibia and Talus")))
F_S04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S04",Type="Shape",Des="Fusion of tibia and talus",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Interphalangeal fusion##############################################################################################
F_S05_UI<-tagList(fixedPage(h3("ID:F_S05"),
                            h4("Description:Fusion of interphalangeal joint(s)"),
                            selectInput("F_S05_1","Bone",multiple=TRUE,selected="Phalanx_p1_r,Phalanx_i1_r",
                                        c("Right 1st Proximal and Intermediate"="Phalanx_p1_r,Phalanx_i1_r","Right 1st Intermediate and distal"="Phalanx_i1_r,Phalanx_d1_r","Right 2nd Proximal and Intermediate"="Phalanx_p2_r,Phalanx_i2_r","Right 2nd Intermediate and distal"="Phalanx_i2_r,Phalanx_d2_r","Right 3rd Proximal and Intermediate"="Phalanx_p3_r,Phalanx_i3_r","Right 3rd Intermediate and distal"="Phalanx_i3_r,Phalanx_d3_r","Right 4th Proximal and Intermediate"="Phalanx_p4_r,Phalanx_i4_r","Right 4th Intermediate and distal"="Phalanx_i4_r,Phalanx_d4_r","Right 5th Proximal and Intermediate"="Phalanx_p5_r,Phalanx_i5_r","Right 5th Intermediate and distal"="Phalanx_i5_r,Phalanx_d5_r",
                                          "Left 1st Proximal and Intermediate"="Phalanx_p1_l,Phalanx_i1_l","Left 1st Intermediate and distal"="Phalanx_i1_l,Phalanx_d1_l","RLeft 2nd Proximal and Intermediate"="Phalanx_p2_l,Phalanx_i2_l","Left 2nd Intermediate and distal"="Phalanx_i2_l,Phalanx_d2_l","Left 3rd Proximal and Intermediate"="Phalanx_p3_l,Phalanx_i3_l","Left 3rd Intermediate and distal"="Phalanx_i3_l,Phalanx_d3_l","Left 4th Proximal and Intermediate"="Phalanx_p4_l,Phalanx_i4_l","Left 4th Intermediate and distal"="Phalanx_i4_l,Phalanx_d4_l","Left 5th Proximal and Intermediate"="Phalanx_p5_l,Phalanx_i5_l","Left 5th Intermediate and distal"="Phalanx_i5_l,Phalanx_d5_l"))))
F_S05_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S05",Type="Shape",Des="Fusion of interphalangeal joints",Loc=NA,Feat="Head,Base:NA",Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Loc<-paste("Foot",paste(input$F_S05_1,collapse=","),"NA",sep=":")
  Table }

#Metatarsalphangeal fusion##############################################################################################
F_S06_UI<-tagList(fixedPage(h3("ID:F_S06"),
                            h4("Description:Fusion of metatarsophalangeal joint(s)"),
                            selectInput("F_S06_1","Ray(s)",multiple=TRUE,selected="Metatarsal_1_r,Phalanx_p1_r",c("Right 1st ray"="Metatarsal_1_r,Phalanx_p1_r","Right 2nd Ray"="Metatarsal_2_r,Phalanx_p2_r","Right 3rd ray"="Metatarsal_3_r,Phalanx_p3_r","Right 4th ray"="Metatarsal_4_r,Phalanx_p4_r","Right 5th ray"="Metatarsal_5_r,Phalanx_p5_r",
                                                             "Leftt 1st ray"="Metatarsal_1_l,Phalanx_p1_l","Leftt 2nd Ray"="Metatarsal_2_l,Phalanx_p2_l","Left 3rd ray"="Metatarsal_3_l,Phalanx_p3_l","Left 4th ray"="Metatarsal_4_l,Phalanx_p4_l","Left 5th ray"="Metatarsal_5_l,Phalanx_p5_l")),
                            column(width=4,
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_1_r,Phalanx_p1_r')>=0",h4("Right 1st Ray"),
                                                    selectInput("F_S06_1_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_1_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_1_r_2!='180'",selectInput("F_S06_1_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_2_r,Phalanx_p2_r')>=0",h4("Right 2nd Ray"),
                                                    selectInput("F_S06_2_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_2_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_2_r_2!='180'",selectInput("F_S06_1_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_3_r,Phalanx_p3_r')>=0",h4("Right 3rd Ray"),
                                                    selectInput("F_S06_3_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_3_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_3_r_2!='180'",selectInput("F_S06_1_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_4_r,Phalanx_p4_r')>=0",h4("Right 4th Ray"),
                                                    selectInput("F_S06_4_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_4_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_4_r_2!='180'",selectInput("F_S06_4_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_5_r,Phalanx_p5_r')>=0",h4("Right 5th Ray"),
                                                    selectInput("F_S06_5_r_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_5_r_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_5_r_2!='180'",selectInput("F_S06_5_r_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral"))))),
                            column(width=4,
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_1_l,Phalanx_p1_l')>=0",h4("Left 1st Ray"),
                                                    selectInput("F_S06_1_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_1_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_1_l_2!='180'",selectInput("F_S06_1_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_2_l,Phalanx_p2_l')>=0",h4("Left 2nd Ray"),
                                                    selectInput("F_S06_2_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_2_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_2_l_2!='180'",selectInput("F_S06_1_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_3_l,Phalanx_p3_l')>=0",h4("Left 3rd Ray"),
                                                    selectInput("F_S06_3_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_3_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_3_l_2!='180'",selectInput("F_S06_1_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_4_l,Phalanx_p4_l')>=0",h4("Left 4th Ray"),
                                                    selectInput("F_S06_4_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_4_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_4_l_2!='180'",selectInput("F_S06_4_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral")))),
                                   conditionalPanel("input.F_S06_1.indexOf('Metatarsal_5_l,Phalanx_p5_l')>=0",h4("Left 5th Ray"),
                                                    selectInput("F_S06_5_l_1","Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete")),
                                                    selectInput("F_S06_5_l_2","Angulation of the joint",selected="180",c("None (180)"="180","180-170","170-150","150-125","125-100","<100")),
                                                    conditionalPanel("input.F_S06_5_l_2!='180'",selectInput("F_S06_5_l_3","Direction of angulation",selected=NA,c("None"=NA,"Palmer","Dorsal","Medial","Lateral"))))),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("F_S06_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("F_S06_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
F_S06_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S06",Type="Shape",Des="Fusion of metotarsophalangeal joint(s)",Loc=NA,Feat="Head,base:NA",Size=NA,Shape="NA:NA",Nature=NA,Add=NA,Link=NA)
  Table$Loc<-paste("Foot",paste(input$F_S06_1,collapse=","),"NA",sep=":")
  Extent<-NULL
  Angulation<-NULL
  Angle<-NULL
  for(i in 1:length(input$F_S06_1)){
    x<-substr(input$F_S06_1[i], nchar(input$F_S06_1[i])-3+1, nchar(input$F_S06_1[i]))
    Extent<-c(Extent,input[[as.character(paste0("F_S06_",x,"_1"))]])
    Angulation<-c(Angulation,input[[as.character(paste0("F_S06_",x,"_2"))]])
    if(input[[as.character(paste0("F_S06_",x,"_2"))]]!="180"){Angle<-c(Angle,input[[as.character(paste0("F_S06_",x,"_3"))]])}else{Angle<-c(Angle,"NA")}
  }
  Table$Size<-paste0("Angulation:",paste(Angulation,collapse="/"))
  Table$Nature<-paste0("FusionExtent,AngulationDirection:",paste(paste(Extent,collapse="/"),paste(Angle,collapse="/"),sep=","))
  if(input$F_S06_Link1 != "None"){Table$Link<-paste(paste(input$F_S06_Link1,collapse=","),paste(input$F_S06_Link2,collapse=","),sep=":")}
  Table }

#Intertarsal fusion##############################################################################################
F_S07_UI<-tagList(fixedPage(h3("ID:F_S07"),
                            h4("Description:Fusion of intertarsal joint(s)")))
F_S07_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_S07",Type="Shape",Des="Fusion of intertarsal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Destruction of small bones##############################################################################################
F_L01_UI<-tagList(fixedPage(h3("ID:F_L01"),
                            h4("Description:Destruction of metatarsals/phalanges")))
F_L01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_L01",Type="Loss",Des="Destruction of metatarsals/phalanges",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Enlarged Nutrient foramen##############################################################################################
F_L02_UI<-tagList(fixedPage(h3("ID:F_L02"),
                            h4("Description:Enlarged nutrient foramen"),
                            column(width=4,h4("Right"),
                                   selectInput("F_L02_1","Bone",multiple=TRUE,selected="Talus",c("Talus","Calcaneous","Navicular","Cuboid","Medial Cuniform"="Cuniform_med","Intermediate Cuniform"="Cuniform_inter","Lateral Cuniform"="Cuniform_lat",
                                                                                                 "1st Metatarsal"="Metatarsal_1","2nd Metatarsal"="Metatarsal_2","3rd Metatarsal"="Metatarsal_3","4th Metatarsal"="Metatarsal_4","5th Metatarsal"="Metatarsal_5",
                                                                                                 "1st Proximal Phalanx"="Phalanx_p1","2nd Proximal Phalanx"="Phalanx_p2","3rd Proximal Phalanx"="Phalanx_p3","4th Proximal Phalanx"="Phalanx_p4","5th Proximal Phalanx"="Phalanx_p5",
                                                                                                 "1st Intermediate Phalanx"="Phalanx_i1","2nd Intermediate Phalanx"="Phalanx_i2","3rd Intermediate Phalanx"="Phalanx_i3","4th Intermediate Phalanx"="Phalanx_i4","5th Intermediate Phalanx"="Phalanx_i5",
                                                                                                 "1st Distal Phalanx"="Phalanx_d1","2nd Distal Phalanx"="Phalanx_d2","3rd Distal Phalanx"="Phalanx_d3","4th Distal Phalanx"="Phalanx_d4","5th Distal Phalanx"="Phalanx_d5")),
                                   conditionalPanel("input.F_L02_1.indexOf('Talus')>=0",selectInput("F_L02_1_Talus","Talus  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Calcaneous')>=0",selectInput("F_L02_1_Calcaneous","Calcaneous  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Navicular')>=0",selectInput("F_L02_1_Navicular","Navicular  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Cuboid')>=0",selectInput("F_L02_1_Cuboid","cuboid  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Cuniform_med')>=0",selectInput("F_L02_1_Cuniform_med","Medial Cuniform  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Cuniform_inter')>=0",selectInput("F_L02_1_Cuniform_inter","Intermediate Cuniform  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Cuniform_lat')>=0",selectInput("F_L02_1_Cuniform_lat","Lateral Cuniform  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Metatarsal_1')>=0",selectInput("F_L02_1_Metatarsal_1","Metatarsal 1  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Metatarsal_2')>=0",selectInput("F_L02_1_Metatarsal_2","Metatarsal 2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Metatarsal_3')>=0",selectInput("F_L02_1_Metatarsal_3","Metatarsal 3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Metatarsal_4')>=0",selectInput("F_L02_1_Metatarsal_4","Metatarsal 4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Metatarsal_5')>=0",selectInput("F_L02_1_Metatarsal_5","Metatarsal 5  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_p1')>=0",selectInput("F_L02_1_Phalanx_p1","Phalanx p1 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_p2')>=0",selectInput("F_L02_1_Phalanx_p2","Phalanx p2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_p3')>=0",selectInput("F_L02_1_Phalanx_p3","Phalanx p3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_p4')>=0",selectInput("F_L02_1_Phalanx_p4","Phalanx p4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_p5')>=0",selectInput("F_L02_1_Phalanx_p5","Phalanx p5 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_i1')>=0",selectInput("F_L02_1_Phalanx_i1","Phalanx i1 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_i2')>=0",selectInput("F_L02_1_Phalanx_i2","Phalanx i2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_i3')>=0",selectInput("F_L02_1_Phalanx_i3","Phalanx i3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_i4')>=0",selectInput("F_L02_1_Phalanx_i4","Phalanx i4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_i5')>=0",selectInput("F_L02_1_Phalanx_i5","Phalanx i5 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_d1')>=0",selectInput("F_L02_1_Phalanx_d1","Phalanx d1 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_d2')>=0",selectInput("F_L02_1_Phalanx_d2","Phalanx d2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_d3')>=0",selectInput("F_L02_1_Phalanx_d3","Phalanx d3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_d4')>=0",selectInput("F_L02_1_Phalanx_d4","Phalanx d4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_1.indexOf('Phalanx_d5')>=0",selectInput("F_L02_1_Phalanx_d5","Phalanx d5 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                            column(width=4,h4("Left"),
                                   selectInput("F_L02_2","Bone",multiple=TRUE,selected="Talus",c("Talus","Calcaneous","Navicular","Cuboid","Medial Cuniform"="Cuniform_med","Intermediate Cuniform"="Cuniform_inter","Lateral Cuniform"="Cuniform_lat",
                                                                                                 "1st Metatarsal"="Metatarsal_1","2nd Metatarsal"="Metatarsal_2","3rd Metatarsal"="Metatarsal_3","4th Metatarsal"="Metatarsal_4","5th Metatarsal"="Metatarsal_5",
                                                                                                 "1st Proximal Phalanx"="Phalanx_p1","2nd Proximal Phalanx"="Phalanx_p2","3rd Proximal Phalanx"="Phalanx_p3","4th Proximal Phalanx"="Phalanx_p4","5th Proximal Phalanx"="Phalanx_p5",
                                                                                                 "1st Intermediate Phalanx"="Phalanx_i1","2nd Intermediate Phalanx"="Phalanx_i2","3rd Intermediate Phalanx"="Phalanx_i3","4th Intermediate Phalanx"="Phalanx_i4","5th Intermediate Phalanx"="Phalanx_i5",
                                                                                                 "1st Distal Phalanx"="Phalanx_d1","2nd Distal Phalanx"="Phalanx_d2","3rd Distal Phalanx"="Phalanx_d3","4th Distal Phalanx"="Phalanx_d4","5th Distal Phalanx"="Phalanx_d5")),
                                   conditionalPanel("input.F_L02_2.indexOf('Talus')>=0",selectInput("F_L02_2_Talus","Talus  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Calcaneous')>=0",selectInput("F_L02_2_Calcaneous","Calcaneous  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Navicular')>=0",selectInput("F_L02_2_Navicular","Navicular  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Cuboid')>=0",selectInput("F_L02_2_Cuboid","cuboid  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Cuniform_med')>=0",selectInput("F_L02_2_Cuniform_med","Medial Cuniform  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Cuniform_inter')>=0",selectInput("F_L02_2_Cuniform_inter","Intermediate Cuniform  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Cuniform_lat')>=0",selectInput("F_L02_2_Cuniform_lat","Lateral Cuniform  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Metatarsal_1')>=0",selectInput("F_L02_2_Metatarsal_1","Metatarsal 1  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Metatarsal_2')>=0",selectInput("F_L02_2_Metatarsal_2","Metatarsal 2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Metatarsal_3')>=0",selectInput("F_L02_2_Metatarsal_3","Metatarsal 3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Metatarsal_4')>=0",selectInput("F_L02_2_Metatarsal_4","Metatarsal 4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Metatarsal_5')>=0",selectInput("F_L02_2_Metatarsal_5","Metatarsal 5  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_p1')>=0",selectInput("F_L02_2_Phalanx_p1","Phalanx p1 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_p2')>=0",selectInput("F_L02_2_Phalanx_p2","Phalanx p2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_p3')>=0",selectInput("F_L02_2_Phalanx_p3","Phalanx p3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_p4')>=0",selectInput("F_L02_2_Phalanx_p4","Phalanx p4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_p5')>=0",selectInput("F_L02_2_Phalanx_p5","Phalanx p5 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_i1')>=0",selectInput("F_L02_2_Phalanx_i1","Phalanx i1 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_i2')>=0",selectInput("F_L02_2_Phalanx_i2","Phalanx i2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_i3')>=0",selectInput("F_L02_2_Phalanx_i3","Phalanx i3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_i4')>=0",selectInput("F_L02_2_Phalanx_i4","Phalanx i4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_i5')>=0",selectInput("F_L02_2_Phalanx_i5","Phalanx i5 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_d1')>=0",selectInput("F_L02_2_Phalanx_d1","Phalanx d1 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_d2')>=0",selectInput("F_L02_2_Phalanx_d2","Phalanx d2  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_d3')>=0",selectInput("F_L02_2_Phalanx_d3","Phalanx d3  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_d4')>=0",selectInput("F_L02_2_Phalanx_d4","Phalanx d4  relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2"))),
                                   conditionalPanel("input.F_L02_2.indexOf('Phalanx_d5')>=0",selectInput("F_L02_2_Phalanx_d5","Phalanx d5 relative size",c("<1.5x Normal"="<1.5","1.5-2x Normal"="1.5-2",">2x Normal"=">2")))),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("F_L02_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("F_L02_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
F_L02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_L02",Type="Loss",Des="Enlarged nutrient foramen",Loc=NA,Feat="nutrient_for:NA",Size=NA,Shape="NA:NA",Nature="NA:NA",Add=NA,Link=NA)
  right<-NULL;rightSize<-NULL
  left<-NULL;leftSize<-NULL
  if(length(input$F_L02_1)>0){
    right<-paste(paste0(input$F_L02_1,"_r"),collapse=",")
    rs<-paste0("F_L02_1_",input$F_L02_1)
    for(i in 1:length(rs)){rightSize<-c(rightSize,input[[as.character(rs[i])]])}
    rightSize<-paste(rightSize,collapse=",")}
  if(length(input$F_L02_2)>0){
    left<-paste(paste0(input$F_L02_2,"_l"),collapse=",")
    ls<-paste0("F_L02_2_",input$F_L02_2)
    for(i in 1:length(ls)){leftSize<-c(leftSize,input[[as.character(ls[i])]])}
    leftSize<-paste(leftSize,collapse=",")}
  Table$Loc<-paste0("Foot:",paste(right,left,sep=","),":NA")
  Table$Size<-paste0("relativeSize:",paste(rightSize,leftSize,sep=","))
  if(input$F_L02_Link1 != "None"){Table$Link<-paste(paste(input$F_L02_Link1,collapse=","),paste(input$F_L02_Link2,collapse=","),sep=":")}
  Table }

#Symetric joint erosion##############################################################################################
F_L03_UI<-tagList(fixedPage(h3("ID:F_L03"),
                            h4("Description:Symetric erosion of metatarsophalangeal or interphalangeal joint(s)")))
F_L03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_L03",Type="Loss",Des="Symetric erosion of metatarsophalangeal or interphalangeal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Asymetric joint erosion##############################################################################################
F_L04_UI<-tagList(fixedPage(h3("ID:F_L04"),
                            h4("Description:Asymetric erosion of metatarsophalangeal or interphalangeal joint(s)")))
F_L04_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_L03",Type="Loss",Des="Asymetric erosion of metatarsophalageal or interphalangeal joint(s)",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Superior Calcaneal spur##############################################################################################
F_F01_UI<-tagList(fixedPage(h3("ID:F_F01"),
                            h4("Description:Spur(s) on superior calcaneal tuber")))
F_F01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_F01",Type="Formation",Des="Spur(s) on superior calcaneal tuber",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Inferior calcaneal spur##############################################################################################
F_F02_UI<-tagList(fixedPage(h3("ID:F_F02"),
                            h4("Description:Spur(s) on inferior calcaneal tuber")))
F_F02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_F02",Type="Formation",Des="Spur(s) on inferior calcaneal tuber",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Medial Talar spur##############################################################################################
F_F03_UI<-tagList(fixedPage(h3("ID:F_F03"),
                            h4("Description:Spur on medial talar neck")))
F_F03_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_F03",Type="Formation",Des="Spur on medial talar neck",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#High Arch##############################################################################################
F_C01_UI<-tagList(fixedPage(h3("ID:F_C01"),
                            h4("Description:Severe bone loss and deformity creating high arch")))
F_C01_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_C01",Type="Complex",Des="Severe bone loss and deformity creating high arch",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table }

#Tufting##############################################################################################
F_C02_UI<-tagList(fixedPage(h3("ID:F_C02"),
                            h4("Description:Tufting and resorption of distal phalanges"),
                            selectInput("F_C02_1","Bone(s)",multiple = TRUE,selected="Phalanx_d1_r",c("1 right"="Phalanx_d1_r","1 left"="Phalanx_d1_l","2 right"="Phalanx_d2_r","2 left"="Phalanx_d2_l","3 right"="Phalanx_d3_r","3 left"="Phalanx_d3_l","4 right"="Phalanx_d4_r","4 left"="Phalanx_d4_l","5 right"="Phalanx_d5_r","5 left"="Phalanx_d5_l")),
                            column(width=4,
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d1_r')>=0",h4("1st Right Phalanx"), 
                                                    numericInput("F_C02_1_r_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_1_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_1_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_1_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d2_r')>=0",h4("2nd Right Phalanx"), 
                                                    numericInput("F_C02_2_r_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_2_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_2_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_2_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d3_r')>=0",h4("3rd Right Phalanx"), 
                                                    numericInput("F_C02_3_r_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_3_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_3_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_3_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d4_r')>=0",h4("4th Right Phalanx"), 
                                                    numericInput("F_C02_4_r_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_4_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_4_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_4_r_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d5_r')>=0",h4("5th Right Phalanx"), 
                                                    numericInput("F_C02_5_r_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_5_r_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_5_r_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_5_r_4","Shape description",value="Spade-shaped"))),
                            column(width=4,
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d1_l')>=0",h4("1st Left Phalanx"), 
                                                    numericInput("F_C02_1_l_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_1_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_1_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_1_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d2_l')>=0",h4("2nd Left Phalanx"), 
                                                    numericInput("F_C02_2_l_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_2_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_2_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_2_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d3_l')>=0",h4("3rd Left Phalanx"), 
                                                    numericInput("F_C02_3_l_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_3_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_3_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_3_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d4_l')>=0",h4("4th Leftt Phalanx"), 
                                                    numericInput("F_C02_4_l_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_4_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_4_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_4_l_4","Shape description",value="Spade-shaped")),
                                   conditionalPanel("input.F_C02_1.indexOf('Phalanx_d5_l')>=0",h4("5th Left Phalanx"), 
                                                    numericInput("F_C02_5_l_1","Maximum Distal width",value=NA),
                                                    numericInput("F_C02_5_l_2","Maximum Proximal width",value=NA),
                                                    numericInput("F_C02_5_l_3","Maximum Mid shaft width",value=NA),
                                                    textInput("F_C02_5_l_4","Shape description",value="Spade-shaped"))),
                            column(width=4,h4("Connection with other lesions"),
                                   textInput("F_C02_Link1","ID of linked lesion(s)",value="None"),
                                   selectizeInput("F_C02_Link2","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")))))
F_C02_RC<-function(input=input){   
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2="F_C02",Type="Complex",Des="Tufting and resorption of distal phalanges",Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  Table$Loc<-paste("Foot",paste(input$F_C02_1,collapse=","),"NA",sep=":")
  x<-data.frame("Phalanx"=substr(input$F_C02_1, nchar(input$F_C02_1)-3+1, nchar(input$F_C02_1)),DW=NA,PW=NA,MW=NA,Shape=NA)
  for(i in 1:length(input$F_C02_1)){
    x$Shape[i]<-input[[as.character(paste0("F_C02_",x$Phalanx[i],"_4"))]]
    x$DW[i]<-input[[as.character(paste0("F_C02_",x$Phalanx[i],"_1"))]]
    x$PW[i]<-input[[as.character(paste0("F_C02_",x$Phalanx[i],"_4"))]]
    x$MW[i]<-input[[as.character(paste0("F_C02_",x$Phalanx[i],"_4"))]]
  }
  Table$Shape<-paste(paste(x$Phalanx,collapse=","),paste(x$Shape,collapse=","),sep=":")
  Table$Size<-paste0("Distal,Proximal,Mid:",paste(paste(x$DW,collapse="/"),paste(x$PW,collapse="/"),paste(x$MW,collapse="/"),sep=","))
  if(input$F_C02_Link1 != "None"){Table$Link<-paste(paste(input$F_C02_Link1,collapse=","),paste(input$F_C02_Link2,collapse=","),sep=":")}
  Table }

#Data table#################################################################################################
library(data.table)
Foot_DT<-data.table(ref=c("F_S01","F_S02","F_S03","F_S04","F_S05","F_S06","F_S07","F_L01","F_L02","F_L03","F_L04","F_F01","F_F02","F_F03","F_C01","F_C02"),
                    uioptions=c(F_S01_UI,F_S02_UI,F_S03_UI,F_S04_UI,F_S05_UI,F_S06_UI,F_S07_UI,F_L01_UI,F_L02_UI,F_L03_UI,F_L04_UI,F_F01_UI,F_F02_UI,F_F03_UI,F_C01_UI,F_C02_UI),
                    RecordCreator=c(F_S01_RC,F_S02_RC,F_S03_RC,F_S04_RC,F_S05_UI,F_S06_RC,F_S07_RC,F_L01_RC,F_L02_RC,F_L03_RC,F_L04_RC,F_F01_RC,F_F02_RC,F_F03_RC,F_C01_RC,F_C02_RC))