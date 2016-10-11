library(shiny);library(markdown)
shinyUI(navbarPage("Bio-Profiler",
                   tabPanel("Ancestry",
                            fluidPage(
                              selectInput("knownac", "Known?", c("known","unknown"),selected="known"),
                              conditionalPanel("input.knownac=='known'",
                                               radioButtons("kan", label = h3("ancestry"),
                                                            choices = list("Asian" = "asian", "European" = "european", "African" = "african"), 
                                                            selected = "asian")),
                              conditionalPanel("input.knownac=='unknown'",
                                               fluidRow(
                                                 column(width=6,
                                                        selectInput("orbit","Orbit shaped",c("Angular"="Eu","Rectangular"="Af","Round"="As","Undeterminable"="U"),selected="U"),
                                                        selectInput("nRoot","Nasal root",c("High and Narrow"="Eu","Low and Round"="Af","Low and Ridged"="As","Undeterminable"="U"),selected="U"),
                                                        selectInput("LNB","Lower Nasal Border",c("Sharp sill"="Eu","Guttered"="Af","Flat and sharp"="As","Undeterminable"="U"),selected="U"),
                                                        selectInput("palate","Palate shape",c("Parabolic"="Eu","Hyperbolic"="Af","Ellipitical"="As","Undeterminable"="U"),selected="U"),
                                                        selectInput("profile","Face profile",c("Straight"="Eu","Projecting"="Af","Intermediate"="As","Undeterminable"="U"),selected="U"),
                                                        selectInput("nwidth","Nasal width",c("Narrow"="Eu","Wide"="Af","Intermediate"="As","Undeterminable"="U"),selected="U"),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br()
                                                 ),
                                                 column(width=6,
                                                        selectInput("BR","Prominant brow ridge",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("VS","Complex vault structure",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("PBD","Post-bregma depression",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("SI","Shovel shaped incisors",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("nBridge","High nasal bridge",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("LEB","Projecting Lower Eye Border",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("nSpine","Prominant nasal spine",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("fShape","Wide face shape",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("mMarks","Rugged vault muscle marks",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        selectInput("Jaw","Small Jaw",c("True"="T","False"="F","Undeterminable"="U"),selected="U"),
                                                        br(),
                                                        br(),
                                                        br()
                                                 )
                                               ))
                              
                            )
                   ),
                   tabPanel("Age",
                            fluidPage(
                              selectInput("knownage", "Known?", c("known","unknown")),
                              conditionalPanel("input.knownage=='known'",
                                               radioButtons("kage","Age group",choices=list("adult"="Adult","subadult"="SubAdult"),select="Adult"),
                                               sliderInput("krange","Age range",min=0,max=100,value=c(40,60))
                              ),
                              conditionalPanel("input.knownage=='unknown'",
                                               fluidRow(
                                                 column(width=4,
                                                        h4("epiphyses closure"),
                                                        checkboxGroupInput("epiphysis","tick all open epiphyses",
                                                                           choices=list("proximal radius"="pRadius","distal fibula"="dFibula","distal tibia"="dTibia","distal femur"="dFemur","proximal fibula"="pFibula","acromion process"="acromion",
                                                                                        "illiac crest"="iliac","humeral head"="hHead","femoral head"="fHead","lesser trochanter"="lTrochanter","proximal tibia"="pTibia",
                                                                                        "greater trochanter"="gTrochanter","distal radius"="dradius","sacrum s3-s5"="s3s5","sacrum s3-s2"="s2s3","sacrum s1-s2"="s1s2","sternal end of clavicle"="clavicle","spheno-occipital synchondrosis"="SOS"))
                                                 ),
                                                 column(width=4,
                                                        h4("phase scoring"),
                                                        selectInput("todd","pubic synthesis Todd",c("NA","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10")),
                                                        selectInput("SucheyBrookes","pubic synthesis Suchey-Brookes",c("NA","p1","p2","p3","p4","p5","p6")),
                                                        selectInput("lovejoy","auricular surface Lovejoy",c("NA","p1","p2","p3","p4","p5","p6","p7","p8")),
                                                        h4("Sternal 4th rib ends"),
                                                        selectInput("rib1","Surface Bone",c("NA","Smooth"="p1/p2","More Porous"="p3/p4","Light and Porous"="p5","Lighter and More Porous"="p6","Deteriorating"="p7")),
                                                        selectInput("rib2","Surface Contour",c("NA","Flat/Indented with Billows"="p1","Indented:U to V shape"="p2","U to V shape"="p3","U-Shape with flaring"="p4","U-shape and deeper"="p5/p6","U-Shape"="p7")),
                                                        selectInput("rib3","Rim Edge",c("NA","Rounded"="p1/p2","Sharp"="p3/p4/p5/p6","Sharp with thin walls"="p7")),
                                                        selectInput("rib4","Rim Contour",c("NA","Regular to slightly wavy"="p1","Wavy to Irregular"="p2","Irregular"="p3","Irregular with projections"="p4/p5/p6","Irregular with projections and windows"="p7"))
                                                 ),
                                                 column(width=4,
                                                        h4("suture closure"),
                                                        selectInput("ML","midlambdoid",c("NA","0","1","2","3")),
                                                        selectInput("L","lambda",c("NA","0","1","2","3")),
                                                        selectInput("O","obelion",c("NA","0","1","2","3")),
                                                        selectInput("AS","anterior sagittal",c("NA","0","1","2","3")),
                                                        selectInput("B","bregma",c("NA","0","1","2","3")),
                                                        selectInput("P","pterion",c("NA","0","1","2","3")),
                                                        selectInput("MC","midcoronal",c("NA","0","1","2","3")),
                                                        selectInput("SF","sphenofrontal",c("NA","0","1","2","3")),
                                                        selectInput("IST","inferior sphenotemporal",c("NA","0","1","2","3")),
                                                        selectInput("SST","superior sphenotemporal",c("NA","0","1","2","3")),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br()
                                                        
                                                 )
                                               ))
                            )      
                   ),
                   tabPanel("Sex",
                            fluidPage(
                              selectInput("knownsex", "Known?", c("known","unknown")),
                              conditionalPanel("input.knownsex=='known'",
                                               radioButtons("ksex","sex",choices=list("male"="male","female"="female"),select="male")
                              ),
                              conditionalPanel("input.knownsex=='unknown'",
                                               fluidRow(
                                                 column(width=3,
                                                        h3("non metric traits"),
                                                        h4("pelvis"),
                                                        sliderInput("va","ventral arch",min=0,max=3,value=0),
                                                        sliderInput("spc","subpubic concavity",min=0,max=3,value=0),
                                                        sliderInput("ipr","ishio-pubic ramus",min=0,max=3,value=0),
                                                        sliderInput("sn","sciatic notch",min=0,max=5,value=0),
                                                        sliderInput("pas","preauricular sulcus",min=0,max=5,value=0),
                                                        selectInput("pSize","Pelvis Size",c("Undeterminable"=NA,"Large and Rugged"="m","Small and Gracile"="f")),
                                                        selectInput("iShape","Illium Shape",c("Undeterminable"=NA,"High and Vertical"="m","Low and Flat"="f")),
                                                        selectInput("pInlet","Shape of pelvic inlet",c("Undeterminable"=NA,"Heart shaped"="m","Circular or Ellipitical"="f")),
                                                        selectInput("pShape","Pubic Shape",c("Undeterminable"=NA,"Narrow and Rectangular"="m","Broad and Square"="f")),
                                                        selectInput("spAngle","Subpubic angle",c("Undeterminable"=NA,"V-shaped"="m","U-Shaped"="f")),
                                                        selectInput("OF","Obturator foramen",c("Undeterminable"=NA,"Large and Ovoid"="m","Small and Triangular"="f")),
                                                        selectInput("sShape","Sacrum Shape",c("Undeterminable"=NA,"Long and Narrow"="m","Short and Broad"="f")),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br()
                                                        ),
                                                 column(width=3,
                                                        h4("cranial"),
                                                        sliderInput("nc","nuchal crest",min=0,max=5,value=0),
                                                        sliderInput("m","mastoid process",min=0,max=5,value=0),
                                                        sliderInput("som","supraorbital margin",min=0,max=5,value=0),
                                                        sliderInput("g","glabella",min=0,max=5,value=0),
                                                        sliderInput("me","mental eminence",min=0,max=5,value=0),
                                                        selectInput("Chin","Chin Shape (view from below)",c("Undeterminable"=NA,"Broad"="m","Pointed"="f")),
                                                        selectInput("cSize","Skull Size",c("Undeterminable"=NA,"Large and Rugged"="m","Small and Gracile"="f")),
                                                        selectInput("flare","Flaring of the Gonial Angle",c("Undeterminable"=NA,"Yes"="m","No"="f")),
                                                        selectInput("flex","Ramus flexture",c("Undeterminable"=NA,"No"="m","Yes"="f")),
                                                        selectInput("gAngle","Gonial Angle",c("Undeterminable"=NA,"Close to 90 degrees"="m","Obtuse(~110-120)"="f"))
                                                 ),
                                                 column(width=3,
                                                        h3("metric assessment"),
                                                        h4("pelvis"),
                                                        numericInput("pLength","pubis length",value=NA),
                                                        numericInput("pLength2","pubis length from acetabular rim",value=NA),
                                                        numericInput("iLength","ishial length",value=NA),
                                                        numericInput("iLength2","ishial length from acetabular rim",value=NA),
                                                        numericInput("awidth","Maximum horizontal width of the acetabulum",value=NA),
                                                        h4("Other post cranial"),
                                                        numericInput("sHeight","Scapula height",value=NA),
                                                        numericInput("gHeight","Glenoid height",value=NA),
                                                        numericInput("hHead","Vertical humeral head diameter",value=NA),
                                                        numericInput("rHead","Maximum radial head diameter",value=NA),
                                                        numericInput("fHead","Femoral head diameter",value=NA)
                                                        
                                                 ),
                                                 column(width=3,
                                                        h4("cranial"),
                                                        numericInput("maxLength","maximum length",value=NA),
                                                        numericInput("maxBreadth","maximum breadth",value=NA),
                                                        numericInput("BaBr","basion to bregma",value=NA),
                                                        numericInput("BaNa","basion to nasion",value=NA),
                                                        numericInput("BB","bizygomatic breadth",value=NA),
                                                        numericInput("BaPr","basion to prosthion",value=NA),
                                                        numericInput("NaAl","nasion to alveolar border",value=NA),
                                                        numericInput("pBreadth","palate breadth",value=NA),
                                                        numericInput("mLength","mastoid length",value=NA))
                                               )
                              )
                            )
                   ),
                   tabPanel("Result",
                            fluidPage(
                              titlePanel("Results"),
                              mainPanel(
                                h4("Ancestry"),
                                conditionalPanel("input.knownac=='known'",
                                                 h5("Known ancestry:"),
                                                 h5(textOutput("Kan"))),
                                
                                conditionalPanel("input.knownac=='unknown'",
                                                 h5("Calculated ancestry:"),
                                                 h5(textOutput("an")),
                                                 tableOutput("anT"),
                                                 h5(textOutput("anw"))),
                                h4("Age"),
                                conditionalPanel("input.knownage=='known'",
                                                 h5("Known age:"),
                                                 h5(textOutput("Kag")),
                                                 h5(textOutput("Kagr"))),
                                conditionalPanel("input.knownage=='unknown'",
                                                 h5("Calculated age:"),
                                                 h5(textOutput("ag")),
                                                 h5(textOutput("agw4")),
                                                 h5(textOutput("EScore")),
                                                 conditionalPanel("input.epiphysis!='' & input.epiphysis!='s1s2'",
                                                                  h5(textOutput("agr4"))),
                                                 conditionalPanel("input.epiphysis==''|input.epiphysis=='s1s2'",
                                                                  conditionalPanel("input.knownsex=='known'",
                                                                                   h5(textOutput("agr1")),
                                                                                   tableOutput("agt1"),
                                                                                   h5(textOutput("agw1"))),
                                                                  conditionalPanel("input.knownsex=='unknown'& input.knownac=='known'",
                                                                                   h5(textOutput("agr3")),
                                                                                   tableOutput("agt3"),
                                                                                   h5(textOutput("agw3"))),
                                                                  conditionalPanel("input.knownsex=='unknown'& input.knownac=='unknown'",
                                                                                   h5(textOutput("agr2")),
                                                                                   tableOutput("agt2"),
                                                                                   h5(textOutput("agw2")))
                                                                  
                                                 )
                                ),
                                h4("Sex"),
                                conditionalPanel("input.knownsex=='known'",
                                                 h5("Known sex:"),
                                                 h5(textOutput("Ks"))),
                                conditionalPanel("input.knownsex=='unknown'",
                                                 h5("Calculated sex:"),
                                                 conditionalPanel("input.knownac=='known'&input.knownage=='known'",
                                                                  h5(textOutput("sx1")),
                                                                  tableOutput("st1"),
                                                                  h5(textOutput("sw1"))),
                                                 conditionalPanel("input.knownac=='known'&input.knownage=='unknown'",
                                                                  h5(textOutput("sx2")),
                                                                  tableOutput("st2"),
                                                                  h5(textOutput("sw2"))),
                                                 conditionalPanel("input.knownac=='unknown'&input.knownage=='known'",
                                                                  h5(textOutput("sx3")),
                                                                  tableOutput("st3"),
                                                                  h5(textOutput("sw3"))),
                                                 conditionalPanel("input.knownac=='unknown'&input.knownage=='unknown'",
                                                                  h5(textOutput("sx4")),
                                                                  tableOutput("st4"),
                                                                  h5(textOutput("sw4")))
                                                 
                                                 
                                                 
                                )
                              )
                              
                            )
                   ),
                   tabPanel("Write files",
                            textInput("dir","Data Folder",value="../data/"),
                            column(width=4,
                                   h3("Population Info"),
                                   textInput("POPID","Population ID",value="NA"),
                                   textInput("POPName","Population name",value="NA"),
                                   textInput("Person1","Investigator",value="NA"),
                                   actionButton("Create","Create new files"),
                                   h4(textOutput("PMessage"))
                            ),
                            column(width=4,
                                   h3("Individual Info"),
                                   textInput("ID","Individual ID",value="NA"),
                                   textInput("Person2","Investigator intials",value="NA"),
                                   actionButton("Append","Add individual"),
                                   h4(textOutput("IMessage"))
                            ),
                            column(width=4,
                                   h3("Compatability check"),
                                   actionButton("Compat","Check file"),
                                   h4(textOutput("ComMessage"))
                            ))
                   
                   
)
)