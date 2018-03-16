library(shiny);library(markdown)

shinyUI(navbarPage("Skeletal Abnormalities",
                   tabPanel("Location",
                            sidebarLayout(
                              sidebarPanel(width=3,selectInput("AbType","Type of Abnormality",c("Trauma","Pathology","Taphonomic")),
                                           selectInput("Region","Region",c("Skull","Vertebrae","Pelvis","Shoulder","Thorax","Arm","Hand","Leg","Foot")),
                                           uiOutput("Bone")),
                            mainPanel(h3("Location Details"),
                                      #tableOutput("LocationTable"),#Test table
                                      actionButton("LocationGo","Go"),
                                      uiOutput("LocationOptions")))),
                   tabPanel("Abnormality description",
                            conditionalPanel("input.AbType=='Trauma'",sidebarLayout(
                              sidebarPanel(width=3,selectInput("Trauma_Type","Type of Trauma (select all that apply)",multiple=TRUE,choices=c("Chip","Hole","Puncture","Pit(s)"="Pit","Incision(s)"="Incision","Chop Mark(s)"="ChopMark","Furrow(s)"="Furrow","Impression","Depression","Embeded Foreign object/inclusion"="Inclusion","Fracture","Displacement","Amputation","Other")),
                                           uiOutput("FractOptions"),
                                           strong("Trauma type definations"),
                                           h5("Chip = Separation of a small fragment of bone usually from along an edge, can be complete or incomplete"),
                                           h5("Hole = An abnormality of any shape which goes completly through the section of bone"),
                                           h5("Puncture = A deep abnormality usally with a small cross sectional area that penetrates into the bone but does not go completly through it"),
                                           h5("Pit = shallow abnormality with a small cross sectional area, which is usually circular. Normally has more rounded side than a puncture"),
                                           h5("Incision = Linear abnormality (cut mark) that is longer than it is wide"),
                                           h5("Chop Mark= wider and more irregular incision like abnormality with atleast on straight edge, often accompanied by fractures and mising portions of bone"),
                                           h5("Furrow = Linear abnormality, generally shallower and wider than an incision with a more rounded profile. Can resemble a groove or sulcus but are loated where these are not normally present"),
                                           h5("Impression = A abnormality of any shape, with a wider cross sectional area than a pit. Generally shallow with sloping or rounded edges"),
                                           h5("Depression = An abnormality of any shape in which a section of bone is pushed inwards. Often accompanied by fractures and has stepper/sharper edges than an impression"),
                                           h5("Inclusion = A foreign object, of any material, which is embeded in in any way attached to the bone"),
                                           h5("Fracture = Discontinuity in the bone caused by the application of force, This includes incomplete breaks"),
                                           h5("Displacement = When surfaces of the bone that were once continous no longer meet or meet at an unnatural angle, often related to fractures"),
                                           h5("Amputation = the complete loss of a section of bone with evidence of forced removal on the remaining section")),
                              mainPanel(img(src="Trauma.png",height=100),
                                        h3("Trauma details"),
                                        #tableOutput("TraumaTable"),#Test table
                                        actionButton("TraumaGo","Go"),
                                        uiOutput("TraumaOptions")))),
                            conditionalPanel("input.AbType=='Taphonomic'",sidebarLayout(
                              sidebarPanel(width=3,selectInput("Taph_Type","Type of Taphonomic anomaly (select all that apply)",multiple=TRUE,choices=c("Cortical Flaking"="CorticalFlaking","Burning","Discolouration","Increase in dry bone Mass"="IncreasedMass","Reduction in dry bone mass"="ReducedMass","Shrinkage","Mineral deposit(S)"="MineralDeposition","Inclusion","Other"))),
                              mainPanel(h3("Taphonomy details"),
                                        #tableOutput("TaphTable"),#Test Table
                                        actionButton("TaphGo","Go"),
                                        uiOutput("TaphOptions")))),
                            conditionalPanel("input.AbType=='Pathology'",sidebarLayout(
                              sidebarPanel(width=3,selectInput("Path_Type","Type of Pathology",multiple=TRUE,selected="Loss",choices=c("Abnormal Bone Loss"="Loss","Abnormal Bone Formation"="Formation","Shape/Size Abnormality"="Shape")),
                                           uiOutput("Specific")),
                              mainPanel(h3("Pathology Details"),
                                        #tableOutput("PathTable"),#TestTable
                                        actionButton("PathGo","Go"),
                                        uiOutput("PathologyOptions"))))),
                   tabPanel("Link",
                            sidebarLayout(
                              sidebarPanel(width=3,
                                textInput("directory","Data Folder",value="../../data/"),
                                textInput("File","Popualtion ID",value="001"),
                                textInput("Individual","Skeleton ID",value="ShowAll"),
                                actionButton("show","Show Abnormalities"),
                                uiOutput("Link")),
                              mainPanel(
                                #tableOutput("LinkTable"),#TestTable
                                tableOutput("PopTable"))
                            )),
                   tabPanel("Write",
                            mainPanel(textInput("dir","Data Folder",value="../../data/"),
                                      tableOutput("ResultsTable"),
                                      column(width=4,
                                             h3("Population Info"),
                                             textInput("POPID","Population ID",value="NA"),
                                             textInput("POPName","Population name",value="NA"),
                                             textInput("Person1","Investigator",value="NA"),
                                             actionButton("Create","Create new file"),
                                             h4(textOutput("PMessage"))
                                      ),
                                      column(width=4,
                                             h3("Individual Info"),
                                             textInput("ID","Individual ID",value="NA"),
                                             textInput("Person2","Investigator intials",value="NA"),
                                             textInput("Photo","Photographic reference number(s)/filepath(s)",value=NA),
                                             actionButton("Append","Add"),
                                             h4(textOutput("IMessage1"))
                                      ),
                                      column(width=4,
                                             h3("Compatability check"),
                                             actionButton("Compat","Check file"),
                                             h4(textOutput("ComMessage"))
                                      )))))