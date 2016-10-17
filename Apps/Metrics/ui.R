library(shiny);library(markdown)

shinyUI(navbarPage("Metrics",
                   tabPanel("Cranial"),
                   tabPanel("Pelvis"),
                   tabPanel("LongBone"),
                   tabPanel("Write",
                            textInput("dir","Data Folder",value="../../data/"),
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
                                   actionButton("Append","Add individual"),
                                   h4(textOutput("IMessage"))
                            ),
                            column(width=4,
                                   h3("Compatability check"),
                                   actionButton("Compat","Check file"),
                                   h4(textOutput("ComMessage"))
                            )
                   )
                   ))