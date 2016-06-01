library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub <- readShapePoly("subctr60")
dfsub <- data.frame(sub)
dfsub[dfsub==0] = NA

shinyUI(tabPanel("Employment Centers in Southern California", div(class="outer",
                                                              
                  tags$head(
                  # custom, taken from Shiny's "superZIP"
                            includeCSS("styles.css")
                            #includeScript("gomap.js")
                            ),
                                                                
                  leafletOutput("myMap", width="100%", height="100%"),
                                                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                                                                
                                              h2("Employment Centers in Southern California"),
                                              #p(em("Displays subcenters based on employment and specialization by industry in 1997 and 2014. 'NA' indicates subcenters that did not exist in that year.")),
                                              textInput("zip", label=strong("Zoom to 5-digit ZIP:"), value=90012),
                                              actionButton("recenter", label="Re-center"),
                                                                                
                                              selectInput("year", label="Select Timeframe", choices=list("", "1997", "2014", "Change Over 1997-2014"), selected=""),
                                                                                
                                              conditionalPanel("input.year == '1997' | input.year == '2014'", 
                                                               selectInput("cstopic", label = "Select Topic:", selected="",
                                                                           choices = list("", "Employment", "Specialization")),
                                                                                                 
                                              conditionalPanel("input.cstopic == 'Employment' | input.cstopic == 'Specialization' ",
                                                              selectInput("cstype", label= "Select Category", selected="",
                                                                          choices = list("High Tech", "Business Services (KIBS)", "Creative Class", "Retail", "Industrial", "Total", "Highest Category")),
                                                              actionButton("csgo", label="Go"))),

                                              conditionalPanel("input.cstype != 'Highest Category' & (input.year == '1997' | input.year == '2014')",
                                                               selectInput("ctr", label=em("Select Subcenter for Detail"), selected="DTLA",
                                                                           choices = as.list(as.character(dfsub$subctrNAME)))),
                                                                                            
                                              conditionalPanel("input.year == 'Change Over 1997-2014' ",
                                                              selectInput("change", label="Choose Analysis", selected="",
                                                                         choices = list("", "Subcenters by Boundary Change", "Subcenters by Status", "Subcenters by Persistence Score")),
                                                              actionButton("longo", label="Go!")),
                                              # Generate Histogram
                                              plotOutput("hist", height = 225)
                                              )
)))

