library(shiny)
library(leaflet)
library(sp)
library(maptools)

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
                                                                                h5("Zoom to your ZIP code:"),
                                                                                textInput("zip", label=strong("5-digit ZIP:"), value=92697),
                                                                                actionButton("recenter", label="Re-center"),
                                                                                
                                                                                selectInput("year", label="Select Timeframe", choices=list("", "1997", "2014", "Change Over 1997-2014"), selected=""),
                                                                                
                                                                                conditionalPanel("input.year == '1997' | input.year == '2014'", 
                                                                                                 selectInput("cstopic", label = "Select Topic:", selected="",
                                                                                                             choices = list("", "Employment", "Specialization")),
                                                                                                 
                                                                                                 conditionalPanel("input.cstopic == 'Employment' | input.cstopic == 'Specialization' ",
                                                                                                                  selectInput("cstype", label= "Select Category", selected="Industrial",
                                                                                                                              choices = list("Highest Category", "Total", "Business Services (KIBS)", "Creative Class", "Retail", "High Tech", "Industrial")),
                                                                                                                  actionButton("csgo", label="Go"))),
                                                              
                                                                                                 
                                                                                conditionalPanel("input.year == 'Change Over 1997-2014' ",
                                                                                                 selectInput("change", label="Choose Analysis", selected="",
                                                                                                            choices = list("", "Subcenters by Boundary Change", "Subcenters by Status", "Subcenters by Persistence Score")),
                                                                                                 actionButton("longo", label="Go"))
                                                                                
                                                                                                
                                                                  )
)))

