library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub_list = c('Beverly Hills/West Hollywood', 'San Bernardino', 'Burbank/LA 2', 'Lake Arrowhead', 'Palm Springs', 'Anaheim', 'Corona', 'Santa Paula', 'Adelanto', 'Rancho Cucamonga', 'Chino', 'Santa Fe Springs', 'Temecula', 'Torrance', 'Pasadena', 'Glendale', 'DTLA', 'Riverside', 'Malibu', 'Burbank/LA 1', 'South El Monte', 'Perris', 'Oxnard', 'Needles', 'Hawthorne', 'San Fernando Valley 1', 'Victorville', 'Banning', 'Covina', 'City of Industry', 'Yucaipa', 'Menifee', 'Downey', 'Moreno Valley', 'Riverside SW', 'Lake Elsinore SE', 'Pinon Hills', 'Barstow', 'Santa Clarita', 'Dana Point', 'Thousand Oaks', 'Montclair', 'Lancaster', 'Lake Elsinore', 'Yucca Valley', 'Palm Desert', 'Blythe', 'Ventura', 'Ojai', 'Fillmore', 'Hemet', 'Palmdale', 'Big Bear Lake', 'Simi Valley', 'El Segundo', 'San Fernando Valley 2', 'Camarillo', 'Long Beach', 'Irvine/Lake Forest', 'Irvine/SNA')

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
                                                                           choices = sub_list)),
                                                                                            
                                              conditionalPanel("input.year == 'Change Over 1997-2014' ",
                                                              selectInput("change", label="Choose Analysis", selected="",
                                                                         choices = list("", "Subcenters by Boundary Change", "Subcenters by Status", "Subcenters by Persistence Score")),
                                                              actionButton("longo", label="Go!")),
                                              # Generate Histogram
                                              plotOutput("hist", height = 225)
                                              )
)))

