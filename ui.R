library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub_list14 = c('', 'Adelanto',  'Anaheim',  'Banning',  'Barstow', 'Beverly Hills/West Hollywood',  'Big Bear Lake',  'Blythe', 'Burbank/LA 2',  'Camarillo',  'Chino',  'City of Industry',  'Corona',  'Covina',  'Dana Point',  'Downey',  'DTLA',  'El Segundo',  'Fillmore',  'Glendale', 'Hemet',  'Irvine/Lake Forest',  'Irvine/SNA',  'Lake Arrowhead',  'Lake Elsinore',  'Lake Elsinore SE',  'Lancaster',  'Long Beach',  'Malibu',  'Menifee',  'Montclair',  'Moreno Valley',  'Ojai',  'Palm Desert',  'Palm Springs',  'Palmdale',  'Pasadena',  'Pinon Hills',  'Rancho Cucamonga',  'Riverside',  'Riverside SW',  'San Bernardino',  'San Fernando Valley 2',  'Santa Clarita',  'Santa Fe Springs',  'Santa Paula',  'Simi Valley',  'Temecula',  'Thousand Oaks',  'Torrance',  'Ventura',  'Victorville',  'Yucaipa',  'Yucca Valley')
sub_list97 = c('', 'Adelanto',  'Anaheim',  'Banning',  'Barstow', 'Beverly Hills/West Hollywood',  'Big Bear Lake',  'Blythe',  'Burbank/LA 1', 'City of Industry',  'Corona',  'Covina',  'Dana Point', 'DTLA', 'Fillmore',  'Glendale',  'Hawthorne',  'Hemet',  'Irvine/Lake Forest',  'Irvine/SNA',  'Lake Arrowhead',  'Lake Elsinore',  'Lancaster',  'Long Beach',  'Malibu',  'Montclair',  'Needles',  'Ojai',  'Oxnard',  'Palm Desert',  'Palm Springs',  'Palmdale',  'Pasadena',  'Perris', 'Riverside',  'San Bernardino',  'San Fernando Valley 1', 'Santa Clarita',  'Santa Paula',  'Simi Valley',  'South El Monte',  'Temecula',  'Thousand Oaks',  'Torrance',  'Ventura',  'Victorville', 'Yucca Valley')
shinyUI(tabPanel("Southern California Employment Centers", div(class="outer",
                                                              
                  tags$head(
                  # custom, taken from Shiny's "superZIP"
                            includeCSS("styles.css")
                            #includeScript("gomap.js")
                            ),
                                                                
                  leafletOutput("myMap", width="100%", height="100%"),
                                                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                                                                
                                              h2("Southern California Employment Centers"),
                                              #p(em("Displays subcenters based on employment and specialization by industry in 1997 and 2014. 'NA' indicates subcenters that did not exist in that year.")),
                                              textInput("zip", label=strong("Zoom to 5-digit ZIP:"), value=90012),
                                              actionButton("recenter", label="Re-center"),
                                                                                
                                              selectInput("year", label="Select Timeframe", choices=list("", "1997", "2014"), selected=""),
                                                                                
                                              conditionalPanel("input.year == '1997' | input.year == '2014'", 
                                                               selectInput("cstopic", label = "Select Topic:", selected="",
                                                                           choices = list("", "Employment", "Specialization"))),
                                                                                                 
                                              conditionalPanel("input.cstopic == 'Employment' | input.cstopic == 'Specialization' ",
                                                              selectInput("cstype", label= "Select Category", selected="",
                                                                          choices = list("", "High Tech", "KIBS", "Creative Class", "Retail", "Industrial", "Total", "Highest Category"))),
                                              
                                              conditionalPanel("input.cstype != 'Highest Category' & input.cstype != '' & input.year == '1997'",
                                                               selectInput("ctr97", label=em("Select Subcenter for Detail"), selected="DTLA",
                                                                           choices = sub_list97)),
                                              
                                              conditionalPanel("input.cstype != 'Highest Category' & input.cstype != '' & input.year == '2014'",
                                                               selectInput("ctr14", label=em("Select Subcenter for Detail"), selected="DTLA",
                                                                           choices = sub_list14)),
                                              
                                              conditionalPanel("input.cstype != '' ",
                                                               actionButton("csgo", label="Go/Refresh")),            
                                              # Generate Histogram
                                              plotOutput("hist", height = 225)
                                              ),
                              absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                                            draggable=TRUE, top=110, left=10, right="auto", bottom="auto",
                                            width=150, height="auto",
                                            p("Data Notes:"),
                                            h6("-- 'View Changes' displays changing subcenter boundaries using blue for 1997 and yellow for 2014"),
                                            h6("-- NA indicates that the selection does NOT meet the definition of a subcenter in the year indicated."),
                                            h6(textOutput("var_desc")),
                                            h6(textOutput("var_desc2"))
                                            )
)))

