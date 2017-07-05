library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub <- readShapePoly("ACS_2015_5YR_MSA_M1")
dfsub <- data.frame(sub)
names = as.character(unique(unlist(dfsub$NAME)))

shinyUI(
  
  navbarPage("Mixing in Neighborhoods - part of the New Urban Crisis?", id="nav",
             
             tabPanel("TAB PANEL", div(class="outer",
                                                     
                                                     tags$head(
                                                       includeCSS("styles.css")
                                                     ),
                                                     
                                                     leafletOutput("myMap", width="100%", height="100%"),
                                                     
                                                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
                                                                   radioButtons("year", label=strong("Select Timeframe: "), choices=list("2010", "2015", "Change"), selected="2015", inline=T),
                                                                   radioButtons("cstopic", label=strong("Select Topic: "), choices=list("Income", "Age", "Education", "Occupation"), selected="Income", inline=T),
                                                                   actionButton("csgo", label="Click to Refresh After Changing Selection"),
                                                                   br(""),
                                                                   selectInput("msa", label=strong("Select Metro Area to Display Below: "), selected=" ", choices = names),
                                                                   
                                                                   # Top Five
                                                                   textOutput("tablelabel"),
                                                                   tableOutput("topfive"),
                                                                   # Generate Histogram
                                                                   plotOutput("hist", height = 225),
                                                                   h6(em("by the ", a("Metropolitan Futures Initiative", href="http://mfi.soceco.uci.edu", target="_blank"), "at UC-Irvine (2016).  Webmap by ", a("Kevin Kane, PhD", href="http://kevinkane.org", target="_blank"), "and", a("UCI Data Science Initiative", href="http://datascience.uci.edu", target="_blank")))
                                                     ),
                                                     absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                                                                   draggable=TRUE, top=110, left=10, right="auto", bottom="auto",
                                                                   width=160, height="auto",
                                                                   p("Data Notes:"),
                                                                   h6("-- Please be patient while the map loads! Allow 20-30 sec."),
                                                                   h6("-- Please click to refresh after making new selections to ensure correct map and legend are displayed, and to clear any error messages."),
                                                                   h6("-- See", a("our website", href="http://mfi.soceco.uci.edu", target="_blank"), "for details.")
                                                     )
             ))
))
