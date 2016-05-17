library(shiny)
library(leaflet)
#library(rgdal)
library(sp)
#library(rsconnect)

#leafletOutput("map")


shinyUI(fluidPage(
  titlePanel(strong("Socioeconomic Mixing")),
  sidebarLayout(
    sidebarPanel(
      h3(strong("User Options")),
      h5("Zoom to your ZIP code:"),
      textInput("zip", label=strong("5-digit ZIP:"), value=92697),
      actionButton("recenter", label="Re-center"),
      h5(strong("Select Type of Mixing to Explore")),
      selectInput("variable", label=NULL,
                  choices=list("", "Age", "Race", "Income", "Education", "Housing Type", "Housing Age", "Land Use"), selected="Income"),
      h5(strong("Select Year")),
      selectInput("year", label=NULL,
                  choices=list("", "1990", "2000", "2012"), selected="2012"),
      actionButton("go", label="Go"),
      br(),
      p(strong("Data Notes:")),
      textOutput("var_desc"),
      br(),
      br(),
      a("UCI's Metropolitan Futures Initiative", href="http://socialecology.uci.edu/mfi"),
      #div("UCI's Metropolitan Futures Initiative", style="color:blue"),
      img(src="mfi.jpg", height=250, width=250)
    ),
    mainPanel(
      h2("Orange County, California Census Tracts"),
      p(em("Values of mixing are displayed below. Darker colors generally indicate an area is more mixed.")),
      leafletOutput('myMap', height = 600, width = 900)
    )
  )
))