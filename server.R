library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub <- readShapePoly("subctr60")
dfsub = data.frame(sub)
dfsub[dfsub==0] = NA
zips <- read.csv("ZIP_centroids.csv")


shinyServer(function(input, output) {

  # Grab ZIP code input
  center <- reactiveValues(xcoord=-117.8414, ycoord=33.647)
  observeEvent(input$recenter, {
    center$xcoord = zips$x_centr[zips$CODE==input$zip]
    center$ycoord = zips$y_centr[zips$CODE==input$zip]
  })
  
  
  
  
  # Grab Inputs
  options = reactiveValues(choose="Em_all_14")
  observeEvent(input$csgo, {
    
    type_link = switch(input$cstype, "Highest Category"="_max_", "Total"="_all_", "Business Services (KIBS)"="_kibs_",
                       "Creative Class"="_crtv_", "Retail"="_ret_", "High Tech"="_tech_", "Industrial"="_ind_")
    
    options$choose = paste(substr(input$cstopic,1,2), type_link, substr(input$year,3,4), sep="")
  })

  
  
  finalMap <- reactive ({
    # Define values to list as "choice"  
    choice = dfsub[,grep(options$choose, colnames(dfsub))]
    # Create map as 'm'
    m = leaflet(sub) %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles() %>%
    addPolylines(stroke=TRUE, weight=2, color="black", fill=FALSE)  %>%
    addPolygons(stroke=F, color = ~colorQuantile("Blues", choice, na.color="#B0171F")(choice)) 
  })
  
  # Generate Map Output
  output$myMap = renderLeaflet(finalMap())
  

})

