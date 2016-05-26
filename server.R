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
  
  # Grab Inputs - Cross-Sectional
  options = reactiveValues(choose="Em_all_14")
  observeEvent(input$csgo, {
    type_link = switch(input$cstype, "Highest Category"="_max_", "Total"="_all_", "Business Services (KIBS)"="_kibs_",
                       "Creative Class"="_crtv_", "Retail"="_ret_", "High Tech"="_tech_", "Industrial"="_ind_")
    
    options$choose = paste(substr(input$cstopic,1,2), type_link, substr(input$year,3,4), sep="")
  })

  # Grab whether qual or quant - Cross-Sectional
  options2 = reactiveValues(choose="quant")
  observeEvent(input$csgo, {
    link = switch(input$cstype, "Highest Category"="qual", "Total"="quant", "Business Services (KIBS)"="quant",
                       "Creative Class"="quant", "Retail"="quant", "High Tech"="quant", "Industrial"="quant")
    options2$choose = link
  })
  
  # Grab Inputs - Longitudinal
  options3 = reactiveValues(choose="persistenc")
  observeEvent(input$longo,{
    long_link = switch(input$change, "Subcenters by Boundary Change"="bound", "Subcenters by Status"="status", "Subcenters by Persistence Score"="persistenc")
    options3$choose = long_link
  })
  
  # Grab whether qual or quant - Longitudinal
  options4 = reactiveValues(choose="quant")
  observeEvent(input$longo, {
    link = switch(input$change, "Subcenters by Boundary Change"="other", "Subcenters by Status"="qual", "Subcenters by Persistence Score"="quant")
    options4$choose = link
  })
  
  finalMap <- reactive ({
    if(input$year=="Changes Over 1997-2014"){choice=dfsub[,grep(options3$choose, colnames(dfsub))]} else {choice = dfsub[,grep(options$choose, colnames(dfsub))]} 
    if(options2$choose=="qual" | options4$choose=="qual"){col=~colorFactor("RdYlBu", choice)(choice)} else {col=~colorQuantile("Blues", choice, na.color="#B0171F")(choice)}
    # Create map as 'm'
    m = leaflet(sub) %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles() %>%
    addPolylines(stroke=TRUE, weight=2, color="black", fill=FALSE)  %>%
    addPolygons(stroke=F, color = col) 
  })
  
  # Generate Map Output
  output$myMap = renderLeaflet(finalMap())
  

})

