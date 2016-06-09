library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub <- readShapePoly("subctr60")
dfsub <- data.frame(sub)
dfsub[dfsub==0] = NA
zips <- read.csv("ZIP_centroids.csv")


shinyServer(function(input, output) {

  # Grab ZIP code input
  center <- reactiveValues(xcoord=-118.2386, ycoord=34.06583)
  observeEvent(input$recenter, {
    center$xcoord = zips$x_centr[zips$CODE==input$zip]
    center$ycoord = zips$y_centr[zips$CODE==input$zip]
  })
  
  # Grab Inputs - Cross-Sectional
  options = reactiveValues(choose="Em_all_14")
  observeEvent(input$csgo, {
    type_link = switch(input$cstype, "Highest Category"="_max_", "Total"="_all_", "KIBS"="_kibs_",
                       "Creative Class"="_crtv_", "Retail"="_ret_", "High Tech"="_tech_", "Industrial"="_ind_")
    
    options$choose = paste(substr(input$cstopic,1,2), type_link, substr(input$year,3,4), sep="")
  })

  # Grab whether qual or quant - Cross-Sectional
  options2 = reactiveValues(choose="quant")
  observeEvent(input$csgo, {
    link = switch(input$cstype, "Highest Category"="qual", "Total"="quant", "KIBS"="quant",
                       "Creative Class"="quant", "Retail"="quant", "High Tech"="quant", "Industrial"="quant")
    options2$choose = link
  })
  
  # Grab Inputs - Longitudinal
  options3 = reactiveValues(choose="status")
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
  
    # Conditional Inputs
    if(input$year=="Changes Over 1997-2014"){choice = dfsub[,grep(options3$choose, colnames(dfsub))]} 
      else {choice = dfsub[,grep(options$choose, colnames(dfsub))]} 
    
    if(options2$choose=="qual" | options4$choose=="qual"){pal <- colorFactor("RdYlBu", choice, na.color="#FFFFFF")} 
      else {pal <- colorNumeric("Blues", choice, na.color="#B0171F")}
    
    # Create map as 'm'
    m = leaflet(sub) %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles() %>%
    addPolylines(stroke=TRUE, weight=2, color="black", fill=FALSE)  %>%
    addPolygons(stroke=F, color = ~pal(choice), popup=~subctrNAME) %>%
    addLegend("bottomleft", pal=pal, values=~choice, opacity=0.75, 
              title=~paste(input$year, input$cstype, input$cstopic, sep=" ")) 
  })
  
  # Generate Map Output
  output$myMap = renderLeaflet(finalMap())
  
  # Generate Histogram
  observeEvent(input$csgo, {
  output$hist <- renderPlot({
    if(options2$choose=="qual" | options4$choose=="qual" | input$year=="Changes Over 1997-2014"){return(NULL)} 
    else{ 
    data = dfsub[,grep(options$choose, colnames(dfsub))]
    hist(data, xlab=NULL, breaks=10, col="orange",
         ylab="# of Subcenters", border="white", main=paste(input$year, input$cstype, input$cstopic, sep=" "))
    legend("topright", c(input$ctr), lwd=2, box.col="white")
    if(input$cstopic=="Specialization"){
      abline(v=1, lty=2)
      legend("topright", c(input$ctr, "1.0"), lwd=c(2,1), lty=c(1,2), box.col="white")}
    if(input$cstopic=="Employment"){
      abline(v=mean(data, na.rm=T), lty=2)
      legend("topright", c(input$ctr, "Subctr Avg"), lwd=c(2,1), lty=c(1,2), box.col="white")}
    abline(v=dfsub[,grep(options$choose, colnames(dfsub))][dfsub$subctrNAME==input$ctr], lwd=2) }
  })
  })

  # Add Topic Descriptions
  output$var_desc <- renderText({
    data_notes = switch(input$cstopic,
                       "Employment" = "is the total number of employees in the subcenter.",
                       "Specialization"= "displays the selected category's location quotient in each subcenter. A value above 1 indicates a high concentration of that industry relative to the whole region. TOTAL cannot be selected.")
    paste("-- ", input$cstopic, data_notes, sep=" ")
  })
  
  # Add Variable Description
  output$var_desc2 <- renderText({
    data_notes = switch(input$cstype,
                        "High Tech" = "is ...",
                        "KIBS"= "stands for Knowledge-Intensive Business Services and...",
                        "Creative Class" = "is ...",
                        "Retail" = "is ...",
                        "Industrial" = "is ...",
                        "Total" = "cannot be selected for SPECIALIZATION",
                        "Highest Category"= "is the industry (of the 5 options shown) with the highest employment or location quotient in each subcenter.")
    paste("-- ", input$cstype, data_notes, sep=" ")
  })
  
})

