# PLACES # 

library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub <- readShapePoly("SoCal_place_2010")
dfsub <- data.frame(sub)
dfsub[dfsub==0] = NA
coords <- read.csv("city_coords.csv")

shinyServer(function(input, output) {
  
  # Grab ZIP code input
  center <- reactiveValues(xcoord=-117.7736, ycoord=33.67801)
  observeEvent(input$recenter, {
    center$xcoord = coords$x_cent[coords$NAME10==input$cent]
    center$ycoord = coords$y_cent[coords$NAME10==input$cent]
  })
  
  # Grab Inputs 
  options = reactiveValues(choose="Em_all_14")
  observeEvent(input$csgo, {
    type_link = switch(input$cstype, "Highest Category"="_max_", "Total"="_all_", "KIBS"="_kibs_",
                       "Creative Class"="_crtv_", "Retail"="_ret_", "High Tech"="_tech_", "Industrial"="_ind_")
    
    options$choose = paste(substr(input$cstopic,1,2), type_link, substr(input$year,3,4), sep="")
  })
  
  # Grab whether qual or quant
  options2 = reactiveValues(choose="quant")
  observeEvent(input$csgo, {
    link = switch(input$cstype, "Highest Category"="qual", "Total"="quant", "KIBS"="quant",
                  "Creative Class"="quant", "Retail"="quant", "High Tech"="quant", "Industrial"="quant")
    options2$choose = link
  })
  
  # Clearing function
  observeEvent(input$clear, {
    options$choose = "welcome"
  })
  
  finalMap <- reactive ({
    choice = dfsub[,grep(options$choose, colnames(dfsub))]
    if(input$cstype=="Highest Category"){pal <- colorFactor("RdYlBu", choice, na.color="#FFFFFF")}
    else if(input$cstopic=="Specialization" & input$cstype!="Highest Category"){pal <- colorBin("Blues", choice, bins=c(0, 0.33, 0.66, 1, 1.5, 2, 4, 15), na.color="#B0171F")}
    else if(input$cstopic=="Employment" & input$cstype!="Highest Category"){pal <- colorQuantile("Blues", choice, na.color="#B0171F", n=5)}
    else {pal <- colorFactor("blue", domain=dfsub$welcome)}
    # Create map 
    m = leaflet(sub) %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles() %>%
      addPolygons(data=sub, stroke=T, weight=1.1, fillColor = ~pal(choice), color="black", fillOpacity=0.5, 
                  opacity=1, popup=~NAME10) %>%
      addLegend("bottomleft", pal=pal, values=~choice, opacity=0.75, na.label=~paste('No', input$cstype, 'Businesses', sep=' '),
                title=~paste(input$year, input$cstype, input$cstopic, sep=" "))
  })
  
  # Generate Map Output
  output$myMap = renderLeaflet(finalMap())

 # Generate Histogram
  observeEvent(input$city!=" ", {
    output$hist <- renderPlot({
      if(input$cstype=="Highest Category"){return(NULL)}   else{ 
        par(mar=c(2.5,4,4,2))
        par(oma=c(1.5,0,0,0))
        data = dfsub[,grep(options$choose, colnames(dfsub))]
        data[is.na(data)] = 0
        q2 = max(0, as.numeric(quantile(data, 0.02)))
        q98 = min(as.numeric(quantile(data, 0.98)), 9999999)
        brks = max(2, ((max(data)-min(data))/(q98-q2))*12)
        hist(data, col="dodgerblue", breaks=brks, xlim=c(q2, q98), xlab=NULL, 
             ylab="# of Cities", border="white", main=paste(input$year, input$cstype, input$cstopic, sep=" "))
        legend("topright", c(input$city), lwd=2, box.col="white")
        if(input$cstopic=="Specialization"){
          abline(v=1, lty=2)
          legend("topright", c(input$city, "1.0"), lwd=c(2,1), lty=c(1,2), box.col="white")}
        if(input$cstopic=="Employment"){
          abline(v=mean(data, na.rm=T), lty=2)
          legend("topright", c(input$city, "Avg"), lwd=c(2,1), lty=c(1,2), box.col="white")}
        abline(v=dfsub[,grep(options$choose, colnames(dfsub))][dfsub$NAME10==input$city], lwd=2)
        mtext("Note: cities with very high or low values may not be visible.", side=1, cex=0.85, font=3, outer=TRUE)}
    })
  }) 
  
  # Add Topic Descriptions
  output$var_desc <- renderText({
    data_notes = switch(input$cstopic,
                        "Employment" = "displays each city's employment based on its percentile, compared to cities region-wide.",
                        "Specialization"= "displays the selected category's location quotient in each city. A value above 1 indicates a high concentration of that industry relative to the whole region. TOTAL cannot be selected.")
    paste("-- ", input$cstopic, data_notes, sep=" ")
  })
  
  # Add Variable Description
  output$var_desc2 <- renderText({
    data_notes = switch(input$cstype,
                        "High Tech" = "spans several industries and includes tech manufacturing.",
                        "KIBS"= "stands for Knowledge-Intensive Business Services.",
                        "Creative Class" = "employment consists of arts, entertainment, recreation, and information.",
                        "Retail" = "selected.",
                        "Industrial" = "includes manufacturing and utilities.",
                        "Total" = "cannot be selected for SPECIALIZATION",
                        "Highest Category"= "is the industry (of the 5 options shown) with the highest employment or location quotient in each city.")
    paste("-- ", input$cstype, data_notes, sep=" ")
  })
  
})
