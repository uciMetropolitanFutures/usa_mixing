library(shiny)
library(leaflet)
library(sp)
library(maptools)

ch <- readShapePoly("trt10_churning_selected")
dfch <- data.frame(ch)
city <- read.csv("city_churn_fast.csv")
zips <- read.csv("ZIP_centroids.csv")

shinyServer(function(input, output) {
  
  # Grab ZIP code input
  center <- reactiveValues(xcoord=-118.2386, ycoord=34.06583)
  observeEvent(input$recenter, {
    center$xcoord = zips$x_centr[zips$CODE==input$zip]
    center$ycoord = zips$y_centr[zips$CODE==input$zip]
  })
 
  # Grab Inputs - ALL
  options = reactiveValues(choose="ones")
  observeEvent(input$empgo, {
    emplink = switch(input$emp, "Employment in 1997"="1997", "Employment in 2000"="2000", "Employment in 2012"="12",  
                     "Employment in 2014"="14", "Employment Growth, 1997-2014"="9714", "Employment Growth, 2000-2012"="0012")
    options$choose = paste("totemp", emplink, sep="")
  })
  observeEvent(input$churngo, {
    churnlink = switch(input$churn, "Churning, 1997-2014"="9714", "Churning, 2000-2012"="0012")
    options$choose = paste("churn", churnlink, sep="")
  })
  observeEvent(input$clustgo, {
    clustlink = switch(input$clust, "Socioeconomically-derived Clusters"="fac2", "Spatially-derived Clusters"="LISA")
    options$choose = paste("clust", clustlink, sep="")
  })
  observeEvent(input$growgo, {
    growlink = switch(input$grow, "Churning and Income Growth"="inc", "Churning and Job Growth"="job", "Churning and Home Value Growth"="hov")
    options$choose = paste("grow", growlink, sep="")
  })
  observeEvent(input$clear, {
    options$choose = "ones"
  })
  
  # Reactive function to generate a color palette based on the variable chosen
  colorpal <- reactive({
    datause = dfch[,grep(options$choose, colnames(dfch))]
    if(input$analysis == 3 | input$analysis == 5){colorpal <- colorFactor("RdYlBu", datause, na.color="#FFFFFF")}
    else if(input$analysis == 4) {colorpal <- colorBin("RdYlBu", datause, bins=c(-1, -0.2, -0.1, 0, 0.1, 0.2, 1), na.color="#B0171F")}
    else if(input$analysis == 2){colorpal <- colorBin("Blues", datause, bins=5, na.color="#B0171F")}
    else if(input$analysis == 1 & (input$emp == "Employment Growth, 2000-2012" | input$emp == "Employment Growth, 1997-2014")) {colorpal <- colorBin("RdYlBu", datause, bins=c(-55000,-2000,-500,0,500,2000,70000), na.color="#FFFFFF")}
    else {colorpal <- colorBin("Blues", datause, bins=c(0,750,2000,5000,15000,100000), na.color="#FFFFFF")}
  })
  
  # Generate the basemap
  output$map <- renderLeaflet({
    leaflet(ch) %>% setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles()
  })
  
  # Observe function to add polygons and legend to basemap based on color palette 
  observe({
    pal <- colorpal()
    datause <- dfch[,grep(options$choose, colnames(dfch))]
    leafletProxy("map") %>% clearControls() %>% clearShapes() %>% 
      addPolygons(data=ch, stroke=T, weight=1, fillColor = ~pal(datause), color="black",
                  fillOpacity=0.6, opacity=1, popup=~NAME10) %>%
      addLegend("bottomleft", pal=pal, values=datause, opacity=0.75, title=options$choose)
    })
  
  # Generate Histogram
  observeEvent(input$histgo, {   ### NEED TO FIX THIS
    output$hist <- renderPlot({
      if(input$analysis == 5 | input$analysis == 3){return(NULL)}   else{ 
        datause = city$churn9714
        hist(datause, xlab=NULL, breaks=30, col="dodgerblue", xlim=c(0.2, 0.7),
             ylab="# of SoCal Cities", border="white", main="Churning, 1997-2014")
        legend("topright", c(input$city), lwd=2, box.col="white")
        abline(v=mean(datause, na.rm=T), lty=2)
        legend("topright", c(input$city, "Avg"), lwd=c(2,1), lty=c(1,2), box.col="white")
        abline(v=city$churn9714[city$NAME10==input$city], lwd=2)
        }
    })
  })
  
  # Add Descriptions
  output$var_desc <- renderText({
    data_notes = switch(input$analysis,
                        "1" = "Employment is derived from ReferenceUSA and shows the number of jobs in each tract. Employment growth shows the net increase/decrease in job count per tract (not a percent).",
                        "2" = "Churning is calculated as the (annualized) sum of business establishment births and deaths divided by total employment. It is a measure of local economic turnover.",
                        "3" = "Socioeconomic clusters group similarly-churning tracts based on things like income, race, and homeownership into 6 categories. Spatial clusters identify hotspots where nearby tracts display similar levels of churn. You must click CLEAR RESULTS before switching to another analysis.",
                        "4" = "These are results from a geographically-weighted regression showing how the relationship between churning and growth varies over space. In some areas, churning is associated with growth but in other areas it is associated with decline.")
    paste("-- ", data_notes, sep="")
  })
  
  
 
})