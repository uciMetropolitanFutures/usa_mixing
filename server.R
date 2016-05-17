library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(maptools)
#library(rsconnect)

#tr <- readOGR("OC_tr.json", "OGRGeoJSON")
tr <- readShapePoly("OC_tr")
dftr = data.frame(tr)
zips <- read.csv("ZIP_centroids.csv")

descr = data.frame(c("Age", "Race", "Income", "Education", "Housing Type", "Housing Age", "Land Use"),
                   c("mixing is derived from four categories from the US Census: 0-19, 20-34, 35-64, and 65+ years.",
                     "mixing is derived from five categories from the US Census: white, black, hispanic, asian, and other/mixed/undefined.",
                     "mixing is from five categories of household median annual income from the US Census: <$15k, $15k-$35k, $35k-$75k, $75k-$150k, >$150k",
                     "mixing is derived from 5 categories of education levels for people above 25 from the US Census: no high school diploma, high school diploma, some college, Bachelor's degree, graduate degree.",
                     "mixing, from the US Census, represents single-family attached, single-family detached, multifamily, and other unit types",
                     "mixing is derived from the US Census' measure of housing age and is in 4-5 (approximately) 20-year increments depending on the year selected above.",
                     "mixing is from the Southern California Association of Governments (SCAG): single-family residential, multifamily residential, commercial, industrial, and vacant/open space."))
colnames(descr) = c("var", "explain")


shinyServer(function(input, output) {

  # Grab ZIP code input
  center <- reactiveValues(xcoord=-117.8414, ycoord=33.647)
  observeEvent(input$recenter, {
    center$xcoord = zips$x_centr[zips$CODE==input$zip]
    center$ycoord = zips$y_centr[zips$CODE==input$zip]
  })
  
  # Grab Variable and Year input
  options = reactiveValues(choose="ageEnt90")
  observeEvent(input$go, {
    name_link = switch(input$variable, "Age"="ageEnt", "Race"="raceEnt", "Income"="incEnt", 
                       "Education"="eduEnt", "Housing Type"="htEnt", "Housing Age"="homeEnt", "Land Use"="LUent")
    options$choose = paste(name_link, substr(input$year,3,4), sep="")
  })
  
  finalMap <- reactive ({
    # Define values to list as "choice"  
    choice = dftr[,grep(options$choose, colnames(dftr))]
    # Create map as 'm'
    m = leaflet(tr) %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=12) %>% addTiles() %>%
    addPolylines(stroke=TRUE, weight=0.75, color="black", fill=FALSE) %>%
    addPolygons(stroke=F, color = ~colorQuantile("Greys", choice)(choice))
  })
  
  # Generate Map Output
  output$myMap = renderLeaflet(finalMap())
  
  # Add Variable Descriptions
  output$var_desc <- renderText({
    data_link = switch(input$variable,
                       "Age" = descr$explain[descr$var=="Age"],
                       "Race"= descr$explain[descr$var=="Race"],
                       "Income"= descr$explain[descr$var=="Income"],
                       "Education"= descr$explain[descr$var=="Education"],
                       "Housing Type"= descr$explain[descr$var=="Housing Type"],
                       "Housing Age"= descr$explain[descr$var=="Housing Age"],
                       "Land Use"= descr$explain[descr$var=="Land Use"]
                       )
    paste(input$variable, data_link)
  })
  
})

