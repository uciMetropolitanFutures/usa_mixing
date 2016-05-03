library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(rsconnect)

tr <- readOGR("OC_tr.json", "OGRGeoJSON")
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

  m = leaflet() %>%  setView(lng=-117.8414, lat=33.647 , zoom=12) %>% addTiles() %>%
    addPolylines(data=tr, stroke=TRUE, weight=0.75, color="black", fill=FALSE)
  
  finalMap <- reactive ({
    
    #xcoord = zips$x_centr[zips$CODE==input$zip]
    #ycoord = zips$y_centr[zips$CODE==input$zip]
    #if(input$recenter) return(m %>% setView(lng=xcoord, lat=ycoord, zoom=14)) 
    
    if(input$variable=="Age" & input$year=="1990") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Greys", ageEnt90)(ageEnt90)))
    if(input$variable=="Age" & input$year=="2000") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Greys", ageEnt00)(ageEnt00)))
    if(input$variable=="Age" & input$year=="2012") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Greys", ageEnt12)(ageEnt12)))
    
    if(input$variable=="Race" & input$year=="1990") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("YlOrRd", raceEnt90)(raceEnt90)))
    if(input$variable=="Race" & input$year=="2000") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("YlOrRd", raceEnt00)(raceEnt00)))
    if(input$variable=="Race" & input$year=="2012") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("YlOrRd", raceEnt12)(raceEnt12)))
    
    if(input$variable=="Income" & input$year=="1990") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("RdGy", incEnt90)(incEnt90)))
    if(input$variable=="Income" & input$year=="2000") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("RdGy", incEnt00)(incEnt00)))
    if(input$variable=="Income" & input$year=="2012") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("RdGy", incEnt12)(incEnt12)))
    
    if(input$variable=="Education" & input$year=="1990") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Blues", eduEnt90)(eduEnt90)))
    if(input$variable=="Education" & input$year=="2000") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Blues", eduEnt00)(eduEnt00)))
    if(input$variable=="Education" & input$year=="2012") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Blues", eduEnt12)(eduEnt12)))
    
    if(input$variable=="Housing Type" & input$year=="1990") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Greys", htEnt90)(htEnt90)))
    if(input$variable=="Housing Type" & input$year=="2000") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Greys", htEnt00)(htEnt00)))
    if(input$variable=="Housing Type" & input$year=="2012") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("Greys", htEnt12)(htEnt12)))
    
    if(input$variable=="Housing Age" & input$year=="1990") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("BrBG", homeEnt90)(homeEnt90)))
    if(input$variable=="Housing Age" & input$year=="2000") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("BrBG", homeEnt00)(homeEnt00)))
    if(input$variable=="Housing Age" & input$year=="2012") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("BrBG", homeEnt12)(homeEnt12)))
    
    if(input$variable=="Land Use" & input$year=="1990") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("PuOr", LUent9)(LUent9)))
    if(input$variable=="Land Use" & input$year=="2000") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("PuOr", LUent0)(LUent0)))
    if(input$variable=="Land Use" & input$year=="2012") return(m %>% addPolygons(data=tr, stroke=F, color = ~colorQuantile("PuOr", LUent12)(LUent12)))
    
    else return (m)
  })
  
  output$myMap = renderLeaflet(finalMap())
  
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

