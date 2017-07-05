# PLACES # 

library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub <- readShapePoly("ACS_2015_5YR_MSA_M1")
dfsub <- data.frame(sub)
dfsub[dfsub==0] = NA

shinyServer(function(input, output) {

  # Grab Inputs 
  options = reactiveValues(choose="gini10b")
  observeEvent(input$csgo, {
    type_link = switch(input$cstopic, "Income"="gini", "Age"="ageEnt", 
                       "Education"="eduEnt", "Occupation"="occEnt")
    options$choose = paste(type_link, substr(input$year,3,4), "b", sep="")
  })
  
  finalMap <- reactive ({
    withProgress(message='Please Wait: Map Loading', {
      choice = dfsub[,grep(options$choose, colnames(dfsub))]
      pal <- colorQuantile("Blues", choice, na.color="#B0171F", n=5)
      # Create map 
      m = leaflet(sub) %>%  setView(lng=-117.7736, lat=33.67801 , zoom=4) %>% addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data=sub, stroke=T, weight=1.1, fillColor = ~pal(choice), color="black", fillOpacity=0.5, 
                    opacity=1, popup=~NAME) %>%
        addLegend("bottomleft", pal=pal, values=~choice, opacity=0.75,
                  title=~paste(input$year, input$cstopic, sep=" "))
    })
  })
  
  # Generate Map Output
  output$myMap = renderLeaflet(finalMap())
  
  # Top Five
  output$tablelabel <- renderText(paste("Top 5 metros in mixing by", input$cstopic))
  output$topfive <- renderTable({
    a <- cbind(as.character(dfsub[order(-dfsub[,grep(options$choose, colnames(dfsub))]),][2:6,5]),
               as.numeric(dfsub[order(-dfsub[,grep(options$choose, colnames(dfsub))]),][2:6,grep(options$choose, colnames(dfsub))])  )
  })
  
  # Generate Histogram
  observeEvent(input$city!=" ", {
    output$hist <- renderPlot({
        par(mar=c(2.5,4,4,2))
        par(oma=c(1.5,0,0,0))
        data = dfsub[,grep(options$choose, colnames(dfsub))]
        data[is.na(data)] = 0
        q2 = max(0, as.numeric(quantile(data, 0.02)))
        q98 = min(as.numeric(quantile(data, 0.98)), 9999999)
        brks = max(2, ((max(data)-min(data))/(q98-q2))*12)
        hist(data, col="dodgerblue", breaks=brks, xlim=c(q2, q98), xlab=NULL, 
             ylab="# of MSAs", border="white", main=paste(input$year, input$cstopic, sep=" "))
        legend("topright", c(input$msa), lwd=2, box.col="white")
          abline(v=mean(data, na.rm=T), lty=2)
          legend("topright", c(input$msa, "Avg"), lwd=c(2,1), lty=c(1,2), box.col="white")
        abline(v=dfsub[,grep(options$choose, colnames(dfsub))][dfsub$NAME==input$msa], lwd=2)
        mtext("Note: metros with very high or low values may not be visible.", side=1, cex=0.85, font=3, outer=TRUE)
    })
  }) 
})