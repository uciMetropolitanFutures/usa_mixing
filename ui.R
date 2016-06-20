library(shiny)
library(leaflet)
library(sp)
library(maptools)

citynames = c('Adelanto', 'Agoura Hills', 'Alhambra', 'Aliso Viejo', 'Alpine', 'Altadena', 'Anaheim', 'Apple Valley', 'Arcadia', 'Artesia', 'Azusa', 'Baldwin Park', 'Banning', 'Barstow', 'Beaumont', 'Bell', 'Bell Gardens', 'Bellflower', 'Bermuda Dunes', 'Beverly Hills', 'Big Bear City', 'Big Bear Lake', 'Blythe', 'Brea', 'Buena Park', 'Burbank', 'Calexico', 'Calimesa', 'Camarillo', 'Canyon Lake', 'Carlsbad', 'Carson', 'Casa de Oro-Mount Helix', 'Cathedral City', 'Cerritos', 'Chino', 'Chino Hills', 'Chula Vista', 'Claremont', 'Coachella', 'Colton', 'Commerce', 'Compton', 'Corona', 'Coronado', 'Costa Mesa', 'Coto de Caza', 'Covina', 'Cudahy', 'Culver City', 'Cypress', 'Dana Point', 'Desert Edge', 'Desert Hot Springs', 'Diamond Bar', 'Downey', 'Duarte', 'East Hemet', 'East La Mirada', 'East Los Angeles', 'East San Gabriel', 'Eastvale', 'El Cajon', 'El Centro', 'El Monte', 'El Segundo', 'Encinitas', 'Escondido', 'Fallbrook', 'Florence-Graham', 'Fontana', 'Fountain Valley', 'French Valley', 'Fullerton', 'Garden Grove', 'Gardena', 'Glen Avon', 'Glendale', 'Glendora', 'Hacienda Heights', 'Hawaiian Gardens', 'Hawthorne', 'Hemet', 'Hermosa Beach', 'Hesperia', 'Highland', 'Home Gardens', 'Huntington Beach', 'Huntington Park', 'Imperial Beach', 'Indio', 'Inglewood', 'Irvine', 'Irwindale', 'La Cañada Flintridge', 'La Habra', 'La Mesa', 'La Mirada', 'La Palma', 'La Presa', 'La Puente', 'La Quinta', 'La Verne', 'Ladera Ranch', 'Laguna Hills', 'Laguna Niguel', 'Laguna Woods', 'Lake Elsinore', 'Lake Forest', 'Lakeside', 'Lakewood', 'Lancaster', 'Lawndale', 'Lemon Grove', 'Lennox', 'Loma Linda', 'Lomita', 'Long Beach', 'Los Alamitos', 'Los Angeles', 'Lynwood', 'Manhattan Beach', 'Maywood', 'Mead Valley', 'Menifee', 'Mira Loma', 'Mission Viejo', 'Monrovia', 'Montclair', 'Montebello', 'Monterey Park', 'Moorpark', 'Moreno Valley', 'Murrieta', 'National City', 'Newport Beach', 'Norco', 'North Tustin', 'Norwalk', 'Oak Park', 'Oceanside', 'Ontario', 'Orange', 'Oxnard', 'Palm Desert', 'Palm Springs', 'Palmdale', 'Paramount', 'Pasadena', 'Pedley', 'Perris', 'Phelan', 'Pico Rivera', 'Placentia', 'Pomona', 'Port Hueneme', 'Poway', 'Ramona', 'Rancho Cucamonga', 'Rancho Mirage', 'Rancho Palos Verdes', 'Rancho San Diego', 'Rancho Santa Margarita', 'Redlands', 'Redondo Beach', 'Rialto', 'Riverside', 'Rolling Hills Estates', 'Rosemead', 'Rowland Heights', 'Rubidoux', 'San Bernardino', 'San Buenaventura (Ventura)', 'San Clemente', 'San Diego', 'San Fernando', 'San Gabriel', 'San Jacinto', 'San Juan Capistrano', 'San Marcos', 'San Marino', 'San Pasqual', 'Santa Ana', 'Santa Clarita', 'Santa Fe Springs', 'Santa Monica', 'Santa Paula', 'Santee', 'Seal Beach', 'Sierra Madre', 'Signal Hill', 'Simi Valley', 'South El Monte', 'South Gate', 'South Pasadena', 'South San Jose Hills', 'South Whittier', 'Spring Valley', 'Temecula', 'Temple City', 'Thousand Oaks', 'Torrance', 'Tustin', 'Twentynine Palms', 'Upland', 'Valinda', 'Valle Vista', 'Victorville', 'View Park-Windsor Hills', 'Vista', 'Walnut', 'Walnut Park', 'West Athens', 'West Carson', 'West Covina', 'West Hollywood', 'West Rancho Dominguez', 'Westlake Village', 'Westminster', 'Westmont', 'Whittier', 'Wildomar', 'Willowbrook', 'Winter Gardens', 'Yorba Linda', 'Yucaipa')

shinyUI(tabPanel("Business Churning in Southern California", div(class="outer",
                                                               
        tags$head(includeCSS("styles.css")),
                                                               
        leafletOutput("map", width="100%", height="100%"),
                                                               
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                        right = 20, bottom = "auto",  width = 330, height = "auto",
                      h2("Business Churning in Southern California"),
                      textInput("zip", label=strong("Zoom to 5-digit ZIP:"), value=90012),
                      br(),
                      actionButton("recenter", label="Re-center"),
                      br(),
                      p(strong("Select Topic of Analysis:")), 
                      
                      radioButtons("analysis", label="", choices=list(" " = 5, "Employment" = 1, 
                                        "Business Establishment Churning" = 2, "Clusters based on Churning" = 3, "Churning and Growth (2000-2012)" = 4), selected = 5),
                      br(),
                      conditionalPanel("input.analysis == 1",
                                      selectInput("emp", label=strong("Select Further Options:"), choices=list("Employment in 1997", "Employment in 2000", "Employment in 2012", "Employment in 2014", "Employment Growth, 1997-2014", "Employment Growth, 2000-2012"), selected="Employment in 2014"),
                                      actionButton("empgo", label="Go/Refresh")),
                      
                      conditionalPanel("input.analysis == 2",
                                       selectInput("churn", label=strong("Select Further Options:"), choices=list("Churning, 1997-2014", "Churning, 2000-2012"), selected="Churning, 1997-2014"),
                                       actionButton("churngo", label="Go/Refresh")),
                    
                      conditionalPanel("input.analysis == 3", 
                                      selectInput("clust", label=strong("Select Further Options:"), choices=list("Socioeconomically-derived Clusters", "Spatially-derived Clusters"), selected="Socioeconomically-derived Clusters"),
                                      actionButton("clustgo", label="Go/Refresh"),
                                      actionButton("clear", label="Clear Results")),
                      
                      conditionalPanel("input.analysis == 4",
                                      selectInput("grow", label=strong("Select Further Options:"), choices=list("Churning and Job Growth", "Churning and Income Growth", "Churning and Home Value Growth"), selected="Churning and Job Growth"),
                                      actionButton("growgo", label="Go/Refresh")),
                                      
                      conditionalPanel("input.analysis != 5 | input.analysis != 3",
                                       selectInput("city", label=em("Select City to display churn, 1997-2014:"), selected="Los Angeles", choices=citynames),
                                       actionButton("histgo", label="Generate Histogram")),
                      
                      plotOutput("hist", height = 225)
                      ),
    
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE, draggable=TRUE, top=110, left=10, right="auto",
                        bottom="auto", width=150, height="auto",
                      p("Data Notes:"),
                      h6("-- Map data are presented in census tracts (2010 boundaries). The histogram reports the total values for all tracts in a city (not available for clusters)."),
                      h6(textOutput("var_desc")),
                      h6("Please see the full MFI report for details.")
                      )
           
)))