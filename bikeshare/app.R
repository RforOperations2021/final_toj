
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)

#To-do:
# Base map: Census tracts in Pittsburgh
#Layers: 1. HealthyRide Stations
#        2. Demographic information by Census Tract



#Loading in the Pittsburgh HealthyRide Stations data using WPRDC API
# URL Encode the query
q <- 'SELECT * FROM "395ca98e-75a4-407b-9b76-d2519da28c4a"'
formatQuery <- URLencode(q, repeated = TRUE)
# Build URL for GET request
url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=", formatQuery)
# Run Get Request
g <- GET(url)
stations <- fromJSON(content(g, "text"))$result$records

#subsetting data to desired columns
stations <- stations %>% 
    select("Station #","Station Name", "Latitude", "Longitude", "# of Racks")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Micro-Transit Access in Pittsburgh"),

    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),

        # Show a plot of the generated distribution
        mainPanel(
            # Map Output
            leafletOutput("pitt"),
            dataTableOutput("stations_table")
        )
    )
#)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$pitt <- renderLeaflet({

        pitt.map <- leaflet() %>%
            #selecting a basemap
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            addProviderTiles(provider = providers$Esri.NatGeoWorldMap, group = "NatGeo") %>% 
            #setting the view to just pittsburgh
            setView(-79.995888, 40.440624, 12)  %>% 
            addLayersControl(baseGroups = c("Google", "NatGeo"))

        pitt.map
    })
    
   output$stations_table <- DT::renderDataTable({
       
       DT::datatable(data = stations,
                     rownames = FALSE)
   })
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
