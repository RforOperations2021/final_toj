
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(rgdal)# readOGR
library(tidyverse)

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

#changing the datatypes for some of the columns in the stations dataset
options(digits=6)

stations <- stations %>% 
    mutate(Longitude = as.numeric(Longitude),
           Latitude = as.numeric(Latitude),
           `# of Racks` = as.numeric(`# of Racks`))

#loading in the shapefile with Pittsburgh neighborhoods
pitt_census_tracts <- rgdal::readOGR("~/Documents/GitHub/final_toj/2010_Census_Tracts.geojson")

pitt_demog <- read.csv("~/Documents/GitHub/final_toj/pitt_demographics_by_census_tract.csv")
demog_filtered <-
    pitt_demog %>%
    rename(census.tract = Label) %>%
    filter(grepl("Census Tract", census.tract) | grepl("Estimate", census.tract)) %>%  #filtering to only include rows containing the words "census tract"
    mutate_at(-1, ~lead(., 1))

#now that the data has been shifted up by one row, remove the empty row
toDelete <- seq(0,nrow(demog_filtered),2)

#remove the undesired rows from the dataframe
demog_filtered <- demog_filtered[-toDelete,]

#update the formatting of the census tract column to match the shapefile data 
demog_filtered <- demog_filtered %>%
    mutate(census.tract = as.character(census.tract),
           census.tract = parse_number(census.tract),
           census.tract = round(census.tract, digits = 0),
           census.tract = as.character(census.tract),
           census.tract = ifelse(nchar(census.tract) == 3, paste0("0", census.tract, "00"), paste0(census.tract, "00")))

#changes the census tract column in the shapefile dataset to be a character
pitt_census_tracts$tractce10 <- as.character(pitt_census_tracts$tractce10)


#only include the census tracts for pittsburgh by filtering based on the pittsburgh census info
pitt_demog_info <-
    demog_filtered %>%
    filter(census.tract %in% pitt_census_tracts$tractce10)

#getting rid of empty "RACE" column
pitt_demog_info <- pitt_demog_info[,-2]

#changes demographic info columns to numbers instead of characters
pitt_demog_info <- pitt_demog_info %>%
    mutate_if(is.factor, as.character) %>%
    mutate_all(funs(str_replace(., ",", ""))) %>%
    mutate_at(names(pitt_demog_info)[-1], as.numeric)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bike Share Access in Pittsburgh"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # Map Output
            leafletOutput("pitt_map"),
            dataTableOutput("demog_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$pitt_map <- renderLeaflet({

        pitt.map <- leaflet(data = stations) %>%
            #selecting a basemap
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            addProviderTiles(provider = providers$Esri.NatGeoWorldMap, group = "NatGeo") %>% 
            #setting the view to just pittsburgh
            setView(-79.995888, 40.440624, 12)  %>% 
            addLayersControl(baseGroups = c("Google", "NatGeo")) %>% 
            addPolygons(data = pitt_census_tracts,
                        weight = 2,
                        color = "green",
                        stroke = TRUE,
                        highlightOptions = highlightOptions(color = "black", weight = 5) ) %>% 
            #add markers on the map for the healthy ride bike station locations
            addMarkers(~Longitude, ~Latitude)#, clusterOptions = markerClusterOptions())
            
        # observe({
        #     boros <- boroInputs()
        #     
        #     leafletProxy("leaflet", data = boros) %>%
        #         # In this case either lines 107 or 108 will work
        #         # clearShapes() %>%
        #         clearGroup(group = "boros") %>%
        #         addPolygons(popup = ~paste0("<b>", boro_name, "</b>"), group = "boros", layerId = ~boro_code, fill = FALSE, color = "green") %>%
        #         setView(lng = boros$x[1], lat = boros$y[1], zoom = 9)
        # })

        pitt.map
    })
    
    
    
    
   #leafletProxy("pitt", data = stations) 
   
   output$demog_table <- DT::renderDataTable({
       
       DT::datatable(data = pitt_demog_info,
                     rownames = FALSE)
   })
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
