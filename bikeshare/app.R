
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(rgdal)# readOGR
library(tidyverse)
library(tigris) # to merge spatial data with demographic information
library(httr) # http rest requests
library(jsonlite) # fromJSON
library(utils) # URLencode functions
library(sp)
library(sf)
#library(GISTools) # used to get overlapping polygons


#To-do:
# Base map: Census tracts in Pittsburgh
#Layers: 1. HealthyRide Stations
#        2. Demographic information by Census Tract
#to-do: User inputs the demographic information they want to display, and causes the shading of the polygons
# on the map to change --- done! :)
# count the number of times that there is an overlap between the polygons and the markers on top of that  -- done!
#rename the columns & select options  --- done! :)
#create percentages --- done! :)
#change the dataTable output
#edit the download button 
#update the legend  -- done! :)
# if there is time, change to shinydashboard 

#select mutiple select inputs


#add tabset panel 
#create graph with percentages of demographics on one axis and the number of bike racks available
#add the popups and labels 
#change zoom based on census tract input

#selects a census tract and the dataTable changes, and zooms to that census tract on the map

# Accessing Data and Pre-Processing ---------------------------------------------------------------------------------------------------------

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
    select("Station #","Station Name", "Latitude", "Longitude", "# of Racks") %>% 
    mutate("# of Racks" = as.factor(`# of Racks`),
           "# of Racks" = as.numeric(`# of Racks`))

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

#selects desired columns and renames columns
pitt_demog_info <-
  pitt_demog_info %>%
  select(census.tract, RACE..Total.population, RACE..Total.population..One.race..White, RACE..Total.population..One.race..Black.or.African.American, RACE..Total.population..One.race..American.Indian.and.Alaska.Native, RACE..Total.population..One.race..Asian, HISPANIC.OR.LATINO.AND.RACE..Total.population..Hispanic.or.Latino..of.any.race.) %>%
  rename("Total Population" = RACE..Total.population,
         "Number of White Residents" = RACE..Total.population..One.race..White,
         "Number of Black Residents" = RACE..Total.population..One.race..Black.or.African.American,
         "Number of American Indian Residents" = RACE..Total.population..One.race..American.Indian.and.Alaska.Native,
         "Number of Asian Residents" = RACE..Total.population..One.race..Asian,
         "Number of Hispanic/Latino Residents" = HISPANIC.OR.LATINO.AND.RACE..Total.population..Hispanic.or.Latino..of.any.race.)


#creates new columns
pitt_demog_info <- pitt_demog_info %>%
  mutate("Percentage of White Residents" = ifelse(`Number of White Residents` != 0,round((`Number of White Residents`/`Total Population`)*100, digits = 0), 0),
         "Percentage of Black Residents" = ifelse(`Number of Black Residents` != 0,round((`Number of Black Residents`/`Total Population`)*100, digits = 0), 0),
         "Percentage of American Indian Residents" = ifelse(`Number of American Indian Residents` != 0,round((`Number of American Indian Residents`/`Total Population`)*100, digits = 0), 0),
         "Percentage of Asian Residents" = ifelse(`Number of Asian Residents` != 0,round((`Number of Asian Residents`/`Total Population`)*100, digits = 0), 0),
         "Percentage of Hispanic/Latino Residents" = ifelse(`Number of Hispanic/Latino Residents` != 0,round((`Number of Hispanic/Latino Residents`/`Total Population`)*100, digits = 0), 0)
  )



#merging census shape files and demographic information
census_and_demog <- geo_join(pitt_census_tracts,
                             pitt_demog_info, by_sp = "tractce10", by_df = "census.tract", how = "left")



#counting the number of bikeshare stations in each census tract

#convert the stations data into a spatial dataframe
stations_spatial <-
  stations %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  as_Spatial()


#count the number of bikeshare stations within the census tract polygons
num.stations.per.tract <- GISTools::poly.counts(stations_spatial, census_and_demog)

#adding data about overlaps to the census and demog dataset
census_and_demog@data$num.stations.per.tract <- num.stations.per.tract




#Start creating the app --------------------------------------------------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Is Bike Share Access Equitably Distributed In Pittsburgh?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("census_tract", 
                        label = "Choose a Census Tract in Pittsburgh:",
                        choices = pitt_census_tracts$tractce10),
         
            
            #adding an action button for the census tract select option
            actionButton("addtract",
                         label = "Select Census Tract"),
            
            #select input for the demographic information that you want to look at
            selectInput("demogSelect",
                        label = "Choose the Demographic Information to Map:",
                        choices = c("Total.Population", "Number.of.White.Residents", 
                                    "Number.of.Black.Residents", "Number.of.American.Indian.Residents",
                                    "Number.of.Asian.Residents", "Number.of.Hispanic.Latino.Residents",
                                    "Percentage.of.White.Residents", "Percentage.of.Black.Residents",
                                    "Percentage.of.American.Indian.Residents",
                                    "Percentage.of.Asian.Residents",
                                    "Percentage.of.Hispanic.Latino.Residents")),
            
            #adding an action button for the demographics select option
            actionButton("add_demog",
                         label = "Click to Change Demographic Information"),
            
            #creating some visual space 
            br(), br(),
            
            #creates download button for users
            downloadButton(outputId = "downloadData",
                           label = "Download "
            )
            
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # Map Output
            leafletOutput("pittmap"),
            dataTableOutput("demog_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
         #adding basic labels to the leaflet map
 
        tract_labels <- sprintf("<strong> Census Tract: </strong><br/> %s <br/> Number of People: %g ",
                          census_and_demog$census.tract, census_and_demog$Total.Population) %>% 
                         lapply(htmltools::HTML)

         output$pittmap <- renderLeaflet({
        
               pal <- colorNumeric("YlOrRd", domain = census_and_demog$Total.Population)
       

        
      
        pitt.map <- leaflet(data = stations) %>%
            #selecting a basemap
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            addProviderTiles(provider = providers$Esri.NatGeoWorldMap, group = "NatGeo") %>% 
            #setting the view to just pittsburgh
            setView(-79.995888, 40.440624, 12)  %>% 
            addLayersControl(baseGroups = c("Google", "NatGeo")) %>% 
            addPolygons(data = census_and_demog,
                       weight = 2,
                       opacity = 1,
                        layerId = ~census_and_demog$tractce10,
                        color = ~pal(Total.Population),
                        fillOpacity = 0.7,
                        stroke = TRUE,
                        highlightOptions = highlightOptions(color = "black", 
                                                            weight = 5),
                        label = tract_labels)%>%
            # #add markers on the map for the healthy ride bike station locations
            addMarkers(~Longitude, ~Latitude, 
                       label = ~`Station Name`,
                       clusterOptions = markerClusterOptions()) %>% 
            addLegend(pal = pal, values = ~census_and_demog$Total.Population, 
                      title = "Total.Population",  position = "bottomright")
        
        pitt.map
        

      
    })
    
    

    #pitt_demog_info
    #pitt_census_tracts

    #Subset for the census tract of interest
    census_subset <- reactive({
        census_and_demog@data %>% 
           filter(census.tract == input$census_tract) %>% 
           select(census.tract, input$demogSelect)
    })
    
    
    
   #updating the map each time a new demographic select option is picked
    observeEvent(input$add_demog, {

      #change the criteria for the color palette based on demographic info input
      census_and_demog@data$select_demog <- census_and_demog@data[[input$demogSelect]]
      pal <- colorNumeric("YlOrRd", domain = census_and_demog$select_demog)
      

      leafletProxy("pittmap") %>%
       clearShapes() %>%
         addPolygons(data = census_and_demog,
                    weight = 2,
                    opacity = 1,
                    color = ~pal(select_demog),
                    fillOpacity = 0.7,
                    highlightOptions = highlightOptions(color = "black",
                                                        weight = 5))


    })

    
    #update the legend as needed
    observeEvent(input$add_demog, {

      census_and_demog@data$select_demog <- census_and_demog@data[[input$demogSelect]]
      pal <- colorNumeric("YlOrRd", domain = census_and_demog$select_demog)

      leafletProxy("pittmap", data = census_and_demog) %>%
        clearControls() %>%
        addLegend(pal = pal, values = ~select_demog, 
                  title = input$demogSelect,  position = "bottomright")
      

        

    })
    
    
    
    observeEvent(input$pittmap_shape_click, {
        #if a census tract polygon is clicked, change the zoom
        click_tract <- input$pittmap_shape_click

        leafletProxy("pittmap") %>%
         setView(lng = click_tract$lng, lat = click_tract$lat, zoom = 15)
    })

    #does something when the census tract is selected
    
    # eventReactive(input$addtract,{
    #   leafletProxy("pittmap") %>% 
    #     
    #   
    #   
    # })
    

   
   output$demog_table <- DT::renderDataTable({
       
       DT::datatable(data = census_subset(),
                     rownames = FALSE)
   })
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
