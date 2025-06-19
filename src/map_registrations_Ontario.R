###########################################################################
###
### Interactive Map of Zero-Emission Vehicle Registrations for Ontario
###
### This Shiny application displays an interactive map of ZEV registrations
### by census tract in Ontario, allowing users to switch between viewing
### the total number of registrations or registrations per household.
###
##########################################################################


library(dplyr)
library(ggplot2)    
library(here)
library(htmltools)
library(leaflet)
library(sf)
library(shiny)
library(readr)

# Read Census Data (GeoJSON)
census_df <- st_read(here("data", "census_df.geoJSON"), quiet = TRUE) %>%
  na.omit() %>% # Remove rows with any NA values
  dplyr::select(Households, CTUID, geometry) # Select only necessary columns

# Ensure correct data types for columns used in calculations and joins
census_df$Households <- as.numeric(census_df$Households)
census_df$CTUID <- as.character(census_df$CTUID)

# Read ZEV Fleet Data (CSV)

fleet_df <- read_csv(here("data", "CT_ZEV_Fleet_ON.csv"), show_col_types = FALSE) %>%
  as.data.frame() %>% # Convert to data.frame for consistent handling before spatial join
  na.omit() %>%       # Remove rows with any NA values
  filter(year == 2021 & fuel_type == "ZEV")

# Ensure CTUID is formatted as character for accurate joining with census data
fleet_df$CTUID <- as.character(fleet_df$CTUID)

#  Join Datasets and Calculate Derived Variables
ZEV_sf <- right_join(fleet_df, census_df, by = "CTUID") %>%
  st_as_sf() %>% # Convert the joined data frame back to an sf object
  na.omit()      # Remove any rows that might have become NA during the join process

# Calculate the key metric: ZEV registrations per household
ZEV_sf$ZEV_perHouse <- ZEV_sf$value / ZEV_sf$Households

message("Data loading and processing complete.")


ui <- fluidPage(
  
  titlePanel("Zero-Emission Vehicle Registrations in Ontario, 2021"),
  
  # Use a sidebar layout with a control panel on the left and the map on the right
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu (selectInput) for the user to choose which variable to visualize
      selectInput("dataset",
                  label = "Choose a Variable:",
                  choices = c("Number of registrations",
                              "Registrations per household"))
    ),
    
    mainPanel(
      leafletOutput(outputId = "MyMap", height = 600)
    )
  )
)


server <- function(input, output) {
  
  variableInput <- reactive({
    req(input$dataset)
    switch(input$dataset,
           "Number of registrations" = ZEV_sf$value,
           "Registrations per household" = ZEV_sf$ZEV_perHouse
    )
  })
  
  binsInput <- reactive({
    req(input$dataset)
    switch(input$dataset,
           "Number of registrations" = c(0, 20, 40, 60, 80, Inf),
           "Registrations per household" = c(0, 0.02, 0.04, 0.06, Inf)
    )
  })
  
  titleInput <- reactive({
    req(input$dataset)
    switch(input$dataset,
           "Number of registrations" = "Number of Registrations",
           "Registrations per household" = "Registrations per Household"
    )
  })
  

  output$MyMap <- renderLeaflet({
    

    Pal <- colorBin("Reds", domain = variableInput(), bins = binsInput())
    
    Labels <- sprintf("%s", prettyNum(variableInput(), big.mark = ",")) %>%
      lapply(htmltools::HTML)

    leaflet(ZEV_sf) %>%
      # Add a base map tile layer (e.g., CartoDB.Positron provides a light, clean base map)
      addProviderTiles(providers$CartoDB.Positron) %>%
      # Add polygons representing census tracts
      addPolygons(
        fillColor = ~Pal(variableInput()), # Fill color based on the selected variable
        color = "white",                   # Border color for polygons
        weight = 1,                        # Thickness of the polygon borders
        opacity = 1,                       # Opacity of the polygon borders
        fillOpacity = 0.65,                # Opacity of the polygon fill color
        
        # `highlightOptions` define the visual effect when a user hovers over a polygon
        highlightOptions = highlightOptions(
          color = "grey",         # Change border color on hover
          weight = 3,             # Make border thicker on hover
          bringToFront = TRUE     # Ensure the hovered polygon is on top
        ),
        
        # `label` (CTUID or selected variable) content that appears when hovering over a polygon
        # It displays the CTUID and the value of the selected variable.
        label = ~paste0("CTUID: ", CTUID, "<br/>", titleInput(), ": ", Labels),
        # `labelOptions` customize the appearance and behavior of the labels
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto" # Automatically position the label
        )
      ) %>%
      # Add a legend to the map, explaining the color scale
      addLegend(pal = Pal, values = variableInput(), opacity = 0.9,
                title = titleInput(), # Legend title reflects the selected variable
                position = "bottomright") # Position the legend on the map
  })
}


shinyApp(ui = ui, server = server)

