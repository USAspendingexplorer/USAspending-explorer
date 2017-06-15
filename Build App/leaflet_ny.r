#
# Author:   Cristian Nuno
# Date:     June 7, 2017
# Purpose:  Using Leaflet for FY16 NY Federal Spending
#
# Load necessary packages
library( leaflet )
library( magrittr )
library( htmltools )
library( htmlwidgets )
library( geojsonio )
library( scales )

# Build the map
# Load necessary data frames
geojson_url <- "https://raw.githubusercontent.com/USAspendingexplorer/USAspending-explorer/master/Data/Raw/ny_county_code.geojson"
ny_counties <- geojson_read( geojson_url, what = "sp" )

# define the the breaks
bins <- c(0, 2500000, 5500000, 10000000
          , 20000000, 60000000, 250000000, Inf)
# per capita breaks
bin_pc <- c( 0, 50 , 75, 100
          , 150, 250, 400, Inf)

# Design a color palette for the map
pal <- colorBin( palette = "YlGnBu"
                 , domain = ny_counties$federal_funding
                 , bins = bins
)
pal_pc <- colorBin( palette = "YlGnBu"
                    , domain = ny_counties$funding_per_capita
                    , bins = bin_pc
)
# Make hover layer
labels_fed_fund <- sprintf(
  "<strong>%s</strong><br/>%s dollars",
  ny_counties$name, dollar(ny_counties$federal_funding)
) %>% lapply(htmltools::HTML)
labels_fed_fund_pc <- sprintf(
  "<strong>%s</strong><br/>%s dollars",
  ny_counties$name, dollar(ny_counties$funding_per_capita)
) %>% lapply(htmltools::HTML)
# make map
ny_map <- leaflet(ny_counties) %>%
  # set zoom level
  setView( lng = -76.126197, lat = 43.034706
           , zoom = 7) %>%
  # set max bounds view to cover the state of New York
  setMaxBounds( lng1 = -72, lat1 = 46
                , lng2 = -80, lat2 = 40  ) %>% 
  # add Total FY16 Federal Grant Spending Polygons
  addPolygons( smoothFactor = 0.2
               , fillOpacity = 0.7
               , fillColor = ~pal(ny_counties$federal_funding)
               , color = "white"
               # Whenever someone hovers their mouse above a polygon,
               # make sure they know which community they are in
               #, popup = dollar( update_map$federal_funding )
               , label = labels_fed_fund
               , labelOptions = labelOptions(style = list("font-weight" = "normal"
                                                          , padding = "3px 8px")
                                             , textsize = "15px", direction = "auto"
               )
               , highlightOptions = highlightOptions( color = "orange"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Total Spending"
  ) %>%
  # add Per Capita Total FY16 Federal Grant Spending Polygons
  addPolygons( smoothFactor = 0.2
               , fillOpacity = 0.7
               , fillColor = ~pal_pc(ny_counties$funding_per_capita)
               , color = "white"
               # Whenever someone hovers their mouse above a polygon,
               # make sure they know which community they are in
               #, popup = dollar( update_map$federal_funding )
               , label = labels_fed_fund_pc
               , labelOptions = labelOptions(style = list("font-weight" = "normal"
                                                          , padding = "3px 8px")
                                             , textsize = "15px", direction = "auto"
               )
               , highlightOptions = highlightOptions( color = "orange"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Per Capita Spending"
  ) %>%
  # add Layers control
  addLayersControl( baseGroups = c("Total Spending", "Per Capita Spending")
                    , options = layersControlOptions(collapsed = FALSE)
                  # , position = "topright"
  ) %>%
  # add background to map
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  # add mini map
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap
    , toggleDisplay = TRUE
    , minimized = TRUE
  ) %>%
  # add zoom out button
  addEasyButton( easyButton(
    icon = "ion-android-globe", title = "Zoom Back Out"
    , onClick = leaflet::JS("function(btn, map){ map.setZoom(7); }")
  ) )
