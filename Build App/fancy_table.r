#
# Author:   Cristian Nuno
# Date:     June 15, 2017
# Purpose:  Creating a datatable for New York counties aggregated federal grant funding 
#           and aggregated per capita federal grant funding
#
#
# Load necessary packages
library( DT )
library( magrittr )
library( dplyr )

# select a few columns from the ny_counties@data data frame
# note that this data frame comes from the ny_county_codes.geojson file
# available here: https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Data/Raw/ny_county_code.geojson

ny_counties_agg <- select( ny_counties@data
                           , county_name
                           , federal_funding
                           , funding_per_capita
)

# Create the data table
fancy_table <- datatable( ny_counties_agg[ order( -ny_counties_agg$federal_funding ), ]
                          , rownames = as.character(c(1:62))
                          , caption = htmltools::tags$caption(
                            style = 'caption-side: top; text-align: left;'
                            , div("Table is ordered from most to least total federal grant funding received in FY16")
                          )
                          , colnames = c("County Name", "Total Federal Grant Funding Received", "Total Per Capita Federal Grant Funding Received")
                          , options = list(lengthChange = FALSE)
                          ) %>% # end of creating datatable
  formatCurrency(columns = c("federal_funding", "funding_per_capita"))
