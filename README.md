# USAspending-explorer

This application runs a dashboard to explore federal grant spending in FY 2016.

You can launch this application from R using the following commands:

```
devtools::install_github("rstudio/leaflet")

devtools::install_github("ramnathv/rCharts")

install.packages( c( "ggthemes", "shiny", "shinydashboard", "geojsonio"
                    , "magrittr", "scales", "htmltools", "htmlwidgets", "DT"
                    , "dplyr", "stringr", "stringi", "MatchIt", "plotly", "censusapi"
                    , "ggplot2", "igraph", "networkD3", "pander" 
                    ) )

library( shiny )

shiny::runGitHub(“USAspendingexplorer/USAspending-explorer”)
```

*Last updated on June 16, 2017*
