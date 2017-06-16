# USAspending-explorer

This application runs a dashboard to explore federal grant spending in FY 2016.

You can launch this application from R using the following commands:

```R
devtools::install_github("rstudio/leaflet")

devtools::install_github("ramnathv/rCharts")

install.packages( c( "ggthemes", "shiny", "shinydashboard", "geojsonio"
                    , "magrittr", "scales", "htmltools", "htmlwidgets", "DT"
                    , "dplyr", "stringr", "stringi", "MatchIt", "plotly", "censusapi"
                    , "ggplot2", "igraph", "networkD3", "pander" 
                    ) )

library( shiny )

shiny::runGitHub("USAspendingexplorer/USAspending-explorer")
```

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.57.19%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.58.06%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.58.38%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.58.52%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.59.10%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.59.43%20PM.png)

*Last updated on June 16, 2017*
