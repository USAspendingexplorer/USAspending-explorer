# USAspending-explorer

USAspending Explorer is an open source tool designed to assist in the navigation of federal data made publicly available through usaspending.gov. This explorer serves as a tool to understand federal funding at the county level. It focuses on federal assistance in the form of grants and excludes grants that go directly to the state government because of the complexity of determining the final recipient of those funds.

With USAspending Explorer, you can understand how tax dollars are spent, explore who receives federal grants in your area, and see what that funding is directed towards. This tool shows total funding going to each county, and how funds are distributed geographically within the state adjusted by population. It also offers flexible comparisons between counties and the state average, displays county demographic information to inform county comparisons, and breaks down county funding by the funding agency, recipient, and program to better understand what is driving variations across counties.

Explore the our GitHub Repository to find the code for producing the visualizations and running the shiny app.


## To launch this application use the following commands in R:

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


## Screenshots of USAspending-explorer tabs:

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.57.19%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.58.06%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.58.38%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.58.52%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.59.10%20PM.png)

![](https://github.com/USAspendingexplorer/USAspending-explorer/raw/master/Images/Screenshots/Screen%20Shot%202017-06-16%20at%202.59.43%20PM.png)


## Collaborators:

* Cristian Nuno (https://github.com/cenuno)
* Ignacio Pezo (https://github.com/icps86)
* Linnea Powell (https://github.com/lpowell12)
* Stephanie Wilcoxen (https://github.com/swilcoxen)


*Last updated on June 16, 2017*
