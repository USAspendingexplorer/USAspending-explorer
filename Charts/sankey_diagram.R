

library(dplyr)
library(igraph)
library(rCharts)
library(networkD3)
library(shiny)


# Download the data, clean
gra16.3 <- readRDS(gzcon(url("https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Data/Processed/NYgra16_cleaned.rds?raw=true")))




#Function for ordering data into usable form for sankey plots, returns data frame which can be used in sanktifyPlot
#Labels all agencies below the top 10 funders as other
sankeyPrep = function( df ){
  
  # choose desired columns, filter for postive outlays only
  gra16.sankey <- df[c("maj_agency_cat", "recip_cat_type", "fed_funding_amount")]
  
  gra16.sankey <- dplyr::filter(gra16.sankey, fed_funding_amount > 0)
  
  # create a vector of the 10 most common 
  gra16.sankey.agencies.1 <- aggregate(gra16.sankey$fed_funding_amount, by= list( gra16.sankey$maj_agency_cat ), FUN = sum  )
  gra16.sankey.agencies.2 <- arrange(gra16.sankey.agencies.1, desc(x))[1:10, ]$Group.1
  
  #aggregate by agency and recipient
  gra16.sankey.2 <- aggregate(gra16.sankey$fed_funding_amount, by= list( gra16.sankey$maj_agency_cat , gra16.sankey$recip_cat_type ), FUN = sum  )
  colnames(gra16.sankey.2) <- c("maj_agency_cat", "recip_cat_type", "fed_funding_amount")
  
  #label agencies not in top 10 as other, reaggregate to collapse other category
  gra16.sankey.3 <- mutate(gra16.sankey.2 , maj_agency_cat = ifelse( maj_agency_cat %in% gra16.sankey.agencies.2 , maj_agency_cat, "Other Agencies" ) )
  gra16.sankey.4 <- aggregate(gra16.sankey.3$fed_funding_amount, by= list( gra16.sankey.3$maj_agency_cat , gra16.sankey.3$recip_cat_type ), FUN = sum  )
  
  #rename for sankey function
  colnames(gra16.sankey.4) <- c("source", "target", "value")
  
  return(gra16.sankey.4)
  
}



# The function used to create the plots
sanktify <- function(x) {
  
  x$source <- as.character( x$source )
  x$target <- as.character( x$target )
  
  # Create nodes DF with the unique sources & targets from input
  
  nodes <- data.frame(unique(c(x$source,x$target)),stringsAsFactors=FALSE)
  
  nodes$ID <- as.numeric(rownames(nodes)) - 1 # sankeyNetwork requires IDs to be zero-indexed
  names(nodes) <- c("name", "ID")
  
  # use dplyr join over merge since much better; in this case not big enough to matter
  # Replace source & target in links DF with IDs
  links <- inner_join(x, nodes, by = c("source"="name")) %>%
    rename(source_ID = ID) %>%
    inner_join(nodes, by = c("target"="name")) %>%
    rename(target_ID = ID) 
  
  # Create Sankey Plot
  sank <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source_ID",
    Target = "target_ID",
    Value = "value",
    NodeID = "name",
    units = "USD",
    fontSize = 12,
    nodeWidth = 30
  )
  
  return(sank)
  
}







ui <- fluidPage(
  
  selectInput( inputId='county', 
               label = "Pick a county",
               choices= c("NY State", sort(unique(gra16.3$county))),
               selected=c("NY State")
  ),
  
  sankeyNetworkOutput( "sankey" )
  
) # end of ui



server <- function( input, output ) {
  
  
  
  output$sankey <- renderSankeyNetwork({
    
    if (input$county == "NY State") {
      
      df <- gra16.3
      
      df.2 <- sankeyPrep(df)
      
      sanktify( df.2 )  
      
    } else {
      
      df <- dplyr::filter( gra16.3, county == input$county )
      
      df.2 <- sankeyPrep(df)
      
      sanktify( df.2 )
      
    }
    
  })
  
}

shinyApp(ui= ui , server = server)


