
library(igraph)
library(rCharts)
library(networkD3)
library(censusapi)
library(plotly)
library( shiny )
library( dplyr )
# Download the data, clean
# from RDS
gra16.3_url <- "https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Data/Processed/NYgra16_cleaned.rds?raw=true"
gra16.3 <- readRDS( gzcon( url( gra16.3_url )))
#Load the population data
population <- readRDS(gzcon(url("https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Data/Processed/NYcensus.rds?raw=true")))

# Aggregation function
agg.county <- function(df , var){
  
  ag <- aggregate(df$fed_funding_amount, by= list( df$county, var), FUN = sum  )
  
  colnames(ag) <- c("county", "var", "fund")
  
  return(ag)
}




# Per capita aggregation function
agg.county.percap <- function(df, df.p , var){
  
  ag <- aggregate(df$fed_funding_amount, by= list( df$county, var), FUN = sum  )
  
  colnames(ag) <- c("county", "var", "fund")
  
  ag.pop <- merge(ag , df.p, by.x = "county", by.y = "county.name")
  
  ag.pop.2 <- mutate(ag.pop , percap =  fund / Pop )
  
  ag.pop.3 <- ag.pop.2[c("var", "fund", "percap", "county")]
  
  return(ag.pop.3)
}




# Per capita aggregation function, statewide
agg.percap <- function(df, df.p, var){
  
  ag <- aggregate(df$fed_funding_amount, by= list( var), FUN = sum  )
  
  colnames(ag) <- c("var", "fund")
  
  ag.2 <- mutate(ag , county = "NY Average")
  
  ag.per <- mutate(ag.2 , percap = fund / (sum(df.p$Pop)))
  
  return(ag.per)
}




# National Aggregation

ny.grp <- mutate(gra16.3 , assistance_type.2 = ifelse( assistance_type == "04: Project grant", "Project Grants" , "Other Grants" ) )

ny.per <- agg.percap(ny.grp , population , ny.grp$assistance_type.2) #Function

colnames(ny.per)[1] <- "assistance_type.2"

ny.per.2 <- ny.per[c("assistance_type.2", "fund", "percap", "county")]



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



# The function used to create sankey plots
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

