#
# Author:   Linnea Powell, Stephanie Wilcoxen, 
#           Ignacio Pezo, and Cristian Nuno
# Purpose:  Draft Dashboard
#

# Load necessary packages
library( ggthemes)
library( shiny )
library( shinydashboard )
library( leaflet )
library( geojsonio )
library( magrittr )
library( scales )
library( htmltools )
library( htmlwidgets )
library( DT )
library( dplyr )
library( stringr )
library( stringi )
library( MatchIt )
library( plotly )
library( censusapi )
library( ggplot2 )
library( igraph )
library( networkD3 )
library( rCharts )
library( pander )

# Import data from github function
source_github <- function( url ) {
  # load package
  require(RCurl)
  
  # read script lines from website and evaluate
  script <- getURL(url, ssl.verifypeer = FALSE)
  eval(parse(text = script), envir=.GlobalEnv)
} 

#################################################################
######################## Building the Server ####################
##########aka the Infrastructure of the User Interface ##########
#################################################################
server <- function(input, output) {
  ######################################
  #### State Overview Leaflet Output####
  ######################################
  
  #### If statement for Dynamic Leaflet Legend ####
  observeEvent(input$mymap_groups,{
    
    mymap <- leafletProxy("mymap") %>% clearControls()
    
    if (input$mymap_groups == "Total Spending")
    {mymap <- mymap %>% addLegend("bottomleft"
                                  , pal = pal # use the same color palette we made earlier
                                  , values = ny_counties$federal_funding # assign values to the legend
                                  , title = "Total Federal Grant Spending"
                                  , labFormat = labelFormat(prefix = "$")
                                  , opacity = 1
    )} # end of if statement
    else if (input$mymap_groups == "Per Capita Spending")
    {mymap <- mymap %>% addLegend("bottomleft"
                                  , pal = pal_pc # use the same color palette we made earlier
                                  , values = ny_counties$funding_per_capita # assign values to the legend
                                  , title = "Per Capita Federal Grant Spending"
                                  , labFormat = labelFormat(prefix = "$")
                                  , opacity = 1
    ) } # end of else if statement
  }
  ) # end of observe event
  
  # Render the map
  output$mymap <- renderLeaflet({
    ny_map
    
  }) # end of render map
  
  #### State Overview Datatable Output ####
  # Render the data table
  output$tbl <- DT::renderDataTable({
    fancy_table
    
  }) # end of render datatable
  
  
  
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
  
  output$top <- renderInfoBox({
    
    
    if (input$county == "NY State") {
      
      top.rec <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name), FUN = sum)
      
      top.rec.2 <- top.rec
      top.rec.3 <- arrange(top.rec.2 , desc(x))
      top <- top.rec.3[1,]$Group.1
      
    } else {
      
      top.rec <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name, gra16.3$county), FUN = sum)
      
      top.rec.2 <- filter(top.rec , Group.2 == input$county)
      top.rec.3 <- arrange(top.rec.2 , desc(x))
      top <- top.rec.3[1,]$Group.1
      
    }
    
    infoBox(
      "Top Recipient", paste0(top), icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$top.dollars <- renderInfoBox({
    
    
    if (input$county == "NY State") {
      
      top.rec <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name), FUN = sum)
      
      top.rec.2 <- top.rec
      top.rec.3 <- arrange(top.rec.2 , desc(x))
      top.dollars <- top.rec.3[1,]$x
      
    } else {
      
      top.rec <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name, gra16.3$county), FUN = sum)
      
      top.rec.2 <- filter(top.rec , Group.2 == input$county)
      top.rec.3 <- arrange(top.rec.2 , desc(x))
      top.dollars <- top.rec.3[1,]$x
      
    }
    
    infoBox(
      "Top Recipient Funding", paste0("$", prettyNum(top.dollars, big.mark = ",")), icon = icon("credit-card"),
      color = "purple"
    )
  })
  
  output$top.num <- renderInfoBox({
    
    
    if (input$county == "NY State") {
      
      top.rec.num <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name), FUN = length )
      
      top.rec.num.2 <- top.rec.num
      top.rec.num.3 <- arrange(top.rec.num.2 , desc(x))
      top.num <- top.rec.num.3[1,]$x  
      
    } else {
      
      top.rec.num <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name, gra16.3$county), FUN = length )
      
      top.rec.num.2 <- filter(top.rec.num , Group.2 == input$county)
      top.rec.num.3 <- arrange(top.rec.num.2 , desc(x))
      top.num <- top.rec.num.3[1,]$x
      
    }
    
    infoBox(
      "Top Recipient Number of Transactions", paste0(top.num), icon = icon("list"),
      color = "green"
    )
  })
  
  # create all county datatable
  output$countyTbl <- DT::renderDataTable({
    if( input$county == "NY State"){
      # filter only positive outlays
      # do not filter by county
      gra16.all <- filter( gra16.3, fed_funding_amount > 0 )
      # display the table
      colnames(gra16.all) <- c("Recipient Type", "County", "Funding", "Agency", "Assistance Type", "Recipient Name", "Program")
      
      gra16.all
    } else {
      # filter only positive outlays
      # do filter by county
      gra16.all <- filter(gra16.3, county %in% input$county
                          #, assistance_type == "04: Project grant"
                          , fed_funding_amount > 0
                          #, recip_cat_type == input$recipient
                          #, maj_agency_cat == input$maj
      )
      # call the table
      colnames(gra16.all) <- c("Recipient Type", "County", "Funding", "Agency", "Assistance Type", "Recipient Name", "Program")
      
      gra16.all
    } # end of else
  })
  
  
  #######################################
  #### County Overview Shiny Elements####
  #######################################
  
  #Census Table
  
  output$censusTable <- DT::renderDataTable({
    
    census.table <- population[,c("county.name", "Pop", "MHincome", "pov.rate")] 
    census.table$Pop.rank <- rank(-census.table$Pop)
    census.table$MHincome.rank <- rank(-census.table$MHincome)
    census.table$pov.rate.rank <- rank(-census.table$pov.rate)
    census.table$pov.rate <- round(census.table$pov.rate*100, digits = 1)
    census.table <- census.table[,c("county.name", "Pop", "Pop.rank", "MHincome", "MHincome.rank", "pov.rate", "pov.rate.rank")]
    colnames(census.table) <- c("County", "Population", "Population Rank", "Median Household Income", "Median Household Income Rank", "Poverty Rate (%)", "Poverty Rate Rank")

    census.table
  }, options = list(lengthMenu = c(5,10), pageLength = 5, scrollX = TRUE))
  
  
  #Plotly plot
  output$plotlyplot <- renderPlotly({
    dem2 <- population 
    dem2$pov.rate <- round(100*dem2$pov.rate, 1)
    
    
    hovertxt5 <- paste("County:",dem2$county.name, "
", "Poverty Rate:", paste(dem2$pov.rate
                          , "%"
                          , sep=""
)
, "
", "Population:", prettyNum( dem2$Pop
                             , big.mark = ","
                             , preserve.width = "none"
    )
    )
    
    
    
    
    plot_ly(data = dem2, x = ~Pop, y = ~pov.rate, name = "",
            marker = list(color = "#F67670", size = 7)) %>%
      add_markers(hoverinfo="text", text=hovertxt5) %>%
      layout(xaxis = list(title = 'Population (millions)', showticklabels=TRUE, showgrid=FALSE),
             yaxis =list(title = 'Poverty Rate (%)', showgrid=FALSE, showticklabels=TRUE))
    
  })
  
  #Percapita bar plot  
  output$percapPlot <- shiny::renderPlot({
    
    
    gra16.4 <- filter(gra16.3 , county %in% input$your_county )
    
    pop.filtered <- filter(population , county.name %in% input$your_county )
    
    gra16.4.2 <- mutate(gra16.4 , assistance_type.2 = ifelse( assistance_type == "04: Project grant", "Project Grants" , "Other Grants" ) )
    
    gra16.agg <- agg.county.percap(gra16.4.2 , pop.filtered, gra16.4.2$assistance_type.2) #Function
    
    colnames(gra16.agg)[1] <- "assistance_type.2"
    
    gra16.agg.2 <- gra16.agg[c("assistance_type.2", "fund", "percap", "county")]
    
    gra16.agg.3 <- rbind(gra16.agg.2 , ny.per.2)
    
    cols <- c("#EBEBEB", "#649EFC")
    
    ggplot(gra16.agg.3, aes(x = county, y = percap, fill = assistance_type.2)) + 
      geom_bar(stat = "identity") + 
      labs(x="County", y="Per Capita Funding") +
      # ggtitle("Per Capita Federal Funding by County") +
      scale_y_continuous(labels = scales::dollar_format(prefix="$", big.mark = ",")) + 
      scale_fill_manual(values = cols) +   
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank() , legend.title = element_blank())
    
    
  }) # end of per capita plot
  
  # Render census plot
  output$censusPlot <- shiny::renderPlot({
    
    #################### FILTERING THE DATA #######################
    
    x <- population$county.name %in% input$your_county
    population_plot_filter <- population[x,]
    population_plot_filter$county.name <- factor(population_plot_filter$county.name, ordered= TRUE)
    
    #################### MAKING THE BARPLOT #######################
      
    krzycensuz(population_plot_filter)
    
  }) # end of census plot
  
  output$smallMultiples <- renderPlot({
    
    
    #filter by county
    county.filter <- filter(agg.pop.percap, County %in% input$your_county)
    
    
    ggplot(county.filter, aes(x=County, y= percap)) + geom_bar( aes(fill=County), stat="identity")+ scale_y_continuous(position = "right", labels = scales::dollar_format(prefix="$", big.mark = ","))+ facet_grid(Agency ~ Recipient_Type, switch="y") + labs(caption = "*This chart excludes negative outlays as well as agencies that had less than 10 entries total across recipient types and counties.") + theme_minimal() + theme (strip.text.y = element_text(size=12, angle = 180), strip.text.x = element_text(size=12), plot.title = element_text(size=16), plot.subtitle = element_text(size=13), legend.position="top", legend.title = element_blank(), axis.title.x=element_blank(), legend.key.size = unit(.5, "line"), legend.text=element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                                                                                                          axis.title.y= element_blank(), axis.ticks=element_blank(), axis.text.x= element_blank(), panel.background = element_rect(colour = 'gray80'),panel.grid.minor = element_blank(), panel.grid.major =element_blank())
    
    
  })
  
  
  
  
  
  
  
  output$cfdaTable <- DT::renderDataTable({
    # edit fancy table 2
    # gra16.4 <- filter(gra16.3 , county %in% input$your_county , assistance_type == "04: Project grant", fed_funding_amount > 0, recip_cat_type == input$recipient) 
    
    #gra16.5 <- gra16.4[c("county" , "agency_name",  "recipient_name", "recip_cat_type", "cfda_program_title", "fed_funding_amount")]
    
    # colnames(gra16.5) <- c("County", "Agency", "Recipient", "Recipient Type", "Program Title", "Funding Recieved")
    
    # gra16.5
    # edit fancy table 2
    gra16.4 <- filter(gra16.3, county %in% input$your_county
                      , assistance_type == "04: Project grant"
                      , fed_funding_amount > 0, recip_cat_type == input$recipient
                      , maj_agency_cat == input$maj
    )
    # call the table
    colnames(gra16.4) <- c("Recipient Type", "County", "Funding", "Agency", "Assistance Type", "Recipient Name", "Program")
    
    gra16.4
  })
  
  
} # end of server
