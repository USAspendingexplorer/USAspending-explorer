#
# Author:   Linnea Powell, Stephanie Wilcoxen, 
#           Ignacio Pezo, and Cristian Nuno
# Purpose:  Draft Dashboard
#

# Load necessary packages
library(ggthemes)
library( shiny )
library( shinydashboard )
library( leaflet )
library(geojsonio )
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

# Import data from github function
source_github <- function( url ) {
  # load package
  require(RCurl)
  
  # read script lines from website and evaluate
  script <- getURL(url, ssl.verifypeer = FALSE)
  eval(parse(text = script), envir=.GlobalEnv)
} 
################# Importing External Objects ##############
# Import leaflet
leaflet_url <- "https://raw.githubusercontent.com/USAspendingexplorer/USAspending-explorer/master/Build%20App/leaflet_ny.r"
source_github( leaflet_url )

# Import table
fancy_table_url <- "https://raw.githubusercontent.com/USAspendingexplorer/USAspending-explorer/master/Build%20App/fancy_table.r"
source_github( fancy_table_url )

# import donuts
ig_url <- "https://raw.githubusercontent.com/DataCapstone/Data-Capstone/master/Ignacio/donutzz.R"
source_github(ig_url)

# import county overview 
co_url <- "https://raw.githubusercontent.com/DataCapstone/Data-Capstone/master/Raw-Data/county_comparison_2.R"
source_github(co_url)

# load the donutzz function using the RAW link
census_url <- "https://raw.githubusercontent.com/icps86/Functions/master/krzycensuz.r"
source_github( census_url )

# normalized census
NYcen_norm <- readRDS( gzcon(url("https://github.com/DataCapstone/Data-Capstone/blob/master/Raw-Data/NYcen_norm.RDS?raw=true")))

# import small multiples aggregated data
agg_url <- "https://raw.githubusercontent.com/DataCapstone/Data-Capstone/master/Linnea/small%20multiples%20aggregated%20data.R"
source_github(agg_url)

############ Building the Dashboard##################

# A dashboard has 2 parts: a user-interface (ui) and a server

# The UI consists of a header, a sidebar, and a body.

# The server consists of functions that produce any objects
# that are called inside the UI

## customize header ##
header <- dashboardHeader(title = "USAspending Explorer"
                          , tags$li( a( href = "https://www.maxwell.syr.edu/deans/"
                                        , img( src = "https://www.maxwell.syr.edu/uploadedimages/deans/branding/SUMaxSigWM.allW.png"
                                               , title = "Maxwell School Homepage", height = "30px")
                                        , style = "padding-top:10px; padding-bottom:10px;")
                                     , class = "dropdown"
                          ) # end of Maxwell Logo
                          , tags$li(a(href = "https://github.com/DataCapstone/Data-Capstone"
                                      , img( src = "https://github.com/DataCapstone/Data-Capstone/raw/10b7bdc7876d446aba3ddf6dd8dc080744fd4309/Raw-Data/GitHub_Logo.png"
                                             , title = "Fork Me on GitHub", height = "30px")
                                      , style = "padding-top:10px; padding-bottom:10px;")
                                    , class = "dropdown"
                          ) # end of GitHub logo
) # end of header
## customize sidebar ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "Over", icon = icon("home"))
    , menuItem("Explore", tabName = "Explore", icon = icon("search"))
    , menuItem("Compare", tabName = "Compare", icon = icon("bar-chart"))
    , menuItem("Use Case", tabName = "Use", icon = icon("briefcase"))
    #, menuItem("About", tabName = "About", icon = icon("users"))
  )
)

## customize body ##
body <- dashboardBody(
  # initialize tabs
  tabItems(
    # first tab content
    tabItem(tabName = "Over"
            , h2("Welcome to the USAspending Explorer")
            # , shiny::p("Under the U.S. Treasury Department’s leadership, the new site will allow taxpayers to examine", span( strong(" nearly $4 trillion in federal spending each year ")), "and see how this money flows from Congressional appropriations to local communities and businesses. The data is compiled by Treasury from federal agencies and published quarterly beginning in May 2017. The data will be updated each quarter.")
            # , shiny::p("Generally, federal spending can be separated between mandatory and discretionary spending. USAspending Explorer examines the discretionary side of federal spending by only look at spending transactions classified as grants. Given the open-source nature of our work, it is entirely possible to explore contracts, loans, and other financial assistance spending types.")
            # , shiny::p("All spending totals exclude grants which were directly award to the state government.")
            # , shiny::p("USAspending Explorer is an open source tool designed to assist in the navigation of federal data made publicly available through usaspending.gov. Through dynamic visualizations and search filters, the Explorer can answer the who, what, where, why and how of the complex federal government spending landscape.")
            , shiny::p("USAspending Explorer is an open source tool designed to assist in the navigation of federal data made publicly available through ", span( a("usaspending.gov", href = "https://www.usaspending.gov/DownloadCenter/Pages/DataDownload.aspx", target = "_blank")), ". Through dynamic visualizations and search filters, the Explorer can answer the who, what, where, why and how of the complex federal government spending landscape.")
            , shiny::p("This explorer serves as a tool to understand federal funding at the county level. It focuses on federal assistance in the form of grants and excludes grants that go directly to the state government because of the complexity of determining the final recipient of those funds.") 
            , shiny::p("With USAspending Explorer, You can understand how tax dollars are spent, explore who receives federal grants in your area, and see what that funding is directed towards. This tool shows total funding going to each county, and how funds are distributed geographically within the state adjusted by population. It also offers flexible comparisons between counties and the state average, displays county demographic information to inform county comparisons, and breaks down county funding by the funding agency, recipient, and program to better understand what is driving variations across counties.")
            , h3("All Federal Grant Funding Received by County, FY 2016")
            , fluidRow(
              column(width = 4
                     , DT::dataTableOutput("tbl")
              ) # end of column 1
              , column( width = 8
                        , leafletOutput("mymap", height=600)
              ) # end of column 2
            ) # end of row 1
    ) # end of Overview tab
    # second tab Explore
    , tabItem( tabName = "Explore"
               , h2("County Explorer") # blank space
               , shiny::p("This diagram shows the flow of funding from funding agencies to recipients with widths proportional to the amount of funding. For simplicity it displays the top 10 funding agencies in each county and groups all other agencies into an other category.")
               , br()
               , fluidRow(
                 column( width = 2
                         , box( title = "County Exploration", status = "primary"
                                , solidHeader = TRUE, collapsible = FALSE, width = NULL
                                , selectizeInput(
                                  inputId='county',
                                  label='Select a county:',
                                  choices= c("NY State", sort(unique(gra16.3$county))),
                                  selected=c("NY State")
                                ) ) # end of box 1
                 ) # end of column 1
                 , column( width = 10
                           , box( title = "Flow of Funds from the Top 10 Agencies", status = "primary",
                                  solidHeader = TRUE, collapsible = FALSE, width = NULL
                                  , sankeyNetworkOutput( "sankey" ) ) # end of box 2
                 ) # end of column 2
               ) # end of row 2
               , fluidRow(
                 column( width = 12
                         , box( title = "Top Recipient Details", status = "primary"
                                , solidHeader = TRUE, collapsible = FALSE, width = NULL
                                , infoBoxOutput("top")
                                , infoBoxOutput("top.dollars")
                                , infoBoxOutput("top.num")
                         ) # end of box 3
                 ) # end of column 3
               ) # end of row 3
               , fluidRow(
                 column( width = 12
                         , box( title = "All FY16 Federal Grants by County", status = "primary"
                                , solidHeader = TRUE, collapsible = FALSE, width = NULL
                                , DT::dataTableOutput("countyTbl")
                         ) # end of box
                 ) # end of column
               ) # end of row
               
    ) # end of second tab
    , # Third tab content
    tabItem(tabName = "Compare"
            , h2("County Comparison of Project Grant Funding")
            , shiny::p("These visuals build on the same federal grant data for New York state in FY 2016 but focus solely on project grants. Project grants are awarded for a specific purpose and based on the merit of the grant application, meaning that any variation from county to county is at least in part under the applicant’s control. This type of grant may be of more interest to users wishing to understand differences between counties than grants distributed based on a formula determined by law and often based on demographic information or less competitive block grants would be.")
            , shiny::p("This comparison tool allows you to select up to 4 counties in New York state, view how similar those counties are in terms of demographics and in comparison to the state average, and then view the total federal grant funding received by each county in per capita terms.")
            
            , fluidRow(
              column( width = 2
                      , box( title = "County Comparison", status = "primary"
                             , solidHeader = TRUE, collapsible = FALSE, width = NULL
                             , selectizeInput(
                               inputId='your_county', 
                               label='Select up to 4 counties to compare:', 
                               choices= sort(unique(gra16.3$county)),
                               selected=c("Onondaga", "Herkimer", "Erie"), 
                               multiple = TRUE, 
                               options = list(maxItems = 4)
                             ) ) # end of box 1
              ) # end of column 1
              , column( width = 5
                        , box( title = "County Demographics", status = "primary"
                               , solidHeader = TRUE, collapse = FALSE, width = NULL
                               , shiny::plotOutput("censusPlot")
                        ) # end of box 2
              ) # end of column 2
              , column( width = 5
                        , box( title = "Grant Types by County"
                               , status = "primary"
                               , solidHeader = TRUE, collapse = FALSE, width = NULL
                               , shiny::plotOutput("percapPlot")
                        ) # end of box 3
              ) # end of column 3
            ) # end of row 1
            , br()
            , shiny::p("You can then further examine the project grant funding received by each county broken down by the recipient type and federal agency awarding the funds. This allows you to identify what areas or recipients are driving the variation in funds. You can further explore the specific grants and recipients that make up any of this variation in the data table below.")
            , fluidRow(
              column( width = 12
                      , box( title = "Federal Project Grant Funding by County, Agency, and Recipient (Per Capita)"
                             , status = "primary", solidHeader = TRUE, collapse = FALSE
                             , width = NULL
                             , shiny::plotOutput("smallMultiples", height = 1800)
                      ) # end of box 4
              ) # end of column 4
            ) # end of row 2
            , fluidRow(
              column( width = 12
                      , box( title = "Federal Spending Details", status = "primary"
                             , solidHeader = TRUE, collapsible = FALSE, width = NULL
                             , selectInput(
                               inputId = 'recipient'
                               , label='Select a recipient:'
                               , choices = c( sort(unique(as.character(gra16.3$recip_cat_type))))
                               , selected = "Higher Ed"
                             ) # end of radio buttons
                             , selectInput(
                               inputId = 'maj'
                               , label='Select an Agency:'
                               , choices = c( sort(unique(as.character(gra16.3$maj_agency_cat))))
                               , selected = "Health And Human Services"
                             ) # end of radio buttons
                             , DT::dataTableOutput("cfdaTable")
                      ) # end of box 7
              ) # end of column 7
            ) # end of row 4
    ) # end of 3rd Tab Item
    # 4th tab
    , tabItem( tabName = "Use"
               , h1("Welcome to the Use Case")
               , h2("User:")
               #, shiny::p("State-level official creating a report of federal funding received by county.")
               , shiny::p("Ms. Sherry Garcia. Public official at the Onondaga County’s Industrial Development Agency (OCIDA)")
               , h2("Interest:")
               #, shiny::p("An employee in the New York state economic development office wants to generate a report of federal spending in New York state broken down by per capita spending in each county. Their goal is to better understand federal funding structure and come up with insights about why funding patterns vary across counties. Specifically, the user will focus on project grant activity within each county with the long term goal of helping counties compete more successfully for additional federal funding, which means winning more project grants. The user will generate a dashboard to display their analysis and use it as a tool for their continuing research.")
               , shiny::p("The county of Onondaga is alarmed by its rising fiscal deficit and the County Executive has asked her staff to look for new sources of revenue. Ms. Garcia, a public official at OCIDA, is interested in exploring publicly available federal spending data to better understand the structure of federal project grants coming into the county and to compare Onondaga to other local governments. Her main objective is to identify federal funding opportunities that the county could compete for by examining where counties similar to Onondaga have successfully found funds. Ms. Garcia is planning on using the USAspending Explorer tool to navigate through the data and produce a final report to the County Executive.")
               #, h2("Steps:")
               , h3( strong("1. Overview of Counties in New York State using the 'Overview' Tab") )
               # , shiny::p("The user begins by", a(" downloading data"
               #                                    , href = "https://www.usaspending.gov/DownloadCenter/Pages/DataDownload.aspx"
               # ) # end of hyperlink
               # , " on FY2016 grants in New York state with the following selections:"
               # , br()
               # , img( src = "https://github.com/DataCapstone/Data-Capstone/raw/35fa7a046b52fd7204a2b87ed38f79e207cdac90/Raw-Data/Screen%20Shot%202017-06-09%20at%202.13.57%20PM.png"
               #        , height = 400, width = 700
               # ) # end of screenshot
               # ) # end of long paragraph
               , shiny::p("From the landing tab of USAspending Explorer, the user selects NY State and locates Onondaga on the map. The scaled colors in the county map allow the user to quickly compare Onondaga County’s total and per capita federal grant funding with the other counties in NY State. The data table allows the user a more detailed look.")
               , shiny::p("The current version of USAspending Explorer uses FY 2016 data and excludes all funds going directly to state government because these funds will likely be redistributed to other localities in the state. This allows the user to compare counties more meaningfully than if funds were reported as going to a single county when they’re actually redistributed to many.")
               , shiny::p("The user quickly finds that Onondaga is ranked 13th in total grants ($101,573,642) but 18th in per capita federal grants ($216.9) received. When looking at total grants, Onondaga is similar to Nassau ($107,817,884.25) and Cattaraugus ($97,653,885), however, Nassau receives much less on a per capita as compared to Onondaga while Cattaraugus receives much more. When looking only at per capita grants received, counties like St. Lawrence ($256.98) and Richmond ($296.13) are receiving similar amounts to Onondaga.")
               , shiny::p("These insights have given Ms. Garcia potential counties to compare Onondaga with.  To find out more about where funding is coming from and who is receiving it within Onondaga, she moves on to the next tab.")
               , h3( strong("2. Exploring the Flow of Funds using the 'Explore' Tab") )
               #, h4(em("General Picture of NY Federal Funding"))
               , shiny::p("The Flow of Funds diagram allows Ms. Garcia to see the relationship between funding agencies (to the left) and recipients (to the right) within Onondaga. She may also choose to see this diagram at the State level and for other counties.")
               , shiny::p("Ms Garcia quickly realizes that the Health and Human Services Agency (HHS) is the dominant funder of project grants for Onondaga County, and the money is primarily going into local higher education institutions and non-for-profits. Reflecting on this information, she realizes that the clear dominance of higher education institutions as recipients of federal grants in Onondaga county can be explained by the prominence of local higher education institutions like Syracuse University. Many questions arise for Ms. Garcia. Do other counties with big universities receive as much grant money as Onondaga? Are those grants coming from the same main funding agencies?")
               , shiny::p("Ms. Garcia also remembers that Rensselaer county was the top per capita grants recipient in NY State and decides to explore how grants are flowing in that particular county. She finds that HHS is again the main funding agency, however and contrary to Onondaga, HHS funds are delivered primarily to non-for-profit organizations in Rensselaer.")
               , shiny::p("St. Lawrence, which was close to Onondaga in per capita spending, has a very different structure. In St. Lawrence, Transportation is the main funding agency and government institutions are the main recipients of these funds. Why is St. Lawrence receiving more money from Transportation? How similar are St. Lawrence and Onondaga in terms of their demographics? What other similar counties should be explored?")
               #, shiny::p("Initially, the user wants to develop a general picture of federal funding in New York state - both in terms of total funding and per capita funding in each county. To do this, they create a data table and map. They exclude all funding going to the New York state government because those funds will likely be redistributed to localities by the state. They then aggregate federal funding amount by county. Finally, they bring in population data on each county using the Census API in order to generate per capita funding. From this map they are able to identify counties they would be interested in comparing. Additionally, the user wants to understand where is funding coming from and who is receiving for the State of NY. To have a better idea of this, they create two donut charts by aggregating the money by funding agency and recipient type.")
               #, h4(em("Examine Federal Grants"))
               , shiny::p("After establishing a general picture of funding in New York, the user hopes to compare what kind of federal funding exists within each county. They choose counties based on their own knowledge or county demographics. First, they examine how much of the federal funding going to each county is in the form of project grants (because these awards are assigned through an open competitive process). They use the variable assistance type to hone in on spending from project grants versus spending from other types of grants such as block or formula grants. They assume that counties who have a large amount of their funding coming from project grants are successfully finding and applying for competitive opportunities while counties with low project grant funding may be missing out on potential awards. The next step is to drill down the analysis for a better understanding of these awards.")
               , h3( strong( "3. Comparing Counties using the 'Compare' Tab" ) )
               , shiny::p("The County comparator tool offers Ms. Garcia key visuals that can help her to answer these questions. In this tab, the user finds a dynamic county demographic visualization, which helps her better understand how comparable the counties she is interested in examining are based on their population size, median household income and the percentage of the population which are living below the federal poverty level.")
               , shiny::p("Ms. Garcia is still able to refer to the amount of grants received by each county by looking at the per capita bar chart. This chart expands on what Ms. Garcia saw in the county map by also breaking funding down by project grants and other types of grants. As a County official, Ms Garcia knows that project grants are awarded through a competitive process and may be a source of new funding for Onondaga, while other types of grants are normally determined by formulas or other fixed methods.")
               , shiny::p("Due to their demographic similarities, Ms Garcia decides to explore the comparison between Onondaga and Orange county. Orange county is getting very similar amounts of project grants. To do a drill down comparison and see where any differences in funding between the two counties is coming from, the user looks at the multiple comparator graphs (located beneath the county demographic comparison and grants type barchart visuals.")
               , shiny::p("This visualization allows the user to look at how the funds flowing into each county are distributed between different awarding agencies and are given to different recipient types. Ms Garcia skims the multiple graphs and finds that the Government and Other recipients in Orange county is receiving a lot of money from the Department of Transportation. Onondaga is not receiving any significant funds from the Department of Transportation. In addition, the Department of Education is also providing Government institutions at Orange County with significant funds, while this agency is not funding Onondaga at all. Orange and Onondaga counties are receiving similar amounts of funds from the Health and Human Services Agency.")
               , shiny::p("Looking for more insights, Ms Garcia includes St. Lawrence county as another comparator. Ms. Garcia finds that St. Lawrence is also getting a lot of money from Transportation. Why are St. Lawrence and Orange county getting so much for Transportation?")
               , h3( strong( "4. Looking at Specific Project Grants" ) )
               , shiny::p("In order to understand why Orange and St. Lawrence counties are getting more grant funding from the Department of Transportation and why Orange county gets more funding from the Department of Education than Onondaga is, Ms Garcia needs to drill down further. To do so, Ms Garcia uses the Federal Spending Program table in this tab. This is a dynamic table that shows the specific grants going to each county and allows the user to filter by keywords in order to find specific funding programs.")
               , h4(em("Transportation Programs"))
               # , shiny::p("Next, they compare the recipients of project grants within each county and determine which agencies are funding those recipients. For this part of their analysis they only look at positive outlays of funding because they are primarily interested in counties which are bringing in funds. To consider project grants going to different sectors within each county, the user works with the variable recipient category type. They focus on recipients because a county may appear to have much of its funding coming from project grants, but those grants may be going almost exclusively to one type of institution.")
               , shiny::p("After filtering through the grants, Ms Garcia finds out that the Department of Transportation is providing large sums of money to St. Lawrence and Orange counties for a Program described as “Airport improvement Program”, while Onondaga is not receiving nearly as much. While this could mean that the airports in Onondaga do not need as much improvement, it might also signify that Onondaga could more aggressively pursue this funding. See the screenshot below:"
                          , br()
                          , img( src = "https://github.com/DataCapstone/Data-Capstone/raw/master/Images/UC_datatable_airpot.JPG"
                                   , height = 200, width = 700
                                 ) # end of first screenshot
               ) # end of long paragraph
               , h4(em("Education Programs"))
               , shiny::p("When looking at the grants provided by the Department of Education to Orange County, Ms Garcia finds that there has been a significant amount of funding provided to a specific School District in the county. However, she quickly realizes that this funding is tied to tax exempt land in the county and isn’t an opportunity for Onondaga to compete for funds. See screenshot below:"
                          , br()
                          , img( src = "https://github.com/DataCapstone/Data-Capstone/raw/master/Images/UC_datatable_schools.jpg"
                                 , height = 400, width = 700
                          ) # end of second screenshot)
               ) # end of second long paragraph
               , h3( strong( "5. What was Learned" ) )
               , shiny::p("Through the navigation of USAspending Explorer, Ms Garcia has learnt more about the federal funding structure in NY State and Onondaga county. Comparing Onondaga with other counties has given her more context to assess the performance of Onondaga in competing for project grants and has also allowed her to identify specific project grants that are providing similar counties with significant amounts of funds.")
               , shiny::p("Naturally, this exploratory tool generates more questions than it answers, but Ms Garcia has learned a lot about federal funding in NY State, Onondaga and other counties. Better understanding the federal funding landscape will lead Ms Garcia to make further inquiries, research more about the specific project grants that she identified, and develop a plan for how to compete for those funds in the future. Ms Garcia could also contact the counties she has been looking at and ask them about program details now that she has a better picture of what funding she is interested in.")
               #, shiny::p("The user next identifies the primary agencies funding project grants for each recipient within the counties of interest. To do this they use the agency identifier. They now have a much fuller picture of what kinds of institutions are successfully winning federal funding within each county, and can thus tailor any further action to the particular deficiencies or strengths of each county’s federal funding portfolio. The user generates a dashboard report to display their analysis.")
    ) # end of 4th tab customization
    # 5th tab 
    #, tabItem( tabName = "About"
    #           , h1("We made this app! :)")
    #) # end of 5th tab
  ) # end of Tab Items
) # end of dasboard body

## Shiny UI ##
ui <- dashboardPage(
  header
  , sidebar
  , body
)
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
    
    top.rec <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name, gra16.3$county), FUN = sum)
    
    if (input$county == "NY State") {
      
      top.rec.2 <- top.rec
      top.rec.3 <- arrange(top.rec.2 , desc(x))
      top <- top.rec.3[1,]$Group.1
      
    } else {
      
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
    
    top.rec <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name, gra16.3$county), FUN = sum)
    
    if (input$county == "NY State") {
      
      top.rec.2 <- top.rec
      top.rec.3 <- arrange(top.rec.2 , desc(x))
      top.dollars <- top.rec.3[1,]$x
      
    } else {
      
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
    
    top.rec.num <- aggregate(gra16.3$fed_funding_amount, by= list(gra16.3$recipient_name, gra16.3$county), FUN = length )
    
    if (input$county == "NY State") {
      
      top.rec.num.2 <- top.rec.num
      top.rec.num.3 <- arrange(top.rec.num.2 , desc(x))
      top.num <- top.rec.num.3[1,]$x  
      
    } else {
      
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
      gra16.all
    } # end of else
  })
  
  
  #######################################
  #### County Overview Shiny Elements####
  #######################################
  
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
    
    #################FORMATTING THE DF##########################
    #only working with four comparatos and NY state
    #"Matches for Onondaga: 1.Broome, 2.St. Lawrence, 3.Orange, 4.Sullivan, 5.Monroe"
    x <- NYcen_norm$county.name %in% input$your_county
    NYcen_norm_filter <- NYcen_norm[x,]
    NYcen_norm_filter$county.name <- factor(NYcen_norm_filter$county.name, ordered= TRUE)
    rownames(NYcen_norm_filter) <- 1:nrow(NYcen_norm_filter)
    
    #################### MAKING THE BARPLOT #######################
    
    krzycensuz(NYcen_norm_filter)
    
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
    gra16.4
  })
  
  
} # end of server

## call the Shiny App ##
shinyApp(ui, server)
