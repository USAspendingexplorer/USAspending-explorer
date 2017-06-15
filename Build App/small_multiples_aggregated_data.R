
library(dplyr)
library(pander)
library(shiny)
library(censusapi)
library(plotly)
library(ggthemes)

# read in the county grants data
gra16.named <- readRDS(gzcon(url("https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Data/Processed/NYgra16_counties_named.rds?raw=true")))

# exclude money going to state government and rename county variable as "Name"
gra16.without.state <- filter(gra16.named , recipient_type != "00: State government" )

gra16.2 <- mutate(gra16.without.state, county = as.character(Name))

# select only necessary columns for aggregating project grants by county, agency, and recipient and creating table with recipient name and project title
gra16.3 <- gra16.2[ c("county" , "fed_funding_amount" , "assistance_type", "maj_agency_cat",  "recipient_name", "recip_cat_type", "cfda_program_title") ]

# combine similar recipient type categories
x <- gra16.3$recip_cat_type == "i: Private agencies"
gra16.3$recip_cat_type[x] <- "f: Private agencies"

y <- gra16.3$recip_cat_type == "h: Government"
gra16.3$recip_cat_type[y] <- "h: Private agencies"

#making recip_cat_type into a factor and changing the levels into more friendly ones
gra16.3$recip_cat_type <- factor(x= gra16.3$recip_cat_type)

levels(gra16.3$recip_cat_type) <- c("Private Firm",
                                    "Government",
                                    "Higher Ed",
                                    "Nonprofit",
                                    "Other")

# clean agency names so that they're consistent when aggregating and easier to read on the chart
simpleCap <- function(x) {
  s <- tolower(x) 
  s <- strsplit(s, " ")[[1]] 
  paste(toupper(substring(s, 1,1)), substring(s, 2), 
        sep="", collapse=" ")
} 
gra16.3$maj_agency_cat <-sapply(gra16.3$maj_agency_cat, simpleCap)

gra16.3$maj_agency_cat<-gsub( "Department Of ", "", as.character(gra16.3$maj_agency_cat), n)

gra16.3$maj_agency_cat<-(substring(gra16.3$maj_agency_cat, 7, nchar(gra16.3$maj_agency_cat)))

# select only project grants
gra16.4<- filter(gra16.3 , assistance_type== "04: Project grant")

# drop rows with negative funding values
gra16.4 <- subset (gra16.4, fed_funding_amount > 0)

# read in the population data
population <- readRDS(gzcon(url("https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Data/Processed/NYcensus.rds?raw=true")))

# select the 2 columns needed - county name and population
pop.dat <- population[ c("county.name", "Pop") ]

# create row for state population
pop.dat.state <- rbind(pop.dat, data.frame(county.name="State Average", Pop=sum(pop.dat$Pop)))

# aggregate funding by recipient type and agency to find state average
gra16.4$maj_agency_cat<- as.character(gra16.4$maj_agency_cat)

ny.agency.agg <- aggregate (gra16.4$fed_funding_amount, by=list(gra16.4$recip_cat_type, gra16.4$maj_agency_cat), FUN=sum, na.rm=TRUE)

# rename columns after aggregation
colnames(ny.agency.agg)<- c("Recipient_Type", "Agency", "Federal_Funding")

ny.agency.agg["county"] <- "State Average"

colnames(ny.agency.agg)<- c("Recipient_Type", "Agency", "Federal_Funding", "County")

# aggregate by recipient type, agency, and county
county.agency.agg <- aggregate (gra16.4$fed_funding_amount, by=list(gra16.4$recip_cat_type, gra16.4$maj_agency_cat, gra16.4$county), FUN=sum, na.rm=TRUE)

# rename columns after aggregation
colnames(county.agency.agg)<- c("Recipient_Type", "Agency", "County", "Federal_Funding")

# combine county aggregation with state aggregation
agency.agg <- rbind(ny.agency.agg, county.agency.agg)

# merge aggregated grants data with population data by county name
agg.pop <- merge(agency.agg , pop.dat.state, by.x = "County", by.y = "county.name", all.x=TRUE)

# create new variable for per capita funding
agg.pop.percap <- mutate(agg.pop , percap =  Federal_Funding / Pop )

# round percap to be more understandable
agg.pop.percap <- mutate(agg.pop.percap, percap = round(percap, 2))

#drop agencies with less than 10 rows in total
agg.pop.percap<-agg.pop.percap[as.numeric(ave(agg.pop.percap$Agency, agg.pop.percap$Agency, FUN=length)) >= 10, ]
