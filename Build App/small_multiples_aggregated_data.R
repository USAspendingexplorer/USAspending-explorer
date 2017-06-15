
library(dplyr)
library(pander)
library(shiny)
library(censusapi)
library(plotly)
library(ggthemes)

# read in the county grants data that has been cleaned for aggregation
gra16.3 <- readRDS(gzcon(url("https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Data/Processed/NYgra16_cleaned.rds?raw=true")))

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
