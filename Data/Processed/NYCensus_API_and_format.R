library(censusapi)

#CREATING A NYCENSUS COUNTY RDS

# download acs5 2015 for Population, Median Household income, White, Black, Asian, Hispanic or latino, Unemployed, Income < poverty level last 12 months, In Labor Force
censuskey <- "your_key_here" #you need a Census API key, which you can request for free from the Census bureau

#census variables have coded names which are difficult to understand intuitively. In the following code we make the API get request and then rename the variables.
dem <- getCensus(name="acs5", vintage=2015, key=censuskey, 
                 vars=c("B01001_001E", "B19013_001E", "B01001A_001E", "B01001B_001E", "B01001D_001E", "B01001I_001E", "B23025_005E", "B17001_002E", "B23025_002E"), region="county:*", regionin = "state: 36")

colnames(dem)[3:11] <- c("Pop", "MHincome", "White", "Black", "Asian", "Hispanic", "Unemployed", "Poverty", "Labor")

#"White", "Black", "Asian", "Hispanic", "Unemployed", "Poverty", "Labor" are all absolute quantities of people.

#We will create rates for this variables:
dem$pov.rate <- dem$Poverty/dem$Pop
dem$une.rate <- dem$Unemployed/dem$Labor
dem$whi.rate <- dem$White/dem$Pop
dem$bla.rate <- dem$Black/dem$Pop
dem$asi.rate <- dem$Asian/dem$Pop
dem$his.rate <- dem$Hispanic/dem$Pop


#Now we need to add a county.name variable. to do this we use the following object that had the county code and names equivalencies
#getting a DF with the names of the counties and codes.
cou <- read.csv("https://raw.githubusercontent.com/USAspendingexplorer/USAspending-explorer/master/Data/Raw/countycodesNY.csv", stringsAsFactors = F)
dem$county.name <- as.factor(dem$county)
levels(dem$county.name) <- as.character(cou$Name)
#I used levels to standardize the names, but another way to do this could be using the match function:
#order <- match(as.numeric(dat$county), cou$Fips)

colnames(dem)[2] <- "county.code"

#ordering the DF nad dropping racial variables because we do not use them in the app.
dem <- dem[,c( "state", "county.code","county.name", "Pop", "MHincome", "pov.rate")]
dem$county.code <- as.numeric(dem$county.code)
saveRDS(dem, file = "NYcensus.rds")

```