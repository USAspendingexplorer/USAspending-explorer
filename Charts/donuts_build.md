donuts yummy
================

nEED TO: \* collapse recip\_cat\_types? \* processed source DFs

USING PIE FUNCTION and exploring both total and no state grants for NY
======================================================================

Main Federal Funding Agencies in NY
-----------------------------------

``` r
#Possible variables:
#unique(gra16$maj_agency_cat) #27 levels
#unique(gra16$agency_name) #72 levels
#unique(gra16$agency_code) #95 levels

#grouping by Agencies (maj_agency_cat)
age <- group_by(gra16, maj_agency_cat)
age <- arrange(as.data.frame(summarize(age, Fed = sum(fed_funding_amount), Freq = n())), desc(Fed))

#grouping by Age TOTAL
ageT <- group_by(graTOTAL, maj_agency_cat)
ageT <- arrange(as.data.frame(summarize(ageT, Fed = sum(fed_funding_amount), Freq = n())), desc(Fed))

#barplot
par(mar = c(10,6,3,3), mfrow=c(1,2))
dat <- age
barplot(dat$Fed/1000000, names.arg = dat$maj_agency_cat, las = 2, cex.names = .7, cex.axis = .7 , ylab = "in USD 1MLL", cex.lab = .8, col="#4979FF", main= "Main Federal Funding Agencies in NY")
dat <- ageT
barplot(dat$Fed/1000000, names.arg = dat$maj_agency_cat, las = 2, cex.names = .7, cex.axis = .7 , ylab = "in USD 1MLL", cex.lab = .8, col="#4979FF", main = "Including State government")
```

![](donuts_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#####pie charts!########

#making an others category
dat <- age
pie <- dat[1:5,]
pie[6,] <- c("OTHERS", sum(dat$Fed[6:length(dat$Fed)]), sum(dat$Freq[6:length(dat$Fed)])) 
pie$Fed <- as.numeric(pie$Fed)
pie <- arrange(pie, desc(Fed))

#making an others category TOTAL
dat <- ageT
pieT <- dat[1:5,]
pieT[6,] <- c("OTHERS", sum(dat$Fed[6:length(dat$Fed)]), sum(dat$Freq[6:length(dat$Fed)])) 
pieT$Fed <- as.numeric(pieT$Fed)
pieT <- arrange(pieT, desc(Fed))

#colors
col <- c("#4979FF", "#FF9858",  "#8DFFFF", "#FF84FF", "#55D473", "#FFFF78", "grey80")

#plotting pies
plot.new()
par(mfrow = c(1,2), mar=c(1,1,1,1))
```

![](donuts_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
pie <- pie
pie(as.numeric(pie$Fed), labels = paste0(pie$maj_agency_cat, "\n" , round(pie$Fed/sum(pie$Fed)*100, digits=1), "%"), main = "Main Federal Funding Agencies in NY", cex= .7, col=col)

pie<- pieT
pie(as.numeric(pie$Fed), labels = paste0(pie$maj_agency_cat, "\n" , round(pie$Fed/sum(pie$Fed)*100, digits=1), "%"), main = "Including State Government", cex= .7, col=col)
```

![](donuts_files/figure-markdown_github/unnamed-chunk-1-3.png)

Federal Funding by Recipient Types (recip\_cat\_type)
-----------------------------------------------------

``` r
#possible variables:
#unique(gra16$recip_cat_type) #12 levels
#unique(gra16$recipient_type) #7 levels

#grouping by recipients 
rec <- group_by(gra16, recip_cat_type)
rec <- arrange(as.data.frame(summarize(rec, Fed = sum(fed_funding_amount), Freq = n())), desc(Fed))

#grouping by recipients TOTAL
recT <- group_by(graTOTAL, recip_cat_type)
recT <- arrange(as.data.frame(summarize(recT, Fed = sum(fed_funding_amount), Freq = n())), desc(Fed))

#barplot
par(mar = c(7,6,3,3), mfrow=c(1,2))

barplot(rec$Fed/1000000, names.arg = rec$recip_cat_type, las = 2, cex.names = .7, cex.axis = .7 , ylab = "in USD 1MLL", cex.lab = .8, col="#4979FF", main= "Federal Funding by Recipient Types")

barplot(recT$Fed/1000000, names.arg = recT$recip_cat_type, las = 2, cex.names = .7, cex.axis = .7 , ylab = "in USD 1MLL", cex.lab = .8, col="#4979FF", main = "Including State government")
```

![](donuts_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#pie chart!
col <- c("#4979FF", "#FF9858",  "#8DFFFF", "#FF84FF", "#55D473", "#FFFF78", "grey80")

plot.new()
par(mfrow = c(1,2), mar=c(1,1,1,1))
```

![](donuts_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
pie <- rec
pie(as.numeric(pie$Fed), labels = paste0(pie$recip_cat_type, "\n" , round(pie$Fed/sum(pie$Fed)*100, digits=1), "%"), main = "Federal Funding by Recipient Types", cex= .7, col=col)

pie<- recT
pie(as.numeric(pie$Fed), labels = paste0(pie$recipient_name, "\n" , round(pie$Fed/sum(pie$Fed)*100, digits=1), "%"), main = "Including State Government", cex= .7, col=col)
```

![](donuts_files/figure-markdown_github/unnamed-chunk-2-3.png)

USING DONUTZZ FUNCTION: Major Agencies in NY State
==================================================

First we need to create a function to read functions from github into Rstudio:

``` r
#calling the function from git
source_github <- function( url ) {
  # load package
  require(RCurl)
  
  # read script lines from website and evaluate
  script <- getURL(url, ssl.verifypeer = FALSE)
  eval(parse(text = script), envir=.GlobalEnv)
} 

#load the donutzz function using the RAW link
source_github("https://raw.githubusercontent.com/icps86/Functions/master/krzydonutzz")
source_github("https://raw.githubusercontent.com/icps86/Functions/master/donutzz.R")
```

Now we need a data frame.

``` r
#We first need to make a DF
dat <- group_by(gra16, maj_agency_cat)
dat <- arrange(as.data.frame(summarize(dat, Fed = sum(fed_funding_amount))), desc(Fed))

#Removing code
dat$maj_agency_cat <- substr(dat$maj_agency_cat, 7,nchar(dat$maj_agency_cat))

#making an others category
dat[6,] <- c("Others", sum(dat$Fed[6:length(dat$Fed)])) 
dat <- dat[c(1:6),]
dat$Fed <- as.numeric(dat$Fed)
dat <- arrange(dat, desc(Fed))

#this is the dataframe we use
dat %>% pander
```

<table style="width:57%;">
<colgroup>
<col width="43%" />
<col width="13%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">maj_agency_cat</th>
<th align="center">Fed</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Department of Health and Human Services</td>
<td align="center">4613172994</td>
</tr>
<tr class="even">
<td align="center">Others</td>
<td align="center">1533693043</td>
</tr>
<tr class="odd">
<td align="center">Department of Transportation</td>
<td align="center">1277061100</td>
</tr>
<tr class="even">
<td align="center">Department of Housing and Urban Development</td>
<td align="center">902251658</td>
</tr>
<tr class="odd">
<td align="center">National Science Foundation</td>
<td align="center">480960107</td>
</tr>
<tr class="even">
<td align="center">Department of Homeland Security</td>
<td align="center">377451815</td>
</tr>
</tbody>
</table>

Now we can use the function to call the donut

``` r
donutzz(x=dat$Fed, lev=dat$maj_agency_cat, main="Main Funding Agencies in NY State")
```

![](donuts_files/figure-markdown_github/unnamed-chunk-5-1.png)

KRZYDONUTZZ FUNCTION: Recipient types in 4 NY Counties
======================================================

Making mutiple donuts with facet wrap function

``` r
#making dataset 
dat <- gra16
dat <- group_by(dat, Name, recip_cat_type)
dat <- arrange(as.data.frame(summarize(dat, Fed = sum(fed_funding_amount))), Name)
table(dat$Fed < 0) #has negative numbers
```

    ## 
    ## FALSE  TRUE 
    ##   329     9

``` r
dat$Fed[0 >= dat$Fed] <- 0
table(is.na(dat$Fed)) #no NAs
```

    ## 
    ## FALSE 
    ##   338

``` r
x <- dat$Name%in% c("Albany", "Onondaga", "Wyoming", "Queens")
dat <- dat[x,]

#trying the function
krzydonutzz(x= dat, values = "Fed", labels = "recip_cat_type", multiple = "Name", main = "Federal Funding by Recipient type in NY Counties", percent.cex = 3, columns = 2, sbgfill = "grey95", sbgcol = "grey60")
```

![](donuts_files/figure-markdown_github/unnamed-chunk-6-1.png)
