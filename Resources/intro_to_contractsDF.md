Contracts Data exploration
================

``` r
#getting a dataframe with the class for each variable
x <- cnt[1,] 
x <- as.data.frame(lapply(x, class))
x <- as.data.frame(t(x))
x <- data.frame(var = row.names(x), class = as.character(x$V1), stringsAsFactors = F)
cntv <- x
write.csv(cntv, "cnt.variables.csv", row.names = F)
```

Useful links for context
========================

-   **List of other databases** <https://www.fpds.gov/wiki/index.php/PSC,_NAICS_and_more>

-   **System for Award Management (SAM)** Allows to look at contractors by name, DUNS or CAGE (look at search records) <https://www.sam.gov/portal/SAM>

<https://uscontractorregistration.com/>

-   **Federal Acquisition Regulation (FAR)** <https://www.acquisition.gov/?q=browsefar> FAR contains the uniform policies and procedures for acquisitions by executive agencies of the federal government. The FAR is issued and maintained by the Department of Defense, the General Services Administration, and the National Aeronautics and Space Administration.

-   **Small Business Administration**

<https://www.sba.gov/blogs/selling-government-get-started-these-5-steps>

------------------------------------------------------------------------

Glosary of useful codes
=======================

-   **NAICS** The North American Industry Classification System <https://www.naics.com/search/> <https://www.naics.com/naics-drilldown-table/>

NAICS is one of the tools used by government agencies to classify the acquisitions and procurements conducted for goods, services and solutions.

How NAICS are used. -agencies use NAICS Codes to classify how they will use goods and services they purchase. -companies use NAICS Codes to classify their offerings or the segment of an industry sector in which they do business. -agencies reference NAICS Codes in documents related to requirements such as forecasts, pre-solicitations and solicitations, and subsequent contracts and agreements. -companies reference NAICS Codes in their SAM profiles, Dynamic Small Business Search profiles (for small business concerns) and often on their marketing materials such as websites, business cards and capability statements.

Higher level NAICS categories:

``` r
naics %>% pander
```

<table style="width:68%;">
<colgroup>
<col width="26%" />
<col width="41%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">X2017.NAICS.Code</th>
<th align="center">X2017.NAICS.Title</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">11</td>
<td align="center">Agriculture, Forestry, Fishing and Hunting</td>
</tr>
<tr class="even">
<td align="center">21</td>
<td align="center">Mining</td>
</tr>
<tr class="odd">
<td align="center">22</td>
<td align="center">Utilities</td>
</tr>
<tr class="even">
<td align="center">23</td>
<td align="center">Construction</td>
</tr>
<tr class="odd">
<td align="center">31</td>
<td align="center">Manufacturing</td>
</tr>
<tr class="even">
<td align="center">32</td>
<td align="center">Manufacturing</td>
</tr>
<tr class="odd">
<td align="center">33</td>
<td align="center">Manufacturing</td>
</tr>
<tr class="even">
<td align="center">42</td>
<td align="center">Wholesale Trade</td>
</tr>
<tr class="odd">
<td align="center">44</td>
<td align="center">Retail Trade</td>
</tr>
<tr class="even">
<td align="center">45</td>
<td align="center">Retail Trade</td>
</tr>
<tr class="odd">
<td align="center">48</td>
<td align="center">Transportation and Warehousing</td>
</tr>
<tr class="even">
<td align="center">49</td>
<td align="center">Transportation and Warehousing</td>
</tr>
<tr class="odd">
<td align="center">51</td>
<td align="center">Information</td>
</tr>
<tr class="even">
<td align="center">52</td>
<td align="center">Finance and Insurance</td>
</tr>
<tr class="odd">
<td align="center">53</td>
<td align="center">Real Estate Rental and Leasing</td>
</tr>
<tr class="even">
<td align="center">54</td>
<td align="center">Professional, Scientific, and Technical Services</td>
</tr>
<tr class="odd">
<td align="center">55</td>
<td align="center">Management of Companies and Enterprises</td>
</tr>
<tr class="even">
<td align="center">56</td>
<td align="center">Administrative and Support and Waste Management and Remediation Services</td>
</tr>
<tr class="odd">
<td align="center">61</td>
<td align="center">Educational Services</td>
</tr>
<tr class="even">
<td align="center">62</td>
<td align="center">Health Care and Social Assistance</td>
</tr>
<tr class="odd">
<td align="center">71</td>
<td align="center">Arts, Entertainment, and Recreation</td>
</tr>
<tr class="even">
<td align="center">72</td>
<td align="center">Accommodation and Food Services</td>
</tr>
<tr class="odd">
<td align="center">81</td>
<td align="center">Other Services (except Public Administration)</td>
</tr>
<tr class="even">
<td align="center">92</td>
<td align="center">Public Administration</td>
</tr>
</tbody>
</table>

When the Federal government intends to acquire goods or services, it identifies the NAICS code that describes the principal purpose of that procurement. Your business may have myriad capabilities, and the NAICS code for a given procurement opportunity may not be the same as your primary NAICS code. That will not keep you from bidding or making an offer, so long as you meet the size standard for the procurement and have the capacity to provide the goods or services.

-   **PSC** Product or Service Code <https://www.fpds.gov/downloads/psc_data_Oct012015.xls>

Product service codes (PSC) are used by the United States government to describe the products, services, and research and development purchased by the government.

They differ from NAICS Codes in that PSC Codes describe WHAT was bought for each contract action reported in the Federal Procurement Data System (FPDS), whereas NAICS Codes describe HOW purchased products and services will be used. NAICS and PSC Codes look different, too. NAICS Codes are six position numeric values and PSC Codes are four position numeric or alphanumeric values.

When compared to NAICS Codes, PSC Codes are much more granular. NAICS Codes are segregated into industry sectors represented by the first two digits of a NAICS Code such as 54 Professional Scientific and Technical Services.The current NAICS Code system consists of twenty industry sectors. Product and Service Codes start with three categories, R&D, Services and Products, that are then broken down into 102 classes indicated by the first one or two digits of a PSC Code.

PSC codes can be descripbed in three groups:

**a) Research and Development (R&D)**

Starts with an A, then a second letter and numbers

ECONOMIC GROWTH AND PRODUCTIVITY R&D (AE) AE1\_ Employment Growth and Productivity AE2\_ Product or Service Improvement AE3\_ Manufacturing Technology AE9\_ Other Economic Growth and Productivity

**b) Service codes**

Starts from B to Z

e.g.: MEDICAL SERVICES (Q) Q101 Dependent Medicare Services Q201 General Health Care Services Q301 Laboratory Testing Services Q401 Nursing Services

When leasing or buying property: MULTIPLE USE CODES FOR BUILDINGS AND FACILITIES Use the three digit identifiers shown below preceded by the appropriate Category Letter "E" for purchase of Structure and Facilities; "M" for Operation of Government-Owned Facilities; "X" for Lease or Rental of Facilities; "Y" for Construction of Structures and Facilities; or "Z" for Maintenance, Repair or Alteration of Real Property.

**c) Supplies and equipment codes**

This Part lists supplies and equipment codes of the Federal Supply Classification (FSC) which were developed for use in classifying items of supply identified under the Federal Cataloging Program.

The FSC is a commodity classification designed to serve the functions of supply and is sufficiently comprehensive in scope to permit the classification of all items of personal property. In order to accomplish this, groups and classes have been established for the universe of commodities, with emphasis on the items known to be in the supply systems of the Federal Government.

The structure of the FSC, as presently established, consists of 78 groups, which are subdivided into 685 classes. Each class covers a relatively homogeneous area of commodities, in respect to their physical or performance characteristics, or in the respect that the items included therein are such as are usually requisitioned or issued together, or constitute a related grouping for supply management purpose.

e.g.:

83 TEXITLES, LEATHER, FURS, APPAREL AND SHOE FINDINGS, TENTS AND FLAGS 8305 Textile Fabrics 8310 Yarn and Thread

-   **DUNS** Data Universal Numbering System <http://fedgov.dnb.com/webform/displayHomePage.do;jsessionid=E1E507FBF0B6F36823D2519B2F4E5981>

D-U-N-S is a code used for identifying business entities on a location-specific basis. This means that there is a DUNS for each physical location of your business. Assigned and maintained solely by D&B, this unique nine-digit identification number has been assigned to over 100 million businesses worldwide. A D-U-N-S Number remains with the company location to which it has been assigned even if it closes or goes out-of-business. The D-U-N-S Number also "unlocks" a wealth of value-added data associated with that entity, including the business name, physical and mailing addresses, tradestyles ("doing business as"), principal names, financial, payment experiences,industry classifications (SICs and NAICS), socio-economic status,government data and more. The D-U-N-S® Number also links members of corporate family trees worldwide.

The D-U-N-S® Number is widely used by both commercial and federal entities and was adopted as the standard business identifier for federal electronic commerce in October 1994.

-   **CAGE** The Commercial and Government Entity Code It is a unique identifier assigned to suppliers to various government or defense agencies, as well as to government agencies themselves and also various organizations. CAGE codes provide a standardized method of identifying a given facility at a specific location.

-   **Small Business**

The U.S. government is the world's largest buyer of products and services. Purchases by military and civilian installations amount to nearly $400 billion a year, and include everything from complex space vehicles to janitorial services. In short, the government buys just about every category of commodity and service available. By law, federal agencies are required to establish small business contracting goals.The current, government‐wide procurement goal is that at least 23% of all government buys should be awarded to small businesses. The key word here, however, is that it’s only a goal.

In addition, Federal contract goals are established for women‐owned businesses, small disadvantaged businesses, firms located in HUBZones and service disabled veteran‐owned businesses. These government‐wide goals, which are not always achieved, are 5%, 5%, 3% and 3%, respectively. They are important because Federal agencies have a statutory obligation to reach‐out and consider small businesses for procurement opportunities. However, again, these are only goals. It is up to you to market and match your business products and services to the buying needs of specific government agencies

Qualifying as a Small Business You may take it for granted that your company is a "small business." The distinction is important if you wish to register for government contracting as a small business. To be a small business, you must adhere to industry size standards established by the U.S. Small Business Administration. As you register as a government contractor in the System for Award Management (SAM), you will also self-certify your business as small.

The SBA, for most industries, defines a "small business" either in terms of the average number of employees over the past 12 months, or average annual receipts over the past three years. In addition, SBA defines a U.S. small business as a concern that:

-   Is organized for profit
-   Has a place of business in the US
-   Operates primarily within the U.S. or makes a significant contribution to the U.S. economy through payment of taxes or use of American products, materials or labor
-   Is independently owned and operated
-   Is not dominant in its field on a national basis

The business may be a sole proprietorship, partnership, corporation, or any other legal form. In determining what constitutes a small business, the definition will vary to reflect industry differences, such as size standards.

Size Standards Because all federal agencies must use SBA size standards for contracts identified as small business, you need to select NAICS codes that best describe your business and then determine if the business meet size standards for the selected NAICS codes <https://www.sba.gov/contracting/getting-started-contractor/make-sure-you-meet-sba-size-standards/summary-size-standards-industry-sector>

How the government buys <https://www.sba.gov/sites/default/files/Business%20Opportunities_Transcript_0.pdf>

Generally speaking, government purchases of individual items under $3,000.00 are considered micro‐ purchases.Such government buys do not require competitive bids or quotes and agencies can simply pay using a Government Purchase Card or credit card, without the involvement of a procurement officer. An important point to remember is that micro‐purchases, unlike other small government buys under $100,000, are not reserved for small businesses

Government agencies, however, are still required to advertise all planned purchases over $25,000 in Federal Business Opportunities, the government’s online listing and database of available procurement opportunities. Simplified procedures require fewer administrative details, fewer approval levels, and less documentation. The procedures require all federal purchases above $3,000, but under $100,000, to be reserved for small businesses, an important point

Sealed bidding is how the government buys competitively when its requirements are very specific, clear and complete. An IFB or “Invitation For Bid” is the method used for the sealed bid process. Typically, an IFB includes a description of the product or service to be acquired, instructions for preparing a bid, the conditions for purchase, delivery, payment and other requirements associated with the bid, including a deadline for bid submissions. Each sealed bid is opened in a public setting by a government contracting officer, at the time designated in the invitation. All bids are read aloud and recorded. A contract is then awarded by the agency to the lowest bidder who is determined to be fully responsive to the needs of the government.

-   **The Central Contractor Registration (CCR)**

The Central Contractor Registration, more commonly known as the CCR, is the primary source for agencies to learn about prospective vendors.The CCR is a government‐maintained database of companies wanting to do business with thegovernment. This database is a marketing tool for businesses and a searchable list of prospective vendors for the government. The CCR provides agencies with the ability to search for companies based on their abilities, size, location, experience, and ownership. In addition, the CCR is the vehicle used to identify or self‐certify your company as a small business, woman‐owned, veteran‐owned and/or a service‐disabled, veteran owned business

-   **HUBZones**

The term HUBZone comes from the phrase, Historically Underutilized Business Zone. The HUBZone Program is designed to stimulate economic development and create jobs in urban and rural communities by providing federal contracting preferences to small businesses. These preferences are available to small firms who qualify because they are located in a HUBZone designated area or employ staff who live in such an area.

-   **8(a) Business Development Program & Small Disadvantaged Business Certifications**

SBA administers two specific business assistance initiatives for small disadvantaged businesses. These initiatives are the 8(a) Program, we just discussed, and Small Disadvantaged Business Certifications. The SDB initiative requires eligible companies to be formally certified by the SBA. While the 8(a) Program offers a broad scope of assistance to socially and economically disadvantaged firms, SDB certification strictly pertains to benefits in federal procurement. Firms that are 8(a) certified automatically qualify for SDB certification

The 8(a) Business Development Program is designed to assist eligible socially and economically disadvantaged small businesses. The program provides qualified firms access to capital and credit, business counseling and training, and Federal and non‐Federal procurement opportunities. Through the award of sole source and limited‐competition contracts, the 8(a) program provides market access and growth for qualified businesses. The 8(a) program is not intended for everyone. You must be eligible to participate and receive formal certification from the SBA.

------------------------------------------------------------------------

Relevant Variables for analysis
===============================

I have included some variables I find interesting and divided them in categories four broad caterories that will be useful for analysis: 1. Contracting agency/office; 2. Contract details; 3. Vendor/Contractor details; 4. Product or service type

Those in **bold** seem the most useful to me...

**1. CONTRACTING AGENCY/OFFICE**

-   **maj\_agency\_cat** : chr "3600: Department of Veterans Affairs"
-   contractingofficeagencyid
-   contractingofficeid
-   agencyid
-   contractingofficeagencyid
-   contractingofficeid
-   contractingofficerbusinesssizedetermination

**2. CONTRACT DETAILS**

-   piid : character 1 The unique identifier for each contract, agreement or order.
-   **dollarsobligated** : num 336828 645347 72898 648209 34164 ...
-   **contractactiontype** : chr "C: DELIVERY ORDER"
-   typeofcontractpricing : chr "J: FIRM FIXED PRICE" "J: FIRM FIXED PRICE"
-   contractfinancing
-   **competitiveprocedures**
-   multipleorsingleawardidc : Indicates whether the contract is one of many that resulted from a single solicitation
-   multiyearcontract : Indicates whether this is a multi-year contract, a contract for the purchase of supplies or services for more than 1, but not more than 5, program years. Such contracts are issued under specific congressional

-   contingencyhumanitarianpeacekeepingoperation
-   currentcompletiondate

**Product service**

-   **productorservicecode** : chr "J041: MAINT/REPAIR/REBUILD OF EQUIPMENT..."
-   **psc\_cat** : The major category that the Federal Procurement Data System Product or Service Code

**Place of performance of Contract**

-   **placeofperformancecity** : chr "MONTROSE" "NEW YORK" "NORTHPORT" "JAMAICA" ...
-   **pop\_state\_code** : chr "NY: New York"
-   placeofperformancecountrycode : chr "USA: UNITED STATES OF AMERICA"
-   **placeofperformancezipcode** : int 105480001
-   pop\_cd : chr "NY17" "NY12" "NY03" "NY05" ...
-   placeofperformancecongressionaldistrict : chr "NY17"
-   locationcode : for the place of Performance

**3. VENDOR/CONTRACTOR DETAILS**

**category**

-   NAICS.code
-   NAICS.name
-   **principalnaicscode** : int 541611 561421

**id**

-   **vendorname** : chr "EASTCO BUILDING SERVICES, INC."
-   **dunsnumber** : int 605119932 18836440 793169512 830335969 79084053
-   parentdunsnumber : int 605119932 18836440
-   mod\_parent : The parent company for the vendor, as provided by D&B.

**descriptives**

-   annualrevenue
-   numberofemployees
-   numberofoffersreceived
-   organizationaltype
-   receivescontracts
-   receivescontractsandgrants
-   receivesgrants

**Location**

-   state
-   statecode
-   city
-   streetaddress
-   zipcode : of contractor address

**Business flags**

-   isconstructionfirm
-   ismanufacturerofgoods

-   emergingsmallbusinessflag
-   issbacertifiedsmalldisadvantagedbusiness : small business certified as disadvant...
-   isdotcertifieddisadvantagedbusinessenterprise : Indicates whether the vendor is DoT Certified Disadvantaged Business Enterprise
-   iscommunitydevelopedcorporationownedfirm
-   iscommunitydevelopmentcorporation

-   iscorporateentitynottaxexempt
-   iscorporateentitytaxexempt
-   isforprofitorganization

**Service flags**

-   hospitalflag
-   educationalinstitutionflag
-   isprivateuniversityorcollege
-   isstatecontrolledinstitutionofhigherlearning

**NonProfits flags**

-   isfoundation
-   isothernotforprofitorganization
-   nonprofitorganizationflag

**Government flags**

-   isschooldistrictlocalgovernment
-   ismunicipalitylocalgovernment
-   iscitylocalgovernment
-   iscouncilofgovernments
-   iscountylocalgovernment
-   isintermunicipallocalgovernment
-   islocalgovernmentowned
-   istownshiplocalgovernment
-   localgovernmentflag

-   isinterstateentity
-   isportauthority
-   stategovernmentflag

-   federalgovernmentflag
-   isfederalgovernmentagency
-   isfederallyfundedresearchanddevelopmentcorp

**Other Flags**

-   isforeigngovernment
-   isforeignownedandlocated
-   isinternationalorganization
-   isotherbusinessororganization

------------------------------------------------------------------------

Peeking into some variables
===========================

### maj\_agency\_cat

``` r
cat(head(unique(cnt$maj_agency_cat)), sep="\n") #to print a list.
```

    ## 3600: Department of Veterans Affairs
    ## 2800: Social Security Administration
    ## 7500: Department of Health and Human Services
    ## 1200: Department of Agriculture
    ## 1500: Department of Justice
    ## 4700: General Services Administration

### productorservicecode

This is the PSC code in string

``` r
cat(head(unique(cnt$productorservicecode)), sep="\n") #to print a list.
```

    ## J041: MAINT/REPAIR/REBUILD OF EQUIPMENT- REFRIGERATION, AIR CONDITIONING, AND AIR CIRCULATING EQUIPMENT
    ## R426: SUPPORT- PROFESSIONAL: COMMUNICATIONS
    ## J065: MAINT/REPAIR/REBUILD OF EQUIPMENT- MEDICAL, DENTAL, AND VETERINARY EQUIPMENT AND SUPPLIES
    ## V999: TRANSPORTATION/TRAVEL/RELOCATION- OTHER: OTHER
    ## J045: MAINT/REPAIR/REBUILD OF EQUIPMENT- PLUMBING, HEATING, AND WASTE DISPOSAL EQUIPMENT
    ## G099: SOCIAL- OTHER

### principalnaicscode

This is the NAICS code in string

``` r
cat(head(unique(cnt$NAICS.name)), sep="\n") #to print a list.
```

    ## Professional, Scientific, and Technical Services
    ## Administrative and Support and Waste Management and Remediation Services
    ## Other Services (except Public Administration)
    ## Transportation and Warehousing
    ## Construction
    ## Health Care and Social Assistance

### vendorname

``` r
cat(head(unique(cnt$vendorname)), sep="\n") #to print a list.
```

    ## EASTCO BUILDING SERVICES, INC.
    ## CORPORATE SOURCE, INC., THE
    ## CARESTREAM HEALTH, INC.
    ## MAINTENANCE MANAGEMENT SERVICES, LLC
    ## DIVISION CONSTRUCTION INC
    ## VOLUNTEERS OF AMERICA - GREATER NEW YORK, INC.

### contractactiontype

``` r
cat(unique(cnt$contractactiontype), sep="\n") #to print a list.
```

    ## C: DELIVERY ORDER
    ## D: DEFINITIVE CONTRACT
    ## B: PURCHASE ORDER
    ## A: BPA CALL
    ## G: GRANT FOR RESEARCH
    ## F: COOPERATIVE AGREEMENT
    ## T: TRAINING GRANT

------------------------------------------------------------------------

Exploring some Descriptives of the data
=======================================

``` r
dat <- cnt[,c("dollarsobligated", "maj_agency_cat", "contractactiontype", "vendorname", "productorservicecode", "NAICS.name")]
```

Main Vendors
------------

``` r
#aggregating by main Vendors
ven <- group_by(dat, vendorname)
ven <- summarize(ven, Monies = sum(dollarsobligated), Freq = n())

x <- ven$Monies < 0 
ven$Monies[x] <- 0
ven <- arrange(ven, desc(Monies))

paste("There are", nrow(ven), "vendors")
```

    ## [1] "There are 3255 vendors"

``` r
head(ven) %>% pander
```

<table style="width:65%;">
<colgroup>
<col width="43%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">vendorname</th>
<th align="center">Monies</th>
<th align="center">Freq</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">PFIZER INC.</td>
<td align="center">295600779</td>
<td align="center">86</td>
</tr>
<tr class="even">
<td align="center">BROOKHAVEN SCIENCE ASSOCIATES, LLC</td>
<td align="center">290494086</td>
<td align="center">24</td>
</tr>
<tr class="odd">
<td align="center">LOCKHEED MARTIN CORPORATION</td>
<td align="center">223505026</td>
<td align="center">609</td>
</tr>
<tr class="even">
<td align="center">SYSTEMS MADE SIMPLE, INC.</td>
<td align="center">139968271</td>
<td align="center">48</td>
</tr>
<tr class="odd">
<td align="center">MCCANN WORLD GROUP INC</td>
<td align="center">138791456</td>
<td align="center">97</td>
</tr>
<tr class="even">
<td align="center">TRUE NORTH COMMUNICATIONS INC.</td>
<td align="center">124969987</td>
<td align="center">1</td>
</tr>
</tbody>
</table>

``` r
barplot(ven$Monies)
```

![](contractsDF_files/figure-markdown_github/unnamed-chunk-9-1.png)

Main PSC
--------

``` r
#aggregating by main PSC

psc <- group_by(dat, productorservicecode)
psc <- summarize(psc, Monies = sum(dollarsobligated), Freq = n())

x <- psc$Monies < 0 
psc$Monies[x] <- 0
psc <- arrange(psc, desc(Monies))

paste("There are", nrow(psc), "PSCs")
```

    ## [1] "There are 1017 PSCs"

``` r
head(psc) %>% pander
```

<table style="width:64%;">
<colgroup>
<col width="41%" />
<col width="13%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">productorservicecode</th>
<th align="center">Monies</th>
<th align="center">Freq</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">6505: DRUGS AND BIOLOGICALS</td>
<td align="center">301336549</td>
<td align="center">418</td>
</tr>
<tr class="even">
<td align="center">M1HA: OPERATION OF GOVERNMENT-OWNED CONTRACTOR-OPERATED (GOCO) R&amp;D FACILITIES</td>
<td align="center">290479785</td>
<td align="center">22</td>
</tr>
<tr class="odd">
<td align="center">R426: SUPPORT- PROFESSIONAL: COMMUNICATIONS</td>
<td align="center">221175520</td>
<td align="center">27</td>
</tr>
<tr class="even">
<td align="center">R701: SUPPORT- MANAGEMENT: ADVERTISING</td>
<td align="center">141159015</td>
<td align="center">128</td>
</tr>
<tr class="odd">
<td align="center">Q201: MEDICAL- GENERAL HEALTH CARE</td>
<td align="center">111425142</td>
<td align="center">47</td>
</tr>
<tr class="even">
<td align="center">R705: SUPPORT- MANAGEMENT: DEBT COLLECTION</td>
<td align="center">106900127</td>
<td align="center">12</td>
</tr>
</tbody>
</table>

``` r
barplot(psc$Monies)
```

![](contractsDF_files/figure-markdown_github/unnamed-chunk-10-1.png)

Main NAICS
----------

``` r
#aggregating by main NAICS
nai <- group_by(dat, NAICS.name)
nai <- summarize(nai, Monies = sum(dollarsobligated), Freq = n())
options(scipen=999)

#eliminating negative sums
x <- nai$Monies < 0 
nai$Monies[x] <- 0

nai <- arrange(nai, desc(Monies))
nai <- nai[-which(is.na(nai$NAICS.name)),] #deleting the NA Row

paste("There are", nrow(nai), "PSCs")
```

    ## [1] "There are 19 PSCs"

``` r
head(nai) %>% pander
```

<table style="width:67%;">
<colgroup>
<col width="43%" />
<col width="15%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">NAICS.name</th>
<th align="center">Monies</th>
<th align="center">Freq</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Manufacturing</td>
<td align="center">1229864256</td>
<td align="center">33979</td>
</tr>
<tr class="even">
<td align="center">Professional, Scientific, and Technical Services</td>
<td align="center">1187089971</td>
<td align="center">2435</td>
</tr>
<tr class="odd">
<td align="center">Administrative and Support and Waste Management and Remediation Services</td>
<td align="center">136818522</td>
<td align="center">954</td>
</tr>
<tr class="even">
<td align="center">Finance and Insurance</td>
<td align="center">125018728</td>
<td align="center">57</td>
</tr>
<tr class="odd">
<td align="center">Health Care and Social Assistance</td>
<td align="center">83811307</td>
<td align="center">779</td>
</tr>
<tr class="even">
<td align="center">Construction</td>
<td align="center">59997144</td>
<td align="center">740</td>
</tr>
</tbody>
</table>

``` r
par(mar = c(6,6,3,3))
barplot(nai$Monies/100000, names.arg = nai$NAICS.name, las = 2, cex.names = .7, cex.axis = .7 , ylab = "USD 100,000", cex.lab = .8)
```

![](contractsDF_files/figure-markdown_github/unnamed-chunk-11-1.png)
