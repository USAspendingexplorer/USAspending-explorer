Grants data frame
================

``` r
#generating a csv with the variables
x <- gra16[1,]
x <- as.data.frame(lapply(x, class))
x <- as.data.frame(t(x))
x <- data.frame(var = row.names(x), class = as.character(x$V1), stringsAsFactors = F)
grav <- x
write.csv(grav, "gra.variables.csv", row.names = F)
```

``` r
#exploring some variables

#types of assistance
#what type of assistances are they getting?
unique(gra16$asst_cat_type)
unique(gra16$assistance_type)

#cfda_program_title
head(gra16$cfda_program_title)
head(gra16$cfda_program_num)
(table(gra16$cfda_program_title == "")) #has 110 blank ones.
table(is.na(gra16$cfda_program_title)) #no NAs here!
class(gra16$cfda_program_title)

#fda_program_num
table(is.na(gra16$cfda_program_num)) #no NAs here!
table(gra16$cfda_program_num == "") #no blanks!
class(gra16$cfda_program_num)

#project_description
unique(gra16$project_description)

######PRINCIPAL PLACE###############
#location: wanna focus on 4 cities, no idea if they are comparable.
sort(unique(gra16$principal_place_cc))
# "Syracuse" | "SYRACUSE"   
# "Rochester" | "ROCHESTER"
# "Albany" | "ALBANY"
#  "Ithaca" | "ITHACA" 

sort(unique(gra16$principal_place_state)) 
#Output is  "Illinois" "Massachusetts" "New York" "NEW YORK"
#this means that the filter in the manual download is not for this variable. 

######RECIPIENTS###############
sort(unique(gra16$recipient_state_code)) 
#output is only NY, so the filter is by recipients place, which makes sense because if we are an IDA we should care about the recipient not the pop.

head(sort(unique(gra16$recipient_city_name)))
# "Syracuse" | "SYRACUSE"   
# "Rochester" | "ROCHESTER"
# "Albany" | "ALBANY"
#  "Ithaca" | "ITHACA" 

head(sort(unique(gra16$recipient_county_code)))
head(sort(unique(gra16$recipient_county_name)))
head(sort(unique(gra16$recipient_state_code)))
```

Issue with consistency of the names
===================================

``` r
#looking for consistency in the names of agencies / programs

x <- gra16[, c("maj_agency_cat", "agency_name", "agency_code", "cfda_program_num", "cfda_program_title")]
x$maj <- substr(x$maj_agency_cat, 1,2)
x$cod <- substr(x$agency_code, 1,2)
x$cfda <- substr(as.character(x$cfda_program_num), 1,2)

#table(x$maj == x$cod) #only 4 inconsistencies between the first two numbers of agency maj and agency code.

#table(x$maj == x$cfda) #only 43 times they are the same
#and this is when:
#x[x$maj == x$cfda,] #19 for department of state in both cfda codes and agency code

length(unique(x$maj_agency_cat)) #24
```

    ## [1] 27

``` r
length(unique(x$maj)) #23
```

    ## [1] 26

``` r
length(unique(x$agency_code)) #83
```

    ## [1] 103

``` r
length(unique(x$cod)) #25
```

    ## [1] 28

``` r
length(unique(x$agency_name)) #67
```

    ## [1] 79

``` r
length(unique(x$cfda)) #24
```

    ## [1] 27

``` r
length(unique(x$cfda_program_num)) #782
```

    ## [1] 908

looking at action type
======================

``` r
#most common cfda

head(arrange(as.data.frame(table(gra16$cfda_program_title)), desc(Freq)))
```

    ##                                               Var1 Freq
    ## 1                Highway Planning and Construction 9792
    ## 2                                                  2375
    ## 3        Biomedical Research and Research Training  802
    ## 4                        Public and Indian Housing  763
    ## 5 Allergy, Immunology and Transplantation Research  593
    ## 6          Awards to Organizations and Individuals  515

``` r
x <- gra16$cfda_program_title == "Highway Planning and Construction"
dat <- gra16[x,]

#unique(gra16$action_type)
#A: new assistance
#B: continuation
#C: Revision
#D: Fnd adjustment

dat$act_typ <- substr(dat$action_type, 1, 1) #making it only the first code letter
head(arrange(as.data.frame(table(dat$act_typ)), desc(Freq)))
```

    ##   Var1 Freq
    ## 1    C 8359
    ## 2    A 1433

**ASSISSTANCE**

-   1 unique\_transaction\_id character
-   16 federal\_award\_id character

-   18 fed\_funding\_amount numeric
-   19 non\_fed\_funding\_amount numeric
-   20 total\_funding\_amount numeric

-   50 asst\_cat\_type character
-   24 assistance\_type character
-   25 record\_type integer : Federal Assistance Awards Data System record type: 1 = county aggregate record, 2 = individual action record.
-   47 fiscal\_year integer

-   23 ending\_date character

-   28 principal\_place\_code character
-   29 principal\_place\_state character
-   30 principal\_place\_cc character
-   32 principal\_place\_zip character
-   48 principal\_place\_state\_code character

**AGENCY/PROGRAM**

-   52 maj\_agency\_cat character
-   35 agency\_name character
-   4 cfda\_program\_num numeric
-   34 cfda\_program\_title character
-   15 agency\_code character

**RECIPIENT**

-   36 project\_description character
-   7 recipient\_name character
-   13 recipient\_type character
-   49 recip\_cat\_type character

-   8 recipient\_city\_code integer
-   9 recipient\_city\_name character
-   10 recipient\_county\_code integer
-   11 recipient\_county\_name character
-   56 recipient\_state\_code character
-   12 recipient\_zip integer
-   42 receip\_addr1 character
-   43 receip\_addr2 character
-   44 receip\_addr3 character

-   57 exec1\_fullname character
-   58 exec1\_amount numeric

recip\_cat\_type and recipient\_type equivalencies
==================================================

-   g: Government -&gt; 00: State government
-   g: Government -&gt; 02: City or township government
-   g: Government -&gt; 01: County government
-   g: Government -&gt; 04: Special district government
-   g: Government -&gt; 05: Independent school district
-   h: Government -&gt; 06: State controlled institution of higher education

-   n: Nonprofit agencies -&gt; "12: Other nonprofit"
-   n: Nonprofit agencies -&gt; "11: Indian tribe"

-   h: Private agencies -&gt; 20: Private higher education
-   f: Private agencies -&gt; 22: Profit organization
-   f: Private agencies -&gt; 23: Small business
-   i: Private agencies -&gt; 21: Individual
-   o: Private agencies -&gt; 25: All other

Questions
=========

1.  how to work with negatives?

2.  when looking at the action type

3.  Continuation (funding in succeeding budget period which stemmed from prior agreement to fund amount of the current action)" This means we are double counting money?
