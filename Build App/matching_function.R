#this is a matching function that provides similar counties for the comparator. 

#this function is using an R package called Match It, developed by Gary King at the Harvard Kennedy School
#For more information look at: https://gking.harvard.edu/matchit

#this function needs to be fed a dataframe with counties and their Pop, Median Home Income, pov.rate.

krzymatch <- function(x, county= "Albany", comparators= 2) 

{
  # Load Required Packages
  require(MatchIt)  
  
  #assigning the selected county into the treatment group
  tmp <- x$county.name == county
  x$Treat <- 0
  x$Treat[tmp] <- 1
  
  #creating objects
  lis <- rep(NA,comparators)
  
  #loop to find the top comparatos
  for (i in c(1:comparators)) 
  {
    mat <- matchit(x$Treat ~  Pop + pov.rate, data = x)
    tmp <- as.numeric(mat$match.matrix)
    lis[i] <- as.character(x$county.name[tmp])
    x <- x[-tmp,]
    
  }
  return(lis)
}

