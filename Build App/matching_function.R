
krzymatch <- function(x, county= "Albany", comparators= 2) 
{
  # Load Required Packages
  require(ggplot2)
  require(MatchIt)  
  
  #assigning the selected county into the treatment group
  tmp <- x$county.name == county
  x$Treat <- 0
  x$Treat[tmp] <- 1
  
  #creating objects
  lis <- rep(NA,comparators)
  dis <- c(rep(FALSE, 62), TRUE)
  
  #loop to find the top comparatos
  for (i in c(1:comparators)) 
  {
    mat <- matchit(x$Treat ~  Pop + MHincome + pov.rate, data = dat, discard = dis)
    tmp <- as.numeric(mat$match.matrix)
    dis[tmp] <- TRUE
    lis[i] <- x$county.name[tmp]
  }
  return(lis)
}