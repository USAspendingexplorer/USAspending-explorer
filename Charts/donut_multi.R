krzydonutz <- function (x, values, labels, multiple = NULL, width.max=3, width.min=2, border.col="white", border.cex =1.5,
                         main = "have to put a title dude/dudette!", mar = 1, percent.cex = 4, columns = 2, sbgfill = "grey95", sbgcol = "grey60")
{
  # Load Required Packages
  require(ggplot2)
  require(dplyr)
  
  #renaming the columns of the DF to work with them
  colnames(x)[colnames(x) == multiple] <- "multiple"
  colnames(x)[colnames(x) == labels] <- "labels"
  colnames(x)[colnames(x) == values] <- "values"
  
  #Warnings
  if (!is.numeric(x$values) || any(is.na(x$values) | x$values < 0))
    stop("'Values' variable cannot handle NA's and must be a numeric vector with only positive numbers.")
  
  if (length(unique(x$multiple)) > 8)
    stop("You are trying to plot too many donutzz! (8 max)")
  
  #making factors for labels
  x$labels <- factor(x=as.character(x$labels), ordered = F)
  
  #loop to format the dataframe and making the variables needed
  i <- 1
  df <- data.frame()
  x2 <- NULL
  
  while (i  <= length(unique(x$multiple))) 
  {
    x2 <- x$multiple == sort(unique(x$multiple))[i]
    x2 <- x[x2,]
    x2 <- x2[order(-x2$values),] 
    x2$fraction <- x2$values/sum(x2$values)
    x2$ymax <- cumsum(x2$fraction)
    x2$ymin <- c(0, x2$ymax[1:length(x2$ymax)-1])
    x2$mid <- (x2$ymin + x2$ymax)/2
    df <- rbind(df, x2)
    i = i + 1
  }
  
  x <- df
  
  pie <- ggplot(data=x,
                aes(fill=labels,
                    xmax=width.max, xmin=width.min,
                    ymax=ymax, ymin=ymin)) +
    xlim(c(0, width.max + mar)) +
    geom_rect(colour=border.col,
              size = border.cex) +
    coord_polar(theta="y") +
    geom_text(aes(x= (width.min+width.max)/2, y = x$mid, label = paste0(round(x$fraction*100, digits = 1),"%")),
              size= percent.cex,
              fontface = "bold",
              hjust = .5,
              vjust = .5,
              colour = "white") +
    ggtitle(label=main) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          axis.text=element_blank(), 
          axis.ticks=element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(fill= sbgfill, color = sbgcol), 
          plot.margin = margin(0,0,0,0, "cm"),
          legend.margin = margin(0,0,0,0, "cm"),
          legend.title = element_blank(),
          legend.position = "right",
          legend.text = element_text(colour = "grey40", size = 9)) +
    facet_wrap(~multiple, ncol = columns)
  return(pie)
}