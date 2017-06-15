
donutz <- function (x, lev, xlim = c(0,4), width.max=3, width.min=2, border="white", border.size =1.5,
                     main = "have to put a title dude/dudette!", mar = 1)
{
  # Load Required Packages
  require(ggplot2)
  # Onto the function!
  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive numeric.")
  lev <- factor(x=as.character(lev), levels=as.character(lev), ordered = TRUE)
  x <- x/sum(x)
  ymax <- cumsum(x)
  ymin <- c(0, ymax[1:length(ymax)-1])
  mid <- (ymin + ymax)/2
  
  pie <- ggplot(data=NULL,
                aes(fill=lev,
                    xmax=width.max, xmin=width.min,
                    ymax=ymax, ymin=ymin)) +
    xlim(c(0, width.max + mar)) +
    geom_rect(colour=border,
              size = border.size) +
    coord_polar(theta="y") +
    geom_text(aes(x= (width.min+width.max)/2, y = mid, label = paste0(round(x*100, digits = 1),"%")),
              size=4,
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
          plot.margin = margin(0,0,0,0, "cm"),
          legend.margin = margin(0,0,0,0, "cm"),
          legend.title = element_blank(),
          legend.position = "right",
          legend.text = element_text(colour = "grey40", size = 8))
  return(pie)
}