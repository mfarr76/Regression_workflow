##Functions---------------------------------------------------------------------------

##count NA function
count_NA <- function(x){
  count_tbl <- sapply( x , function( y ) length ( which ( is.na (y) == TRUE)))
  dt_count <- data.frame( Item = colnames(x), Count = count_tbl)
  return(dt_count)
}

##count zero function
count_zero <- function(x){
  count_tbl <- sapply( x , function( y ) length ( which ( y == 0)))
  dt_count <- data.frame( Item = colnames(x), Count = count_tbl)
  return(dt_count)
}


#-------------------------------------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#----------------------------------------------------------------------------------------
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
    theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
}


##using caret rmse and r2 functions==============================
#rmse_fn <- function(predcol, ycol) {
#  res = predcol-ycol
#  sqrt(mean(res^2))
#}



#r_squared <- function(predcol, ycol) {
#  tss = sum( (ycol - mean(ycol))^2 )
#  rss = sum( (predcol - ycol)^2 )
#  1 - rss/tss
#}
##================================================================  

qqpl <- function(data, col){
  # Find 1st and 3rd quartile for the Alto 1 data
  y <- quantile(data[ ,col], c(0.25, 0.75))
  
  # Find the 1st and 3rd quartile of the normal distribution
  x <- qnorm( c(0.25, 0.75))
  
  # Now we can compute the intercept and slope of the line that passes
  # through these points
  slope <- diff(y) / diff(x)
  int   <- y[1] - slope * x[1]
  
  ggplot(data, aes(sample = data[ ,col])) + 
    stat_qq() + ylab(col) +geom_abline(intercept = int, slope =  slope)
  
}



  








  
