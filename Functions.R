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

#----------------------------------------------------------------------------------------
ggplot_lm <- function (obs, pred) {
  
  require(ggplot2, caret)
  
  
  df <- data.frame(obs, pred) #combine into a data frame
  fit <- lm(obs ~ pred, df) #create linear model
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "MAE =",signif(MAE(pred, obs), 4),
                       " RMSE =",signif(RMSE(pred, obs), 4),
                       " P =",signif(summary(fit)$coef[2,4], 4))) +
    theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
}

ggplot_resid <- function (obs, pred) {
  
  require(ggplot2)
  
  
  df <- data.frame(obs, pred) #combine into a data frame
  fit <- lm(obs ~ pred, df) #create linear model
  df$resid <- fit$residuals
  
  ggplot(df, aes(x = pred, y = resid)) + 
    geom_point() +
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

qqpl <- function(data, col){ # qq plot
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

check_for_NAs <- function(x,useceil=F){
  if(useceil==T){
    apply(as.data.frame(x),2,FUN=function(x){ceiling(sum(is.na(x))/length(x))})
  } else {
    apply(as.data.frame(x),2,FUN=function(x){sum(is.na(x))/length(x)})
  }
}


rmlse <- function(model) { 
  y <- dt_op$CUM365_Mboe
  y.pred <- predict(model, dt_op$CUM365_Mboe)
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(mydat$a +1))^2)))
}
 