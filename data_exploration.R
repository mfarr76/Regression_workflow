##qq plot function
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
    stat_qq() + ylab(col) +geom_abline(intercept = int, slope =  slope)+theme_bw()
  
}

##Explore the data.
expl_data <- function(df, col1, col2){
  g1 <- ggplot(df, aes(df[,col1])) + geom_density(fill="blue") + xlab(col1) + theme_bw()
  g2 <- ggplot(df, aes(log10(df[,col1]))) + geom_density(fill="brown") + xlab(paste("log",col1)) + theme_bw()
  g3 <- ggplot(df, aes(sqrt(df[,col1]))) + geom_density(fill="green") + xlab(paste("sqrt",col1)) + theme_bw()
  g4 <- ggplot(df, aes(df[,col2])) + geom_density(fill="blue") + xlab(paste(col2)) + theme_bw()
  g5 <- ggplot(df, aes(log10(df[,col2]))) + geom_density(fill="brown") + xlab(paste("log",col2)) + theme_bw()
  g6 <- ggplot(df, aes(sqrt(df[,col2]))) + geom_density(fill="green") + xlab(paste("sqrt",col2)) + theme_bw()
  grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 3, nrow = 2)
}

#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##combine train test set with label column
comb_webb <- webb_train %>%
  mutate(test_train = "train") %>%
  bind_rows(., webb_test %>%
              mutate(test_train = "test"))
#write.csv(comb_webb, file = "comb_dt.csv")

comb_webb %>%
  select(TVD,SoPhiH_LEF) %>%
  top_n(-5) %>%
  arrange(SoPhiH_LEF)


expl_data(webb_public, "CUM12_Mboe", "Lbs_Ft")

##qq plots
p1 <- qqpl(comb_webb, "sqrt_CUM12_Mboe")
p2 <- qqpl(comb_webb, "Eff_Lat")
p3 <- qqpl(comb_webb, "Lbs_Ft")
p4 <- qqpl(comb_webb, "Bbl_Ft")
p5 <- qqpl(comb_webb, "log_YIELD_12MO")
p6 <- qqpl(comb_webb, "Spacing_Avg")
grid.arrange(p1, p2, p3, p4, p5, p6 ,ncol=2,nrow=3)

q1 <- ggplot(comb_webb, aes(sample = SoPhiH_LEF)) + stat_qq()
q2 <- ggplot(comb_webb, aes(sample = log(SoPhiH_LEF))) + stat_qq()
q3 <- ggplot(comb_webb, aes(sample = sqrt(SoPhiH_LEF))) + stat_qq()
grid.arrange(q1, q2, q3, ncol = 3, nrow =1)

q1 <- qqnorm(comb_webb$CUM12_Mboe)
q2 <- qqnorm(log(comb_webb$CUM12_Mboe))
q3 <- qqnorm(sqrt(comb_webb$CUM12_Mboe))

g1 <- ggplot(comb_webb, aes(Bbl_Ft)) + geom_histogram(fill="blue")
g2 <- ggplot(comb_webb, aes(log10(Bbl_Ft))) + geom_histogram(fill="blue")
g3 <- ggplot(comb_webb, aes(sqrt(Bbl_Ft))) + geom_histogram(fill="blue")
grid.arrange(g1, g2, g3, ncol = 3, nrow = 1)

p1 <- ggplot(comb_webb, aes(x = test_train, y = CUM12_Mboe_Ft)) + geom_boxplot(fill="dodgerblue") + theme_bw()
p2 <- ggplot(comb_webb, aes(x = test_train, y = Lbs_Ft)) + geom_boxplot(fill="firebrick") + theme_bw()
p3 <- ggplot(comb_webb, aes(x = test_train, y = Bbl_Ft)) + geom_boxplot(fill="salmon") + theme_bw()
p4 <- ggplot(comb_webb, aes(x = test_train, y = TVD)) +  geom_boxplot(fill="seagreen") + theme_bw()
p5 <- ggplot(comb_webb, aes(x = test_train, y = log_YIELD_12MO)) + geom_boxplot(fill="yellowgreen") + theme_bw()
p6 <- ggplot(comb_webb, aes(x = test_train, y = Spacing_Avg)) +  geom_boxplot(fill="steelblue") + theme_bw()
grid.arrange(p1, p2, p3, p4, p5, p6 ,ncol=2,nrow=3)

p1 <- ggplot(comb_webb, aes(x = sqrt(Mboe_Ft))) + geom_density(fill="blue", size=1) + facet_wrap(~ test_train)
p2 <- ggplot(comb_webb, aes(x = log10(Lbs_Ft))) + geom_density(fill="red", size=1) + facet_wrap(~ test_train)
p3 <- ggplot(comb_webb, aes(x = log10(Bbl_Ft))) + geom_density(fill="yellow", size=1) + facet_wrap(~ test_train)
p4 <- ggplot(comb_webb, aes(x = TVD)) +  geom_density(fill="brown", size=1) + facet_wrap(~ test_train)
p5 <- ggplot(comb_webb, aes(x = log10(YIELD_12MO))) + geom_density(fill="black", size=1) + facet_wrap(~ test_train)
p6 <- ggplot(comb_webb, aes(x = Spacing_Avg)) +  geom_density(fill="red", size=1) + facet_wrap(~ test_train)
grid.arrange(p1, p2, p3, p4, p5, p6 ,ncol=2,nrow=3)

ggplot(comb_webb, aes(CUM12_Mboe)) + geom_histogram(fill="blue") + theme_classic()

ggplot(comb_webb, aes(x = OperatorName, y = CUM12_Mboe)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + coord_flip() + 
  labs(title = "CUM12_Mboe")


dflist <- list(webb_public, webb_train, webb_test)
dfname <- c("webb_public", "webb_train", "webb_test")
#dflist <- list(webb_public, webb_train, webb_test)
#dfname <- c("webb_public", "webb_train", "webb_test")
#dflist <- list(webb_public, webb_train, webb_valid, webb_test)
#dfname <- c("webb_public", "webb_train", "webb_validate", "webb_test")

metric <- data.frame()
for(i in 1:length(dflist)){
  df <- data.frame(dflist[i])
  df_tbl <- data.frame(wellcount = nrow(df))
  df_tbl$mean <- mean(df$CUM12_Mboe, na.rm = TRUE)
  df_tbl$median <- median(df$CUM12_Mboe, na.rm = TRUE)
  df_tbl$variance <- var(df$CUM12_Mboe, na.rm = TRUE)
  df_tbl$sd <- sd(df$CUM12_Mboe, na.rm = TRUE)
  df_tbl$P10 <- quantile(df$CUM12_Mboe, probs = (0.90), na.rm = TRUE)
  df_tbl$P90 <- quantile(df$CUM12_Mboe, probs = (0.10), na.rm = TRUE)
  #df_tbl$SoPhiH <- mean(df$SoPhiH_LEF, na.rm = TRUE)
  #df_tbl$tvd <- mean(df$TotalDepthTVD, na.rm = TRUE)
  metric <- rbind(metric, df_tbl)#store the results of each loop
  #rownames(metric) <- dfname[i]
  #print(metric)
}
rownames(metric) <- dfname
metric

comb_webb <- webb_train %>%
  mutate(test_train = "train") %>%
  bind_rows(., webb_test %>%
              mutate(test_train = "test"))
#write.csv(comb_dt, file = "comb_dt.csv")

comb_webb %>%
  group_by(OperatorName) %>%
  summarise(WellCount = n(), 
            Avg_CUM12_Mboe = mean(CUM12_Mboe), 
            Med_CUM12_Mboe = median(CUM12_Mboe), 
            Avg_Lat_Length = mean(Eff_Lat), 
            Avg_Lbs_Ft = mean(Lbs_Ft), 
            Avg_Bbl_Ft = mean(Bbl_Ft))
