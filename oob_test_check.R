
len_fmla <- length(attr(terms(fmla), "term.labels"))

test.err = double(len_fmla)
oob.err = double(len_fmla)

for(mtry in 1:len_fmla)
{
  set.seed(1234)
  rf=randomForest(fmla, webb_train,mtry=mtry,ntree=500) 
  oob.err[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,webb_test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(webb_test, mean( (CUM12_Mboe - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}


matplot(1:mtry , cbind(oob.err,test.err), pch=19,
        col=c("red","blue"),type="b",ylab="Mean Squared Error",
        xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

plot(rf)


##create test / train without caret

set.seed(1234)
N <- nrow(webb_public)
target <- round(N * 0.75)
gp <- runif(N)
webb_train2 <- webb_public[gp < 0.75, ]
webb_test2 <- webb_public[gp > 0.75, ]


##=====================================================================

# Randomly assign rows to ids (1/2/3 represents train/valid/test)
# This will generate a vector of ids of length equal to the number of rows
# The train/valid/test split will be approximately 70% / 15% / 15% 
set.seed(12)
assign <- sample(1:3, size = nrow(webb_public), prob = c(0.7, 0.15, 0.15), replace = TRUE)

# Create a train, validation and tests from the original data frame 
webb_train <- webb_public[assign == 1, ]    # subset the grade data frame to training indices only
webb_valid <- webb_public[assign == 2, ]  # subset the grade data frame to validation indices only
webb_test <- webb_public[assign == 3, ]   # subset the grade data frame to test indices only

##=====================================================================

dflist <- list(dt_op, train_sm, test_sm)
dfname <- c("dt_op", "train_sm", "test_sm")
#dflist <- list(webb_public, webb_train, webb_test)
#dfname <- c("webb_public", "webb_train", "webb_test")
#dflist <- list(webb_public, webb_train, webb_valid, webb_test)
#dfname <- c("webb_public", "webb_train", "webb_validate", "webb_test")

metric <- data.frame()
for(i in 1:length(dflist)){
  df <- data.frame(dflist[i])
  df_tbl <- data.frame(wellcount = nrow(df))
  df_tbl$mean <- mean(df$CUM365_Mboe, na.rm = TRUE)
  df_tbl$median <- median(df$CUM365_Mboe, na.rm = TRUE)
  df_tbl$variance <- var(df$CUM365_Mboe, na.rm = TRUE)
  df_tbl$sd <- sd(df$CUM365_Mboe, na.rm = TRUE)
  df_tbl$P10 <- quantile(df$CUM365_Mboe, probs = (0.90), na.rm = TRUE)
  df_tbl$P90 <- quantile(df$CUM365_Mboe, probs = (0.10), na.rm = TRUE)
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
  
  


##====================================================================
##test/train----
#create test / train dataset
set.seed(1234)
trainRow <- createDataPartition(dt$CUM365_Mboe, p = 0.75, list = FALSE)
train_sm <- dt[trainRow, ]
test_sm <- dt[-trainRow, ]

set.seed(1234)
N <- nrow(webb_public)
target <- round(N * 0.75)
gp <- runif(N)
webb_train_random <- webb_public[gp < 0.75, ]
webb_test_random <- webb_public[gp > 0.75, ]



table(dt$Team)/nrow(dt)
table(train_sm$Team)/nrow(train_sm)
table(test_sm$Team)/nrow(test_sm)


dt %>%
  group_by(Team, ZONE) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/nrow(dt)), "%"))
  
train_sm %>%
  group_by(Team, ZONE) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/nrow(train_sm)), "%"))

test_sm %>%
  group_by(Team, ZONE) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/nrow(test_sm)), "%"))

##====================================================================
#public dataset

table(dt$Team)/nrow(dt)
table(train_sm$Team)/nrow(train_sm)
table(test_sm$Team)/nrow(test_sm)











