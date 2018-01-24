rm(list = ls())
# Parallel
library('doSNOW')

# general visualisation
library('ggplot2') # visualisation
library('RColorBrewer') # visualisation
library('grid') # visualisation

# general data manipulation
library('plyr') # data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# Dates
library('lubridate') # date and time

# Extra vis
library('ggforce') # visualisation
library('ggridges') # visualisation
library('corrplot') # visualisation
library('GGally')

# Model
library('caret')
library('ranger')
library('randomForest')
library('rpart')
library('rpart.plot')
library('broom')

##EagleFord Webb and Dimmit counties 
##Wellhead information in .csv file
#create stx package
stx <- read.csv("stxDataset.csv", header = TRUE)

##format datatypes----
##create column to convert to numeric
factor_col <- c("API","OperatorName","WellName","CountyName","TOE_UP_DWN",
                "Spacing_Category", "DateProductionStart")

##too many NAs and missing values...easier to hard code factor column labels
#factor_col <- colnames(select_if(webb_public, is.factor))
##create factor_col into factors
stx[factor_col] <- lapply(stx[factor_col], as.factor)
##create a list of colnames to turn into numeric
numeric_col <- !colnames(stx) %in% factor_col

##remove 0,000 from the number format
stx[numeric_col] <- data.frame(lapply(stx[numeric_col], gsub, pattern = ',', replacement = '', fixed = TRUE))
##convert numeric_col into numeric datatype
stx[numeric_col] <- sapply(stx[numeric_col], function(x) as.numeric(levels(x))[x])
##change first prod date into date datatype
stx$DateProductionStart <- as.Date(stx$DateProductionStart, "%m/%d/%Y")
##==========================================================================================

##create a sm dataset
stx_sm <- stx %>%
  filter(#OperatorName == "SM ENERGY COMPANY",
         Spacing_Category == "Bounded", 
         CountyName == "WEBB", 
         #str_detect(WellName, 'GAL'),
         FluidAmountTotal > 0, 
         ProppantAmountTotal > 0,
         First12MonthGas > 0, 
         YIELD_12M > 0) %>%
  mutate(Lbs_Ft = ProppantAmountTotal / PerfIntervalGross, 
         Bbl_Ft = (FluidAmountTotal / 42) / PerfIntervalGross,
         CUM12Gas_MMcf = First12MonthGas / 1000,
         CUM12_Mboe = (First12MonthLiquid + First12MonthGas / 6) / 1000,
         category = as.factor(ifelse(CUM12_Mboe > quantile(CUM12_Mboe, p = 0.75), "High", "Low")))
#  select(OperatorName, Lat, Lon, TOE_UP_DWN, Lbs_Ft, Bbl_Ft, Lbs_Ft, Bbl_Ft, TotalDepthTVD, 
#         SoPhiH_LEF, Spacing_Avg, Vintage, Cum12Gas_MMcf, EffLat = PerfIntervalGross, category)
#write.csv(stx_sm, file = "sm_stx.csv")


count_NA(stx_sm)

stx_sm_redu1 <- stx_sm %>%
  select(Cum12Gas_MMcf, Lbs_Ft, Bbl_Ft, TotalDepthTVD, 
         SoPhiH_LEF, Spacing_Avg, Lat, Lon)
#write.csv(stx_sm_redu1, file = "sm_stx_redu.csv")

summary(stx_sm_redu2)
ggplot(stx_sm, aes(x = OperatorName, fill = category)) + 
  geom_bar()


ggplot(high_sand, aes(x = OperatorName, fill = Category)) + 
  geom_bar(position = "fill") + 
  ylab("Relative frequencies")
table(stx_sm$category)

stx_sm_redu2 <- stx_sm %>%
  select(Cum12Gas_MMcf, SoPhiH_LEF)
##==========================================================================================


##---------------make plot of key metrics to check normality----------------------------------

p1 <- ggplot(stx_sm, aes(x = Cum12Gas_MMcf)) + 
  geom_density(fill = "black")

p2 <- ggplot(stx_sm, aes(x = PerfIntervalGross)) +
  geom_density(fill = "white")
  
p3 <- ggplot(stx_sm, aes(x = Lbs_Ft)) + 
  geom_density(fill = "yellow")

p4 <- ggplot(stx_sm, aes(x = Bbl_Ft)) + 
  geom_density(fill = "blue")

p5 <- ggplot(stx_sm, aes(x = SoPhiH_LEF)) + 
  geom_density(fill = "red")

p6 <- ggplot(stx_sm, aes(x = Spacing_Avg, colour = OperatorName)) + 
  geom_density()


multiplot(p1, p2, p3, p4, p5, p6, cols = 3)

##==========================================================================================


stx_num <- stx_sm %>% 
  select( - starts_with("First")) %>%
  select_if(is.numeric)

correlate <- cor(stx_num, use = "everything")
corrplot(correlate, method = "circle", type = "lower", sig.level = 0.01, insig = "blank")
ggpairs(stx_num[,1:10])
##==========================================================================================

##formula -----
sm_fmla <- CUM12_Mboe ~ PerfIntervalGross + TotalDepthTVD + SoPhiH_LEF + Spacing_Avg + 
                Bbl_Ft + Lbs_Ft + Max_Infill_Time + YIELD_12M
fmla <- sm_fmla

##create test / train dataset
set.seed(1234)
trainRow <- createDataPartition(stx_sm$CUM12_Mboe, p = 0.75, list = FALSE)
train_sm <- stx_sm[trainRow, ]
test_sm <- stx_sm[-trainRow, ]

##multi-variant linear model----
lm_stx <- lm(sm_fmla, data = train_sm)
pred_lm_stx <- predict(lm_stx, test_sm)
error_lm_stx <- pred_lm_stx - test_sm$Cum12_Mboe
RMSE_lm_stx <- sqrt(mean(error_lm_stx^2, na.rm = TRUE))
summary(lm_stx)
#evaluate_model(lm_stx)
#tidy(lm_stx)
##==========================================================================================


##randomForest model----
#createFolds/trainControl
set.seed(1234)
cv.5.folds <- createMultiFolds(stx_sm$Cum12_Mboe, k = 5, times = 5)
ctrl.1 <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                       index = cv.5.folds)

#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)


set.seed(1234)
rf_stx <- train(sm_fmla,
                data = train_sm, method = "rf",
                #preProcess = c("center", "scale"),
                tuneLength = 3, ntree = 1000,
                trControl = ctrl.1)

stopCluster(cl)

ggplot(rf_stx) + theme(legend.position = "top")
RMSE_rf_train <- RMSE(predict(rf_stx, train_sm), train_sm$Cum12_Mboe)
RMSE_rf_test <- RMSE(predict(rf_stx, test_sm), test_sm$Cum12_Mboe)


#Plotting Varianle importance for RandomForest
ggplot(varImp(rf_stx, scale = FALSE)) + ggtitle("RandomForest - Variable Importance") +
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
##==========================================================================================

##randomForest-Ranger model----
#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
rf_ranger <- train(sm_fmla,
                      data = train_sm, method = "ranger",
                      importance = "permutation", 
                      #preProcess = c("center", "scale"),
                      tuneLength = 3,
                      trControl = ctrl.1)

stopCluster(cl)

ggplot(rf_ranger) + theme(legend.position = "top")
RMSE_ranger_train <- RMSE(predict(rf_ranger, train_sm), train_sm$Cum12_Mboe)
RMSE_ranger_test <- RMSE(predict(rf_ranger, test_sm), test_sm$Cum12_Mboe)

#Plotting Varianle importance for RandomForest
ggplot(varImp(rf_ranger, scale = FALSE)) + ggtitle("RandomForest - Variable Importance") +
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
##==========================================================================================


##cubist----
#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
stx_cubist <- train(sm_fmla, train_sm, 
                    method = "cubist", 
                    turnLength = 3,
                    trControl = ctrl.1)
stopCluster(cl)


ggplot(stx_cubist) + theme(legend.position = "top")
RMSE_cub_train <- RMSE(predict(stx_cubist, train_sm), train_sm$Cum12_Mboe)
RMSE_cub_test <- RMSE(predict(stx_cubist, test_sm), test_sm$Cum12_Mboe)

#Plotting Varianle importance for Ranger
ggplot(varImp(stx_cubist, scale = FALSE)) + ggtitle("Cubist - Variable Importance") +
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
##==========================================================================================

##gbm model----
#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

#Creating grid
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),
                    shrinkage=c(0.01,0.05,0.1,0.5),
                    n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
#train model
set.seed(1234)
stx_gbm <- train(sm_fmla, data = train_sm, 
                 method = "gbm",
                 tuneGrid = grid,
                 tuneLength = 3,
                 trControl = ctrl.1)

stopCluster(cl)

ggplot(stx_gbm) + theme(legend.position = "top")
RMSE_gbm_train <- RMSE(predict(stx_gbm, train_sm), train_sm$Cum12_Mboe)
RMSE_gbm_test <- RMSE(predict(stx_gbm, test_sm), test_sm$Cum12_Mboe)

#Plotting Varianle importance for GBM
ggplot(varImp(object=stx_gbm)) + ggtitle("GBM - Variable Importance") +
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
##==========================================================================================

##rpart----
#----------

set.seed(1234)
rpart.cv <- train(sm_fmla,
                  data = train_sm,
                  method = "rpart", tuneLength = 30, 
                  trControl = ctrl.1)



#rp_model <- rpart(sm_fmla, data = stx_sm, cp = 0.02)
prp(rpart.cv$finalModel, type = 4)
prp(rpart.cv$finalModel, faclen = 3, cex = 0.8, extra = 1)
rpart.plot(rpart.cv$finalModel, type = 4)
##==========================================================================================

#compare models----
resamps <- resamples(list(RFT = rf_stx,
                          RAN = rf_ranger, 
                          CUB = stx_cubist, 
                          GBM = stx_gbm))
summary(resamps)

trellis.par.set(caretTheme())
xyplot(resamps, what = "BlandAltman")

trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")


mv_train <- list(RFT = rf_stx,
                 RAN = rf_ranger, 
                 CUB = stx_cubist, 
                 GBM = stx_gbm)

r2 <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Train = summary(resamps)$statistics$Rsquared[i,4])
  dt$Test <- R2(predict(mv_train[[i]], test_sm), test_sm$Cum12_Mboe)
  r2 <- rbind(r2, dt) #store the results of each loop
  #print(r2)
}
r2

nam <- c("RF", "RAN", "CUB", "GBM")
#add rownames
rownames(r2) <- nam
r2_t <- t(r2)

library(reshape2)
tmp_r2 <- melt(r2_t)
ggplot(tmp_r2, aes(x=Var2, y=value, fill=factor(Var1))) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  labs(title = "r2 Comparison", x = "Model", y = "Cum12 Mboe") +
  scale_fill_brewer(type="qual", palette="Set1")+
  guides(fill = guide_legend("Data")) + 
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))

rmse <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Train = summary(resamps)$statistics$RMSE[i,4])
  dt$Test <- RMSE(predict(mv_train[[i]], test_sm), test_sm$Cum12_Mboe)
  rmse <- rbind(rmse, dt) #store the results of each loop
  #print(rmse)
}
rmse

#add rownames
rownames(rmse) <- nam
rmse_t <- t(rmse)


tmp_rmse <- melt(rmse_t)

ggplot(tmp_rmse, aes(x=Var2, y=value)) +
  geom_bar(aes(fill = Var1),stat="identity", position = "dodge") +
  labs(title = "RMSE Comparison", x = "Model", y = "Cum12 Mboe") + 
  guides(fill = guide_legend("Data")) + 
  scale_fill_brewer(type="qual", palette="Set1")+
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
#scale_fill_brewer(type="qual", palette=1)

##==========================================================================================

test_sm$model_output <- pred_ranger

ggplot(test_sm, aes(x = model_output, y = Cum12_Mboe)) +
  geom_point() + 
  geom_smooth(method = "lm")

mod <- lm(Cum12_Mboe ~ model_output, data = test_sm)
summary(mod)

ggplotRegression(mod)
##==========================================================================================


#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
model_lm_train <- train(sm_fmla, train_sm,method='lm', metric = 'RMSE', na.action = na.omit)
model_rf_train <- train(sm_fmla, train_sm,method='rf', metric = 'RMSE', na.action = na.omit)
model_ran_train <- train(sm_fmla, train_sm,method='ranger', metric = 'RMSE', na.action = na.omit)
model_cub_train <- train(sm_fmla, train_sm,method='cubist', metric = 'RMSE', na.action = na.omit)
model_gbm_train <- train(sm_fmla, train_sm,method='gbm', metric = 'RMSE', na.action = na.omit)

stopCluster(cl)

#compare models----
resamps <- resamples(list(LM = model_lm_train,
                          RFT = model_rf_train,
                          RAN = model_ran_train, 
                          CUB = model_cub_train, 
                          GBM = model_gbm_train))

mv_train <- list(model_lm_train, 
                 model_rf_train, 
                 model_ran_train, 
                 model_cub_train, 
                 model_gbm_train)


r2 <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Train = summary(resamps)$statistics$Rsquared[i,4])
  dt$Test <- R2(predict(mv_train[[i]], test_sm), test_sm$Cum12_Mboe)
  r2 <- rbind(r2, dt) #store the results of each loop
  #print(r2)
}
r2

nam <- c("LM", "RF", "RAN", "CUB", "GBM")
#add rownames
rownames(r2) <- nam
r2_t <- t(r2)

library(reshape2)
tmp_r2 <- melt(r2_t)
ggplot(tmp_r2, aes(x=Var2, y=value, fill=factor(Var1))) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  labs(title = "r2 Comparison", x = "Model", y = "Cum12 Mboe") +
  scale_fill_brewer(type="qual", palette="Set1")+
  guides(fill = guide_legend("Data")) + 
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))

rmse <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Train = summary(resamps)$statistics$RMSE[i,4])
  dt$Test <- RMSE(predict(mv_train[[i]], test_sm), test_sm$Cum12_Mboe)
  rmse <- rbind(rmse, dt) #store the results of each loop
  #print(rmse)
}
rmse

#add rownames
rownames(rmse) <- nam
rmse_t <- t(rmse)


tmp_rmse <- melt(rmse_t)

ggplot(tmp_rmse, aes(x=Var2, y=value)) +
  geom_bar(aes(fill = Var1),stat="identity", position = "dodge") +
  labs(title = "RMSE Comparison", x = "Model", y = "Cum12 Mboe") + 
  guides(fill = guide_legend("Data")) + 
  scale_fill_brewer(type="qual", palette="Set1")+
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
#scale_fill_brewer(type="qual", palette=1)
