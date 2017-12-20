rm(list = ls())
# Parallel
library('doSNOW')

# general visualisation
library('ggplot2') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
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

# Model
library('caret')
library('ranger')
library('randomForest')
library('rpart')
library('rpart.plot')
library('broom')
library('lars')
library('glmnet')

dt <- read.csv("AT_FinalDataset.csv", header = TRUE) %>%
  mutate(API = as.character(API), 
         Prod_Date = as.Date(Prod_Date, "%m/%d/%Y")) %>%
  mutate_if(is.integer, as.numeric)

#glimpse(dt)

count_NA(dt)
count_zero(dt)

#dt[dt == 0] <- NA 



dt_op <- dt %>%
  filter(!is.na(CUM365_Gas), 
         Stage_Spacing > 0,
         !is.na(LEF_H)) %>%
  select(CompWellName, ZONE, Well_Spacing, Stage_Count, Stage_Spacing, Eff_Lat, Bbl_Ft, 
         Lbs_Ft, NGL_Yield, Team, CUM365_Gas, CUM365_Oil, CUM365_Mboe, Yield = Lifetime.Yield, 
         TOC_LEF_P50, Sw_LEF_P50, Phi_LEF_P50, VCLAY_LEF_P50, Res_Press, Carbon.13.Isotope, 
         PERM_LEF_P50, RHOB_LEF_P50, TMAX_LEF_P50, Dew_P_LEF, NG_3, NG_11, LEF_H, TVD, TEF_H)

#dt_op[dt_op == 0] <- NA 

#write.csv(dt_op, file = "dt_op.csv")
count_NA(dt_op)
count_zero(dt_op)
nrow(dt_op)


##==========================================================================================
##formula----
sm_fmla <- CUM365_Mboe ~ Eff_Lat + Lbs_Ft + Bbl_Ft + 
  Stage_Count + TVD + Well_Spacing + TEF_H

data <- dt_op %>% select(CUM365_Mboe, Eff_Lat, Lbs_Ft, Bbl_Ft, 
                   Stage_Count, TVD, Well_Spacing, TEF_H)

count_NA(data)
count_zero(data)
nrow(data)

##==========================================================================================

##test/train----
#create test / train dataset
set.seed(1234)
trainRow <- createDataPartition(dt_op$CUM365_Mboe, p = 0.8, list = FALSE)
train_sm <- dt_op[trainRow, ]
test_sm <- dt_op[-trainRow, ]

##==========================================================================================

#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)


set.seed(1234)
mod_lm <- train(sm_fmla, train_sm,method='lm', metric = 'RMSE', importance = TRUE, na.action = na.omit)
mod_rf <- train(sm_fmla, train_sm,method='rf', metric = 'RMSE', na.action = na.omit)
mod_ran <- train(sm_fmla, train_sm,method='ranger', metric = 'RMSE', na.action = na.omit)
mod_cub <- train(sm_fmla, train_sm,method='cubist', metric = 'RMSE', na.action = na.omit)
mod_gbm <- train(sm_fmla, train_sm,method='gbm', metric = 'RMSE', na.action = na.omit)

stopCluster(cl)

##==========================================================================================
#createFolds/trainControl
set.seed(1234)
cv.10.folds <- createMultiFolds(train_sm$CUM365_Mboe, k = 10, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                       index = cv.10.folds)


#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
mod_lm_rcv <- train(sm_fmla, train_sm,method='lm', trControl = ctrl.1, metric = 'RMSE')
mod_rf_rcv <- train(sm_fmla, train_sm,method='rf', trControl = ctrl.1, metric = 'RMSE')
mod_ran_rcv <- train(sm_fmla, train_sm,method='ranger', trControl = ctrl.1, metric = 'RMSE')
mod_cub_rcv <- train(sm_fmla, train_sm,method='cubist', trControl = ctrl.1, metric = 'RMSE')
mod_gbm_rcv <- train(sm_fmla, train_sm,method='gbm', trControl = ctrl.1, metric = 'RMSE')

stopCluster(cl)


##compare models----
resamps_rcv <- resamples(list(LM_rcv = mod_lm_rcv,
                          RF_rcv = mod_rf_rcv,
                          RAN_rcv = mod_ran_rcv,
                          CUM_rcv = mod_cub_rcv,
                          GBM_rcv = mod_gbm_rcv))

resamps_train <- resamples(list(LM = mod_lm,
                           RF = mod_rf,
                           RAN = mod_ran,
                           CUB = mod_cub,
                           GBM = mod_gbm))
                 
mv_train <- list(mod_lm,
               mod_rf,
               mod_ran,
               mod_cub,
               mod_gbm)

mv_rcv <- list(mod_lm_rcv,
                mod_rf_rcv,
                mod_ran_rcv,
                mod_cub_rcv,
                mod_gbm_rcv)

nam <- c("LM", "RF", "RAN", "CUB", "GBM")

r2 <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Train = R2(predict(mv_train[[i]], train_sm), train_sm$CUM365_Mboe))
  dt$Test <- R2(predict(mv_train[[i]], test_sm), test_sm$CUM365_Mboe)
  dt$Test_rcv <- R2(predict(mv_rcv[[i]], test_sm), test_sm$CUM365_Mboe)
  r2 <- rbind(r2, dt) #store the results of each loop
  #print(r2)
}
r2


rmse <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Train = RMSE(predict(mv_train[[i]], train_sm), train_sm$CUM365_Mboe))
  dt$Test <- RMSE(predict(mv_train[[i]], test_sm), test_sm$CUM365_Mboe)
  dt$Test_rcv <- RMSE(predict(mv_rcv[[i]], test_sm), test_sm$CUM365_Mboe)
  rmse <- rbind(rmse, dt) #store the results of each loop
  #print(rmse)
}
rmse


##==========================================================================================

modelLookup(model='lm')
modelLookup(model='rf')
modelLookup(model='ranger')
modelLookup(model='cubist')
modelLookup(model='gbm')

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),
                    shrinkage=c(0.01),
                    n.minobsinnode = c(10),
                    interaction.depth=c(30))

#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

model_gbm <- train(sm_fmla, train_sm,method='gbm', trControl = fitControl, 
                   tuneGrid = grid)
#model_gbm <- train(sm_fmla, train_sm,method='gbm', trControl = fitControl, tuneLength = 10)
rmse_gbm <- RMSE(predict(model_gbm, test_sm), test_sm$CUM365_Mboe)

stopCluster(cl)

##==========================================================================================
rmse_lm_full <- RMSE(predict(model_lm_full, dt_op), dt_op$CUM365_Mboe)
rmse_rf_full <- RMSE(predict(model_rf_full, dt_op), dt_op$CUM365_Mboe)
rmse_ran_full <- RMSE(predict(model_ran_full, dt_op), dt_op$CUM365_Mboe)
rmse_gbm_full <- RMSE(predict(model_gbm_full, dt_op), dt_op$CUM365_Mboe)
rmse_cub_full <- RMSE(predict(model_cub_full, dt_op), dt_op$CUM365_Mboe)

rmse_lm_train <- RMSE(predict(model_lm_train, train_sm), train_sm$CUM365_Mboe)
rmse_rf_train <- RMSE(predict(model_rf_train, train_sm), train_sm$CUM365_Mboe)
rmse_ran_train <- RMSE(predict(model_ran_train, train_sm), train_sm$CUM365_Mboe)
rmse_gbm_train <- RMSE(predict(model_gbm_train, train_sm), train_sm$CUM365_Mboe)
rmse_cub_train <- RMSE(predict(model_cub_train, train_sm), train_sm$CUM365_Mboe)

rmse_lm_train_test <- RMSE(predict(model_lm_train, test_sm), test_sm$CUM365_Mboe)
rmse_rf_train_test <- RMSE(predict(model_rf_train, test_sm), test_sm$CUM365_Mboe)
rmse_ran_train_test <- RMSE(predict(model_ran_train, test_sm), test_sm$CUM365_Mboe)
rmse_gbm_train_test <- RMSE(predict(model_gbm_train, test_sm), test_sm$CUM365_Mboe)
rmse_cub_train_test <- RMSE(predict(model_cub_train, test_sm), test_sm$CUM365_Mboe)

rmse_lm_full <- RMSE(predict(model_lm_train, dt_op), dt_op$CUM365_Mboe)
rmse_rf_full <- RMSE(predict(model_rf_train, dt_op), dt_op$CUM365_Mboe)
rmse_ran_full <- RMSE(predict(model_ran_train, dt_op), dt_op$CUM365_Mboe)
rmse_gbm_full <- RMSE(predict(model_gbm_train, dt_op), dt_op$CUM365_Mboe)
rmse_cub_full <- RMSE(predict(model_cub_train, dt_op), dt_op$CUM365_Mboe)



##compare models----
resamps <- resamples(list(LM_full = model_lm_full,
                          LM_train = model_lm_train,
                          RF_full = model_rf_full,
                          RF_train = model_rf_train,
                          RAN_full = model_ran_full,
                          RAN_train = model_ran_train,
                          CUB_full = model_cub_full,
                          CUM_train = model_cub_train,
                          GBM_full = model_gbm_full, 
                          GBM_train = model_gbm_train))

mv_train <- list(model_lm_train, 
                  model_rf_train, 
                  model_ran_train, 
                  model_cub_train, 
                  model_gbm_train)


mv_full <- list(model_lm_full, 
                 model_rf_full, 
                 model_ran_full, 
                 model_cub_full, 
                 model_gbm_full)


trellis.par.set(caretTheme())
xyplot(resamps_rcv, what = "BlandAltman")

trellis.par.set(caretTheme())
dotplot(resamps_rcv, metric = "Rsquared")

nam <- c("LM", "RF", "RAN", "CUB", "GBM")

r2 <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Full = R2(predict(mv_full[[i]], dt_op), dt_op$CUM365_Mboe))
  dt$Train <- summary(resamps)$statistics$Rsquared[i,4]
  dt$Test <- R2(predict(mv_train[[i]], test_sm), test_sm$CUM365_Mboe)
  r2 <- rbind(r2, dt) #store the results of each loop
  #print(r2)
}
r2


rmse <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Full = RMSE(predict(mv_full[[i]], dt_op), dt_op$CUM365_Mboe))
  dt$Train <- summary(resamps)$statistics$RMSE[i,4]
  dt$Test <- RMSE(predict(mv_train[[i]], test_sm), test_sm$CUM365_Mboe)
  rmse <- rbind(rmse, dt) #store the results of each loop
  #print(rmse)
}
rmse

#add rownames
rownames(r2) <- nam
r2_t <- t(r2)

#add rownames
rownames(rmse) <- nam
rmse_t <- t(rmse)


library(reshape2)
tmp_r2 <- melt(r2_t)
ggplot(tmp_r2, aes(x=Var2, y=value, fill=factor(Var1))) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  labs(title = "r2 Comparison", x = "Model", y = "Cum12 Mboe") +
  scale_fill_brewer(type="qual", palette="Set1")+
  guides(fill = guide_legend("Data")) + 
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))

tmp_rmse <- melt(rmse_t)

ggplot(tmp_rmse, aes(x=Var2, y=value)) +
  geom_bar(aes(fill = Var1),stat="identity", position = "dodge") +
  labs(title = "RMSE Comparison", x = "Model", y = "Cum12 Mboe") + 
  guides(fill = guide_legend("Data")) + 
  scale_fill_brewer(type="qual", palette="Set1")+
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
  #scale_fill_brewer(type="qual", palette=1)



ggplot(varImp(mod_lm, useModel = FALSE))
ggplot(varImp(mod_rf, useModel = FALSE))
ggplot(varImp(mod_ran, useModel = TRUE))
ggplot(varImp(model_cub_train, useModel = FALSE))
ggplot(varImp(mod_gbm_rcv, useModel = FALSE))

ggplot(mod_rf_rcv$finalModel$importance)
barchart(mod_rf_rcv$finalModel$importance)

rf_train <- randomForest(sm_fmla, train_sm)
rf_full <- randomForest(sm_fmla, dt_op)
rmse_rf_train <- RMSE(predict(rf, test_sm), test_sm$CUM365_Mboe)
rmse_rf_full <- RMSE(predict(rf_full, dt_op), dt_op$CUM365_Mboe)
plot(importance(rf))

resamps


set.seed(1234)
model_lm_full <- train(sm_fmla, dt_op,method='lm', metric = 'RMSE', na.action = na.omit)
model_rf_full <- train(sm_fmla, dt_op,method='rf', metric = 'RMSE', na.action = na.omit)
model_ran_full <- train(sm_fmla, dt_op,method='ranger', metric = 'RMSE', na.action = na.omit)
model_cub_full <- train(sm_fmla, dt_op,method='cubist', metric = 'RMSE', na.action = na.omit)
model_gbm_full <- train(sm_fmla, dt_op,method='gbm', metric = 'RMSE', na.action = na.omit)

