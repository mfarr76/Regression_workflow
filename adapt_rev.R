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
library('corrplot') # visualisation
library('GGally')

# Model
library('caret')
library('ranger')
library('randomForest')
library('rpart')
library('rpart.plot')
library('broom')
library('gbm')



dt <- read.csv("AT_FinalDataset.csv", header = TRUE) %>%
  mutate(API = as.character(API), 
         Prod_Date = as.Date(Prod_Date, "%m/%d/%Y")) %>%
  mutate_if(is.integer, as.numeric)

#glimpse(dt)

count_NA(dt)
count_zero(dt)

#dt[dt == 0] <- NA 


dt_op <- dt %>%
  mutate(Well_Area = (Eff_Lat * Well_Spacing)/43560) %>%
  filter(!is.na(CUM365_Mboe), 
         Cluster_Spacing > 0,
         !is.na(LEF_H)) 

dt_op1 <- dt_op %>%
  select(CUM365_Mboe, Carbon.13.Isotope, TMAX_TEF_P50, Res_Press, Sw_CALC, NG_3, Dew_P_LEF, Eff_Lat,
         Lifetime.Yield, Pressure_Gradient, Bbl_Ft, VLIME_TEF_P50, Stage_Count, TOC_TEF_P50, Vclay_CALC,
         Lbs_Ft, Stage_Spacing, RHOB_TEF_P50, Well_Spacing, Cluster_Spacing, Sw_CALC, UEF_PERC, LEF_PERC,
         Well_Area)


#dt_op[dt_op == 0] <- NA 

#write.csv(dt_op, file = "dt_op.csv")
count_NA(dt_op1)
count_zero(dt_op)
nrow(dt_op)


##==========================================================================================
##formula----
sm_fmla <- CUM365_Mboe ~ Bbl_Ft + Stage_Count + Well_Spacing + UEF_H + LEF_H + Lbs_Ft + 
            Eff_Lat

at_fmla <-  CUM365_Mboe ~ Carbon.13.Isotope + TMAX_TEF_P50 + Res_Press + Sw_CALC + 
            NG_3 + Dew_P_LEF + Eff_Lat + Lifetime.Yield + Pressure_Gradient + Bbl_Ft + VLIME_TEF_P50 +
            Stage_Count + TOC_TEF_P50 + Vclay_CALC + Lbs_Ft + Stage_Spacing + RHOB_TEF_P50 + Well_Spacing +
            Cluster_Spacing + Sw_CALC + UEF_PERC + LEF_PERC + Well_Area

at1_fmla <- CUM365_Mboe ~ Stage_Count + Eff_Lat + Bbl_Ft + Lbs_Ft + Stage_Spacing + Cluster_Spacing + 
            Lifetime.Yield + Res_Press + Carbon.13.Isotope + RHOB_TEF_P50 + Dew_P_LEF + UEF_PERC + 
            LEF_PERC + Pressure_Gradient + Well_Area + NG_3_CALC + Vclay_CALC + Sw_CALC + Phi_CALC + 
            NG_3_CALC + NG_11_CALC + Vclay_CALC

predictors <-  c("CUM365_Mboe", "Carbon.13.Isotope", "TMAX_TEF_P50", "Res_Press", "Sw_CALC", 
                 "NG_3", "Dew_P_LEF", "Eff_Lat", "Lifetime.Yield", "Pressure_Gradient", 
                 "Bbl_Ft", "VLIME_TEF_P50", "Stage_Count", "TOC_TEF_P50", "Vclay_CALC", 
                 "Lbs_Ft", "Stage_Spacing", "RHOB_TEF_P50", "Well_Spacing", "Cluster_Spacing", 
                 "Sw_CALC", "UEF_PERC", "LEF_PERC", "Well_Area")

count_NA(dt_op1)
count_zero(dt_op1)
nrow(dt_op1)

##==========================================================================================
##test/train----
#create test / train dataset
set.seed(1234)
trainRow <- createDataPartition(dt_op$CUM365_Mboe, p = 0.80, list = FALSE)
train_sm <- dt_op[trainRow, ]
test_sm <- dt_op[-trainRow, ]

count_NA(train_sm)
count_zero(train_sm)

##==========================================================================================
##select formula to use for model runs
fmla <- at_fmla

##speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)


set.seed(1234)
mod_lm <- train(fmla, train_sm,method='lm', metric = 'RMSE',na.action = na.omit)
mod_rf <- train(fmla, train_sm,method='rf', metric = 'RMSE', na.action = na.omit)
mod_ran <- train(fmla, train_sm,method='ranger', metric = 'RMSE', na.action = na.omit)
#mod_cub <- train(fmla, train_sm,method='cubist', metric = 'RMSE', na.action = na.omit)
mod_gbm <- train(fmla, train_sm,method='gbm', metric = 'RMSE', na.action = na.omit)

stopCluster(cl)

set.seed(1234)
mod_gbm <- gbm(fmla, train_sm[, predictors],shrinkage = 0.001, distribution = "gaussian", 
               cv.folds = 3, n.trees = 10000)
(best.iter <- gbm.perf(mod_gbm))

##==========================================================================================
#createFolds/trainControl
set.seed(1234)
cv.5.folds <- createMultiFolds(train_sm$CUM365_Mboe, k = 5, times = 5)
ctrl.1 <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                       index = cv.5.folds)
#folds <- createFolds(train_sm$CUM365_Mboe, k = 5, list = TRUE, returnTrain = TRUE)

#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
mod_lm_rcv <- train(fmla, train_sm,method='lm', trControl = ctrl.1, metric = 'RMSE')
mod_rf_rcv <- train(fmla, train_sm,method='rf', trControl = ctrl.1, metric = 'RMSE')
mod_ran_rcv <- train(fmla, train_sm,method='ranger', trControl = ctrl.1, metric = 'RMSE')
mod_cub_rcv <- train(fmla, train_sm,method='cubist', trControl = ctrl.1, metric = 'RMSE')
mod_gbm_rcv <- train(fmla, train_sm,method='gbm', trControl = ctrl.1, metric = 'RMSE')

stopCluster(cl)

##==========================================================================================
##compare models----

resamps_train <- resamples(list(LM = mod_lm,
                                RF = mod_rf,
                                RAN = mod_ran,
                                #CUB = mod_cub,
                                GBM = mod_gbm))

resamps_rcv <- resamples(list(LM_rcv = mod_lm_rcv,
                          RF_rcv = mod_rf_rcv,
                          RAN_rcv = mod_ran_rcv,
                          CUM_rcv = mod_cub_rcv,
                          GBM_rcv = mod_gbm_rcv))

mv_train <- list(mod_lm,
               #mod_rf,
               mod_ran,
               #mod_cub,
               mod_gbm)

mv_rcv <- list(mod_lm_rcv,
                mod_rf_rcv,
                mod_ran_rcv,
                mod_cub_rcv,
                mod_gbm_rcv)

r2

r2 <- data.frame()
for(i in 1:length(mv_train)){
  df <- data.frame(Train = R2(predict(mv_train[[i]], train_sm), train_sm$CUM365_Mboe))
  df$Test <- R2(predict(mv_train[[i]], test_sm), test_sm$CUM365_Mboe)
  #dt$Test_rcv <- R2(predict(mv_rcv[[i]], test_sm), test_sm$CUM365_Mboe)
  r2 <- rbind(r2, df) #store the results of each loop
  #print(r2)
}
r2


rmse <- data.frame()
for(i in 1:length(mv_train)){
  dt <- data.frame(Train = RMSE(predict(mv_train[[i]], train_sm), train_sm$CUM365_Mboe))
  dt$Test <- RMSE(predict(mv_train[[i]], test_sm), test_sm$CUM365_Mboe)
  #dt$Test_rcv <- RMSE(predict(mv_rcv[[i]], test_sm), test_sm$CUM365_Mboe)
  rmse <- rbind(rmse, dt) #store the results of each loop
  #print(rmse)
}
rmse

#create rownames
nam <- c("LM", "RAN", "GBM")
#add rownames to data.frame
rownames(r2) <- nam
#tranpose the data.frame
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

##==========================================================================================
##predict

apply(train_sm[,predictors],2, mean)

pred_grid <- expand.grid(Eff_Lat = seq(6000, 10000, by = 2000), 
                         Lbs_Ft = seq(1500, 3000, by = 500), 
                         Bbl_Ft = seq(20, 100, by = 20))

##==========================================================================================

trellis.par.set(caretTheme())
xyplot(resamps_rcv, what = "BlandAltman")

trellis.par.set(caretTheme())
dotplot(resamps_train, metric = "Rsquared")

nam <- c("LM", "RF", "RAN", "CUB", "GBM")


ggplot(varImp(mod_lm, useModel = FALSE))
ggplot(varImp(mod_rf, useModel = FALSE))
ggplot(varImp(mod_ran, useModel = FALSE))
#ggplot(varImp(mod_cub, useModel = FALSE))
ggplot(varImp(mod_gbm, useModel = FALSE))

##==========================================================================================

modelLookup(model='lm')
modelLookup(model='rf')
modelLookup(model='ranger')
modelLookup(model='cubist')
modelLookup(model='gbm')
modelLookup(model='mboost')

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),
                    shrinkage=c(0.01),
                    n.minobsinnode = c(10),
                    interaction.depth=c(30))
mb <- mboost(fmla, train_sm)


#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

model_gbm <- train(sm_fmla, train_sm,method='gbm', trControl = fitControl, 
                   tuneGrid = grid)
#model_gbm <- train(sm_fmla, train_sm,method='gbm', trControl = fitControl, tuneLength = 10)
rmse_gbm <- RMSE(predict(model_gbm, test_sm), test_sm$CUM365_Mboe)

stopCluster(cl)

##==========================================================================================

##speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)


set.seed(1234)
mod_ran1 <- train(fmla, train_sm,method='ranger', trControl = rctrl0, metric = 'RMSE')
mod_ran2 <- train(fmla, train_sm,method='ranger', trControl = rctrl2, metric = 'RMSE')
mod_ran3 <- train(fmla, train_sm,method='ranger', trControl = rctrl3, metric = 'RMSE')
mod_ran4 <- train(fmla, train_sm,method='ranger', trControl = rctrl4, metric = 'RMSE')
mod_ranR <- train(fmla, train_sm,method='ranger', trControl = rctrlR, metric = 'RMSE')

stopCluster(cl)
rctrl0 <- trainControl(method = "repeatedcv", repeats = 3)
rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "oob")
rctrl4 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")



## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=1:12,                        ## the predictor columns, by column index
  y=13,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)






