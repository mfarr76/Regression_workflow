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

glimpse(dt)


dt_op <- dt %>%
  filter(CUM365_Gas > 0, 
         Latitude > 0, 
         Bbl_Ft > 0, 
         Lbs_Ft > 100) %>%
  select(CompWellName, ZONE, Well_Spacing, Stage_Count, Stage_Spacing, Eff_Lat, Bbl_Ft, 
         Lbs_Ft, NGL_Yield, Team, CUM365_Gas, CUM365_Oil, CUM365_Mboe, Yield = Lifetime.Yield, 
         TOC_LEF_P50, Sw_LEF_P50, Phi_LEF_P50, VCLAY_LEF_P50, Res_Press, Carbon.13.Isotope, 
         PERM_LEF_P50, RHOB_LEF_P50, TMAX_LEF_P50, Dew_P_LEF, NG_3, NG_11, LEF_H, TVD)


##remove columns that have any NA
dt_op[sapply(dt_op, function(x) any(is.na(x)))] <- NULL

##count NA
sum(is.na(dt_op))

adapt <- dt %>%
  filter(CUM365_Mboe > 0, 
         Eff_Lat > 0, 
         Cluster_Spacing > 0, 
         VLIME_TEF_P50 > 0) %>%
  select(CompWellName, CUM365_Mboe, Carbon.13.Isotope , TMAX_TEF_P50 , Res_Press , Sw_CALC , 
         NG_3 , Dew_P_LEF , Eff_Lat , Lifetime.Yield , Pressure_Gradient , Bbl_Ft , VLIME_TEF_P50 ,
         Stage_Count , TOC_TEF_P50 , Vclay_CALC , Lbs_Ft , Stage_Spacing , RHOB_TEF_P50 , Well_Spacing ,
         Cluster_Spacing, Prod_Date, ZONE)

##look at wells with NA values
adapt %>% filter(is.na(VLIME_TEF_P50)) %>% select(CompWellName, Prod_Date, ZONE)

count_NA(adapt)

correlate <- cor(select_if(adapt, is.numeric),  use = "everything")
corrplot(correlate, method = "circle", type = "lower", sig.level = 0.01, insig = "blank")
##==========================================================================================


##make plot of key metrics to check normality----

p1 <- ggplot(dt_op, aes(x = Eff_Lat)) + 
  geom_histogram(fill = "black")

p2 <- ggplot(dt_op, aes(x = Lbs_Ft)) +
  geom_histogram(fill = "blue")

p3 <- ggplot(dt_op, aes(x = Bbl_Ft)) + 
  geom_density(fill = "yellow")

p4 <- ggplot(dt_op, aes(x = ZONE)) + 
  geom_bar(aes(fill = ZONE), show.legend = FALSE)

p5 <- ggplot(dt_op, aes(x = Stage_Count)) + 
  geom_density(aes(fill = ZONE), alpha = 0.7, show.legend = FALSE)

p6 <- ggplot(dt_op, aes(x = ZONE, y = CUM365_Gas)) + 
  geom_boxplot(aes(fill = ZONE), show.legend = FALSE) +facet_grid(.~ Team)


multiplot(p1, p2, p3, p4, p5, p6, cols = 3)
##==========================================================================================


p7 <- qqpl(dt_op, "Eff_Lat")
p8 <- qqpl(dt_op, "Lbs_Ft")
p9 <- qqpl(dt_op, "Bbl_Ft")
p10 <- qqpl(dt_op, "Stage_Count")
p11 <- qqpl(dt_op, "LEF_H")

multiplot(p7, p8, p9, p10, p11, cols = 3)




##formula----
sm_fmla <- CUM365_Mboe ~ Eff_Lat + Lbs_Ft + Bbl_Ft + 
  Stage_Count + LEF_H + TVD + Well_Spacing

at_fmla <- CUM365_Mboe ~ Carbon.13.Isotope + TMAX_TEF_P50 + Res_Press + Sw_CALC + 
  NG_3 + Dew_P_LEF + Eff_Lat + Lifetime.Yield + Pressure_Gradient + Bbl_Ft + VLIME_TEF_P50 +
  Stage_Count + TOC_TEF_P50 + Vclay_CALC + Lbs_Ft + Stage_Spacing + RHOB_TEF_P50 + Well_Spacing +
  Cluster_Spacing

##VLIME_TEF_CALC, SW_TEF_CALC, Well_Area, RHOB_TEF_Calc, X.UEF 
##========================================================================================


##test/train----
#create test / train dataset
set.seed(1234)
trainRow <- createDataPartition(dt_op$CUM365_Mboe, p = 0.75, list = FALSE)
train_sm <- dt_op[trainRow, ]
test_sm <- dt_op[-trainRow, ]

##create folds and traincontrol----
set.seed(1234)
cv.10.folds <- createMultiFolds(train_sm$CUM365_Mboe, k = 10, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                       index = cv.10.folds)
##==========================================================================================


##multi-variant linear model----
#lm_stx <- lm(Cum12Gas_MMcf ~ TotalDepthTVD, data = stx_sm_redu1)
lm_stx <- train(sm_fmla, 
                data = train_sm, method = "lm", 
                tuneLength = 3, 
                trControl = ctrl.1)

lm_pred <- predict(lm_stx, test_sm)
lm_rmse <- RMSE(lm_pred, test_sm$CUM365_Mboe)
##==========================================================================================


##randomForest model----

#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

#train model
set.seed(1234)
rft_stx <- train(sm_fmla,
                data = train_sm, method = "rf",
                #preProcess = c("center", "scale"),
                tuneLength = 3, ntree = 1000,
                trControl = ctrl.1)

stopCluster(cl)

ggplot(rft_stx) + theme(legend.position = "top")
rft_rmse <- RMSE(predict(rft_stx, test_sm), test_sm$CUM365_Mboe)

#Plotting Varianle importance for RandomForest
ggplot(varImp(rft_stx, scale = FALSE)) + ggtitle("RandomForest - Variable Importance") +
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
##==========================================================================================


##randomForest-Ranger model----
#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

#train model
set.seed(1234)
ran_stx <- train(sm_fmla,
                   data = train_sm, method = "ranger",
                   importance = "permutation", 
                   #preProcess = c("center", "scale"),
                   tuneLength = 3, num.trees = 1000,
                   trControl = ctrl.1)

stopCluster(cl)

ggplot(ran_stx) + theme(legend.position = "top")
ran_pred <- predict(ran_stx, test_sm)
ran_pr <- postResample(ran_pred, test_sm$CUM365_Mboe)
ran_rmse <- RMSE(ran_pred, test_sm$CUM365_Mboe)

#Plotting Varianle importance for Ranger
ggplot(varImp(ran_stx, scale = FALSE)) + ggtitle("Ranger - Variable Importance") +
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
##==========================================================================================

##cubist----
#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

#train model
set.seed(1234)
cub_stx <- train(sm_fmla, train_sm, 
                    method = "cubist", 
                    turnLength = 3,
                    trControl = ctrl.1)

stopCluster(cl)

ggplot(cub_stx) + theme(legend.position = "top")
cub_pred <- predict(cub_stx, test_sm)
cub_pr <- postResample(cub_pred, test_sm$CUM365_Mboe)
cub_rmse <- RMSE(cub_pred, test_sm$CUM365_Mboe)
ggplot(varImp(object = cub_stx)) + ggtitle("GBM - Variable Importance") +
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
gbm_stx <- train(sm_fmla, data = train_sm, 
                 method = "gbm",
                 tuneGrid = grid,
                 tuneLength = 3,
                 trControl = ctrl.1)

stopCluster(cl)

ggplot(gbm_stx) + theme(legend.position = "top")
gbm_preds <- predict(gbm_stx, test_sm)
RMSE_gbm <- RMSE(gbm_preds, test_sm$CUM365_Mboe)
pr_gbm <- postResample(gbm_preds, test_sm$CUM365_Mboe)

#Plotting Varianle importance for GBM
ggplot(varImp(object = gbm_stx)) + ggtitle("GBM - Variable Importance") +
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
##==========================================================================================


##nnet model----
#train model
set.seed(1234)
nnet_stx <- train(sm_formula, train_sm, 
                    method = "nnet", 
                    turnLength = 3,
                    trControl = ctrl.1)


ggplot(nnet_stx) + theme(legend.position = "top")
preds_nnet <- predict(nnet_stx, test_sm)
RMSE_nnet <- RMSE(preds_nnet, test_sm$CUM365_Mboe)
pr_nnet <- postResample(preds_nnet, test_sm$CUM365_Mboe)
##==========================================================================================


##rpart tree building----
#speed up calcs with running in parallel
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
rpart.cv <- train(sm_fmla,
                  data = train_sm,
                  method = "rpart", tuneLength = 30, 
                  trControl = ctrl.1)

stopCluster(cl)

prp(rpart.cv$finalModel, extra = 1, cex = 0.7, box.palette = "RdYlGn")

##use a simple tree model for display
simple <- rpart(CUM365_Mboe ~ Eff_Lat, data = train_sm)
prp(simple, extra = 1, cex = 0.7, box.palette = "RdYlGn")

##check simple mode for accuracy
train_sm %>%
  filter(Eff_Lat < 4496 ) %>%
  summarise(CUM365 = mean(CUM365_Mboe), 
            Count = n())
##==========================================================================================


##compare models----
resamps <- resamples(list(RFT = rf_stx,
                          RAN = rf_ranger, 
                          CUB = stx_cubist, 
                          GBM = gbmFit1))
summary(resamps)

getTrainPerf(stx_cubist)

trellis.par.set(caretTheme())
xyplot(resamps, what = "BlandAltman")

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Rsquared")
##==========================================================================================


##write to estimates to test_sm table----
test_sm$model_output <- pred_cubist
test_sm$model_output_lm <- pred_lm_mod

ggplot(test_sm, aes(x = model_output, y = CUM365_Mboe)) +
  geom_point() + 
  geom_smooth(method = "lm")

mod <- lm(CUM365_Mboe ~ model_output, data = test_sm)
mod_lm <- lm(CUM365_Mboe ~ model_output_lm, data = test_sm)

summary(mod)

ggplotRegression(mod)
ggplotRegression(mod_lm)
##==========================================================================================


test_sm$residuals <- test_sm$CUM365_Mboe - test_sm$model_output
test_sm$residuals_lm <- test_sm$CUM365_Mboe - test_sm$model_output_lm

ggplot(test_sm, aes(x = model_output, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("residuals vs. model prediction") + 
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))

GainCurvePlot(test_sm, "model_output", "CUM365_Mboe", "Model Gain Curve")
r2 <- data.frame(train_r2 = summary(resamps)$statistics$Rsquared[,4])
r2$test_r2 <- c(RMSE_rf, RMSE_ranger, RMSE_cubist, RMSE_cubist)



mv_models <- list(rf_stx, rf_ranger, stx_cubist, gbmFit1)

r2 <- data.frame()
for(i in 1:4){
  test <- data.frame(Test = r_squared(predict(mv_models[[i]], test_sm), test_sm$CUM365_Mboe))
  test$Train <- summary(resamps)$statistics$Rsquared[i,4]
  r2 <- rbind(r2, test) #store the results of each loop
  print(r2)
}
r2


rmse_df <- data.frame()
for(i in 1:4){
  test <- data.frame(Test = rmse(predict(mv_models[[i]], test_sm), test_sm$CUM365_Mboe))
  test$Train <- summary(resamps)$statistics$RMSE[i,4]
  rmse_df <- rbind(rmse_df, test) #store the results of each loop
  print(rmse)
}
rmse_df
##things to do......................
##create function to 
#1 plot r2 and rmse of test train
#2 plot qq plot
#3 try to figure out variable importance for all the models
#4 create lm model in caret
#5 create residual plot




