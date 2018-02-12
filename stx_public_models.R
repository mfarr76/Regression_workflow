rm(list = ls())
##load packages==================================================================
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
library('purrr') # data wrangling

# Dates
library('lubridate') # date and time

# Extra vis
library('corrplot') # visualisation
library('GGally')
library('gridExtra')

# Model
library('broom')
library('caret')
library('ranger')
library('rpart')
library('rpart.plot')
library('gbm')
library('earth')
library('Cubist')

##load data - convert datatypes============================================================
##EagleFord Webb and Dimmit counties 
##Wellhead information in .csv file
#create public package
public <- read.csv("20190112_STX_wellheader.csv", header = TRUE)
public$Sequence <- NULL

##create column to convert to numeric
factor_col <- c("API","OperatorName","WellName","CountyName","TOE_UP_DWN",
                "Spacing_Category", "DateProductionStart", "CompletionDate", 
                "Binned_GOR_6MO", "Binned_Max_Infill_Time", "Binned_Spacing_Avg", 
                "Binned_YIELD_12MO", "PrimaryProduct")
##too many NAs and missing values...easier to hard code factor column labels
#factor_col <- colnames(select_if(public, is.factor))
##create factor_col into factors
public[factor_col] <- lapply(public[factor_col], as.factor)
##create a list of colnames to turn into numeric
#numeric_col <- !colnames(webb_public) %in% factor_col
numeric_col <- !colnames(public) %in% factor_col

##remove 0,000 from the number format
public[numeric_col] <- data.frame(lapply(public[numeric_col], gsub, pattern = ',', replacement = '', fixed = TRUE))
##convert numeric_col into numeric datatype
public[numeric_col] <- sapply(public[numeric_col], function(x) as.numeric(levels(x))[x])
##change first prod date into date datatype
public$DateProductionStart <- as.Date(public$DateProductionStart, "%m/%d/%Y")
public$CompletionDate <- as.Date(public$CompletionDate, "%m/%d/%Y")


na_count <-sapply(public, function(y) sum(is.na(y)))
na_percent <- data.frame(na_count)/nrow(public)
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
public <- public[,na_percent<0.80]


##create a sm dataset=======================================================================
webb_public <- public %>%
  filter(OperatorName != "ANADARKO E & P ONSHORE LLC",
    Spacing_Category == "Bounded", 
    CountyName == "WEBB", 
    #str_detect(WellName, 'GAL'),
    FluidAmountTotal > 500000, 
    ProppantAmountTotal > 100000,
    First12MonthGas > 0, 
    First12MonthLiquid > 0) %>%
  mutate(Lbs_Gal = ProppantAmountTotal / (FluidAmountTotal / 42),
         Lbs_Ft = ProppantAmountTotal / PerfIntervalGross,
         Bbl_Ft = (FluidAmountTotal / 42) / PerfIntervalGross,
         CUM12Gas_MMcf = First12MonthGas / 1000,
         TVD = TotalDepthTVD,
         Eff_Lat = PerfIntervalGross,
         log_Lbs_Ft = log10(Lbs_Ft),
         log_Bbl_Ft = log10(Bbl_Ft),
         sqrt_Lbs_Ft = sqrt(Lbs_Ft), 
         sqrt_Bbl_Ft = sqrt(Bbl_Ft),
         sqrt_CUM12_Mboe = sqrt(CUM12_Mboe),
         log_YIELD_12MO = log10(YIELD_12MO),
         sqrt_YIELD_12MO = sqrt(YIELD_12MO),
         Mboe_Ft = CUM12_Mboe / PerfIntervalGross) %>%
         #Category = as.factor(ifelse(CUM12_Mboe > quantile(CUM12_Mboe, p = 0.75), "High", "Low"))) %>%
  filter(YIELD_12MO >= 5 & YIELD_12MO <= 50, 
         CUM12_Mboe < 600, 
         Lbs_Ft < 4000) %>%
  droplevels()


factor_col <- names(select_if(webb_public, is.factor))
#factor_l <- length(factor_col)

webb_public <- webb_public %>% select(factor_col, everything())

##remove all columns that start with Fi
webb_public <- webb_public[, -grep('^Fi', names(webb_public))] 

##use sapply to get class of columns
#tbl_names <- names(webb_public) %>% tbl_df() %>% 
#  mutate(class = sapply(webb_public, class))

##use purrr to get class of column
tbl_names <- names(webb_public) %>% tbl_df() %>% 
  mutate(class = map(webb_public, class))

webb_public <- webb_public[-c(1:6, 13, 34)]

 
#var <- names(webb_public)
#not_target <- var[var != "CUM12_Mboe"]
#length(var)
#length(not_target)
#trainX <- webb_public[,names(webb_public) != "CUM12_Mboe"]

##multicollinearity=======================================================================

webb_public %>%
  select(Lbs_Ft, Bbl_Ft, OperatorName, WellName, Spacing_Category) %>%
  top_n(5, Lbs_Ft) %>%
  arrange(desc(Lbs_Ft))


#  select(OperatorName, Lat, Lon, TOE_UP_DWN, Lbs_Ft, Bbl_Ft, Lbs_Ft, Bbl_Ft, TotalDepthTVD, 
#         SoPhiH_LEF, Spacing_Avg, Vintage, Cum12Gas_MMcf, EffLat = PerfIntervalGross, category)
#write.csv(public_sm, file = "sm_public.csv")

#count_NA(webb_public)
#count_zero(webb_public)

webb_num <- webb_public %>% 
  #select( - starts_with("First")) %>%
  select_if(is.numeric)


##remove columns that have any NA
webb_num[sapply(webb_num, function(x) any(is.na(x)))] <- NULL

##create a list of columns that have a correlation higher than 0.87
highCor <- names(webb_num[,findCorrelation(abs(cor(webb_num)), 0.85)])

##create a list of colnames from webb_num that have highCor removed 
lowcor <- c(setdiff(setdiff(names(webb_num),"class"),highCor), c("CUM12_Mboe", "TVD", "log_Bbl_Ft",
                                                                 "sqrt_CUM12_Mboe", 
                                                                 "log_YIELD_12MO"))
webb_lowcor <- webb_num[, lowcor] #create a new data.frame with lowcor columns only
#glimpse(webb_lowcor)

#webb_redu <- webb_public %>% 
#  select(CUM12_Mboe, Lon, DISPLACEMENT, SoPhiH_LEF, ProdYear, Spacing_Avg, Max_Infill_Time, Lbs_Ft, 
#         Bbl_Ft, Eff_Lat, TVD, OperatorName, TOE_UP_DWN, CountyName)

#remove highCor from public dataset
webb_public <- webb_public[colnames(webb_public) %in% c(lowcor, factor_col)]
#glimpse(webb_public)

remove_cols <- c("API", "CountyName", "Spacing_Category", "CumGas",
                 "CumLiquid", "CumWater", "FirstMonthGas", "FirstMonthLiquid",
                 "First3MonthGas", "First3MonthWater", "First12MonthGas", 
                 "First12MonthLiquid", "WellName")
webb_public[,remove_cols] <- NULL

#correlate <- cor(webb_num, use = "everything", method = "pearson")
#corrplot(correlate, method = "circle", type = "lower", sig.level = 0.01, insig = "blank")
#correlate <- cor(webb_lowcor, use = "everything") #create a correlation matrix
#corrplot(correlate, method = "circle", type = "lower", sig.level = 0.01, insig = "blank")
#ggpairs(webb_lowcor[var])

##clustering================================================================================

webb_num <- select_if(webb_public, is.numeric) %>%
  select(CUM12_Mboe, everything())

tbl <- names(webb_num) %>% tbl_df()

count_NA(webb_num)

sum(sapply(webb_num, is.na))

tbl <- names(webb_num) %>% tbl_df()
##remove columns that have any NA
webb_num[sapply(webb_num, function(x) any(is.na(x)))] <- NULL

##check if scaling is necessary
##if means and sd of the feature vary then scaling is in order
mean(colMeans(webb_num[-1]))
mean(apply(webb_num[-1], 2, sd))

##will need to normalize the data because the data has different scales of measurement

##produce a new matrix with columns of mean of 0 and sd of 1
scale_webb <- scale(webb_num[-1])
##check scale
mean(colMeans(scale_webb))
mean(apply(scale_webb, 2, sd))

# Create hierarchical clustering model: hclust.out
hclust.out <- hclust(d = dist(scale_webb[,17]), method = "complete")


# Inspect the result
summary(hclust.out)
plot(hclust.out)
abline(h = 0.75, col = "red")

webb_public$clusters <- cutree(hclust.out, h = 0.75)

table(webb_test$Eff_Lat, webb_test$clust)


##==============
#https://nishanthu.github.io/articles/ClusteringUsingRandomForest.html

webb_pc <- prcomp(webb_num[-1], center = FALSE, scale. = FALSE)$x %>% as.data.frame()

km.cluster <- kmeans(webb_num, centers = 3, iter.max = 20, nstart = 2)
webb_pc$kmeans.cluster <- km.cluster$cluster
table(webb_num$YIELD_12MO, km.cluster$cluster)

g1 <- ggplot(webb_pc, aes(x=PC1, y=PC2)) + geom_point(aes(colour = kmeans.cluster))



rf.fit <- randomForest(x = webb_num[-1], y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=3)
webb_pc$rf.clusters <- rf.cluster
table(rf.cluster, webb_num$YIELD_12MO)

g2 <- ggplot(webb_pc, aes(x=PC1, y=PC2)) + geom_point(aes(colour = rf.cluster))
grid.arrange(g1, g2 ,ncol=2,nrow=1)


webb_dist <- dist(webb_num_norm)
webb_hclust = hclust(webb_dist)
plot(webb_hclust,main='Default from hclust')

groups.3 = cutree(webb_hclust,40)
table(groups.3)

k_means_fit <- kmeans(webb_num_norm, 15)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(webb_num, nc=20)

library(cluster)
clusplot(webb_num, k_means_fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

km_fit <- kmeans(webb_num[c("YIELD_12MO", "TVD")], centers = 10, nstart = 10)

o <- order(km_fit$cluster)

df <- data.frame(webb_num$CUM12_Mboe[o], km_fit$cluster[o])

library("factoextra")
fviz_cluster(km_fit, data = webb_num,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Compute hierarchical clustering
res.hc <- webb_num %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering
# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 6, # Cut in six groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#2edf9e", "#a4df2e"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)



##create test / train dataset 80,20==========================================================
#webb_cluster <- read.csv("comb_dt.csv")
#webb_public <- left_join(webb_public, webb_cluster %>%
#              select(WellName, Cluster_Row))

set.seed(1234)
trainRow <- createDataPartition(webb_public$CUM12_Mboe, p = 0.8, list = FALSE)
#trainRow <- createDataPartition(webb_public$Mboe_Ft, p = 0.8, list = FALSE)
#trainRow <- createDataPartition(webb_public$Mboe_Lbs_Bbl_ft2, p = 0.8, list = FALSE)
#trainRow <- createDataPartition(webb_public$Mboe_SoPhiH, p = 0.8, list = FALSE)
#trainRow <- createDataPartition(webb_public$Mboe_tvd, p = 0.8, list = FALSE)

#webb_public <- webb_public %>% 
#  mutate(Mboe_SoPhiH = CUM12_Mboe / SoPhiH_LEF, 
#         Mboe_tvd = CUM12_Mboe / TVD )

webb_train <- webb_public[trainRow, ]  %>% droplevels()
webb_test <- webb_public[-trainRow, ]  %>% droplevels()

#rm(webb_lowcor, webb_num, numeric_col, factor_col, 
#   highCor, lowcor, trainRow, na_percent, na_count, remove_cols)
#webb_train <- droplevels(webb_train$OperatorName)
#webb_test <- droplevels(webb_test$OperatorName)
##forward and backward selection============================================================
lmMod <- lm(CUM12_Mboe ~ ., webb_train)
summary(lmMod)
step_back_Mod <- step(lmMod, direction = "backward")
step_forward_Mod <- step(lmMod, direction = "forward")
summary(step_back_Mod)
summary(step_forward_Mod)

##formula============================================================================
sm_fmla <- paste("Mboe_Ft ~ Eff_Lat + TVD + SoPhiH_LEF + Spacing_Avg + Lbs_Ft +", 
                 "Bbl_Ft + Lon + Lat + c.FirstProdYear + c.Max_Infill_Time +",
                  "API_Azimuth + OperatorName + Binned_YIELD_12MO")
fmla <- as.formula(sm_fmla)
predictors <- c("Eff_Lat", "TVD", "SoPhiH_LEF", "Spacing_Avg","Lbs_Gal", "Lbs_Ft", "Bbl_Ft",
                "Lon", "Lat", "c.FirstProdYear", "c.Max_Infill_Time", "API_Azimuth", "log_YIELD_12MO", 
                "Binned_YIELD_12MO")
#train_pred <- webb_train[, !names(webb_train) %in% c("CUM12_Mboe")]

##test formula============================================================================
##
lm_fmla <- lm(fmla, webb_train)
#summary(lm_fmal)
##randomForest
library('randomForest')

mtry <- sqrt(ncol(webb_train[predictors]))
set.seed(1234)
rf <- randomForest(sqrt(CUM12_Mboe) ~ Eff_Lat + TVD + SoPhiH_LEF + Spacing_Avg + 
                     Lbs_Ft + Bbl_Ft + Lon + Lat + c.FirstProdYear + c.Max_Infill_Time + 
                     API_Azimuth, 
                   webb_train, mtry = mtry, ntree = 1000)
pred_rf <- (predict(rf, webb_test))^2
(MAE(pred_rf, webb_test$CUM12_Mboe))
(rmse_rf <- RMSE(pred_rf, webb_test$CUM12_Mboe))
(R2(pred_rf, webb_test$CUM12_Mboe))

##ranger
set.seed(1234)
rngr <- ranger(sqrt(CUM12_Mboe) ~ Eff_Lat + TVD + SoPhiH_LEF + Spacing_Avg +
                 Lon + Lat + c.FirstProdYear + c.Max_Infill_Time + Lbs_Ft + Bbl_Ft +
                 API_Azimuth + OperatorName + Binned_YIELD_12MO, 
                webb_train, importance = "permutation", num.trees = 1000)
pred_rngr <- predictions(predict(rngr, webb_test, num.trees = 1000))^2
#pred_rngr <- 10^pred_rngr
(MAE(pred_rngr, webb_test$CUM12_Mboe))
(rmse_ranger <- RMSE(pred_rngr, webb_test$CUM12_Mboe))
(R2(pred_rngr, webb_test$CUM12_Mboe))
barchart(sort(rngr$variable.importance))
var <- (sort(rngr$variable.importance))


library(class)
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
knnfit <- train(sqrt(CUM12_Mboe) ~ Eff_Lat + TVD + SoPhiH_LEF + Spacing_Avg + log_Bbl_Ft + 
                log_Lbs_Ft + Lon + Lat + ProdYear + Max_Infill_Time + Lbs_Gal + Lbs_Bbl_ft2 +
                API_Azimuth + log_YIELD_12MO, 
                webb_train, 
                method = "knn", 
                preProcess = c("center","scale"), 
                tuneLength = 20)
stopCluster(cl)
pred_knn <- (predict(knnfit, webb_test))^2
(MAE(pred_knn, webb_test$CUM12_Mboe))
(rmse_knn <- RMSE(pred_knn, webb_test$CUM12_Mboe))
(R2(pred_knn, webb_test$CUM12_Mboe))


set.seed(1234)
webb_boost = gbm(sqrt(CUM12_Mboe) ~ Eff_Lat + TVD + SoPhiH_LEF + Spacing_Avg +
                   Lon + Lat + c.FirstProdYear + c.Max_Infill_Time + Lbs_Gal +Lbs_Ft + Bbl_Ft +
                   API_Azimuth + OperatorName + clusters,
                 data = webb_train, distribution = "gaussian", 
                 n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
pred_boost <- (predict(webb_boost, webb_test, n.trees = 5000))^2
(MAE(pred_boost, webb_test$CUM12_Mboe))
(rmse_boost <- RMSE(pred_boost, webb_test$CUM12_Mboe))
(R2(pred_boost, webb_test$CUM12_Mboe))
summary(webb_boost)
(webb_rmse = data.frame(Model = c("Random Forest", "KNN", "Ranger", "Boosting"),
  TestError = c(rmse_rf, rmse_knn, rmse_ranger, rmse_boost)))

##create models==============================================================================
set.seed(1234)
cv.5.folds <- createMultiFolds(webb_train$CUM12_Mboe, k = 5, times = 5)
ctrl.1 <- trainControl(method = "repeatedcv", #resample with repeated cv
                       number = 5,            #number of folds for resampling    
                       repeats = 5,           #repeated k-folds cross-validation
                       index = cv.5.folds)    #createMultFolds 

cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
mod_lm <- train(fmla, webb_train, method = 'lm', 
                trControl = ctrl.1, metric = 'RMSE', 
                #preProcess = c("center", "scale"), 
                na.action = na.omit)
set.seed(1234)
mod_rpt <- train(fmla, webb_train, method = 'rpart', 
                 trControl = ctrl.1, metric = 'RMSE',  
                 #preProcess = c("center", "scale"),
                 na.action = na.omit)
set.seed(1234)
mod_ctree <- train(fmla, webb_train, method = 'ctree', 
                   trControl = ctrl.1, metric = 'RMSE',  
                   #preProcess = c("center", "scale"),
                   na.action = na.omit)
set.seed(1234)
mod_ran <- train(fmla, webb_train, method = 'ranger', importance = "permutation",
                 trControl = ctrl.1, metric = 'RMSE', num.trees = 500, 
                 #preProcess = c("center", "scale"),
                 na.action = na.omit)
set.seed(1234)
mod_gbm <- train(fmla, webb_train, method = 'gbm', 
                 trControl = ctrl.1, metric = 'RMSE',  
                 #preProcess = c("center", "scale"),
                 na.action = na.omit)
set.seed(1234)
mod_cub <- train(fmla, webb_train, method = 'cubist', 
                 trControl = ctrl.1, metric = 'RMSE',  
                 #preProcess = c("center", "scale"),
                 na.action = na.omit)
set.seed(1234)
mod_earth <- train(fmla, webb_train, method = 'earth', 
                   trControl = ctrl.1, metric = 'RMSE',  
                   #preProcess = c("center", "scale"),
                   na.action = na.omit)

stopCluster(cl)
##model performance============================================================================
resamps_train <- resamples(list(LM = mod_lm,
                                RPT = mod_rpt,
                                CTR = mod_ctree,
                                RAN = mod_ran,
                                CUB = mod_cub,
                                GBM = mod_gbm, 
                                MARS = mod_earth))
summary(resamps_train)

mv_mods <- list(mod_lm,
                #mod_rpt,
                mod_ctree,
                mod_ran,
                #mod_cub,
                mod_gbm) 
                #mod_earth)



r2 <- data.frame()
for(i in 1:length(mv_mods)){
  df <- data.frame(R2(predict(mv_mods[[i]], webb_train)^2, webb_train$CUM12_Mboe))
  colnames(df) <- "Train"
  df$Test <- R2(predict(mv_mods[[i]], webb_test)^2, webb_test$CUM12_Mboe)
  #df <- data.frame(df)
  #df$Test_rcv <- R2(predict(mv_mods_rcv[[i]], webb_test), webb_test$CUM12_Mboe)
  r2 <- rbind(r2, df) #store the results of each loop
  #print(rmse)
}

rmse <- data.frame()
for(i in 1:length(mv_mods)){
  dt <- data.frame(Train = RMSE(predict(mv_mods[[i]], webb_train)^2, webb_train$CUM12_Mboe))
  dt$Test <- RMSE(predict(mv_mods[[i]], webb_test)^2, webb_test$CUM12_Mboe)
  #dt$Test_rcv <- RMSE(predict(mv_mods_rcv[[i]],  webb_test), webb_test$CUM12_Mboe)
  rmse <- rbind(rmse, dt) #store the results of each loop
  #print(rmse)
}

#create rownames
#nam <- c("LM", "RPT", "CTR" ,"RAN", "GBM", "CUB", "MARS")
nam <- c("LM", "CTR" ,"RAN", "GBM")
#add rownames to data.frame
rownames(r2) <- nam
#tranpose the data.frame
r2_t <- t(r2)

#add rownames
rownames(rmse) <- nam
rmse_t <- t(rmse)

r2
rmse



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
  scale_fill_brewer(type="qual", palette="Dark2")+
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5))
#scale_fill_brewer(type="qual", palette=1)


##varable importance===========================================================================
v1 <- ggplot(varImp(mod_lm, useModel =TRUE, scale = TRUE)) + theme_bw()
#v2 <- ggplot(varImp(mod_rpt, useModel = TRUE, scale = TRUE))+ theme_bw()
v3 <- ggplot(varImp(mod_ctree, useModel = TRUE, scale = TRUE)) + theme_bw()
v4 <- ggplot(varImp(mod_ran, useModel= TRUE, scale = TRUE)) + theme_bw()
v5 <- ggplot(varImp(mod_gbm, useModel = TRUE, scale = TRUE)) + theme_bw()
#v6 <- ggplot(varImp(mod_cub, useModel = TRUE, scale = TRUE))+ theme_bw()
#v7 <- ggplot(varImp(mod_earth, useModel = TRUE, scale = TRUE))+ theme_bw()
grid.arrange(v1, v3, v4, v5, ncol = 2, nrow = 2)


# plot the RMSE values
p_rmse <- parallelplot(resamps_train, metric = "RMSE")
## plot the MAE values
p_mae <- parallelplot(resamps_train, metric = "MAE")
## plot the R2 values
p_r2 <- parallelplot(resamps_train, metric = "Rsquared")
grid.arrange(p_rmse, p_mae, p_r2, ncol = 3, nrow = 1)

##predictions=================================================================================
pred <- predict(mod_ran_rcv, webb_test$CUM12_Mboe)
ggplot_lm(pred_rngr_pretune, webb_test$CUM12_Mboe)

#apply(train_sm[,predictors],2, mean)

pred_grid <- expand.grid(Eff_Lat = seq(6000, 10000, by = 2000), 
                         TVD = 8327,
                         SoPhiH_LEF = 8,
                         Spacing_Avg = 500, 
                         Lbs_Ft = seq(1500, 3000, by = 500), 
                         Bbl_Ft = seq(20, 100, by = 25),
                         Lon = -99.6184, 
                         Lat = 28.09715,
                         c.FirstProdYear = 2017,
                         c.Max_Infill_Time = 0,
                         API_Azimuth = 312,
                         OperatorName = "SM ENERGY COMPANY",
                         Binned_YIELD_12MO = "20_30")
                         

write.csv(pred_grid, file = "pred_grid.csv")

mod_lm <- lm(fmla, webb_train)
pred <- predict(mod_lm, webb_test)^2

pred_grid$model_lm <- predict(mod_lm, pred_grid)^2 ##lm model prediction
#pred_grid$model_lm_crt <- predict(mod_lm_rcv, pred_grid) ##lm model prediction
#pred_grid$model_ctree_rcv <- predict(mod_ctree, pred_grid)^2 ##ctree model prediction
#pred_grid$model_rf <- predictions(predict(rngr, pred_grid, num.trees = 1000))^2 ##rf model prediction
pred_grid$model_rf <-  predict(mod_ran, pred_grid)^2 ##gbm model prediction
pred_grid$model_gbm <- predict(mod_gbm, pred_grid)^2 ##gbm model prediction
#pred_grid$model_cubic <- predict(mod_cub_rcv, pred_grid) ##cubic model prediction
#pred_grid$model_mars <- predict(mod_earth_rcv, pred_grid) ##mars model prediction
pred_grid$group <- rep(rep(LETTERS[1:16], each = 3), 1)

xy <- rep(rep(LETTERS[1:12], each = 3), 1)

write.csv(pred_grid, file = "webb_public_predicitons.csv")


pred_grid %>% filter(group == "A") %>% ggplot(aes(x = Eff_Lat, y = model_lm, group=group)) + 
  geom_line() + geom_point()

ggplot(pred_grid, aes(x=Eff_Lat, y=model_cubic, group=group, color=group)) + geom_line() + geom_point()



resamps_train <- resamples(list(LM = mod_lm,
                                RPT = mod_rpt,
                                CTR = mod_ctree,
                                RAN = mod_ran,
                                CUB = mod_cub,
                                GBM = mod_gbm, 
                                MARS = mod_earth))



predict(mod_rpt_rcv, webb_test)
mod_ran_rcv$pred





##test sqrt predic========================================

(mod_rngr <- ranger(sqrt(CUM12_Mboe) ~ Eff_Lat + TVD + SoPhiH_LEF + Spacing_Avg
                   + Bbl_Ft + sqrt_Lbs_Ft + Max_Infill_Time + API_Azimuth +YIELD_12MO, 
                   webb_train, importance = "permutation"))
(mod_rngr <- ranger(fmla, 
                    webb_train, importance = "permutation", num.trees = 1000))

mod_lm1<-lm(CUM12_Mboe ~ Bbl_Ft, webb_train)
mod_lm2<-lm(sqrt(CUM12_Mboe) ~ sqrt(Bbl_Ft), webb_train)
summary(mod_lm1)
summary(mod_lm2)

test <- expand.grid(Bbl_Ft = seq(20,100,20))
(pred1 <- predict(mod_lm1, test))
(pred2 <- predict(mod_lm2, test))
pred2^2

##pca=======================================================================

prComp <- prcomp(webb_train[predictors], scale. = TRUE, center = TRUE)
summary(prComp)
std_dev <- prComp$sdev
pr_var <-std_dev^2
prop_varex <- pr_var / sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b") 
abline(h=0.96,col='red',v=9)

train_data<-data.frame(CUM12_Mboe = webb_train$CUM12_Mboe, prComp$x)
metric <- "RMSE"
control <- trainControl(method="repeatedcv", number=10, repeats=3)
mtry <- sqrt(ncol(webb_train[predictors]))
tunegrid <- expand.grid(.mtry=mtry)

##pca model==================================================================
rf_pca <- randomForest(CUM12_Mboe ~., train_data, mtry = mtry, ntree = 1000)
test_data <- predict(prComp, webb_test)
test_data <- as.data.frame(test_data)
pred_test <- predict(rf_pca, test_data)

(RMSE(pred_test, webb_test$CUM12_Mboe))
(R2(pred_test, webb_test$CUM12_Mboe))

##caret pca model============================================================
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
rf_pca_caret <- train(sqrt(CUM12_Mboe)~.,data=train_data, method="rf", metric=metric, 
                      tuneGrid=tunegrid, trControl=control)
stopCluster(cl)

test_data <- predict(prComp, webb_test)
test_data <- as.data.frame(test_data)
pred_test <- predict(mrf_pca_caret, test_data)
#pred_test

RMSE(pred_test, webb_test$CUM12_Mboe)
R2(pred_test, webb_test$CUM12_Mboe)

##pca compare====================================================================

mtry <- sqrt(ncol(webb_train[predictors]))
tunegrid <- expand.grid(.mtry=mtry)

pp_train <- preProcess(webb_train, method = "pca")
##compare
webb_num <- select_if(webb_train, is.numeric)
prComp <- prcomp(webb_num, scale. = TRUE)
plot(prComp)

set.seed(1234)
cv.5.folds <- createMultiFolds(webb_train$CUM12_Mboe, k = 5, times = 5)
ctrl.1 <- trainControl(method = "repeatedcv",                #resample with repeated cv
                       number = 5,                           #number of folds for resampling    
                       repeats = 5,                          #repeated k-folds cross-validation
                       index = cv.5.folds)                   #createMultFolds 
                       #preProcOptions = list(thresh = 0.98)) #create threshold of 98%   

cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(1234)
model_rf21 <- train(fmla, webb_train, 
                   method = "rf", 
                   preProcess = c("pca"), 
                   trControl = ctrl.1,
                   #tunegrid = tunegrid,
                   metric = 'RMSE', 
                   na.action = na.omit)
stopCluster(cl)
model_rf21

pred_test2 <- predict(model_rf2, webb_test)
RMSE(pred_test2, webb_test$CUM12_Mboe)
R2(pred_test2, webb_test$CUM12_Mboe)


##model lookup====================================================================

modelLookup(model='lm')
modelLookup(model='rf')
modelLookup(model='ctree')
modelLookup(model='ranger')
modelLookup(model='cubist')
modelLookup(model='gbm')
modelLookup(model='mboost')

##leaps testing====================================================================
library(leaps)
regsubsets.out <-
  regsubsets(CUM12_Mboe ~ Lon + Lat + CompletionDate + DateProductionStart + PerfIntervalGross + 
               TVD + API_Azimuth + DISPLACEMENT + SoPhiH_LEF + Spacing_Avg + c.Max_Infill_Time,
             data = webb_train,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

plot(regsubsets.out, scale = "adjr2", main = "Adjusted R2")



