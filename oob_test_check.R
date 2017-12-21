
len_fmla <- length(attr(terms(at_fmla), "term.labels"))

test.err = double(len_fmla)
oob.err = double(len_fmla)

for(mtry in 1:len_fmla)
{
  set.seed(1234)
  rf=randomForest(fmla, train_sm,mtry=mtry,ntree=500) 
  oob.err[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,test_sm) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test_sm, mean( (CUM12_Mboe - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}


matplot(1:mtry , cbind(oob.err,test.err), pch=19,
        col=c("red","blue"),type="b",ylab="Mean Squared Error",
        xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

plot(rf)


##create test / train without caret

set.seed(1234)
N <- nrow(stx_sm)
target <- round(N * 0.75)
gp <- runif(N)
train_sm1 <- stx_sm[gp < 0.75, ]
test_sm1 <- stx_sm[gp > 0.75, ]


highCor_features <- names(dt_op1[,findCorrelation(abs(cor(dt_op1)), 0.88)])

lowcor_features <- setdiff(setdiff(names(train_sm),"classe"),highCor_features)
