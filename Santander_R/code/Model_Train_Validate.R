# TITLE  : Santander Model training & validation
# Author : Nagesh Kommuri - https://in.linkedin.com/in/iamknagesh


### cross-validation 5 fold splitting
set.seed(1234)
five_fold <- as.data.frame(createFolds(y = train.y, k = 5, list = T))

CV <- NULL
NROUNDS <- NULL
AUC_ALL <- NULL
### calculate AUC on holdout set
AUC<-function(actual,predicted)
{
  auc<-auc(as.numeric(actual),as.numeric(predicted))
  auc 
}

for(i in 1:length(five_fold)){
  holdout <- as.vector(five_fold[,1])
  holdout_set <- train[holdout,]
  train <- train[-holdout,]
  trainx <- sparse.model.matrix(TARGET ~ ., data = train)
  
  dtrain <- xgb.DMatrix(data=trainx, label=train$TARGET)
  watchlist <- list(trainx=dtrain)
  train$TARGET <- NULL
  
  param <- list(  objective           = "binary:logistic", 
                  booster             = "gbtree",
                  eval_metric         = "auc",
                  eta                 = 0.01,
                  max_depth           = 5,
                  subsample           = 0.68,
                  colsample_bytree    = 0.7
  )
  set.seed(1234)
  xgb_cv <- xgb.cv(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 2000, #460, # BEST AFTER SALDO #550, #ACTUAL
                      verbose             = 2,
                      watchlist           = watchlist,
                      maximize            = FALSE,
                      nfold               = 10,
                      print.every.n       = 10,
                      stratified          = T
  )
  which.max(xgb_cv$test.auc.mean)
  xgb_cv$test.auc.mean[which.max(xgb_cv$test.auc.mean)]
  set.seed(1234)
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 770,#which.max(xgb_cv$test.auc.mean)+1, 
                      verbose             = 1,
                      watchlist           = watchlist,
                      maximize            = FALSE, 
                      print.every.n       = 100
  )
  
  ### predicting on holdout set
  holdout.y <- holdout_set$TARGET
  holdout_set$TARGET <- -1
  holdout_setx <- sparse.model.matrix(TARGET ~ ., data = holdout_set)
  holdout_set$TARGET <- holdout.y
  
  holdout_preds <- predict(clf, holdout_setx)
  
  AreaUnderCurve <- AUC(holdout_set$TARGET,holdout_preds) ##AUC
  CV <- c(CV, xgb_cv$test.auc.mean[which.max(xgb_cv$test.auc.mean)])
  NROUNDS <- c(NROUNDS, which.max(xgb_cv$test.auc.mean)+1)
  AUC_ALL <- c(AUC_ALL, AreaUnderCurve)
}


### Creating the final model basing on the results of the above CV


trainx <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=trainx, label=train$TARGET)
watchlist <- list(trainx=dtrain)
train$TARGET <- NULL

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.01,
                max_depth           = 5,
                subsample           = 0.68,
                colsample_bytree    = 0.7
)
set.seed(1234)
xgb_cv <- xgb.cv(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000, #460, # BEST AFTER SALDO #550, #ACTUAL
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    nfold               = 10,
                    print.every.n       = 10,
                    stratified          = T
)
which.max(xgb_cv$test.auc.mean)
xgb_cv$test.auc.mean[which.max(xgb_cv$test.auc.mean)]
set.seed(1234)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = which.max(xgb_cv$test.auc.mean)+1, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE, 
                    print.every.n       = 100
)

### predicting on holdout set
holdout.y <- holdout_set$TARGET
holdout_set$TARGET <- -1
holdout_setx <- sparse.model.matrix(TARGET ~ ., data = holdout_set)
holdout_set$TARGET <- holdout.y

holdout_preds <- predict(clf, holdout_setx)
AUC(holdout_set$TARGET,holdout_preds) ##AUC

### generating predictions on test set

test$TARGET <- -1
testx <- sparse.model.matrix(TARGET ~ ., data = test)
test$TARGET <- NULL

preds <- predict(clf, testx)
submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submission, "./submission/submission.csv", row.names = F)