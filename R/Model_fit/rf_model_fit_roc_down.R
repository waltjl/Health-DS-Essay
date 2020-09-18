##model fitting with down sampling within resamples


#ssh jlrwalto@biglinux.math.uwaterloo.ca
#tmux new -s jo4
#cd User/Documents/Health-DS-Essay/
  
  #R
  
  #added 
  
  
library(caret)
library(doParallel)
cl <- makePSOCKcluster(3)
registerDoParallel(cl)


#setwd("../../../Data/Built_Tables/Model_fit")
setwd("Data/Built_Tables/Model_fit")
train_vital_data <- readRDS("train_vital_input_data.RDS")
train_labCount_data <- readRDS("train_labCount_input_data.RDS")

set.seed(87)
ntrees = c(100,200,500)
mtrys = c(4,6,5,7,3,16,15,8,17,10)
mtrys


train_indices = readRDS("train_indices.RDS")
tunegrid1 <- expand.grid(.mtry=mtrys)

#Initial Model Fitting with random Forest on both data sets
setwd("../../../R/Model_fit_roc/rf_down")
rffitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 20,
                             index = train_indices,
                             savePredictions = TRUE,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             sampling = "down")

tunegrid <- expand.grid(.mtry=round(sqrt(ncol(train_vital_data))))


# rfFit_vital<- train(SepsisLabel~., data = train_vital_data,
#                     method = "rf",
#                     trControl = rffitControl
#                     , tuneGrid=tunegrid
#                     , metric = "ROC"
#                     , ntree = 500)
# saveRDS(rfFit_vital,"rf_fit_vital.RDS")
# rfFit_vital


############### with some parameter tuning for ntree and mtry
rfFit_vital100<- train(SepsisLabel~., data = train_vital_data,
                       method = "rf",
                       trControl = rffitControl
                       , tuneGrid=tunegrid1
                       , metric = "ROC"
                       , ntree = ntrees[1])
saveRDS(rfFit_vital100,"rf_fit_vital100.RDS")
rfFit_vital100

rfFit_vital200<- train(SepsisLabel~., data = train_vital_data,
                       method = "rf",
                       trControl = rffitControl
                       , tuneGrid=tunegrid1
                       , metric = "ROC"
                       , ntree = ntrees[2])
saveRDS(rfFit_vital200,"rf_fit_vital200.RDS")
rfFit_vital200


rfFit_vital500<- train(SepsisLabel~., data = train_vital_data,
                       method = "rf",
                       trControl = rffitControl
                       , tuneGrid=tunegrid1
                       , metric = "ROC"
                       , ntree = ntrees[3])
saveRDS(rfFit_vital500,"rf_fit_vital500.RDS")
rfFit_vital500





tunegrid_labCount <- expand.grid(.mtry=round(sqrt(ncol(train_labCount_data))))

# rfFit_labCount<- train(SepsisLabel~., data = train_labCount_data,
#                        method = "rf",
#                        trControl = rffitControl
#                        , tuneGrid=tunegrid_labCount
#                        , metric = "ROC"
#                        , ntree = 500)
# 

# rfFit_labCount
# saveRDS(rfFit_labCount,"rf_fit_labcount.RDS")

#with parameter tuning

rfFit_labCount100<- train(SepsisLabel~., data = train_labCount_data,
                          method = "rf",
                          trControl = rffitControl
                          , tuneGrid=tunegrid1
                          , metric = "ROC"
                          , ntree = ntrees[1])


rfFit_labCount100
saveRDS(rfFit_labCount100,"rf_fit_labcount100.RDS")

rfFit_labCount200<- train(SepsisLabel~., data = train_labCount_data,
                          method = "rf",
                          trControl = rffitControl
                          , tuneGrid=tunegrid1
                          , metric = "ROC"
                          , ntree = ntrees[2])


rfFit_labCount200
saveRDS(rfFit_labCount200,"rf_fit_labcount200.RDS")


rfFit_labCount500<- train(SepsisLabel~., data = train_labCount_data,
                          method = "rf",
                          trControl = rffitControl
                          , tuneGrid=tunegrid1
                          , metric = "ROC"
                          , ntree = ntrees[3])


rfFit_labCount500
saveRDS(rfFit_labCount500,"rf_fit_labcount500.RDS")

