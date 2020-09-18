##model fitting with down sampling within resamples

#ssh jlrwalto@biglinux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R

library(caret)
library(doParallel)
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

setwd("Data/Built_Tables/Model_fit")
train_vital_data <- readRDS("train_vital_input_data.RDS")
train_labCount_data <- readRDS("train_labCount_input_data.RDS")

set.seed(87)
train_indices = readRDS("train_indices.RDS")

#Initial Model Fitting with random Forest on both data sets
setwd("../../../R/Model_fit_roc/lr_down")
lrfitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 20,
                             index = train_indices,
                             savePredictions = TRUE,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             sampling = "down")


lrFit_vital<- train(SepsisLabel~., data = train_vital_data
                    , method = "glm"
                    , family = "binomial"
                    , trControl = lrfitControl
                    , metric = "ROC")


lrFit_vital


saveRDS(lrFit_vital,"lr_fit_vital.RDS")
lrFit_vital = readRDS("lr_fit_vital.RDS")

#model including labcounts as well
lrFit_labCounts<- train(SepsisLabel~., data = train_labCount_data
                        , method = "glm"
                        , family = "binomial"
                        , trControl = lrfitControl
                        , metric = "ROC")

lrFit_labCounts
saveRDS(lrFit_labCounts,"lr_fit_labCounts.RDS")
lrFit_labCounts = readRDS("lr_fit_labCounts.RDS")


