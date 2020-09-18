#!/usr/bin/env Rscript

##model fitting

#ssh jlrwalto@linux.math.uwaterloo.ca

ssh jlrwalto@fastlinux.math.uwaterloo.ca


#cd User/Documents/Health-DS-Essay/

#R
library(naniar)
library(caret)
library(randomForest)

setwd("../../../Data/Built_Tables/Model_fit")

setwd("Data/Built_Tables/Model_fit")
#setwd("/Users/jordyn/Documents/Health-DS-Essay/R/eda_vital_notebook/")
train_data <- readRDS("train_vital_data_reduced.RDS")
train_label <- readRDS("train_SepsisLabel_A12H_red.RDS")

#setwd("/Users/jordyn/Documents/Health-DS-Essay/R/eda_labCount_notebook/")
train_label1 <- readRDS("train_SepsisLabel_A12H_red.RDS")

sum(train_label1$patientid %in% train_label$patientid) #all correct and the sameeee checkmark


#need to append label to the matrix in train_data
train_data = merge(train_data,train_label,by = "patientid")
dim(train_data)

#remove non-patient characteristics
input_train_data = train_data[,-c(1,4)]
input_train_data$SepsisLabel <- ifelse(input_train_data$SepsisLabel,"Sepsis","Non.Sepsis")
input_train_data$SepsisLabel <- as.factor(input_train_data$SepsisLabel)



vis_miss(input_train_data)

set.seed(87)
train_indices = createMultiFolds(input_train_data$SepsisLabel,k = 3,times = 2)
#save for other created models
#saveRDS(train_indices,"train_indices.RDS")
#gives you the training indices

setwd("../../../R/Model_fit/rf")
rffitControl1 <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 2,
                           index = train_indices,
                           ## Estimate class probabilities
                           classProbs = TRUE)

tunegrid <- expand.grid(.mtry=round(sqrt(ncol(input_train_data))))

rfFit1<- train(SepsisLabel~., data = input_train_data,
               method = "rf",
               trControl = rffitControl1
               , tuneGrid=tunegrid
               , metric = "Kappa"
               , ntree = 500)




saveRDS(rfFit1,"testrfFit.RDS")
plot(rfFit1$finalModel)


rfFit = randomForest(SepsisLabel~Age+Gender+O2Sat_mean, data = input_train_data,na.action =na.roughfix )

LRfitControl1 <- trainControl(method = "repeatedcv",
                              number = 3,
                              repeats = 2,
                              index = train_indices,
                              ## Estimate class probabilities
                              classProbs = TRUE)



LRFit1<- train(SepsisLabel~., data = input_train_data
               , method = 'glm'
               , family= "binomial"
               , trControl = LRfitControl1 
               , metric = "Kappa"
               )

summary(LRFit1$finalModel)

LRFit2<- train(SepsisLabel~Gender+Resp_mean+HR_min
               +MAP_min + Resp_max+HR_sd, data = input_train_data
               , method = 'glm'
               , family= "binomial"
               , trControl = LRfitControl1 
               , metric = "Kappa"
)

LRFit2
