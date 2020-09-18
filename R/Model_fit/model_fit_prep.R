##model fitting

#ssh jlrwalto@fastlinux.math.uwaterloo.ca
#ssh jlrwalto@biglinux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R

library(caret)

setwd("Data/Built_Tables/Model_fit")
train_data <- readRDS("train_vital_data_reduced.RDS")
train_label <- readRDS("train_SepsisLabel_A12H.RDS")

#need to append label to the matrix in train_data
train_data = merge(train_data,train_label,by = "patientid")
dim(train_data)

#remove non-patient characteristics
input_train_data = train_data[,-c(1,4)] #remove patient id and hospital admit time
rownames(input_train_data)<- train_data$patientid

input_train_data$SepsisLabel <- ifelse(input_train_data$SepsisLabel,"Sepsis","Non.Sepsis")
input_train_data$SepsisLabel <- as.factor(input_train_data$SepsisLabel)
head(input_train_data)

saveRDS(input_train_data,"train_vital_input_data.RDS")




#now with labcounts in too
train_labCount_data <- readRDS("train_labCount_data_reduced.RDS")

train_labCount_data = merge(train_labCount_data,train_label,by = "patientid")
dim(train_labCount_data)
# [1] 20281    42
#remove non-patient characteristics
rownames(input_labCount_train_data)<- train_labCount_data$patientid
input_labCount_train_data = train_labCount_data[,-c(1,4)] #remove patient id and hospital admit time

input_labCount_train_data$SepsisLabel <- ifelse(input_labCount_train_data$SepsisLabel,"Sepsis","Non.Sepsis")
input_labCount_train_data$SepsisLabel <- as.factor(input_labCount_train_data$SepsisLabel)
head(input_labCount_train_data)

saveRDS(input_labCount_train_data,"train_labCount_input_data.RDS")


all(sum(train_labCount_data$patientid == train_data$patientid))
#[1] TRUE
#both have the same order so we can use the same set of indicies for both


set.seed(87)
train_indices = createMultiFolds(input_train_data$SepsisLabel,k = 10,times = 20)
saveRDS(train_indices,"train_indices.RDS")

#now with test data
setwd("/Users/jordyn/Documents/Health-DS-Essay/")




#try model fitting again but with downsampled data
train_labCount_data <- readRDS("train_labCount_data_reduced.RDS")

train_labCount_data = merge(train_labCount_data,train_label,by = "patientid")
dim(train_labCount_data)
# [1] 20281    42

train_labCount_data$SepsisLabel <- ifelse(train_labCount_data$SepsisLabel,"Sepsis","Non.Sepsis")
train_labCount_data$SepsisLabel <- as.factor(train_labCount_data$SepsisLabel)
head(train_labCount_data)

down_train_labCount <- downSample(x = train_labCount_data[, -ncol(train_labCount_data)],
                         y = train_labCount_data$SepsisLabel)

table(down_train_labCount$Class)  
head(down_train_labCount )
rownames(down_train_labCount) <- down_train_labCount$patientid
down_train_labCount = down_train_labCount[,-c(1,4)]
down_train_labCount$SepsisLabel <- down_train_labCount$Class
down_train_labCount$Class<- NULL
head(down_train_labCount)

saveRDS(down_train_labCount,"down_train_labCount_data.RDS")

down_train_vital = down_train_labCount[,!grepl("count",colnames(down_train_labCount))]
head(down_train_vital)
saveRDS(down_train_vital,"down_train_vital_data.RDS")

down_train_indices = createMultiFolds(down_train_vital$SepsisLabel,k = 10,times = 20)
saveRDS(down_train_indices,"down_train_indices.RDS")

