#similarity functions


#distance function, take top K most similar rows (indices)
library(randomForest)
library(caret)


#compares each row to each row
cos.sim=function(ma, mb){
  mat=tcrossprod(ma, mb)
  t1=sqrt(apply(ma, 1, crossprod))
  t2=sqrt(apply(mb, 1, crossprod))
  mat / outer(t1,t2)
}

#wrapper around the function        
cosineSimilarity <- function(index_patients,train_patients,K, rescale = FALSE){
  #handle NAs?
  #distance with any NAs = NAs
  index_patients = index_patients[,!colnames(index_patients) %in% "SepsisLabel"]
  train_patients = train_patients[,!colnames(train_patients) %in% "SepsisLabel"]
  
  index_patients = as.matrix(index_patients)
  train_patients = as.matrix(train_patients)
  cosine = cos.sim(index_patients,train_patients)

  #order according to similarity
  cosineOrder = apply(cosine,1, order,decreasing = TRUE)
  topKindices = t(cosineOrder[1:K,])

  lapply(1:nrow(topKindices), function(i) topKindices[i,])
  
}




euclideanDist<- function(index_patients,train_patients,K){
  require(pdist)
  index_patients = index_patients[,!colnames(index_patients) %in% "SepsisLabel"]
  train_patients = train_patients[,!colnames(train_patients) %in% "SepsisLabel"]
  
  index_patients = as.matrix(index_patients)
  train_patients = as.matrix(train_patients)
  euclidean = pdist(index_patients,train_patients)
  euclidean = as.matrix(euclidean)
  
  #order according to similarity
  EuclideanOrder = apply(euclidean,1, order, decreasing = FALSE)
  
  topKindices = t(EuclideanOrder[1:K,])
  
  lapply(1:nrow(topKindices), function(i) topKindices[i,])
  
  
}


#implement it for random forest
rf_ps_predict = function(train_data,test_data,K,similarity_metric = "cosine",mtry=NULL,ntree= 500,...){
  stopifnot(any(similarity_metric %in% c("cosine","euclidean")))
  require(randomForest)
  
  #get list of indices for similar patients
  if(similarity_metric == "cosine"){
    Similar_patients_ind = cosineSimilarity(test_data,train_data,K)
  } else if (similarity_metric == "euclidean"){
    Similar_patients_ind = euclideanDist(test_data,train_data,K)
  }
  
  predict_index  = sapply(1:nrow(test_data), function(i){
    indices = Similar_patients_ind[[i]]
    index_patient = test_data[i,]
    Similar_patients = train_data[indices,]
    model = randomForest(SepsisLabel~.,data = Similar_patients
                         ,xtest = index_patient[,!colnames(index_patient) %in% "SepsisLabel"]
                         ,ytest = index_patient[,"SepsisLabel"]
                         ,ntree = ntree,mtry = mtry,...)
    model$test$votes[,2]
  })
  names(predict_index) = rownames(test_data)
  predict_index
}



#implement it for random forest
rf_ps_predict_parallel = function(train_data,test_data,K,similarity_metric = "cosine",mtry=NULL,ntree= 500,ncore=1,...){
  stopifnot(any(similarity_metric %in% c("cosine","euclidean")))
  require(randomForest)
  require(parallel)
  
  #get list of indices for similar patients
  if(similarity_metric == "cosine"){
    Similar_patients_ind = cosineSimilarity(test_data,train_data,K)
  } else if (similarity_metric == "euclidean"){
    Similar_patients_ind = euclideanDist(test_data,train_data,K)
  }
  
  predict_index  = mclapply(1:nrow(test_data), function(i){
    indices = Similar_patients_ind[[i]]
    index_patient = test_data[i,]
    Similar_patients = train_data[indices,]
    if(all(Similar_patients$SepsisLabel == "Sepsis")){
      prob = 1
    } else if(all(Similar_patients$SepsisLabel == "Non.Sepsis")) {
      prob = 0
    } else{
      model = try(randomForest(SepsisLabel~.,data = Similar_patients
                         ,xtest = index_patient[,!colnames(index_patient) %in% "SepsisLabel"]
                         ,ytest = index_patient[,"SepsisLabel"]
                         ,ntree = ntree,mtry = mtry,...))
    
      prob = model$test$votes[,2]
    }
    prob
    
  }, mc.cores = ncore)
  
  names(predict_index) = rownames(test_data)
  simplify2array(predict_index)
  
}





#implement it for Logistic regression
lr_ps_predict = function(train_data,test_data,K,similarity_metric = "cosine",...){
  stopifnot(any(similarity_metric %in% c("cosine","euclidean")))

  
  #get list of indices for similar patients
  if(similarity_metric == "cosine"){
    Similar_patients_ind = cosineSimilarity(test_data,train_data,K)
  } else if (similarity_metric == "euclidean"){
    Similar_patients_ind = euclideanDist(test_data,train_data,K)
  }
  
  predict_index  = sapply(1:nrow(test_data), function(i){
    indices = Similar_patients_ind[[i]]
    index_patient = test_data[i,]
    Similar_patients = train_data[indices,]
    model = glm(SepsisLabel~.,data = Similar_patients, family = binomial(link = "logit"))
    prob = predict(model,index_patient[,!colnames(index_patient) %in% "SepsisLabel"],"response")
    prob
    
  })
  predict_index 

  
}

#implement it for Logistic regression
lr_ps_predict_parallel = function(train_data,test_data,K,similarity_metric = "cosine",ncore=1,...){
  stopifnot(any(similarity_metric %in% c("cosine","euclidean")))
  require(parallel)
  
  #get list of indices for similar patients
  if(similarity_metric == "cosine"){
    Similar_patients_ind = cosineSimilarity(test_data,train_data,K)
  } else if (similarity_metric == "euclidean"){
    Similar_patients_ind = euclideanDist(test_data,train_data,K)
  }
  
  predict_index  = mclapply(1:nrow(test_data), function(i){
    indices = Similar_patients_ind[[i]]
    index_patient = test_data[i,]
    Similar_patients = train_data[indices,]
    model = glm(SepsisLabel~.,data = Similar_patients, family = binomial(link = "logit"))
    prob = predict(model,index_patient[,!colnames(index_patient) %in% "SepsisLabel"],"response")
    prob
    
  })
  simplify2array(predict_index)
  
}

fastROC <- function(probs, class) {
  class_sorted <- class[order(probs, decreasing=T)]
  TPR <- cumsum(class_sorted) / sum(class)
  FPR <- cumsum(class_sorted == 0) / sum(class == 0)
  return(list(tpr=TPR, fpr=FPR))
}

# Helpful function adapted from stackoverflow
fastAUC <- function(probs, class) {
  x <- probs
  y <- class
  x1 = x[y==1]; n1 = length(x1); 
  x2 = x[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))  
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}


auroc.fun = function(data){
  cf_mat =confusionMatrix(factor(ifelse(data$pred>0.5,"Sepsis","Non.Sepsis"))
                          , data$obs, positive="Sepsis")
  c(AUROC = fastAUC(data$pred,ifelse(data$obs=="Sepsis",1,0)),cf_mat$byClass[1:2])
  
}


#implement for K-fold Cross-validation for random forest
crossval_rf_ps <- function(train_data,mtry=NULL,ntree = 500,nSimilar = 100,similarity_metric = "cosine"
                           ,indices=NULL,folds=10
                           ,repeats=2, doparallel = FALSE, ncore = 1, downSamples = NULL, ...){
  if(is.null(indices)){ #generate indices
    indices = createMultiFolds(train_data$SepsisLabel,k = folds,times = repeats)
  }
  if(is.null(downSamples)){
    downSamples = indices
  }
  
  #get predictions
  predictions <- lapply(1:length(indices), function(i){
    test_fold_indices_to_remove = indices[[i]]
    train_fold_indices = downSamples[[i]]
    train_fold = train_data[train_fold_indices,]
    test_fold = train_data[-test_fold_indices_to_remove,]
    
    
    if(doparallel == TRUE){
      pred = rf_ps_predict_parallel(train_fold,test_fold,nSimilar,similarity_metric = similarity_metric,mtry=mtry
                                    ,ntree = ntree,ncore = ncore)
    }else{
      pred = rf_ps_predict(train_fold,test_fold,nSimilar,similarity_metric = similarity_metric,mtry=mtry
                           ,ntree = ntree)
    }
    #print(pred)
    
    pred_ref = data.frame(pred = factor(ifelse(pred>0.5,"Sepsis","Non.Sepsis"), levels = c("Sepsis","Non.Sepsis"))
                          ,obs = factor(test_fold$SepsisLabel, levels = c("Sepsis","Non.Sepsis"))
                          ,Sepsis = pred, Non.Sepsis = 1-pred) 
    
    #calculate AUROC,AUPRC, perf metric
    AUPRC = prSummary(pred_ref,lev = c("Sepsis","Non.Sepsis"))
    names(AUPRC)[1] <- c("AUPRC")
    AUROC = twoClassSummary(pred_ref,lev = c("Sepsis","Non.Sepsis"))
    names(AUROC)[1] <- c("AUROC")
    
    list(predictions = pred,performance = c(AUROC,AUPRC))
  })
  names(predictions)<- names(indices)
  
  performance <- sapply(predictions, function(fold) fold$performance)
  colnames(performance) <-names(indices)
  #take the average and sd of these values
  perf_summary = apply(performance,1,function(x){c(mean(x),sd(x))})
  perf_summary = t(perf_summary)
  colnames(perf_summary)<- c("MEAN","SD")
  
  model = list(mtry=mtry,ntree = ntree,nSimilar = nSimilar,patients = train_data)
  list(performance = perf_summary, predictions = predictions, model = model)
}


#implement for K-fold Cross-validation for logistic regression
crossval_lr_ps <- function(train_data,nSimilar = 100,similarity_metric = "cosine"
                           ,indices=NULL,folds=10,repeats=2, doparallel = FALSE,ncore = 1, downSamples = NULL, ...){
  if(is.null(indices)){ #generate indices
    indices = createMultiFolds(train_data$SepsisLabel,k = folds,times = repeats)
  }
  if(is.null(downSamples)){
    downSamples = indices
  }
  
  
  #get predictions
  predictions <- lapply(1:length(indices), function(i){
    test_fold_indices_to_remove = indices[[i]]
    train_fold_indices = downSamples[[i]]
    train_fold = train_data[train_fold_indices,]
    test_fold = train_data[-test_fold_indices_to_remove,]
    
    
    if(doparallel == TRUE){
      pred = lr_ps_predict_parallel(train_fold,test_fold,nSimilar,similarity_metric = similarity_metric,ncore = ncore)
    } else{
      pred = lr_ps_predict(train_fold,test_fold,nSimilar,similarity_metric = similarity_metric)
    }
    
    pred_ref = data.frame(pred = factor(ifelse(pred>0.5,"Sepsis","Non.Sepsis"), levels = c("Sepsis","Non.Sepsis"))
                          ,obs = factor(test_fold$SepsisLabel, levels = c("Sepsis","Non.Sepsis"))
                          ,Sepsis = pred, Non.Sepsis = 1-pred) 
    
    #calculate AUROC,AUPRC, perf metric
    AUPRC = prSummary(pred_ref,lev = c("Sepsis","Non.Sepsis"))
    names(AUPRC)[1] <- c("AUPRC")
    AUROC = twoClassSummary(pred_ref,lev = c("Sepsis","Non.Sepsis"))
    names(AUROC)[1] <- c("AUROC")
    
    tmp = list()
    tmp$predictions = pred
    tmp$performance = c(AUROC,AUPRC)
    tmp
  })
  names(predictions)<- names(indices)
  
  performance <- sapply(predictions, function(fold) fold$performance)
  colnames(performance) <-names(indices)
  #take the average and sd of these values
  perf_summary = apply(performance,1,function(x){c(mean(x),sd(x))})
  perf_summary = t(perf_summary)
  colnames(perf_summary)<- c("MEAN","SD")
  
  model = list(nSimilar = nSimilar,patients = train_data)
  return(list(performance = perf_summary, predictions = predictions, model = model))
}



#implement it for parameter tuning
tune_rf_ps = function(train_data,similarity_metric = "cosine",indices=NULL,K=10,repeats=2, tuneGrid =NULL,metric = "AUROC"
                      , doparallel = FALSE, ncore = 1, sampling = "No", saveGrid = "rf_ps_perf_progress.RDS"){
  
  if(!is.null(tuneGrid)){
    stopifnot((is.data.frame(tuneGrid)) & all(colnames(tuneGrid) %in% c("mtry","ntree","nSimilar")))
  } else{
    tuneGrid <- expand.grid(mtry=round(sqrt(ncol(train_data)-1)),ntree = 100,nSimilar = c(100))
  }
  if(is.null(indices)){ #generate indices
    indices = createMultiFolds(train_data$SepsisLabel,k = folds,times = repeats)
  }
  
  if( sampling %in% c("down","Down")){
    downSamples = lapply(indices, function(ind){tmp = downSample(ind, train_data[ind,"SepsisLabel"]
                                                                      , list = TRUE)
                                                 tmp$x[,1]})
    sampling = "Down"
  } else{ downSamples = NULL
    
  }
  savegrid = NULL
  
  perf_grid = sapply(1:nrow(tuneGrid), function(i){
    mtry = tuneGrid$mtry[i]
    ntree= tuneGrid$ntree[i]
    nSimilar = tuneGrid$nSimilar[i]
    cv_results = crossval_rf_ps(train_data,mtry=mtry,ntree = ntree,nSimilar = nSimilar
                                ,similarity_metric = similarity_metric
                                ,indices=indices,folds=K,repeats=repeats
                                , doparallel = doparallel, ncore = ncore
                                , downSamples = downSamples)$performance
    
    cv_vec = c(cv_results)
    names(cv_vec) = c(paste0(rownames(cv_results),".MEAN"),paste0(rownames(cv_results),".SD"))
    
    savegrid = rbind(savegrid,c(mtry = mtry,ntree=ntree, nSimilar = nSimilar,cv_vec))
    saveRDS(savegrid,saveGrid)
    
    c(mtry = mtry,ntree=ntree, nSimilar = nSimilar,cv_vec)
    
  }) #test this sapply
  
  #select best AUROC or AUPRC
  
  perf_grid = as.data.frame(t(perf_grid))
  metric_column = perf_grid[,paste0(metric,".MEAN")]
  best_i = which(metric_column == max(metric_column))  
  if(length(best_i)){print(paste(length(best_i),"-way tie of Best models"))}
  
  best_performance = perf_grid[best_i,]
  
  if( is.null(sampling)) sampling = "No"
  
  #show the performance metrics
  print(paste0("Random Forest on Similar Patients using ",similarity_metric, " metric, with ",sampling, " Sampling"))
  print(paste0("Cross-Validated (",K," fold, repeated ", repeats, " times)"))
  print(paste0("Tuned using ",metric))
  print("Resampling Results")
  print(perf_grid[,1:10])
  
  
  #final giveaway
  finalModel = list(train_data = train_data, best_performance = best_performance
                    , nSimilar = perf_grid$nSimilar[best_i]
                    , mtry = perf_grid$mtry[best_i]
                    , ntree = perf_grid$ntree[best_i]
                    , similarity_metric = similarity_metric
                    , AUROC = perf_grid$AUROC.MEAN
                    , AUPRC = perf_grid$AUPRC.MEAN
                    , Modeltype = "Random Forest"
                    , Sampling = sampling)
  class(finalModel)<- c("list","SimilarityModel")
  
  list(finalModel = finalModel, performance_grid = perf_grid)
  
  
  
}


#implement it for parameter tuning
tune_lr_ps = function(train_data,similarity_metric = "cosine",indices=NULL,K=10,repeats=2
                      , tuneGrid =NULL,metric = "AUROC", doparallel = FALSE
                      , ncore = 1, sampling = "No", saveGrid = "lr_ps_perf_progress.RDS"){
  
  if(!is.null(tuneGrid)){
    stopifnot((is.data.frame(tuneGrid)) & all(colnames(tuneGrid) %in% c("nSimilar")))
  } else{
    tuneGrid <- expand.grid(nSimilar = c(100))
  }
  
  if(is.null(indices)){ #generate indices
    indices = createMultiFolds(train_data$SepsisLabel,k = folds,times = repeats)
  }
  
  if( sampling %in% c("down","Down")){
    downSamples = lapply(indices, function(ind){tmp = downSample(ind, train_data[ind,"SepsisLabel"]
                                                                  , list = TRUE)
    tmp$x[,1]})
    sampling = "Down"
    } else{ downSamples = NULL
  
    }
  
  savegrid = NULL
  
  perf_grid = sapply(1:nrow(tuneGrid), function(i){

    nSimilar = tuneGrid$nSimilar[i]
    cv_results = crossval_lr_ps(train_data,nSimilar = nSimilar
                                ,similarity_metric = similarity_metric
                                ,indices=indices,folds=K,repeats=repeats
                                ,doparallel = doparallel, ncore = ncore
                                ,downSamples = downSamples)$performance
    
    cv_vec = c(cv_results)
    names(cv_vec) = c(paste0(rownames(cv_results),".MEAN"),paste0(rownames(cv_results),".SD"))
    
    savegrid = rbind(savegrid,c(nSimilar = nSimilar,cv_vec))
    saveRDS(savegrid,saveGrid)
    
    c(nSimilar = nSimilar,cv_vec)
    
  }) #test this sapply
  
  #select best AUROC or AUPRC
  
  perf_grid = as.data.frame(t(perf_grid))
  metric_column = perf_grid[,paste0(metric,".MEAN")]
  best_i = which(metric_column == max(metric_column))  
  if(length(best_i)){print(paste(length(best_i),"-way tie of Best models"))}
  
  best_performance = perf_grid[best_i,]
  
  sampling= if(is.null(sampling)) sampling = "No"
    
  
  #show the performance metrics
  print(paste0("Logistic Regression on Similar Patients using ",similarity_metric, " metric, with ",sampling," Sampling"))
  print(paste0("Cross-Validated (",K," fold, repeated ", repeats, " times)"))
  print(paste0("Tuned using ",metric))
  print("Resampling Results")
  print(perf_grid[,1:8])
  
  
  #final giveaway
  finalModel = list(train_data = train_data, best_performance = best_performance
                    , nSimilar = perf_grid$nSimilar[best_i]
                    , similarity_metric = similarity_metric
                    , AUROC = perf_grid$AUROC.MEAN
                    , AUPRC = perf_grid$AUPRC.MEAN
                    , Modeltype = "Logistic Regression"
                    , Sampling = sampling)
  class(finalModel)<- c("list","SimilarityModel")
  
  list(finalModel = finalModel, performance_grid = perf_grid)
}


model_predict <- function(SimilarityModel, test_data, doparallel = FALSE, ncore = 1){
  stopifnot(any(class(SimilarityModel) %in% "SimilarityModel"))
  
  train_data = SimilarityModel$train_data
  nSimilar = SimilarityModel$nSimilar
  
  #does not run if test_data is missing columns that are in train_data
  stopifnot(all(colnames(train_data) %in% colnames(test_data)))
  
  #gets rid of excess columns in test_data
  test_data = test_data[,colnames(test_data) %in% colnames(train_data)]
  
  #is there downsampling in the model
  if( SimilarityModel$Sampling == "Down"){
    train_data = downSample(train_data[,!colnames(train_data) %in% "SepsisLabel" ], train_data[,"SepsisLabel"])
    train_data$SepsisLabel = train_data$Class
    train_data$Class = NULL
  }
  
  if(SimilarityModel$Modeltype == "Logistic Regression"){
    similarity_metric = SimilarityModel$similarity_metric
    
    if(doparallel==TRUE){
      pred = lr_ps_predict_parallel(train_data = train_data,test_data = test_data,K = nSimilar
                           ,similarity_metric = similarity_metric, ncore = ncore)
    } else{
      pred = lr_ps_predict(train_data = train_data,test_data = test_data,K = nSimilar
                         ,similarity_metric = similarity_metric)
    }
    
  }
  if(SimilarityModel$Modeltype == "RandomForest"){
    mtry = SimilarityModel$mtry
    ntree = SimilarityModel$ntree
    similarity_metric = SimilarityModel$similarity_metric
    
    if( doparallel== TRUE){
      pred = rf_ps_predict_parallel(train_data = train_data,test_data = test_data,K = nSimilar
                           , similarity_metric = similarity_metric
                           , mtry = mtry,ntree = ntree, ncore = ncore)
    }else{
      pred = rf_ps_predict(train_data = train_data,test_data = test_data,K = nSimilar
                         , similarity_metric = similarity_metric
                         , mtry = mtry,ntree = ntree)
    }
    
  }
  
  pred_ref = data.frame(pred = factor(ifelse(pred>0.5,"Sepsis","Non.Sepsis"), levels = c("Non.Sepsis","Sepsis"))
                        ,obs = test_data$SepsisLabel
                        ,Sepsis = pred, Non.Sepsis = 1-pred) 
  
  #calculate AUROC,AUPRC, perf metric
  AUPRC = prSummary(pred_ref,lev = c("Sepsis","Non.Sepsis"))
  names(AUPRC)[1] <- c("AUPRC")
  AUROC = twoClassSummary(pred_ref,lev = c("Non.Sepsis","Sepsis"))
  names(AUROC)[1] <- c("AUROC")
  
  list(predictions = pred, performance = c(AUROC,AUPRC))
  
  
}



