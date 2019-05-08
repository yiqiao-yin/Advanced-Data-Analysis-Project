####################### BEGIN SCRIPT ##########################

# README
# This script has two parts:
# (I) NONPARAMETRIC FEATURE SELECTION
#     PCA, KMEANS
# (II) MACHINE LEARNING
#      Logistic, Random Forest, NN

############## (I) NONPARAMETRIC FEATURE SELECTION ############

##################### PRINCIPLE COMPONENT #####################

# Define function
PC.VS <- function(
  x = all[, -1],
  y = all[, 1],
  cutoff = 0.9,
  select = 1:10
) {
  # Compile data
  all <- data.frame(cbind(y,x))
  
  # Split data
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)):nrow(all),]; dim(test) # Testing set
  train_x_pc_total <- princomp(train[, -1])
  train_x_pc <- princomp(train[, -1])$scores
  test_x_pc_total <- princomp(test[, -1])
  test_x_pc <- princomp(test[, -1])$scores
  X <- rbind(train_x_pc, test_x_pc); Y <- cbind(all$y)
  all <- data.frame(cbind(Y, X))[, c(select)]
  colnames(all)[1] <- "y"
  
  # Return
  return(list(
    all = data.frame(rbind(train, test)),
    train = train,
    test = test,
    train.x = train_x_pc,
    train.x.pc.loading = train_x_pc_total$loadings,
    train.y = train[, 1],
    test.x = test_x_pc,
    test.x.pc.loading = test_x_pc_total$loadings,
    test.y = test[, 1]
  ))
} # End of function

############################ KMEANS ###########################

# Define function
KMEANS.VS <- function(
  x = all[, -1],
  y = all[, 1],
  cutoff = 0.9,
  k = 3
) {
  # Compile data
  all <- data.frame(cbind(y,x))
  
  # Split
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)):nrow(all),]; dim(test) # Testing set
  train_x_pc <- data.frame(t(kmeans(t(train[, -1]), k)$centers)); dim(train_x_pc)
  test_x_pc <- data.frame(t(kmeans(t(test[, -1]), k)$centers)); dim(test_x_pc)
  X <- rbind(train_x_pc, test_x_pc); Y <- cbind(all$y)
  all <- data.frame(cbind(Y, X))
  colnames(all)[1] <- "y"
  
  # Return
  return(list(
    all = data.frame(rbind(train, test)),
    train = train,
    test = test,
    train.x = train[, -1],
    train.y = train[, 1],
    test.x = test[, -1],
    test.y = test[, 1]
  ))
} # End of function

################### (II) MACHINE LEARNING #####################
######################## GLM: LOGISTIC ########################

# Library
library(pROC)

# Define function:
logistic <- function(
  x = all[, -1],
  y = all[ 1],
  cutoff = .67,
  fam = binomial,
  cutoff.coefficient = 1) {
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- data.frame(train[,-1]); colnames(train.x) <- colnames(train)[-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- data.frame(test[,-1]); dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  # GLM or # LM
  model <- glm(
    train.y ~.,
    data = train.x,
    family = fam
    # gaussian
    # binomial
    # quasibinomial
  )
  sum <- summary(model)
  
  # Make prediction on training:
  preds.train.prob <- predict(model, train.x)
  preds.mean.train <- mean(preds.train.prob)
  preds.train <- ifelse(preds.train.prob > cutoff.coefficient*preds.mean.train, 1, 0)
  table.train <- as.matrix(cbind(preds.train,train.y))
  tab.train <- table(table.train[,1], table.train[,2])
  percent.train <- sum(diag(tab.train))/sum(tab.train)
  
  # ROC
  actuals <- train.y
  scores <- preds.train.prob
  roc_obj.train <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj.train$auc
  
  # Make prediction on testing:
  colnames(test.x) <- colnames(train.x)
  preds.prob <- predict(model, test.x) # nrow(test.x)
  preds.mean <- mean(preds.prob)
  preds <- ifelse(preds.prob > cutoff.coefficient*preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds,test.y)
  )
  dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table)
  
  # ROC
  actuals <- test.y
  scores <- preds.prob
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, predict(model, test.x))
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = sum,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      #Train.ROC = plot(roc_obj.train),
      y.hat = preds,
      y.truth = test.y,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1#,
      #Test.ROC = plot(roc_obj)
    )
  )
} # End of function

############## RANDOMFOREST: RANDOM FOREST ######################

# Initiate library
library('randomForest'); library("pROC")

# Define function:
Random.Forest <- function(
  x = all[, -1],
  y = all[, 1],
  cutoff = .9, 
  num.tree = 10,
  num.try = sqrt(ncol(all)),
  cutoff.coefficient = 1,
  SV.cutoff = 1:10
) {
  
  # Compile data
  all <- data.frame(cbind(y,x))
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  model <- randomForest(
    x = as.matrix(train.x),
    y = as.factor(train.y),
    xtest = as.matrix(test.x),
    ytest = as.factor(test.y),
    ntree = num.tree,
    mtry = num.try
  )
  sum <- summary(model)
  
  # Extract imporance
  feature.and.score <- data.frame(model$importance)
  feature.score <- feature.and.score[order(feature.and.score, decreasing = TRUE), ]
  feature.order <- rownames(feature.and.score)[order(feature.and.score, decreasing = TRUE)]
  new.feature.and.score <- data.frame(cbind(feature.order, feature.score))
  head(new.feature.and.score)
  #SV.cutoff = 1:5
  selected.variable <- feature.order[SV.cutoff]
  selected.variable
  
  # Make prediction on training:
  preds.train <- model$predicted
  preds.train[is.na(preds.train) == TRUE] <- 0
  #preds.mean.train <- mean(preds.train)
  #preds.train <- ifelse(preds.train > preds.mean.train, 1, 0)
  table.train <- as.matrix(
    cbind(preds.train,train.y)
  )
  tab.train <- table(table.train[,1], table.train[,2]); tab.train
  percent.train <- sum(diag(tab.train))/sum(tab.train); percent.train
  
  # ROC
  actuals <- train.y
  scores <- as.numeric(preds.train)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj$auc; auc.train
  
  # Make prediction on testing:
  #preds.binary <- model$test$predicted # colMeans(model$yhat.test)
  preds.probability <- model$test$votes[,2]
  preds.mean <- mean(preds.probability)
  preds.binary <- ifelse(preds.probability > cutoff.coefficient*preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds.binary,test.y)
  ); dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table); percent
  
  # ROC
  actuals <- test.y
  scores <- preds.binary 
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc; auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, model$test$votes[,2])
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = model,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      Important.Variables = selected.variable,
      y.hat = preds.binary,
      y.truth = test.y,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob
      #AUC.Plot = plot(
      #  1 - spec, sens, type = "l", col = "red", 
      #  ylab = "Sensitivity", xlab = "1 - Specificity")
    )
  )
} # End of function

################# KERAS: NEURAL NETWORK ######################

# Library already installed
# YinsKerasNN::YinsKerasNN(x = all[, -1], y = all[, 1], cutoff = 0.9, epochs = 30)

################### e1071: SUPPORT VECTOR MACHINE ####################

# Begin Function:
Support_Vector_Machine <- function(
  x = all[, -1],
  y = all[ ,1],
  cutoff = 0.9,
  c = 1.5,
  g = 1.3,
  cutoff.coefficient = 1){
  
  # Compile
  all <- data.frame(cbind(y,x))
  
  # Split:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  ## Apply SVM
  # Ex: c<-1; g<-1
  svm.fit <- e1071::svm(
    formula = train.y ~.,
    data = train.x,
    type = "C-classification",
    kernel = "sigmoid",
    cost = c,
    gamma = g
  )
  
  # Make prediction on training:
  preds.train.prob <- predict(svm.fit, train.x); preds.train.prob <- as.numeric(as.character(preds.train.prob))
  preds.mean.train <- mean(preds.train.prob)
  preds.train <- ifelse(preds.train.prob > cutoff.coefficient*preds.mean.train, 1, 0)
  table.train <- as.matrix(cbind(preds.train,train.y))
  tab.train <- table(table.train[,1], table.train[,2])
  percent.train <- sum(diag(tab.train))/sum(tab.train)
  
  # ROC
  actuals <- train.y
  scores <- preds.train.prob
  roc_obj.train <- pROC::roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj.train$auc
  
  # Make prediction on testing:
  colnames(test.x) <- colnames(train.x)
  preds.prob <- predict(svm.fit, test.x); preds.prob <- as.numeric(as.character(preds.prob))
  preds.mean <- mean(preds.prob)
  preds <- ifelse(preds.prob > cutoff.coefficient*preds.mean, 1, 0)
  table <- as.matrix(cbind(preds,test.y))
  dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table)
  
  # ROC
  actuals <- test.y
  scores <- preds.prob
  roc_obj <- pROC::roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, preds.prob)
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = list(svm.fit, summary(svm.fit)),
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      #Train.ROC = plot(roc_obj.train),
      y.hat = preds,
      y.truth = test.y,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1#,
      #Test.ROC = plot(roc_obj)
    )
  )
} ## End of function

##################### REAL DATA APPLICATION ######################

# Define function
real_data_application <- function(
  data = data,
  how.many.folds = 4
) {
  #################### REAL DATA: CV + HELD OUT TEST SET ################
  
  # Data
  #data <- read.csv(paste0(path,"data/adult.csv"))
  #head(data); dim(data)
  
  # Clean data
  mu_wage <- mean(data$WAGE.HR)
  data_new <- data.frame(cbind(
    Wage = ifelse(data$WAGE.HR > mu_wage, 1, 0),
    data[, -1]
  ))

  ###################### K-FOLD CV: REG & CLASS ########################
  
  # READ ME:
  # This script loops through k folds.
  # Each fold the algorithm fits a selected machine learning technique.
  # The algorithm outputs k-fold accuracy (or other selected results). 
  
  # Two classes:
  # This is for classification only
  all <- data_new
  all.A <- all[all[,1] == 0, ]; dim(all.A)
  all.B <- all[all[,1] == 1, ]; dim(all.B)
  
  # Null Result:
  result <- NULL
  
  # CV:
  # Write a k-fold CV loop:
  print(paste0("This is ", how.many.folds-1, "-fold CV."))
  print(paste0("We leave ", how.many.folds, "th fold untouched as held-out test."))
  print(paste0("Here let us do fold 1 up to fold ", how.many.folds-1, " as CV procedure."))
  for (folds.i in c(1:(how.many.folds-1))){
    # Create k-fold training data sets for CV:
    
    # Create:
    # folds: a list of numbers with different index;
    # testIndexes: the index that equals to each index in folds;
    
    # For classification
    # Then we can create test and train data sets:
    folds.A <- cut(seq(1,nrow(all.A)),breaks=how.many.folds,labels=FALSE)
    folds.B <- cut(seq(1,nrow(all.B)),breaks=how.many.folds,labels=FALSE)
    #folds <- cut(seq(1,nrow(all)),breaks=how.many.folds,labels=FALSE)
    
    # For regression
    
    # For classifiction: 
    # Set:
    #folds.i <- 1
    testIndexes.A <- which(folds.A==folds.i, arr.ind = TRUE)
    testIndexes.B <- which(folds.B==folds.i, arr.ind = TRUE)
    testData.A <- all.A[testIndexes.A, ]; trainData.A <- all.A[-testIndexes.A, ]
    testData.B <- all.B[testIndexes.B, ]; trainData.B <- all.B[-testIndexes.B, ]
    all <- data.frame(rbind(
      trainData.A, trainData.B,
      testData.A, testData.B 
    ))
    
    # MODEL FITTING / MACHINE LEARNING:
    # One can change to use Regression or Classification:
    cutoff <- round((nrow(trainData.A)+nrow(trainData.B))/nrow(all), 1) # Check this ratio out
    # (I) NONPARAMETRIC
    # PC
    PC_VS_Result <- PC.VS(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      select = 1:(3*folds.i)
    )
    
    # KMEANS
    KMEANS_VS_Result <- KMEANS.VS(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      k = 3*folds.i
    )
    
    # (II) MACHINE LEARNING
    # NONE + LOGISTIC / RF / SVM / NN
    Algo_NONE_LOGISTIC <- logistic(
      x = all[, -1],
      y = all[ 1],
      cutoff = cutoff,
      fam = binomial,
      cutoff.coefficient = 1)
    Algo_NONE_RF <- Random.Forest(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      num.tree = 10*folds.i,
      num.try = sqrt(ncol(all))*folds.i,
      cutoff.coefficient = 1,
      SV.cutoff = 1:10)
    Algo_NONE_SVM <- Support_Vector_Machine(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      c = 1.5*folds.i,
      g = 1.3*folds.i)
    Algo_NONE_NN <- YinsKerasNN::YinsKerasNN(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      epochs = 10*folds.i)
    
    # PC + LOGISTIC / RF / SVM /  NN
    all <- data.frame(PC_VS_Result$all)
    Algo_PC_LOGISTIC <- logistic(
      x = all[, -1],
      y = all[ 1],
      cutoff = cutoff,
      fam = binomial,
      cutoff.coefficient = 1)
    Algo_PC_RF <- Random.Forest(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff, 
      num.tree = 10*folds.i,
      num.try = sqrt(ncol(all[,-1])),
      cutoff.coefficient = 1,
      SV.cutoff = 1:10)
    Algo_PC_SVM <- Support_Vector_Machine(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      c = 1.5*folds.i,
      g = 1.3*folds.i)
    Algo_PC_NN <- YinsKerasNN::YinsKerasNN(
      x = all[, -1], 
      y = all[, 1], 
      cutoff = cutoff, 
      epochs = 10*folds.i)
    
    # KMEANS + LOGISTIC / RF / SVM / NN
    all <- data.frame(KMEANS_VS_Result$all)
    Algo_KMEANS_LOGISTIC <- logistic(
      x = all[, -1],
      y = all[ 1],
      cutoff = cutoff,
      fam = binomial,
      cutoff.coefficient = 1)
    Algo_KMEANS_RF <- Random.Forest(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff, 
      num.tree = 10*folds.i,
      num.try = sqrt(ncol(all)),
      cutoff.coefficient = 1,
      SV.cutoff = 1:10)
    Algo_KMEANS_SVM <- Support_Vector_Machine(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      c = 1.5*folds.i,
      g = 1.3*folds.i)
    Algo_KMEANS_NN <- YinsKerasNN::YinsKerasNN(
      x = all[, -1], 
      y = all[, 1], 
      cutoff = cutoff, 
      epochs = 10*folds.i)
    
    # Print result
    result <- c(
      result, 
      paste("Fold", folds.i),
      c(round(Algo_NONE_LOGISTIC$AUC,3),
        round(Algo_NONE_RF$AUC,3),
        round(Algo_NONE_SVM$AUC,3),
        round(Algo_NONE_NN$Testing.Accuracy,3)),
      c(round(Algo_PC_LOGISTIC$AUC,3),
        round(Algo_PC_RF$AUC,3),
        round(Algo_PC_SVM$AUC,3),
        round(Algo_PC_NN$Testing.Accuracy,3)),
      c(round(Algo_KMEANS_LOGISTIC$AUC,3),
        round(Algo_KMEANS_RF$AUC,3),
        round(Algo_KMEANS_SVM$AUC,3),
        round(Algo_KMEANS_NN$Testing.Accuracy,3))
    )
    print(paste("Done with fold", folds.i))
  } # End of CV
  
  # Result
  print(paste0("Cross Validation Result: "))
  common_procedure <- c("Logistic", "RF", "SVM", "NN")
  Result <- data.frame(t(matrix(result,13)))
  colnames(Result) <- c(
    "kth_Fold",
    common_procedure,
    paste0("PC+",common_procedure),
    paste0("KMEANS+",common_procedure)
  ); DT::datatable(Result)
  
  ######################## BEST PARAMETERS #############################
  
  # Record best parameter from CV results above:
  best_param <- NULL
  for (j in 2:ncol(Result)) { best_param <- c(best_param, which.max(Result[,j])) }
  
  # Comment:
  # By recording the best parameters from CV results above,
  # we can directly apply this parameter to each algorithm
  # because we believe in the fairness and robustness from 
  # the results of cross validation. 
  
  ######################### HELD OUT TEST SET #############################
  
  # CV:
  # Write a k-fold CV loop:
  print(paste0("With the above CV results, we can test performance out held-out test test."))
  for (folds.i in how.many.folds){
    # Create k-fold training data sets for CV:
    
    # Create:
    # folds: a list of numbers with different index;
    # testIndexes: the index that equals to each index in folds;
    
    # For classification
    # Then we can create test and train data sets:
    folds.A <- cut(seq(1,nrow(all.A)),breaks=how.many.folds,labels=FALSE)
    folds.B <- cut(seq(1,nrow(all.B)),breaks=how.many.folds,labels=FALSE)
    #folds <- cut(seq(1,nrow(all)),breaks=how.many.folds,labels=FALSE)
    
    # For regression
    
    # For classifiction: 
    # Set:
    #folds.i <- 1
    testIndexes.A <- which(folds.A==folds.i, arr.ind = TRUE)
    testIndexes.B <- which(folds.B==folds.i, arr.ind = TRUE)
    testData.A <- all.A[testIndexes.A, ]; trainData.A <- all.A[-testIndexes.A, ]
    testData.B <- all.B[testIndexes.B, ]; trainData.B <- all.B[-testIndexes.B, ]
    all <- data.frame(rbind(
      trainData.A, trainData.B,
      testData.A, testData.B 
    ))
    
    # MODEL FITTING / MACHINE LEARNING:
    # One can change to use Regression or Classification:
    cutoff <- round((nrow(trainData.A)+nrow(trainData.B))/nrow(all), 1) # Check this ratio out
    # (I) NONPARAMETRIC
    # PC
    PC_VS_Result <- PC.VS(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      select = 1:(2*folds.i)
    )
    
    # KMEANS
    KMEANS_VS_Result <- KMEANS.VS(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      k = 1*folds.i
    )
    
    # (II) MACHINE LEARNING
    # NONE + LOGISTIC / RF / SVM / NN
    Algo_NONE_LOGISTIC <- logistic(
      x = all[, -1],
      y = all[ 1],
      cutoff = cutoff,
      fam = binomial,
      cutoff.coefficient = 1)
    Algo_NONE_RF <- Random.Forest(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      num.tree = 10*best_param[2],
      num.try = sqrt(ncol(all))*best_param[2],
      cutoff.coefficient = 1,
      SV.cutoff = 1:10)
    Algo_NONE_SVM <- Support_Vector_Machine(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      c = 1.5*best_param[3],
      g = 1.3*best_param[3])
    Algo_NONE_NN <- YinsKerasNN::YinsKerasNN(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      epochs = 10*best_param[4])
    
    # PC + LOGISTIC / RF / SVM /  NN
    all <- data.frame(PC_VS_Result$all)
    Algo_PC_LOGISTIC <- logistic(
      x = all[, -1],
      y = all[ 1],
      cutoff = cutoff,
      fam = binomial,
      cutoff.coefficient = 1)
    Algo_PC_RF <- Random.Forest(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff, 
      num.tree = 10*best_param[6],
      num.try = sqrt(ncol(all))*best_param[6],
      cutoff.coefficient = 1,
      SV.cutoff = 1:10)
    Algo_PC_SVM <- Support_Vector_Machine(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      c = 1.5*best_param[7],
      g = 1.3*best_param[7])
    Algo_PC_NN <- YinsKerasNN::YinsKerasNN(
      x = all[, -1], 
      y = all[, 1], 
      cutoff = cutoff, 
      epochs = 10*best_param[8])
    
    # KMEANS + LOGISTIC / RF / SVM / NN
    all <- data.frame(KMEANS_VS_Result$all)
    Algo_KMEANS_LOGISTIC <- logistic(
      x = all[, -1],
      y = all[ 1],
      cutoff = cutoff,
      fam = binomial,
      cutoff.coefficient = 1)
    Algo_KMEANS_RF <- Random.Forest(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff, 
      num.tree = 10*best_param[9],
      num.try = sqrt(ncol(all))*best_param[9],
      cutoff.coefficient = 1,
      SV.cutoff = 1:10)
    Algo_KMEANS_SVM <- Support_Vector_Machine(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      c = 1.5*best_param[10],
      g = 1.3*best_param[11])
    Algo_KMEANS_NN <- YinsKerasNN::YinsKerasNN(
      x = all[, -1], 
      y = all[, 1], 
      cutoff = cutoff, 
      epochs = 10*best_param[12])
    
    # Print result
    result <- c(
      result, 
      paste("Fold", folds.i),
      c(round(Algo_NONE_LOGISTIC$AUC,3),
        round(Algo_NONE_RF$AUC,3),
        round(Algo_NONE_SVM$AUC,3),
        round(Algo_NONE_NN$Testing.Accuracy,3)),
      c(round(Algo_PC_LOGISTIC$AUC,3),
        round(Algo_PC_RF$AUC,3),
        round(Algo_PC_SVM$AUC,3),
        round(Algo_PC_NN$Testing.Accuracy,3)),
      c(round(Algo_KMEANS_LOGISTIC$AUC,3),
        round(Algo_KMEANS_RF$AUC,3),
        round(Algo_KMEANS_SVM$AUC,3),
        round(Algo_KMEANS_NN$Testing.Accuracy,3))
    )
    print(paste("Done with fold", folds.i))
  } # End of Test
  
  # Result
  print(paste0("Fold ", how.many.folds, " as Test Set Result: "))
  common_procedure <- c("Logistic", "RF", "SVM", "NN")
  Test.Result <- data.frame(t(matrix(result,13)))
  colnames(Test.Result) <- c(
    "kth_Fold",
    common_procedure,
    paste0("PC+",common_procedure),
    paste0("KMEANS+",common_procedure)
  ); #DT::datatable(Test.Result)
  
  ######################### END OF SCRIPT #################################
  # Output
  return(Test.Result)
  ######################### END OF SCRIPT #################################
} # End of function

######################## REAL DATA REGRESSION PROBLEM ###########################

# Define function:
real_data_application_regression <- function(
  seed = 1,
  data = data,
  how.many.folds = 5
) {
  
  # Shuffle
  set.seed(seed)
  data <- data[sample(nrow(data), nrow(data)), ]
  
  ###################### K-FOLD CV: REG & CLASS ########################
  
  # READ ME:
  # This script loops through k folds.
  # Each fold the algorithm fits a selected machine learning technique.
  # The algorithm outputs k-fold accuracy (or other selected results). 
  
  # Null Result:
  result <- NULL
  
  # CV:
  # Write a k-fold CV loop:
  how.many.folds = 5
  print(paste0("This is ", how.many.folds-1, "-fold CV."))
  print(paste0("We leave ", how.many.folds, "th fold untouched as held-out test."))
  print(paste0("Here let us do fold 1 up to fold ", how.many.folds-1, " as CV procedure."))
  for (folds.i in 1:how.many.folds){
    # Create k-fold training data sets for CV:
    
    # Create:
    # folds: a list of numbers with different index;
    # testIndexes: the index that equals to each index in folds;
    
    # For regression
    # Then we can create test and train data sets:
    folds <- cut(seq(1,nrow(data)),breaks=how.many.folds,labels=FALSE)
    
    # For regression
    # Set:
    #folds.i <- 1
    testIndexes <- which(folds==folds.i, arr.ind = TRUE)
    all <- data.frame(rbind(
      data[-testIndexes, ],
      data[testIndexes, ]
    ))
    
    # MODEL FITTING / MACHINE LEARNING:
    # One can change to use Regression or Classification:
    cutoff <- round((nrow(data[-testIndexes, ])/nrow(all)), 1)
    # (I) NONPARAMETRIC
    # PC
    PC_VS_Result <- PC.VS(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      select = 1:(4+folds.i)
    )
    
    # KMEANS
    KMEANS_VS_Result <- KMEANS.VS(
      x = all[, -1],
      y = all[, 1],
      cutoff = cutoff,
      k = 4
    )
    
    # (II) MACHINE LEARNING
    # NONE + REGRESSION/GBM/GD/LR/SVM
    Algo_NONE_REGRESSION <- YinsLibrary::Linear_Regression_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff
    )
    Algo_NONE_GBM <- YinsLibrary::Gradient_Boosting_Machine_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff,
      interaction.depth = folds.i+1
    )
    Algo_NONE_GD <- YinsLibrary::Gradient_Descent_Predictor(
      x = all[, -1], 
      y = all[, 1],
      alpha = 1e-8,
      cutoff = cutoff
    )
    Algo_NONE_RL <- YinsLibrary::Lasso_Ridge_Regression_Predictor(
      x = all[, -1], 
      y = all[, 1],
      alpha = c(0,0.1,0.5,0.75,1)[folds.i],
      cutoff = cutoff
    )
    Algo_NONE_SVM <- YinsLibrary::Support_Vector_Machine_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff
    )
    
    # PC + REGRESSION/GBM/GD/LR/SVM
    all <- data.frame(PC_VS_Result$all)
    Algo_PC_REGRESSION <- YinsLibrary::Linear_Regression_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff
    )
    Algo_PC_GBM <- YinsLibrary::Gradient_Boosting_Machine_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff,
      interaction.depth = folds.i+1
    )
    Algo_PC_GD <- YinsLibrary::Gradient_Descent_Predictor(
      x = all[, -1], 
      y = all[, 1],
      alpha = 1e-8,
      cutoff = cutoff
    )
    Algo_PC_RL <- YinsLibrary::Lasso_Ridge_Regression_Predictor(
      x = all[, -1], 
      y = all[, 1],
      alpha = c(0,0.1,0.5,0.75,1)[folds.i],
      cutoff = cutoff
    )
    Algo_PC_SVM <- YinsLibrary::Support_Vector_Machine_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff
    )
    
    # KMEANS + REGRESSION/GBM/GD/LR/SVM
    all <- data.frame(KMEANS_VS_Result$all)
    Algo_KMEANS_REGRESSION <- YinsLibrary::Linear_Regression_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff
    )
    Algo_KMEANS_GBM <- YinsLibrary::Gradient_Boosting_Machine_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff,
      interaction.depth = folds.i+1
    )
    Algo_KMEANS_GD <- YinsLibrary::Gradient_Descent_Predictor(
      x = all[, -1], 
      y = all[, 1],
      alpha = 1e-8,
      cutoff = cutoff
    )
    Algo_KMEANS_RL <- YinsLibrary::Lasso_Ridge_Regression_Predictor(
      x = all[, -1], 
      y = all[, 1],
      alpha = c(0,0.1,0.5,0.75,1)[folds.i],
      cutoff = cutoff
    )
    Algo_KMEANS_SVM <- YinsLibrary::Support_Vector_Machine_Predictor(
      x = all[, -1], 
      y = all[, 1],
      cutoff = cutoff
    )  
    
    # Print result
    result <- c(
      result, 
      paste("Fold", folds.i),
      c(round(sqrt(Algo_NONE_REGRESSION$Test.MSE),3),
        round(sqrt(Algo_NONE_GBM$Test.MSE),3),
        round(sqrt(Algo_NONE_GD$Test.MSE),3),
        round(sqrt(Algo_NONE_RL$Test.MSE),3),
        round(sqrt(Algo_NONE_SVM$Test.MSE),3)),
      c(round(sqrt(Algo_PC_REGRESSION$Test.MSE),3),
        round(sqrt(Algo_PC_GBM$Test.MSE),3),
        round(sqrt(Algo_PC_GD$Test.MSE),3),
        round(sqrt(Algo_PC_RL$Test.MSE),3),
        round(sqrt(Algo_NONE_SVM$Test.MSE),3)),
      c(round(sqrt(Algo_KMEANS_REGRESSION$Test.MSE),3),
        round(sqrt(Algo_KMEANS_GBM$Test.MSE),3),
        round(sqrt(Algo_KMEANS_GD$Test.MSE),3),
        round(sqrt(Algo_KMEANS_RL$Test.MSE),3),
        round(sqrt(Algo_NONE_SVM$Test.MSE),3))
    )
    print(paste("Done with fold", folds.i))
  } # End of CV
  
  # Result
  print(paste0("Cross Validation Result: "))
  common_procedure <- c("Regression", "GBM", "GD", "RL", "SVM")
  Result <- data.frame(t(matrix(result,16)))
  colnames(Result) <- c(
    "kth_Fold",
    common_procedure,
    paste0("PC+",common_procedure),
    paste0("KMEANS+",common_procedure)
  ); DT::datatable(Result)
  
  # Stats
  FinalResult <- matrix(as.numeric(as.character(unlist(data.frame(Result[, -1])))), how.many.folds)
  colnames(FinalResult) <- c(
    common_procedure,
    paste0("PC+",common_procedure),
    paste0("KMEANS+",common_procedure))
  FinalResult <- rbind(FinalResult,apply(FinalResult,2,mean),apply(FinalResult,2,sd))
  rownames(FinalResult) <- c(paste0("Fold ", 1:(how.many.folds)), "Average CV MSE", "SD of CV MSE")
  t(FinalResult)
  return(t(FinalResult))
} # End of Experiment

###################### END SCRIPT ###########################