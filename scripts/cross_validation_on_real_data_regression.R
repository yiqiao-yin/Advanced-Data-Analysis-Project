#################### REAL DATA: CV + HELD OUT TEST SET ################

# Data
data <- read.csv(paste0(path,"data/wagedata.csv"))
head(data); dim(data)

# Visualize
cor(data); pairs(data)

# Add Variables
data$EDUC2 <- data$EDUC^2
data$EDUCROOT <- sqrt(data$EDUC)
data$EDUCEXP <- data$EDUC*data$EXPERIENCE
data$EDUCEXPAGE <- data$EDUCEXP*data$AGE
data$EDUCOCCP <- data$EDUC*data$OCCUPATION

# Shuffle
set.seed(2019)
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

######################## BEST PARAMETERS #############################

# Record best parameter from CV results above:
best_param <- NULL
for (j in 2:ncol(Result)) { best_param <- c(best_param, which.min(Result[,j])) }

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
  
  # For regression
  # Then we can create test and train data sets:
  folds <- cut(seq(1,nrow(data)),breaks=how.many.folds,labels=FALSE)
  
  # For regression
  
  # For classifiction: 
  # Set:
  #folds.i <- 1
  testIndexes <- which(folds==folds.i, arr.ind = TRUE)
  all <- data.frame(rbind(
    data[-testIndexes, ],
    data[testIndexes, ]
  ))
  
  ## MODEL FITTING / MACHINE LEARNING:
  # One can change to use Regression or Classification:
  cutoff <- round((nrow(data[-testIndexes, ])/nrow(all)), 1)
  # (I) NONPARAMETRIC
  # PC
  PC_VS_Result <- PC.VS(
    x = all[, -1],
    y = all[, 1],
    cutoff = cutoff,
    select = 1:(6+folds.i)
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
    interaction.depth = best_param[2]+1
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
    alpha = c(0.1,0.5,0.75,1)[best_param[4]],
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
    interaction.depth = best_param[7]+1
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
    alpha = c(0.1,0.5,0.75,1)[best_param[9]],
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
    interaction.depth = best_param[12]+1
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
    alpha = c(0.1,0.5,0.75,1)[best_param[14]],
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
    paste("Test: Fold", folds.i),
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
rownames(FinalResult) <- c(paste0("Fold ", 1:how.many.folds), "Average CV MSE", "SD of CV MSE")
t(FinalResult)

######################### END OF SCRIPT #################################