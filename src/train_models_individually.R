
library(Boruta)
library(DMwR2)
library(EnvStats)
library(FNN)
library(GGally)
library(caret)
library(e1071)
library(OneR)
library(ROSE)
library(ggplot2)
library(rpart)
library(pROC)
library(yardstick)


# # Step 2: Read the dataset and convert to data frame
# dt1 <- read.csv("train.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE) #separador pode ser , ou ; ou \t
# #caso tenha ids acrescentar ( rownames= "coluna com ids") acho que é importante para juntar colunas [penso que isto não é necessário]
# dt1 <- as.data.frame(dt1)
# 
# dts1 <- read.csv("test.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
# dts1 <- as.data.frame(dts1)


################# Load dataset ################# 
if (!exists("data")) {
  train_path <- if (file.exists("data/training_data.csv")) "data/training_data.csv" else "../data/training_data.csv"
  test_path <- if (file.exists("data/test_data.csv")) "data/test_data.csv" else "../data/test_data.csv"
  if (file.exists(train_path)) {
    data <- read.csv(train_path, stringsAsFactors = FALSE)
    new_data <- read.csv(test_path, stringsAsFactors = FALSE)
  }
}

####################################### Classify #####################################   
response_var <- colnames(data)[ncol(data)]   
summary(data)   
str(data)   
sapply(data,class)   


cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))] 
#cat_vars <- names(data)[sapply(data, function(x) is.factor(x))] 
#cat_vars <- cbind("type")
#data[,cat_vars] = as.factor(data[,cat_vars])

num_vars <- setdiff(names(data), c(cat_vars, response_var))   
num_vars <- num_vars[-1] 


#################### Custom Normalization Functions###################   
norm <- function(x) {(x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))}   
norm_known_min_max <- function(x, min_train, max_train) {(x - min_train) / (max_train - min_train)}   

#################### Setup for Cross-Validation ###################   
num_folds <- 10   
set.seed(123)   
folds <- createFolds(data[[response_var]], k = num_folds)   

#################### Metrics storage ###################   

metrics_all <- data.frame()   
roc_curves <- list()   

  ## 1. Logistic Regression   
  
  for (fold in 1:num_folds) {   
    cat("Processing Fold", fold, "\n")   
    
    trainset <- data[-folds[[fold]],-1 ]   
    testset  <- data[folds[[fold]], -1]   
    
    #Imbalanced Data 
    # Table of class counts  
    
    table_y <- table(trainset[[response_var]])  
    print(table_y)  
    
    # Proportions  
    prop_y <- prop.table(table_y)  
    print(round(prop_y * 100, 2))  
    
    # Set imbalance threshold  
    imbalance_threshold <- 0.3  
    
    #Set imbalanced variable  
    is_imbalanced <- any(prop_y < imbalance_threshold)  
    
  
    if (is_imbalanced) {  
      trainset <-  ROSE::ovun.sample(as.formula(paste(response_var, "~ .")), data = trainset, method = "under")$data 
      cat("Undersampling was applied to balance the training set.\n")  
    } else {  
      cat("No undersampling needed. Original training set is used.\n")  
    }  
    
    log_model <- glm(as.formula(paste(response_var, "~ .")), data = trainset, family = binomial(),control = glm.control(maxit = 100))   
    log_probs <- predict(log_model, newdata = testset, type = "response")   
    log_preds <- factor(ifelse(log_probs > prop_y[2], "1", "0"), levels = c("0", "1"))   
    
    
    #Metrics 
    
    y_true <- testset[[response_var]] 
    pred <- factor(log_preds, levels = levels(y_true)) 
    prob <- log_probs 
    acc <- accuracy_vec(y_true, pred) 
    f1 <- f_meas_vec(y_true, pred, event_level = "first") 
    brier <- mean((as.numeric(as.character(y_true)) - prob)^2) 
    auc_val <- auc(roc(response = y_true, predictor = prob)) 
    metrics_all <- rbind(metrics_all, data.frame(Fold = fold, Model = "Logistic", Accuracy = acc, F1 = f1, Brier = brier, AUC = auc_val)) 
    roc_curve <- roc(response = y_true, predictor = prob) 
  } 
  
  model_avg <- aggregate(cbind(Accuracy, F1, Brier, AUC) ~ Model, data = metrics_all, mean) 
  print(model_avg[order(-model_avg$F1), ]) 
  
  
  
  ## 2. Decision Tree (optimized with pruning, split criteria, and minbucket)   
  
    performance_summary <- data.frame()
    criterion <- c('information','gini')
    min_num_objects <- 20
    
    for (l in criterion){
      for (i in c(0.01,0)){
        for (j in 2:min_num_objects){
          
          accuracy_folds <- c()
          precision_folds <- c()
          recall_folds <- c()
          f1_folds <- c()
          auc_folds <- c()
          
          for (fold in 1:num_folds){
            cat("Processing Fold", fold, "\n")   
            
            trainset <- data[-folds[[fold]],-1 ]   
            testset  <- data[folds[[fold]], -1]   
            
            #Imbalanced Data 
            # Table of class counts  
            table_y <- table(trainset[[response_var]])  
            print(table_y)  
            
            # Proportions  
            prop_y <- prop.table(table_y)  
            print(round(prop_y * 100, 2))  
            
            # Set imbalance threshold  
            imbalance_threshold <- 0.3  
            
            #Set imbalanced variable  
            is_imbalanced <- any(prop_y < imbalance_threshold)  
            
            if (is_imbalanced) {  
              trainset <-  ROSE::ovun.sample(as.formula(paste(response_var, "~ .")), data = trainset, method = "under")$data 
              cat("Undersampling was applied to balance the training set.\n")  
            } else {  
              cat("No undersampling needed. Original training set is used.\n")  
            }  
            
            Model_tree <- rpart(as.formula(paste(response_var, "~ .")),   
                                
                                data = trainset,   
                                
                                method = "class",   
                                
                                parms = list(split = l), minbucket = j, cp=i)   
            
            
            
            prediction_probs <- predict(Model_tree, newdata = testset)[, "1"]   
            
            predicted_class <- factor(ifelse(prediction_probs > 0.5, "1", "0"), levels = c("0", "1"))  
            
            
            accuracy_folds[fold] <- accuracy_vec(testset[[response_var]], predicted_class)
            precision_folds[fold] <- precision_vec(testset[[response_var]], predicted_class, event_level="first")
            recall_folds[fold] <- recall_vec(testset[[response_var]], predicted_class, event_level="first")
            f1_folds[fold] <- f_meas_vec(testset[[response_var]], predicted_class, event_level="first")
            auc_folds[fold] <- auc(testset[[response_var]], prediction_probs,event_level="first")
          }
          
          performance_summary <- rbind(performance_summary,
                                       data.frame(criterion=l, cp=i, min_objects=j,
                                                  accuracy=mean(accuracy_folds),
                                                  precision=mean(precision_folds),
                                                  recall=mean(recall_folds),
                                                  f1=mean(f1_folds),
                                                  auc=mean(auc_folds)))
        }
      }
    }
    mean(performance_summary$accuracy)
    mean(performance_summary$f1)
    best_index <- which.max(performance_summary$f1)
    l <- performance_summary[best_index,"criterion"]
    i <- performance_summary[best_index,"cp"]
    j <- performance_summary[best_index,"min_objects"]
    
    
    
    Final_model_tree <- rpart(target~.-1, data=data, parms = list(split = l), minbucket = as.numeric(j), cp=i)
    prediction_probs <- predict(Final_model_tree, newdata=new_data)[,'1']
    predicted_class <- factor(ifelse(prediction_probs > 0.5, 1, 0), levels=c(1,0))
    
    submission <- data.frame(candidate_id = new_data[,1], target = predicted_class)
    setwd("C:/Users/exame/Downloads/Hackathon_GroupO/Hackathon_GroupO/Dados Originais")
    write.csv(submission, "sample_submission.csv", row.names = FALSE)

  ## 3. Naive Bayes   
  for (fold in 1:num_folds) {   
    
    cat("Processing Fold", fold, "\n")   
    
    trainset <- data[-folds[[fold]],-1 ]   
    testset  <- data[folds[[fold]], -1]   
    
    #Imbalanced Data 
    # Table of class counts  
    table_y <- table(trainset[[response_var]])  
    print(table_y)  
    
    # Proportions  
    prop_y <- prop.table(table_y)  
    print(round(prop_y * 100, 2))  
    
    # Set imbalance threshold  
    imbalance_threshold <- 0.3  
    
    #Set imbalanced variable  
    is_imbalanced <- any(prop_y < imbalance_threshold)  
    
    if (is_imbalanced) {  
      trainset <-  ROSE::ovun.sample(as.formula(paste(response_var, "~ .")), data = trainset, method = "under")$data 
      cat("Undersampling was applied to balance the training set.\n")  
    } else {  
      cat("No undersampling needed. Original training set is used.\n")  
    }  
    
    nb_model <- naiveBayes(as.formula(paste(response_var, "~ .")), data = trainset)   
    nb_probs <- predict(nb_model, newdata = testset, type = "raw")[, "1"]   
    nb_preds <- factor(ifelse(nb_probs > 0.5, "1", "0"), levels = c("0", "1"))   
    
    #Metrics 
    y_true <- testset[[response_var]] 
    pred <- factor(nb_preds, levels = levels(y_true)) 
    prob <- nb_probs 
    acc <- accuracy_vec(y_true, pred) 
    f1 <- f_meas_vec(y_true, pred, event_level = "first") 
    brier <- mean((as.numeric(as.character(y_true)) - prob)^2) 
    auc_val <- auc(roc(response = y_true, predictor = prob)) 
    metrics_all <- rbind(metrics_all, data.frame(Fold = fold, Model = "Naive Bayes", Accuracy = acc, F1 = f1, Brier = brier, AUC = auc_val)) 
    roc_curve <- roc(response = y_true, predictor = prob) 
  } 
  
  model_avg <- aggregate(cbind(Accuracy, F1, Brier, AUC) ~ Model, data = metrics_all, mean) 
  print(model_avg[order(-model_avg$F1), ]) 
  
  
  ## 4. KNN (tuned k using odd numbers only)   
  for (fold in 1:num_folds) {   
    cat("Processing Fold", fold, "\n")   
    trainset <- data[-folds[[fold]],-1]   
    testset  <- data[folds[[fold]],-1]   
    
    #Imbalanced Data 
    # Table of class counts  
    table_y <- table(trainset[[response_var]])  
    print(table_y)  
    
    # Proportions  
    prop_y <- prop.table(table_y)  
    print(round(prop_y * 100, 2))  
    
    # Set imbalance threshold  
    imbalance_threshold <- 0.3  
    
    #Set imbalanced variable  
    is_imbalanced <- any(prop_y < imbalance_threshold)  
    
    if (is_imbalanced) {  
      trainset <-  ROSE::ovun.sample(as.formula(paste(response_var, "~ .")), data = trainset, method = "under")$data 
      cat("Undersampling was applied to balance the training set.\n")  
    } else {  
      cat("No undersampling needed. Original training set is used.\n")  
    }  
    
    # Normalize for KNN   
    knn_trainset <- trainset[,num_vars]   
    knn_testset <- testset[, num_vars]   
    
    mins <- apply(knn_trainset, 2, min)   
    maxs <- apply(knn_trainset, 2, max)   
    
    train_norm <- as.data.frame(lapply(knn_trainset, norm))   
    test_norm <- as.data.frame(mapply(norm_known_min_max, knn_testset, mins, maxs))   
    
    colnames(test_norm) <- colnames(train_norm) 

    best_k <- 1   
    best_f1 <- 0   
    
    if (!exists("knn_param_tracking_all")) {   
      knn_param_tracking_all <- data.frame(Fold = integer(), K = integer(), F1 = numeric())   
      best_tree_params_per_fold <- list()   
    }   
    
    for (k_try in seq(1, 40, by = 2)) {   
      knn_model <- knn(train = train_norm, test = test_norm, cl = trainset[[response_var]], k = k_try, prob = TRUE)   
      probs_try <- ifelse(knn_model == "1", attr(knn_model, 'prob'), 1 - attr(knn_model, 'prob'))   
      preds_try <- factor(ifelse(probs_try > 0.5, "1", "0"), levels = c("0", "1"))   
      preds_try <- factor(preds_try, levels = levels(trainset[[response_var]]))   
      f1_try <- f_meas_vec(testset[[response_var]], preds_try, event_level = "first")   
      
      if (!is.na(f1_try) && f1_try > best_f1){   
        best_f1 <- f1_try   
        best_k <- k_try   
      }   
    }   
    
    # Salvar resultados do melhor k nesse fold   
    knn_param_tracking_all <- rbind(knn_param_tracking_all,data.frame(Fold = fold, K = best_k, F1 = best_f1))   
    
    knn_model <- knn(train = train_norm, test = test_norm, cl = trainset[[response_var]], k = best_k, prob = TRUE)   
    knn_probs <- ifelse(knn_model == "1", attr(knn_model, 'prob'), 1 - attr(knn_model, 'prob'))   
    knn_preds <- factor(ifelse(knn_probs > 0.5, "1", "0"), levels = c("0", "1"))   
    knn_preds <- factor(knn_preds, levels = levels(trainset[[response_var]]))   
    
    #Metrics 
    
    y_true <- testset[[response_var]] 
    pred <- factor(knn_preds, levels = levels(y_true)) 
    prob <- knn_probs 
    
    acc <- accuracy_vec(y_true, pred) 
    f1 <- f_meas_vec(y_true, pred, event_level = "first") 
    brier <- mean((as.numeric(as.character(y_true)) - prob)^2) 
    auc_val <- auc(roc(response = y_true, predictor = prob)) 
    metrics_all <- rbind(metrics_all, data.frame(Fold = fold, Model = "KNN", Accuracy = acc, F1 = f1, Brier = brier, AUC = auc_val)) 
    roc_curve <- roc(response = y_true, predictor = prob) 
  } 
  
  #KNN Best Parameter   
  knn_best_overall_index <- which.max(knn_param_tracking_all$F1)   
  knn_best_overall_params <- knn_param_tracking_all[knn_best_overall_index, ]   
  print(knn_best_overall_params)  
  
  model_avg <- aggregate(cbind(Accuracy, F1, Brier, AUC) ~ Model, data = metrics_all, mean) 
  print(model_avg[order(-model_avg$F1), ]) 
  
  #Imbalanced Data 
  # Table of class counts  
  table_y <- table(data[[response_var]])  
  print(table_y)  
  
  # Proportions  
  prop_y <- prop.table(table_y)  
  print(round(prop_y * 100, 2))  
  
  # Set imbalance threshold  
  imbalance_threshold <- 0.3  
  
  #Set imbalanced variable  
  is_imbalanced <- any(prop_y < imbalance_threshold)  
  
  if (is_imbalanced) {  
    data <-  ROSE::ovun.sample(as.formula(paste(response_var, "~ .")), data = data, method = "under")$data 
    cat("Undersampling was applied to balance the training set.\n")  
  } else {  
    cat("No undersampling needed. Original training set is used.\n")  
  }  
  knn_train <- as.matrix(data[, num_vars])
  knn_pred <- as.matrix(new_data[, num_vars])
  
  # Verify numeric conversion
  if (!is.numeric(knn_train) || !is.numeric(knn_pred)) {
    stop("KNN requires numeric data. Some columns could not be converted to numeric.")
  }
  
  # Print dimensions for debugging
  cat("\nKNN training data dimensions:", dim(knn_train), "\n")
  cat("KNN prediction data dimensions:", dim(knn_pred), "\n")
  
  # Define normalization functions (local scope to avoid conflicts)
  normalize_column <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (rng[1] == rng[2]) return(rep(0, length(x)))
    (x - rng[1]) / (rng[2] - rng[1])
  }
  
  normalize_new_data <- function(x, train_min, train_max) {
    if (train_min == train_max) return(rep(0, length(x)))
    (x - train_min) / (train_max - train_min)
  }
  
  # Calculate ranges from training data
  train_mins <- apply(knn_train, 2, min, na.rm = TRUE)
  train_maxs <- apply(knn_train, 2, max, na.rm = TRUE)
  
  # Normalize training data
  train_norm <- matrix(0, nrow = nrow(knn_train), ncol = ncol(knn_train))
  for (i in 1:ncol(knn_train)) {
    train_norm[,i] <- normalize_column(knn_train[,i])
  }
  colnames(train_norm) <- colnames(knn_train)
  
  # Normalize prediction data using training data ranges
  pred_norm <- matrix(0, nrow = nrow(knn_pred), ncol = ncol(knn_pred))
  for (i in 1:ncol(knn_pred)) {
    pred_norm[,i] <- normalize_new_data(knn_pred[,i], train_mins[i], train_maxs[i])
  }
  colnames(pred_norm) <- colnames(knn_pred)
  
  # Run KNN
  preds <- FNN::knn(
    train = train_norm,
    test = pred_norm,
    cl = factor(data[[response_var]], levels = c("0", "1")),
    k = knn_best_overall_params$K
  )
  
  # Ensure predictions are properly formatted
  preds <- factor(preds, levels = c("0", "1"))
 
  submission <- data.frame(candidate_id = new_data[,1], target = preds)
  setwd("C:/Users/exame/Downloads/Hackathon_GroupO/Hackathon_GroupO")
 # write.csv(submission, "sample_submission.csv", row.names = FALSE)
  
  
  
  
  
  ## 5. OneR   
  for (fold in 1:num_folds) {   
    cat("Processing Fold", fold, "\n")   
  
    trainset <- data[-folds[[fold]],-1 ]   
    testset  <- data[folds[[fold]], -1]   
    
    #Imbalanced Data 
    # Table of class counts  
    table_y <- table(trainset[[response_var]])  
    print(table_y)  
    
    # Proportions  
    prop_y <- prop.table(table_y)  
    print(round(prop_y * 100, 2))  
    
    # Set imbalance threshold  
    imbalance_threshold <- 0.3  
    
    #Set imbalanced variable  
    is_imbalanced <- any(prop_y < imbalance_threshold)  
    
    
    if (is_imbalanced) {  
      trainset <-  ROSE::ovun.sample(as.formula(paste(response_var, "~ .")), data = trainset, method = "under")$data 
      cat("Undersampling was applied to balance the training set.\n")  
    } else {  
      cat("No undersampling needed. Original training set is used.\n")  
    }  
    
    oner_model <- OneR(trainset)   
    oner_preds <- predict(oner_model, newdata = testset)   
    oner_preds <- factor(oner_preds, levels = levels(trainset[[response_var]]))   
    oner_probs <- ifelse(oner_preds == "1", 0.75, 0.25)   
    
    
    
    #Metrics 
    y_true <- testset[[response_var]] 
    pred <- factor(oner_preds, levels = levels(y_true)) 
    prob <- oner_probs 
    acc <- accuracy_vec(y_true, pred) 
    f1 <- f_meas_vec(y_true, pred, event_level = "first") 
    brier <- mean((as.numeric(as.character(y_true)) - prob)^2) 
    auc_val <- auc(roc(response = y_true, predictor = prob)) 
    metrics_all <- rbind(metrics_all, data.frame(Fold = fold, Model = "OneR", Accuracy = acc, F1 = f1, Brier = brier, AUC = auc_val)) 
    roc_curve <- roc(response = y_true, predictor = prob) 
  } 
  
  model_avg <- aggregate(cbind(Accuracy, F1, Brier, AUC) ~ Model, data = metrics_all, mean) 
  print(model_avg[order(-model_avg$F1), ]) 
  
  
  oner_model <- OneR(data[,-1])   
  oner_preds <- predict(oner_model, newdata = new_data[,-1])   
  oner_preds <- factor(oner_preds, levels = levels(data[[response_var]]))   
  
  submission <- data.frame(candidate_id = new_data[,1], target = oner_preds)
  setwd("C:/Users/exame/Downloads/Hackathon_GroupO/Hackathon_GroupO")
 # write.csv(submission, "sample_submission.csv", row.names = FALSE)
  
  
  

  
