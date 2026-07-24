##########################
# MODEL FUNCTIONS FILE #
##########################

#' Train classification models and perform cross-validation
#'
#' @param data The dataset containing the features and response variable
#' @param response_var Name of the response variable column (target)
#' @param num_folds Number of folds for cross-validation
#' @param plot_results Whether to generate boxplots of metrics
#' @return A list containing metrics, best models, and parameters
train_models <- function(data, response_var, num_folds = 10, plot_results = TRUE) {

  # Verify response variable exists
  if(!(response_var %in% colnames(data))) {
    stop(paste("Response variable", response_var, "not found in the dataset"))
  }

  #A) Setup Phase________________________________________________________________________________________________
  # Setup for Cross-Validation
  set.seed(123)
  folds <- caret::createFolds(data[[response_var]], k = num_folds)
  
  # Metrics storage
  metrics_all <- data.frame()
  roc_curves <- list()
  
  # Define Models to Evaluate
  model_names <- c("Logistic", "Decision Tree", "Naive Bayes", "KNN", "OneR", "Caret Bagged", "Custom Bagged", "SVM", "Neural Net")
  

  "
  data frames are used to track and store the performance of different model parameters during the cross-validation process
  "
  # Initialize parameter tracking
  tree_param_tracking_all <- data.frame(
    Fold = integer(), 
    F1 = numeric(), 
    cp = numeric(), 
    minbucket = integer(), 
    split = character(), 
    stringsAsFactors = FALSE
  )
  
  knn_param_tracking_all <- data.frame(
    Fold = integer(), 
    K = integer(), 
    F1 = numeric()
  )
  
  svm_param_tracking_all <- data.frame(
    Fold = integer(),
    Cost = numeric(),
    Gamma = numeric(),
    F1 = numeric()
  )
  
  nnet_param_tracking_all <- data.frame(
    Fold = integer(),
    Size = integer(),
    Decay = numeric(),
    F1 = numeric()
  )
  

  # Define normalization functions
  norm <- function(x) {(x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))}
  norm_known_min_max <- function(x, min_train, max_train) {(x - min_train) / (max_train - min_train)}
  
  # Identify categorical and numeric variables
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x))]
  all_vars <- names(data)
  id_var <- all_vars[1]  # Assuming first column is ID
  num_vars <- setdiff(all_vars, c(cat_vars, response_var, id_var))


  #B) Cross-Validation Phase________________________________________________________________________________________
  
  for (fold in 1:num_folds) {
    cat("\n-------- Processing Fold", fold, "--------\n")
    
    # Print original data structure
    cat("\nOriginal data columns:", paste(names(data), collapse=", "), "\n")
    cat("First column (ID column):", names(data)[1], "\n")
    
    trainset <- data[-folds[[fold]], !names(data) %in% id_var]
    testset  <- data[folds[[fold]], !names(data) %in% id_var]
    
    # Print structure after ID removal
    cat("\nColumns after ID removal:", paste(names(trainset), collapse=", "), "\n")
    cat("\nFirst few rows of training set:\n")
    print(head(trainset))
    
    # Table of class counts
    table_y <- table(trainset[[response_var]])
    print(table_y)
    
    # Proportions
    prop_y <- prop.table(table_y)
    print(round(prop_y * 100, 2))
    
    # Set imbalance threshold
    ## Checks for class imbalance
    imbalance_threshold <- 0.3 
    
    # Set imbalanced variable
    is_imbalanced <- any(prop_y < imbalance_threshold)

    # # If imbalanced, applies undersampling using ROSE : undersampling is used to balance the training set
    # if (is_imbalanced) {
    #   trainset <- ROSE::ovun.sample(as.formula(paste(response_var, "~ .")), data = trainset, method = "under")$data
    #   cat("Undersampling was applied to balance the training set.\n")
    # } else {
    #   cat("No undersampling needed. Original training set is used.\n")
    # }

    
    # Normalize for KNN
    knn_trainset <- trainset[, num_vars]
    knn_testset <- testset[, num_vars]
    
    mins <- apply(knn_trainset, 2, min, na.rm = TRUE)
    maxs <- apply(knn_trainset, 2, max, na.rm = TRUE)
    
    # Normalize the training and test sets
    train_norm <- as.data.frame(lapply(knn_trainset, norm))
    test_norm <- as.data.frame(mapply(norm_known_min_max, knn_testset, mins, maxs))
    
    colnames(test_norm) <- colnames(train_norm)


    #C) Model Training and Evaluation Phase__________________________________________________________________________
    
    ## -------- MODELS ---------- ##
    predictions_list <- list()
    probs_list <- list()
    

    ## 1. Logistic Regression ##################
    log_model <- glm(as.formula(paste(response_var, "~ .")), data = trainset, family = binomial(), control = glm.control(maxit = 100))
    log_probs <- predict(log_model, newdata = testset, type = "response")
    
    # Calculate threshold from class proportions
    prop_threshold <- prop_y["1"]
    cat("\nUsing threshold", round(prop_threshold, 3), "based on class proportions\n")
    
    # Use proportional threshold instead of fixed 0.5
    log_preds <- factor(ifelse(log_probs > prop_threshold, "1", "0"), levels = c("0", "1"))


    
    ## 2. Decision Tree (optimized with pruning, split criteria, and minbucket) ##################
    best_f1_tree <- 0
    best_tree_params <- list(cp = NA, minbucket = NA, split = NA)
    
    train_bal <- trainset
    
    for (cp_val in c(0, 0.01)) {
      for (minb in 2:10) {
        for (split_criterion in c("information", "gini")) {
          tree_model_tmp <- rpart::rpart(as.formula(paste(response_var, "~ .")),
                                  data = train_bal,
                                  method = "class",
                                  parms = list(split = split_criterion),
                                  control = rpart::rpart.control(cp = cp_val, minbucket = minb))
          
          tree_probs_tmp <- predict(tree_model_tmp, newdata = testset)[, "1"]
          tree_preds_tmp <- factor(ifelse(tree_probs_tmp > 0.5, "1", "0"), levels = c("0", "1"))
         
    
            f1_try <- yardstick::f_meas_vec(truth = testset[[response_var]], tree_preds_tmp, event_level = "first")
     
            tree_param_tracking_all <- rbind(tree_param_tracking_all, data.frame(
              Fold = fold, 
              F1 = f1_try, 
              cp = cp_val, 
              minbucket = minb, 
              split = split_criterion
            ))
            
            if (!is.na(f1_try) && f1_try > best_f1_tree){
              best_f1_tree <- f1_try
              best_tree_params <- list(cp = cp_val, minbucket = minb, split = split_criterion)
              best_tree_model <- tree_model_tmp
              best_tree_probs <- tree_probs_tmp
              best_tree_preds <- tree_preds_tmp
            }
        }
      }
    }
    
    tree_model <- best_tree_model
    tree_probs <- best_tree_probs
    tree_preds <- best_tree_preds



    
    
    ## 3. Naive Bayes #########################################################
    # Detect feature types
    feature_types <- sapply(trainset[, !names(trainset) %in% response_var], function(x) {
      if(is.numeric(x)) {
        # Check if it's continuous or discrete
        if(length(unique(x)) > 10) return("continuous")
        else return("discrete")
      } else return("categorical")
    })
    
    #cat("\nFeature types detected:\n")
    #print(feature_types)
    
    # Original Naive Bayes (default)
    nb_model <- e1071::naiveBayes(as.formula(paste(response_var, "~ .")), data = trainset)
    nb_probs <- predict(nb_model, newdata = testset, type = "raw")[, "1"]
    nb_preds <- factor(ifelse(nb_probs > 0.5, "1", "0"), levels = c("0", "1"))
    
    # Gaussian Naive Bayes (for continuous features)
    continuous_features <- names(feature_types)[feature_types == "continuous"]
    if(length(continuous_features) > 0) {
      nb_gaussian <- e1071::naiveBayes(
        as.formula(paste(response_var, "~ .", sep="")), 
        data = trainset,
        subset = continuous_features
      )
      nb_gaussian_probs <- predict(nb_gaussian, newdata = testset, type = "raw")[, "1"]
      nb_gaussian_preds <- factor(ifelse(nb_gaussian_probs > 0.5, "1", "0"), levels = c("0", "1"))
      
      # Track Gaussian NB performance
      gaussian_f1 <- yardstick::f_meas_vec(testset[[response_var]], nb_gaussian_preds, event_level = "first")
      #cat("\nGaussian NB F1 Score:", gaussian_f1, "\n")
    }
    
    # Bernoulli Naive Bayes (with binarized features)
    # Binarize continuous features using median as threshold
    trainset_binary <- trainset
    testset_binary <- testset
    for(feat in continuous_features) {
      threshold <- median(trainset[[feat]], na.rm = TRUE)
      trainset_binary[[feat]] <- as.factor(ifelse(trainset[[feat]] > threshold, 1, 0))
      testset_binary[[feat]] <- as.factor(ifelse(testset[[feat]] > threshold, 1, 0))
    }
    
    nb_bernoulli <- e1071::naiveBayes(
      as.formula(paste(response_var, "~ .")), 
      data = trainset_binary
    )
    nb_bernoulli_probs <- predict(nb_bernoulli, newdata = testset_binary, type = "raw")[, "1"]
    nb_bernoulli_preds <- factor(ifelse(nb_bernoulli_probs > 0.5, "1", "0"), levels = c("0", "1"))
    
    # Track Bernoulli NB performance
    bernoulli_f1 <- yardstick::f_meas_vec(testset[[response_var]], nb_bernoulli_preds, event_level = "first")
    #cat("\nBernoulli NB F1 Score:", bernoulli_f1, "\n")
    
    # Select best performing NB variant
    nb_variants <- list(
      default = list(probs = nb_probs, preds = nb_preds, 
                    f1 = yardstick::f_meas_vec(testset[[response_var]], nb_preds, event_level = "first")),
      gaussian = list(probs = nb_gaussian_probs, preds = nb_gaussian_preds, f1 = gaussian_f1),
      bernoulli = list(probs = nb_bernoulli_probs, preds = nb_bernoulli_preds, f1 = bernoulli_f1)
    )
    
    best_nb <- names(nb_variants)[which.max(sapply(nb_variants, function(x) x$f1))]
    #cat("\nBest performing NB variant:", best_nb, "\n")
    
    # Use the best variant's predictions
    nb_probs <- nb_variants[[best_nb]]$probs
    nb_preds <- nb_variants[[best_nb]]$preds




    
    ## 4. KNN (tuned k using odd numbers only) #########################################################
    best_k <- 1
    best_f1 <- 0
    
    for (k_try in seq(1, 40, by = 2)) {
      knn_model <- FNN::knn(train = train_norm, test = test_norm, cl = trainset[[response_var]], k = k_try, prob = TRUE)
      probs_try <- ifelse(knn_model == "1", attr(knn_model, 'prob'), 1 - attr(knn_model, 'prob'))
      preds_try <- factor(ifelse(probs_try > 0.5, "1", "0"), levels = c("0", "1"))
      preds_try <- factor(preds_try, levels = levels(trainset[[response_var]]))
      f1_try <- yardstick::f_meas_vec(testset[[response_var]], preds_try, event_level = "first")
      
      if (!is.na(f1_try) && f1_try > best_f1){
        best_f1 <- f1_try
        best_k <- k_try
      }
    }
    
    knn_param_tracking_all <- rbind(knn_param_tracking_all, data.frame(Fold = fold, K = best_k, F1 = best_f1))
    
    knn_model <- FNN::knn(train = train_norm, test = test_norm, cl = trainset[[response_var]], k = best_k, prob = TRUE)
    knn_probs <- ifelse(knn_model == "1", attr(knn_model, 'prob'), 1 - attr(knn_model, 'prob'))
    knn_preds <- factor(ifelse(knn_probs > 0.5, "1", "0"), levels = c("0", "1"))
    knn_preds <- factor(knn_preds, levels = levels(trainset[[response_var]]))


    
    ## 5. OneR #########################################################
    oner_model <- OneR::OneR(trainset)
    oner_preds <- predict(oner_model, newdata = testset)
    oner_preds <- factor(oner_preds, levels = levels(trainset[[response_var]]))
    oner_probs <- ifelse(oner_preds == "1", 0.75, 0.25)

    
    ## 6. Caret's Bagged Trees #########################################################
    set.seed(123 + fold)  # ensure reproducibility while varying across folds
    bag_model <- caret::train(
      as.formula(paste(response_var, "~ .")),
      data = trainset,
      method = "treebag",
      trControl = caret::trainControl(method = "cv", number = 5),
      nbagg = 50  # number of trees
    )
    bag_probs <- predict(bag_model, newdata = testset, type = "prob")[,"1"]
    bag_preds <- factor(ifelse(bag_probs > 0.5, "1", "0"), levels = c("0", "1"))



    ## 7. Custom Bootstrap Aggregated Trees #########################################################
    n_trees <- 50
    boot_preds <- matrix(NA, nrow = nrow(testset), ncol = n_trees)
    
    for(i in 1:n_trees) {
      # Bootstrap sample
      boot_idx <- sample(1:nrow(trainset), replace = TRUE)
      boot_data <- trainset[boot_idx,]
      
      # Train tree
      tree <- rpart::rpart(
        as.formula(paste(response_var, "~ .")),
        data = boot_data,
        method = "class",
        control = rpart::rpart.control(cp = 0.01, minbucket = 5)
      )
      
      # Predict
      boot_preds[,i] <- as.numeric(as.character(
        predict(tree, newdata = testset, type = "class")
      ))
    }
    
    # Average predictions
    custom_bag_probs <- rowMeans(boot_preds)
    custom_bag_preds <- factor(ifelse(custom_bag_probs > 0.5, "1", "0"), levels = c("0", "1"))



    ## 8. Support Vector Machine (SVM) #########################################################
    # Grid search for best parameters
    best_f1_svm <- 0
    best_svm_params <- list(cost = NA, gamma = NA)
    
    for(cost in c(0.1, 1, 10)) {
      for(gamma in c(0.1, 1, "auto")) {
        if(gamma == "auto") {
          gamma_val <- 1 / ncol(trainset)
        } else {
          gamma_val <- gamma
        }
        
        svm_model_tmp <- e1071::svm(
          as.formula(paste(response_var, "~ .")),
          data = trainset,
          kernel = "radial",
          cost = cost,
          gamma = gamma_val,
          probability = TRUE
        )
        
        svm_probs_tmp <- attr(predict(svm_model_tmp, newdata = testset, probability = TRUE), "probabilities")[, "1"]
        svm_preds_tmp <- factor(ifelse(svm_probs_tmp > 0.5, "1", "0"), levels = c("0", "1"))
        
        f1_try <- yardstick::f_meas_vec(testset[[response_var]], svm_preds_tmp, event_level = "first")
        
        svm_param_tracking_all <- rbind(svm_param_tracking_all, data.frame(
          Fold = fold,
          Cost = cost,
          Gamma = ifelse(gamma == "auto", -1, gamma),
          F1 = f1_try
        ))
        
        if (!is.na(f1_try) && f1_try > best_f1_svm) {
          best_f1_svm <- f1_try
          best_svm_params <- list(cost = cost, gamma = gamma_val)
          best_svm_model <- svm_model_tmp
          best_svm_probs <- svm_probs_tmp
          best_svm_preds <- svm_preds_tmp
        }
      }
    }
    
    svm_model <- best_svm_model
    svm_probs <- best_svm_probs
    svm_preds <- best_svm_preds




    ## 9. Neural Network #########################################################
    # Grid search for best parameters
    best_f1_nnet <- 0
    best_nnet_params <- list(size = NA, decay = NA)
    
    # Scale numeric features
    num_cols <- sapply(trainset, is.numeric)
    trainset_scaled <- trainset
    testset_scaled <- testset
    
    if(any(num_cols)) {
      # Get numeric column names from training data
      numeric_cols <- names(trainset)[num_cols]
      
      # Verify these columns exist in test data
      missing_cols <- setdiff(numeric_cols, names(testset))
      if(length(missing_cols) > 0) {
        stop(paste("Missing numeric columns in test data:", paste(missing_cols, collapse=", ")))
      }
      
      # Calculate scaling parameters from training data
      means <- colMeans(trainset[, numeric_cols, drop = FALSE])
      sds <- apply(trainset[, numeric_cols, drop = FALSE], 2, sd)
      
      # Scale both training and test data using the same columns
      trainset_scaled[, numeric_cols] <- scale(trainset[, numeric_cols])
      testset_scaled[, numeric_cols] <- scale(testset[, numeric_cols], center = means, scale = sds)
    }
    
    for(size in c(3, 5, 7)) {  # Number of hidden units
      for(decay in c(0, 0.1, 0.01)) {  # Weight decay parameter
        set.seed(123 + fold)  # Ensure reproducibility while varying across folds
        
        nnet_model_tmp <- caret::train(
          as.formula(paste(response_var, "~ .")),
          data = trainset_scaled,
          method = "nnet",
          trControl = caret::trainControl(method = "none"),
          tuneGrid = data.frame(size = size, decay = decay),
          linout = FALSE,
          trace = FALSE,
          maxit = 200
        )
        
        nnet_probs_tmp <- predict(nnet_model_tmp, newdata = testset_scaled, type = "prob")[, "1"]
        nnet_preds_tmp <- factor(ifelse(nnet_probs_tmp > 0.5, "1", "0"), levels = c("0", "1"))
        
        f1_try <- yardstick::f_meas_vec(testset[[response_var]], nnet_preds_tmp, event_level = "first")
        
        nnet_param_tracking_all <- rbind(nnet_param_tracking_all, data.frame(
          Fold = fold,
          Size = size,
          Decay = decay,
          F1 = f1_try
        ))
        
        if (!is.na(f1_try) && f1_try > best_f1_nnet) {
          best_f1_nnet <- f1_try
          best_nnet_params <- list(size = size, decay = decay)
          best_nnet_model <- nnet_model_tmp
          best_nnet_probs <- nnet_probs_tmp
          best_nnet_preds <- nnet_preds_tmp
        }
      }
    }
    
    nnet_model <- best_nnet_model
    nnet_probs <- best_nnet_probs
    nnet_preds <- best_nnet_preds




    # Combine Predictions
    pred_matrix <- data.frame(
      log = log_preds, 
      tree = tree_preds, 
      nb = nb_preds, 
      knn = knn_preds, 
      oner = oner_preds, 
      caret_bag = bag_preds, 
      custom_bag = custom_bag_preds,
      svm = svm_preds,
      nnet = nnet_preds
    )
    
    ## 10. Ensemble: Majority vote #########################################################
    ensemble_preds <- apply(pred_matrix, 1, function(row) { 
      factor(ifelse(mean(as.numeric(as.character(row))) > 0.5, "1", "0"), levels = c("0", "1"))
    })
    ensemble_preds <- factor(ensemble_preds, levels = levels(trainset[[response_var]]))
    ensemble_probs <- rowMeans(cbind(log_probs, tree_probs, nb_probs, knn_probs, oner_probs, 
                                   bag_probs, custom_bag_probs, svm_probs, nnet_probs))


    
    ## Store Metrics
    model_preds <- list(
      log = log_preds, 
      tree = tree_preds, 
      nb = nb_preds, 
      knn = knn_preds, 
      oner = oner_preds, 
      caret_bag = bag_preds, 
      custom_bag = custom_bag_preds,
      svm = svm_preds,
      nnet = nnet_preds,
      ensemble = ensemble_preds
    )
    model_probs <- list(
      log = log_probs, 
      tree = tree_probs, 
      nb = nb_probs, 
      knn = knn_probs, 
      oner = oner_probs, 
      caret_bag = bag_probs, 
      custom_bag = custom_bag_probs,
      svm = svm_probs,
      nnet = nnet_probs,
      ensemble = ensemble_probs
    )
    
    for (model in names(model_preds)) {
      y_true <- testset[[response_var]]
      pred <- factor(model_preds[[model]], levels = levels(y_true))
      prob <- model_probs[[model]]
      acc <- yardstick::accuracy_vec(y_true, pred)
      f1 <- yardstick::f_meas_vec(y_true, pred, event_level = "first")
      brier <- mean((as.numeric(as.character(y_true)) - prob)^2)
      auc_val <- pROC::auc(pROC::roc(response = y_true, predictor = prob))
      metrics_all <- rbind(metrics_all, data.frame(
        Fold = fold, 
        Model = model, 
        Accuracy = acc, 
        F1 = f1, 
        Brier = brier, 
        AUC = auc_val
      ))
      roc_curves[[paste0(model, "_", fold)]] <- pROC::roc(response = y_true, predictor = prob)
    }
  }
  
  # Decision Tree Best Parameters
  best_overall_index <- which.max(tree_param_tracking_all$F1)
  best_tree_params <- tree_param_tracking_all[best_overall_index, ]
  print("Best Decision Tree Parameters:")
  print(best_tree_params)
  
  # KNN Best Parameter
  knn_best_overall_index <- which.max(knn_param_tracking_all$F1)
  knn_best_params <- knn_param_tracking_all[knn_best_overall_index, ]
  print("Best KNN Parameters:")
  print(knn_best_params)
  
  # SVM Best Parameters
  svm_best_overall_index <- which.max(svm_param_tracking_all$F1)
  svm_best_params <- svm_param_tracking_all[svm_best_overall_index, ]
  print("Best SVM Parameters:")
  print(svm_best_params)
  
  # Neural Network Best Parameters
  nnet_best_overall_index <- which.max(nnet_param_tracking_all$F1)
  nnet_best_params <- nnet_param_tracking_all[nnet_best_overall_index, ]
  print("Best Neural Network Parameters:")
  print(nnet_best_params)
  
  # Generate boxplots if requested
  if (plot_results) {
    for (metric in c("Accuracy", "F1", "Brier", "AUC")) {
      p <- ggplot2::ggplot(metrics_all, ggplot2::aes(x = Model, y = .data[[metric]])) +
        ggplot2::geom_boxplot(fill = "skyblue") +
        ggplot2::labs(title = paste("Boxplot of", metric, "per Model")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      print(p)
    }
  }

  
  # Model ranking
  model_avg <- aggregate(cbind(Accuracy, F1, Brier, AUC) ~ Model, data = metrics_all, mean)
  print("Model Performance Rankings (by F1):")
  print(model_avg[order(-model_avg$F1), ])


  # D: Feature Importance _______________________________________________________________________

  # Add feature importance analysis
  cat("\nAnalyzing Feature Importance...\n")
  importance_results <- analyze_feature_importance(data, response_var, plot_results)
  
  # Return useful information for prediction function
  return(list(
    metrics = metrics_all,
    model_avg = model_avg,
    best_model = model_avg$Model[which.max(model_avg$F1)],
    best_tree_params = best_tree_params,
    best_knn_params = knn_best_params,
    best_svm_params = svm_best_params,
    best_nnet_params = nnet_best_params,
    feature_importance = importance_results
  ))
}

#' Calculate and analyze feature importance across models
#'
#' @param data The dataset containing the features and response variable
#' @param response_var Name of the response variable column (target)
#' @param plot_results Whether to generate importance plots
#' @return A list containing importance metrics and plots
analyze_feature_importance <- function(data, response_var, plot_results = TRUE) {
  # Remove ID column and response variable for importance calculation
  feature_cols <- setdiff(names(data), c(names(data)[1], response_var))
  
  # Initialize importance storage
  importance_list <- list()
  
  # 1. Logistic Regression Importance
  log_model <- glm(as.formula(paste(response_var, "~ .")), 
                   data = data[, c(feature_cols, response_var)], 
                   family = binomial())
  
  log_importance <- abs(summary(log_model)$coefficients[,1] * 
                       apply(data[, feature_cols], 2, sd))[-1]
  importance_list$logistic <- log_importance
  
  # 2. Decision Tree Importance
  tree_model <- rpart::rpart(as.formula(paste(response_var, "~ .")),
                            data = data[, c(feature_cols, response_var)],
                            method = "class")
  tree_importance <- tree_model$variable.importance
  importance_list$tree <- tree_importance
  
  # 3. Custom Bagged Trees Importance
  n_trees <- 50
  bagged_importance <- matrix(0, nrow = length(feature_cols), ncol = n_trees)
  rownames(bagged_importance) <- feature_cols
  
  for(i in 1:n_trees) {
    boot_idx <- sample(1:nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, c(feature_cols, response_var)]
    
    tree <- rpart::rpart(as.formula(paste(response_var, "~ .")),
                        data = boot_data,
                        method = "class")
    
    if(length(tree$variable.importance) > 0) {
      bagged_importance[names(tree$variable.importance), i] <- tree$variable.importance
    }
  }
  
  bagged_mean_importance <- rowMeans(bagged_importance)
  importance_list$bagged <- bagged_mean_importance
  
  # 4. KNN Permutation Importance
  knn_importance <- numeric(length(feature_cols))
  names(knn_importance) <- feature_cols
  
  norm_data <- as.data.frame(scale(data[, feature_cols]))
  baseline_pred <- FNN::knn.cv(norm_data, data[[response_var]], k = 5)
  baseline_acc <- mean(baseline_pred == data[[response_var]])
  
  for(feat in feature_cols) {
    perm_data <- norm_data
    perm_data[[feat]] <- sample(perm_data[[feat]])
    perm_pred <- FNN::knn.cv(perm_data, data[[response_var]], k = 5)
    perm_acc <- mean(perm_pred == data[[response_var]])
    knn_importance[feat] <- baseline_acc - perm_acc
  }
  importance_list$knn <- knn_importance
  
  # 5. Naive Bayes Importance
  nb_importance <- numeric(length(feature_cols))
  names(nb_importance) <- feature_cols
  
  for(feat in feature_cols) {
    single_feat_data <- data[, c(feat, response_var)]
    if(is.factor(single_feat_data[[feat]])) {
      single_feat_data[[feat]] <- as.character(single_feat_data[[feat]])
    }
    nb_model <- e1071::naiveBayes(as.formula(paste(response_var, "~", feat)), data = single_feat_data)
    nb_pred <- predict(nb_model, single_feat_data)
    nb_importance[feat] <- mean(nb_pred == data[[response_var]])
  }
  importance_list$nb <- nb_importance
  
  # 6. OneR Importance
  oner_importance <- numeric(length(feature_cols))
  names(oner_importance) <- feature_cols
  
  for(feat in feature_cols) {
    single_feat_data <- data[, c(feat, response_var)]
    if(is.factor(single_feat_data[[feat]])) {
      single_feat_data[[feat]] <- as.character(single_feat_data[[feat]])
    }
    if(is.factor(single_feat_data[[response_var]])) {
      single_feat_data[[response_var]] <- as.character(single_feat_data[[response_var]])
    }
    oner_model <- OneR::OneR(single_feat_data)
    oner_pred <- predict(oner_model, single_feat_data)
    oner_importance[feat] <- mean(oner_pred == single_feat_data[[response_var]])
  }
  importance_list$oner <- oner_importance

  # 7. SVM Permutation Importance
  # Scale features for SVM
  scaled_data <- as.data.frame(scale(data[, feature_cols]))
  scaled_data[[response_var]] <- data[[response_var]]
  
  # Train base SVM model
  svm_model <- e1071::svm(
    as.formula(paste(response_var, "~ .")),
    data = scaled_data,
    kernel = "radial",
    probability = TRUE,
    scale = FALSE  # Already scaled
  )
  
  # Calculate baseline performance using probabilities
  baseline_probs <- attr(predict(svm_model, scaled_data, probability = TRUE), "probabilities")[, "1"]
  svm_importance <- numeric(length(feature_cols))
  names(svm_importance) <- feature_cols
  
  # Calculate importance through permutation
  for(feat in feature_cols) {
    # Create multiple permutations and average the results
    n_permutations <- 5
    importance_values <- numeric(n_permutations)
    
    for(p in 1:n_permutations) {
      perm_data <- scaled_data
      perm_data[[feat]] <- sample(perm_data[[feat]])
      perm_probs <- attr(predict(svm_model, perm_data, probability = TRUE), "probabilities")[, "1"]
      importance_values[p] <- mean(abs(baseline_probs - perm_probs))
    }
    
    svm_importance[feat] <- mean(importance_values)
  }
  
  # Normalize importance scores
  svm_importance <- svm_importance / max(svm_importance)
  importance_list$svm <- svm_importance

  # 8. Neural Network Sensitivity Analysis
  # Scale features for neural network
  scaled_data <- as.data.frame(scale(data[, feature_cols]))
  scaled_data[[response_var]] <- data[[response_var]]
  
  # Train multiple neural networks to get more stable importance estimates
  n_networks <- 5
  nnet_importance_matrix <- matrix(0, nrow = length(feature_cols), ncol = n_networks)
  rownames(nnet_importance_matrix) <- feature_cols
  
  for(n in 1:n_networks) {
    set.seed(123 + n)  # Different seed for each network
    
    nnet_model <- caret::train(
      as.formula(paste(response_var, "~ .")),
      data = scaled_data,
      method = "nnet",
      trControl = caret::trainControl(method = "none"),
      tuneGrid = data.frame(size = 5, decay = 0.1),
      linout = FALSE,
      trace = FALSE,
      maxit = 200
    )
    
    # Calculate feature importance using multiple perturbation levels
    perturbation_levels <- c(0.1, 0.2, 0.5)  # Different perturbation sizes
    
    for(feat in feature_cols) {
      importance_values <- numeric(length(perturbation_levels))
      
      for(i in seq_along(perturbation_levels)) {
        perturbed_data <- scaled_data
        sd_feat <- sd(scaled_data[[feat]])
        perturbation <- sd_feat * perturbation_levels[i]
        
        # Calculate importance using both positive and negative perturbations
        perturbed_data_plus <- perturbed_data
        perturbed_data_minus <- perturbed_data
        perturbed_data_plus[[feat]] <- perturbed_data_plus[[feat]] + perturbation
        perturbed_data_minus[[feat]] <- perturbed_data_minus[[feat]] - perturbation
        
        orig_pred <- predict(nnet_model, scaled_data, type = "prob")[, "1"]
        pert_pred_plus <- predict(nnet_model, perturbed_data_plus, type = "prob")[, "1"]
        pert_pred_minus <- predict(nnet_model, perturbed_data_minus, type = "prob")[, "1"]
        
        # Average of absolute changes for both perturbations
        importance_values[i] <- mean(c(
          abs(pert_pred_plus - orig_pred),
          abs(pert_pred_minus - orig_pred)
        ))
      }
      
      # Store mean importance across perturbation levels
      nnet_importance_matrix[feat, n] <- mean(importance_values)
    }
  }
  
  # Calculate final neural network importance as mean across all networks
  nnet_importance <- rowMeans(nnet_importance_matrix)
  
  # Normalize importance scores
  nnet_importance <- nnet_importance / max(nnet_importance)
  importance_list$nnet <- nnet_importance

  # Ensure all importance scores exist and are numeric
  if(is.null(importance_list$logistic)) importance_list$logistic <- rep(0, length(feature_cols))
  if(is.null(importance_list$tree)) importance_list$tree <- rep(0, length(feature_cols))
  if(is.null(importance_list$nb)) importance_list$nb <- rep(0, length(feature_cols))
  if(is.null(importance_list$knn)) importance_list$knn <- rep(0, length(feature_cols))
  if(is.null(importance_list$oner)) importance_list$oner <- rep(0, length(feature_cols))
  if(is.null(importance_list$bagged)) importance_list$bagged <- rep(0, length(feature_cols))
  if(is.null(importance_list$svm)) importance_list$svm <- rep(0, length(feature_cols))
  if(is.null(importance_list$nnet)) importance_list$nnet <- rep(0, length(feature_cols))

  # Combine all importance scores and standardize
  all_importance <- data.frame(
    Feature = feature_cols,
    Logistic = as.numeric(scale(importance_list$logistic)),
    Tree = as.numeric(scale(if(length(importance_list$tree) < length(feature_cols)) 
                  rep(0, length(feature_cols)) else importance_list$tree)),
    NB = as.numeric(scale(importance_list$nb)),
    KNN = as.numeric(scale(importance_list$knn)),
    OneR = as.numeric(scale(importance_list$oner)),
    Bagged = as.numeric(scale(importance_list$bagged))
  )
  
  # Add SVM and Neural Network importance scores
  if(!is.null(importance_list$svm)) {
    all_importance$SVM <- as.numeric(scale(importance_list$svm))
  } else {
    all_importance$SVM <- rep(0, length(feature_cols))
  }
  
  if(!is.null(importance_list$nnet)) {
    all_importance$NeuralNet <- as.numeric(scale(importance_list$nnet))
  } else {
    all_importance$NeuralNet <- rep(0, length(feature_cols))
  }

  # Calculate ensemble importance as mean of all models
  model_cols <- setdiff(names(all_importance), c("Feature"))
  all_importance$Ensemble <- rowMeans(all_importance[, model_cols], na.rm = TRUE)
  
  # Calculate overall mean importance
  importance_matrix <- as.matrix(all_importance[, -1])  # Exclude Feature column
  all_importance$Mean_Importance <- rowMeans(importance_matrix, na.rm = TRUE)

  if(plot_results) {
    # 1. Overall Feature Importance Plot
    p1 <- ggplot2::ggplot(all_importance, 
                         ggplot2::aes(x = reorder(Feature, Mean_Importance), 
                                    y = Mean_Importance)) +
      ggplot2::geom_bar(stat = "identity", fill = "#2C3E50", alpha = 0.8) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::labs(title = "Overall Feature Importance",
                   subtitle = "Averaged across all models",
                   x = "Feature",
                   y = "Mean Standardized Importance") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Mean_Importance)),
                        hjust = -0.2,
                        size = 3.5)
    
    print(p1)
    
    # 2. Importance by Model Plot
    importance_long <- tidyr::pivot_longer(
      all_importance,
      cols = c("Logistic", "Tree", "NB", "KNN", "OneR", "Bagged", "SVM", "NeuralNet", "Ensemble"),
      names_to = "Model",
      values_to = "Importance"
    )
    
    # Ensure proper model ordering and labeling
    model_levels <- c("Logistic", "Tree", "NB", "KNN", "OneR", "Bagged", "SVM", "NeuralNet", "Ensemble")
    model_labels <- c("Logistic Regression", "Decision Tree", "Naive Bayes", 
                     "KNN", "OneR", "Bagged Trees", "SVM", "Neural Network", "Ensemble")
    
    importance_long$Model <- factor(importance_long$Model,
                                  levels = model_levels,
                                  labels = model_labels)
    
    importance_long$Feature <- factor(importance_long$Feature,
                                    levels = rev(all_importance$Feature[order(all_importance$Mean_Importance)]))
    
    # Create heatmap with improved aesthetics
    p3 <- ggplot2::ggplot(importance_long, 
                         ggplot2::aes(x = Model, 
                                    y = Feature, 
                                    fill = Importance)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(
        low = "#FFF3E0",
        mid = "#FF9800",
        high = "#E65100",
        midpoint = 0,
        limits = c(min(importance_long$Importance), max(importance_long$Importance))
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 11, face = "bold"),
        panel.grid = ggplot2::element_blank(),
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
      ) +
      ggplot2::labs(
        title = "Feature Importance Heatmap",
        subtitle = "Intensity shows relative importance across all models",
        x = "Model Type",
        y = "Feature",
        fill = "Importance"
      )
    
    print(p3)
  }
  
  return(list(
    importance_by_model = importance_list,
    combined_importance = all_importance,
    top_features = feature_cols[order(all_importance$Mean_Importance, 
                                    decreasing = TRUE)]
  ))
}



# E: Predict New Data _______________________________________________________________________
#' Make predictions on new data using the best model from training
#'
#' @param data Training data used to fit the final model
#' @param new_data New data for prediction (without target variable)
#' @param response_var Name of the response variable column from training
#' @param best_model_name Name of the best model (if NULL, will use the one with best F1)
#' @param model_results Results from train_models function (optional)
#' @return A data frame with predictions
predict_new_data <- function(data, new_data, response_var, 
                             best_model_name = NULL, model_results = NULL) {
  
  # Print initial data structure
  cat("\n-------- Prediction Data Structure --------\n")
  cat("Original training data columns:", paste(names(data), collapse=", "), "\n")
  cat("Test data columns:", paste(names(new_data), collapse=", "), "\n")
  
  # Define id_var as the first column name
  id_var <- names(data)[1]
  cat("\nIdentified ID variable:", id_var, "\n")
  
  # Identify categorical and numeric variables
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x))]
  all_vars <- names(data)
  num_vars <- setdiff(all_vars, c(cat_vars, response_var, id_var))
  cat("\nIdentified numeric variables:", paste(num_vars, collapse=", "), "\n")
  
  # Store original row order
  original_order <- seq_len(nrow(new_data))
  
  # Store original IDs
  original_ids <- new_data[, id_var]  # Using id_var instead of hardcoding first column
  cat("\nFirst few IDs from test data:", head(original_ids), "\n")
  
  # If model_results provided, use best model from there
  if (!is.null(model_results) && is.null(best_model_name)) {
    best_model_name <- model_results$best_model
  }
  
  # If neither provided, default to ensemble
  if (is.null(best_model_name)) {
    best_model_name <- "ensemble"
    warning("No best model specified. Using ensemble by default.")
  }
  
  # Verify response variable exists in training data
  if(!(response_var %in% colnames(data))) {
    stop(paste("Response variable", response_var, "not found in the dataset"))
  }
  
  # Extract best parameters if model_results is provided
  if (!is.null(model_results)) {
    best_tree_params <- model_results$best_tree_params
    best_knn_params <- model_results$best_knn_params
    best_svm_params <- model_results$best_svm_params
    best_nnet_params <- model_results$best_nnet_params
  } else {
    # Default values if no model_results provided
    best_tree_params <- list(
      split = "gini",
      cp = 0.01,
      minbucket = 5
    )
    best_knn_params <- list(
      K = 5
    )
    best_svm_params <- list(
      Cost = 1,
      Gamma = 1
    )
    best_nnet_params <- list(
      Size = 5,
      Decay = 0.1
    )
  }
  
  # Create copy of new_data without ID for predictions
  pred_data <- new_data[, !names(new_data) %in% id_var, drop = FALSE]
  
  # Check for imbalanced classes and potentially rebalance
  table_y <- table(data[[response_var]])
  prop_y <- prop.table(table_y)
  is_imbalanced <- any(prop_y < 0.3)
  
  if (is_imbalanced) {
    balanced_data <- ROSE::ovun.sample(
      as.formula(paste(response_var, "~ .")), 
      data = data[, !names(data) %in% id_var], 
      method = "under"
    )$data
    cat("Undersampling was applied to balance the data set.\n")
  } else {
    balanced_data <- data[, !names(data) %in% id_var]
    cat("No undersampling needed. Original data is used.\n")
  }
  
  # Make predictions based on the selected model
  if (best_model_name == "caret_bag") {
    ################## Caret's Bagged Trees
    bag_model <- caret::train(
      as.formula(paste(response_var, "~ .")),
      data = balanced_data,
      method = "treebag",
      trControl = caret::trainControl(method = "cv", number = 5),
      nbagg = 50
    )
    preds <- predict(bag_model, newdata = pred_data)
    preds <- factor(preds, levels = c("0", "1"))

  } else if (best_model_name == "custom_bag") {
    ################## Custom Bootstrap Aggregated Trees
    n_trees <- 50
    boot_preds <- matrix(NA, nrow = nrow(pred_data), ncol = n_trees)
    
    for(i in 1:n_trees) {
      # Bootstrap sample
      boot_idx <- sample(1:nrow(balanced_data), replace = TRUE)
      boot_data <- balanced_data[boot_idx,]
      
      # Train tree
      tree <- rpart::rpart(
        as.formula(paste(response_var, "~ .")),
        data = boot_data,
        method = "class",
        control = rpart::rpart.control(cp = 0.01, minbucket = 5)
      )
      
      # Predict
      boot_preds[,i] <- as.numeric(as.character(
        predict(tree, newdata = pred_data, type = "class")
      ))
    }
    
    # Average predictions
    bag_probs <- rowMeans(boot_preds)
    preds <- factor(ifelse(bag_probs > 0.5, "1", "0"), levels = c("0", "1"))

  } else if (best_model_name == "log") {
    ################## Logistic
    final_model <- glm(
      as.formula(paste(response_var, "~ .")), 
      data = balanced_data, 
      family = binomial()
    )
    preds <- ifelse(predict(final_model, pred_data, type = "response") > 0.5, "1", "0")
    
  } else if (best_model_name == "tree") {
    ################## Decision Tree
    final_model <- rpart::rpart(
      as.formula(paste(response_var, "~ .")),
      data = balanced_data,
      method = "class",
      parms = list(split = best_tree_params$split),
      control = rpart::rpart.control(cp = best_tree_params$cp, minbucket = best_tree_params$minbucket)
    )
    preds <- predict(final_model, pred_data, type = "class")
    preds <- factor(preds, levels = c("0", "1"))
    
  } else if (best_model_name == "nb") {
    ################## Naive Bayes
    final_model <- e1071::naiveBayes(as.formula(paste(response_var, "~ .")), data = balanced_data)
    preds <- as.character(predict(final_model, pred_data, type = "class"))
    
  } else if (best_model_name == "knn") {
    ################## KNN
    # Extract only numeric variables for KNN
    knn_train <- as.matrix(data[, num_vars])
    knn_pred <- as.matrix(pred_data[, num_vars])
    
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
      k = best_knn_params$K
    )
    
    # Ensure predictions are properly formatted
    preds <- factor(preds, levels = c("0", "1"))
    
    # Return predictions directly for KNN
    return(data.frame(
      id = original_ids,
      type = preds,
      stringsAsFactors = FALSE
    ))
    
  } else if (best_model_name == "oner") {
    ################## OneR
    final_model <- OneR::OneR(balanced_data)
    preds <- as.character(predict(final_model, pred_data))
    
  } else if (best_model_name == "svm") {
    ################## SVM
    final_model <- e1071::svm(
      as.formula(paste(response_var, "~ .")),
      data = balanced_data,
      kernel = "radial",
      cost = best_svm_params$Cost,
      gamma = ifelse(best_svm_params$Gamma == -1, 1/ncol(balanced_data), best_svm_params$Gamma),
      probability = TRUE
    )
    svm_probs <- attr(predict(final_model, pred_data, probability = TRUE), "probabilities")[, "1"]
    preds <- factor(ifelse(svm_probs > 0.5, "1", "0"), levels = c("0", "1"))
    
  } else if (best_model_name == "nnet") {
    ################## Neural Network
    # Scale numeric features
    num_cols <- sapply(balanced_data, is.numeric)
    balanced_data_scaled <- balanced_data
    pred_data_scaled <- pred_data
    
    if(any(num_cols)) {
      # Get numeric column names from training data
      numeric_cols <- names(balanced_data)[num_cols]
      
      # Verify these columns exist in prediction data
      missing_cols <- setdiff(numeric_cols, names(pred_data))
      if(length(missing_cols) > 0) {
        stop(paste("Missing numeric columns in prediction data:", paste(missing_cols, collapse=", ")))
      }
      
      # Calculate scaling parameters from training data
      means <- colMeans(balanced_data[, numeric_cols, drop = FALSE])
      sds <- apply(balanced_data[, numeric_cols, drop = FALSE], 2, sd)
      
      # Scale both training and test data using the same columns
      balanced_data_scaled[, numeric_cols] <- scale(balanced_data[, numeric_cols])
      pred_data_scaled[, numeric_cols] <- scale(pred_data[, numeric_cols], center = means, scale = sds)
    }
    
    final_model <- caret::train(
      as.formula(paste(response_var, "~ .")),
      data = balanced_data_scaled,
      method = "nnet",
      trControl = caret::trainControl(method = "none"),
      tuneGrid = data.frame(
        size = best_nnet_params$Size,
        decay = best_nnet_params$Decay
      ),
      linout = FALSE,
      trace = FALSE,
      maxit = 200
    )
    
    nnet_probs <- predict(final_model, pred_data_scaled, type = "prob")[, "1"]
    preds <- factor(ifelse(nnet_probs > 0.5, "1", "0"), levels = c("0", "1"))
    
  } else if (best_model_name == "ensemble") {
    ################## Ensemble
    # Initialize all model predictions
    predictions <- list()
    
    # 1. Logistic Regression
    log_model <- glm(
      as.formula(paste(response_var, "~ .")), 
      data = balanced_data, 
      family = binomial()
    )
    predictions$log <- factor(
      ifelse(predict(log_model, pred_data, type = "response") > 0.5, "1", "0"), 
      levels = c("0", "1")
    )
    
    # 2. Decision Tree
    tree_model <- rpart::rpart(
      as.formula(paste(response_var, "~ .")),
      data = balanced_data,
      method = "class",
      parms = list(split = best_tree_params$split),
      control = rpart::rpart.control(cp = best_tree_params$cp, minbucket = best_tree_params$minbucket)
    )
    predictions$tree <- factor(
      predict(tree_model, pred_data, type = "class"),
      levels = c("0", "1")
    )
    
    # 3. Naive Bayes
    nb_model <- e1071::naiveBayes(
      as.formula(paste(response_var, "~ .")), 
      data = balanced_data
    )
    predictions$nb <- factor(
      predict(nb_model, pred_data, type = "class"),
      levels = c("0", "1")
    )
    
    # 4. KNN (using the same normalization as standalone KNN)
    knn_train <- as.matrix(data[, num_vars])
    knn_pred <- as.matrix(pred_data[, num_vars])
    
    normalize_column <- function(x) {
      rng <- range(x, na.rm = TRUE)
      if (rng[1] == rng[2]) return(rep(0, length(x)))
      (x - rng[1]) / (rng[2] - rng[1])
    }
    
    normalize_new_data <- function(x, train_min, train_max) {
      if (train_min == train_max) return(rep(0, length(x)))
      (x - train_min) / (train_max - train_min)
    }
    
    train_mins <- apply(knn_train, 2, min, na.rm = TRUE)
    train_maxs <- apply(knn_train, 2, max, na.rm = TRUE)
    
    train_norm <- matrix(0, nrow = nrow(knn_train), ncol = ncol(knn_train))
    for (i in 1:ncol(knn_train)) {
      train_norm[,i] <- normalize_column(knn_train[,i])
    }
    colnames(train_norm) <- colnames(knn_train)
    
    pred_norm <- matrix(0, nrow = nrow(knn_pred), ncol = ncol(knn_pred))
    for (i in 1:ncol(knn_pred)) {
      pred_norm[,i] <- normalize_new_data(knn_pred[,i], train_mins[i], train_maxs[i])
    }
    colnames(pred_norm) <- colnames(knn_pred)
    
    predictions$knn <- factor(
      FNN::knn(
        train = train_norm,
        test = pred_norm,
        cl = factor(data[[response_var]], levels = c("0", "1")),
        k = best_knn_params$K
      ),
      levels = c("0", "1")
    )
    
    # 5. OneR
    oner_model <- OneR::OneR(balanced_data)
    predictions$oner <- factor(
      predict(oner_model, pred_data),
      levels = c("0", "1")
    )
    
    # Combine all predictions
    pred_matrix <- do.call(cbind, predictions)
    
    # Calculate ensemble predictions
    preds <- apply(pred_matrix, 1, function(row) {
      factor(ifelse(mean(as.numeric(as.character(row))) > 0.5, "1", "0"), 
             levels = c("0", "1"))
    })
    
  }
  
  # At the end, ensure predictions align with original IDs
  submission <- data.frame(
    id = original_ids,
    type = preds,
    stringsAsFactors = FALSE
  )
  
  # Verify the number of predictions matches the number of input rows
  if (nrow(submission) != nrow(new_data)) {
    stop("Number of predictions does not match number of input rows")
  }
  
  # Verify IDs are preserved in order
  if (!identical(submission$id, original_ids)) {
    stop("Prediction order does not match original ID order")
  }
  
  return(submission)
}



# F: Robustness Test _______________________________________________________________________
#' Test robustness of the best model for overfitting evidence
#'
#' @param data The original dataset used for training
#' @param response_var Name of the response variable column
#' @param best_model_name Name of the best model to analyze
#' @param n_iterations Number of iterations for stability tests (default = 5)
#' @return A list containing robustness metrics and potential overfitting warnings
robustness_test <- function(data, response_var, best_model_name, n_iterations = 5) {
  # Initialize results storage
  robustness_metrics <- list()
  warnings <- character()
  
  cat("Analyzing robustness for model:", best_model_name, "\n")
  
  # 1. Cross-validation Performance Analysis
  cv_results <- train_models(
    data = data,
    response_var = response_var,
    num_folds = 10,
    plot_results = FALSE
  )
  
  # Get metrics only for the best model
  model_metrics <- cv_results$metrics[cv_results$metrics$Model == best_model_name,]
  avg_metrics <- colMeans(model_metrics[,c("Accuracy", "F1", "AUC")])
  var_metrics <- apply(model_metrics[,c("Accuracy", "F1", "AUC")], 2, var)
  
  cat("\nCross-validation Metrics for", best_model_name, ":\n")
  cat("Mean Accuracy:", round(avg_metrics["Accuracy"], 4), "\n")
  cat("Mean F1:", round(avg_metrics["F1"], 4), "\n")
  cat("Mean AUC:", round(avg_metrics["AUC"], 4), "\n")
  
  # Check for high variance in metrics
  if(any(var_metrics > 0.02)) {
    warnings <- c(warnings, "High variance in cross-validation metrics detected - possible overfitting")
    cat("\nWARNING: High variance in cross-validation metrics\n")
    cat("Variance in Accuracy:", round(var_metrics["Accuracy"], 4), "\n")
    cat("Variance in F1:", round(var_metrics["F1"], 4), "\n")
    cat("Variance in AUC:", round(var_metrics["AUC"], 4), "\n")
  }
  
  robustness_metrics$cv_metrics <- list(
    mean = avg_metrics,
    variance = var_metrics
  )
  
  # 2. Learning Curve Analysis
  cat("\nAnalyzing learning curve...\n")
  set.seed(123)
  train_sizes <- c(0.3, 0.5, 0.7, 0.9)
  learning_curve <- data.frame(
    size = train_sizes,
    accuracy = numeric(length(train_sizes)),
    f1 = numeric(length(train_sizes))
  )
  
  for(i in seq_along(train_sizes)) {
    size <- train_sizes[i]
    train_idx <- sample(nrow(data), size = floor(nrow(data) * size))
    train_subset <- data[train_idx,]
    
    # Train on subset
    subset_results <- train_models(
      train_subset, 
      response_var, 
      num_folds = 5,
      plot_results = FALSE
    )
    
    subset_metrics <- subset_results$model_avg[subset_results$model_avg$Model == best_model_name,]
    learning_curve$accuracy[i] <- subset_metrics$Accuracy
    learning_curve$f1[i] <- subset_metrics$F1
    
    cat("Training size:", sprintf("%d%%", size * 100), 
        "- Accuracy:", round(subset_metrics$Accuracy, 4),
        "- F1:", round(subset_metrics$F1, 4), "\n")
  }
  
  # Check for learning curve patterns
  acc_diff <- diff(learning_curve$accuracy)
  if(all(acc_diff < 0.01)) {
    warnings <- c(warnings, "Learning curve shows minimal improvement with more data - possible underfitting")
    cat("\nWARNING: Learning curve shows minimal improvement with more data\n")
  }
  if(any(acc_diff < 0)) {
    warnings <- c(warnings, "Unstable learning curve - performance decreases with more data")
    cat("\nWARNING: Unstable learning curve detected\n")
  }
  
  robustness_metrics$learning_curve <- learning_curve
  
  # 3. Prediction Stability with Noise
  cat("\nTesting prediction stability with noise...\n")
  numeric_cols <- sapply(data, is.numeric)
  # Exclude id column from noise addition
  numeric_cols[names(data)[1]] <- FALSE  # Exclude first column (ID)
  noisy_predictions <- matrix(NA, nrow = nrow(data), ncol = n_iterations)
  
  for(i in 1:n_iterations) {
    noisy_data <- data
    for(col in names(data)[numeric_cols]) {
      if(col != response_var && col != names(data)[1]) {  # Double check to exclude ID
        # Add 1% random noise
        noise <- rnorm(nrow(data), mean = 0, sd = 0.01 * sd(data[[col]]))
        noisy_data[[col]] <- data[[col]] + noise
      }
    }
    
    # Make predictions with noisy data
    preds <- predict_new_data(
      data = data,
      new_data = noisy_data,
      response_var = response_var,
      best_model_name = best_model_name
    )
    
    noisy_predictions[,i] <- as.numeric(as.character(preds$type))
  }
  
  # Calculate prediction stability
  prediction_changes <- apply(noisy_predictions, 1, function(x) length(unique(x)))
  unstable_predictions <- mean(prediction_changes > 1)
  
  cat("Prediction stability test results:\n")
  cat("Proportion of unstable predictions:", round(unstable_predictions, 4), "\n")
  
  if(unstable_predictions > 0.1) {
    warnings <- c(warnings, "High prediction instability with small input noise - possible overfitting")
    cat("\nWARNING: High prediction instability detected\n")
  }
  
  robustness_metrics$prediction_stability <- list(
    unstable_ratio = unstable_predictions,
    average_changes = mean(prediction_changes)
  )
  
  # Final robustness assessment
  cat("\nFinal Robustness Assessment:\n")
  if(length(warnings) == 0) {
    cat("✓ Model appears robust - no significant overfitting evidence detected\n")
  } else {
    cat("! Potential issues detected:\n")
    for(w in warnings) {
      cat("  - ", w, "\n")
    }
  }
  
  # Return results
  return(list(
    metrics = robustness_metrics,
    warnings = warnings,
    is_robust = length(warnings) == 0
  ))
}

#' Test all models individually to verify they work correctly
#' 
#' @param data Training data to use for testing
#' @param response_var Name of the response variable
#' @param n_test_samples Number of samples to use for testing (default 100)
#' @return List of test results and any errors encountered
test_all_models <- function(data, response_var, n_test_samples = 100) {
  # Store results and errors
  results <- list()
  errors <- list()
  
  # Create a small test set
  set.seed(42)
  n_samples <- min(n_test_samples, nrow(data))
  test_indices <- sample(nrow(data), n_samples)
  
  test_data <- data[test_indices, ]
  train_data <- data[-test_indices, ]
  
  # List of all models to test
  models <- c("log", "tree", "nb", "knn", "oner", "caret_bag", 
              "custom_bag", "svm", "nnet", "ensemble")
  
  cat("\nTesting each model individually:\n")
  cat("================================\n")
  
  for(model in models) {
    cat("\nTesting model:", model, "\n")
    tryCatch({
      # Train and predict
      pred_result <- predict_new_data(
        data = train_data,
        new_data = test_data,
        response_var = response_var,
        best_model_name = model
      )
      
      # Verify predictions
      if(nrow(pred_result) != nrow(test_data)) {
        errors[[model]] <- paste("Prediction count mismatch. Expected:", 
                               nrow(test_data), "Got:", nrow(pred_result))
      } else if(!all(pred_result$type %in% c("0", "1"))) {
        errors[[model]] <- "Invalid prediction values detected"
      } else {
        # Calculate basic metrics
        actual <- test_data[[response_var]]
        predicted <- factor(pred_result$type, levels = levels(actual))
        
        accuracy <- mean(predicted == actual)
        conf_matrix <- table(Actual = actual, Predicted = predicted)
        
        results[[model]] <- list(
          accuracy = accuracy,
          confusion_matrix = conf_matrix,
          predictions = pred_result
        )
        
        cat("Success! Accuracy:", round(accuracy, 4), "\n")
        cat("Confusion Matrix:\n")
        print(conf_matrix)
      }
    }, error = function(e) {
      errors[[model]] <- paste("Error:", e$message)
      cat("Failed with error:", e$message, "\n")
    })
  }
  
  # Print summary
  cat("\nTest Summary:\n")
  cat("============\n")
  for(model in models) {
    if(!is.null(results[[model]])) {
      cat(sprintf("%-12s: Success (Accuracy: %.4f)\n", 
                 model, results[[model]]$accuracy))
    } else {
      cat(sprintf("%-12s: Failed - %s\n", 
                 model, errors[[model]]))
    }
  }
  
  # Data structure verification
  cat("\nData Structure Verification:\n")
  cat("=========================\n")
  
  # Check numeric columns
  num_cols_train <- names(train_data)[sapply(train_data, is.numeric)]
  num_cols_test <- names(test_data)[sapply(test_data, is.numeric)]
  
  cat("\nNumeric columns in training data:", paste(num_cols_train, collapse=", "), "\n")
  cat("Numeric columns in test data:", paste(num_cols_test, collapse=", "), "\n")
  
  # Check factor columns
  factor_cols_train <- names(train_data)[sapply(train_data, is.factor)]
  factor_cols_test <- names(test_data)[sapply(test_data, is.factor)]
  
  cat("\nFactor columns in training data:", paste(factor_cols_train, collapse=", "), "\n")
  cat("Factor columns in test data:", paste(factor_cols_test, collapse=", "), "\n")
  
  # Check for missing values
  missing_cols_train <- names(which(colSums(is.na(train_data)) > 0))
  missing_cols_test <- names(which(colSums(is.na(test_data)) > 0))
  
  if(length(missing_cols_train) > 0) {
    cat("\nWarning: Missing values found in training data columns:", 
        paste(missing_cols_train, collapse=", "), "\n")
  }
  if(length(missing_cols_test) > 0) {
    cat("Warning: Missing values found in test data columns:", 
        paste(missing_cols_test, collapse=", "), "\n")
  }
  
  # Return detailed results
  return(list(
    results = results,
    errors = errors,
    data_verification = list(
      numeric_columns = list(train = num_cols_train, test = num_cols_test),
      factor_columns = list(train = factor_cols_train, test = factor_cols_test),
      missing_values = list(train = missing_cols_train, test = missing_cols_test)
    )
  ))
} 