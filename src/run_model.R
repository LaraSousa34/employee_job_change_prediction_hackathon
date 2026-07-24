##########################
# MAIN SCRIPT EXAMPLE   #
##########################

# Source model functions using relative path
if (file.exists("src/model_functions.R")) {
  source("src/model_functions.R")
} else if (file.exists("model_functions.R")) {
  source("model_functions.R")
}

# Load required libraries
library(Boruta)
library(DMwR)
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



###################### Pre-processing ###############################

#relevel ?
#assuming last col as target and first as Id!



#####################################################################
#################### Train with all models + predictions #############
#####################################################################

## Model functions sourced above

##assign data
data <- dt2 #for train
new_data <- dts2  #for test

##target col name
#response_var <- "target" 
#or
response_var <- names(data)[ncol(data)]


#  FEATURE SELECTION
#dt1 <- dt1[, !names(dt1) %in% c("skin", "bp", "bmi")]

# TRAIN MODEL 🏋️‍♀️💪🏼 _______________________________________________________

model_results <- train_models(data, response_var, num_folds = 10, plot_results = TRUE)

# Print the best model
cat("Best model based on F1 score:", model_results$best_model, "\n")



# Robustness 🤍 __________________________________________________________

# Get the name of the best model
best_model <- model_results$best_model

# Now test the robustness of just that best model
robustness_results <- robustness_test(
  data = dt2,
  response_var = "type",
  best_model_name = best_model
)



# PREDICTIONS 🪄✨ ______________________________________________________

# Use the best model to make predictions on test data
predictions <- predict_new_data(
  data = dt2,
  new_data = dts2,
  response_var = response_var,
  model_results = model_results
)

# Create submission with original IDs from test data
submission <- data.frame(
  id = dts1$id,  # Use original IDs from test data
  type = predictions$type  # Keep predictions
)


# Write predictions to file
write.csv(predictions, "sample_submission.csv", row.names = FALSE)

# Print a sample of the predictions
print("Sample of predictions:")
print(head(predictions, 10)) 


# other tests__________________________________________________________
# # Test all models with your data
# test_results <- test_all_models(
#   data = dt1,
#   response_var = "type"
# )
# 
# # Test just KNN
# knn_test <- predict_new_data(
#   data = dt1,
#   new_data = dt1[1:10,],  # test with first 10 rows
#   response_var = "type",
#   best_model_name = "knn"
# )

# DONE :)👍_______________//____________________________________________


#####################################################################
#################### Train separated + predictions ##################
#####################################################################



