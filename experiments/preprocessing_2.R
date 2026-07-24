# setwd("C:/your/folder/path")

# Step 1: Load Libraries for Data Preprocessing & Modeling

  # Optional: Install all packages at once
  install.packages(c("Boruta", "DMwR2", "EnvStats", "FNN", "GGally", "Hmisc", "MASS",
                     "NbClust", "OneR", "PresenceAbsence", "RODBC", "ROSE", "Rcpp", "RcppRoll", "Rmisc", "TTR",
                     "VIM", "XLConnect", "arules", "arulesViz", "caret", "cluster", "clustertend", "discretization",
                     "dplyr", "dtw", "e1071", "factoextra", "forcats", "fpc", "funModeling", "fuzzyjoin", "gapminder",
                     "ggTimeSeries", "ggalluvial", "gganimate", "ggforce", "ggmap", "ggparallel", "ggpubr",
                     "ggrepel", "ggstatsplot", "hopkins", "infotheo", "lubridate", "mice", "openintro", "openxlsx",
                     "outliers", "pROC", "plyr", "pracma", "rCBA", "rattle", "readr", "readxl", "reshape", "reshape2",
                     "rpart", "rpart.plot", "sampling", "scales", "smotefamily", "splitTools", "stringdist", "stringi",
                     "stringr", "tidyrules", "tidyverse", "wavelets", "writexl", "xlsx", "zoo",
                     "markdown", "esquisse", "yardstick"))
  
  # Load all required libraries
  library(Boruta)
  library(DMwR2)
  library(EnvStats)
  library(FNN)
  library(GGally)
  library(Hmisc)
  library(MASS)
  library(NbClust)
  library(OneR)
  library(PresenceAbsence)
  library(RODBC)
  library(ROSE)
  library(Rcpp)
  library(RcppRoll)
  library(Rmisc)
  library(TTR)
  library(VIM)
  library(XLConnect)
  library(arules)
  library(arulesViz)
  library(caret)
  library(cluster)
  library(clustertend)
  library(discretization)
  library(dplyr)
  library(dtw)
  library(e1071)
  library(factoextra)
  library(forcats)
  library(fpc)
  library(funModeling)
  library(fuzzyjoin)
  library(gapminder)
  library(ggTimeSeries)
  library(ggalluvial)
  library(gganimate)
  library(ggforce)
  library(ggmap)
  library(ggparallel)
  library(ggpubr)
  library(ggrepel)
  library(ggstatsplot)
  library(hopkins)
  library(infotheo)
  library(lubridate)
  library(mice)
  library(openintro)
  library(openxlsx)
  library(outliers)
  library(pROC)
  library(plyr)
  library(pracma)
  library(rCBA)
  library(rattle)
  library(readr)
  library(readxl)
  library(reshape)
  library(reshape2)
  library(rpart)
  library(rpart.plot)
  library(sampling)
  library(scales)
  library(smotefamily)
  library(splitTools)
  library(stringdist)
  library(stringi)
  library(stringr)
  library(tidyrules)
  library(tidyverse)
  library(wavelets)
  library(writexl)
  library(xlsx)
  library(zoo)
  library(markdown)
  library(esquisse)
  train_path <- if (file.exists("data/training_data.csv")) "data/training_data.csv" else "../data/training_data.csv"
  test_path <- if (file.exists("data/test_data.csv")) "data/test_data.csv" else "../data/test_data.csv"
  
  ############Data Integration############
  # Step 2: Read the dataset and convert to data frame
  dt2 <- read.csv(train_path, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
  dt2 <- as.data.frame(dt2)
  
  dts2 <- read.csv(test_path, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
  dts2 <- as.data.frame(dts2)
  
  # Step 3: Exploring The Data
  
  str(dt2) #reveals the data types of each variable and general shape
  summary(dt2) #Min, max, mean, NA count
  lapply(dt2,unique) #ver se temos "" ou "?"
  # Seeing missing value
  
  
  
  
  
  
  summary(dt2) #Min, max, mean, NA count
  #Dimensions of the dataset
  nrow(dt2)
  ncol(dt2)
  dim(dt2) 
  #Names of columns and rows
  colnames(dt2)
  rownames(dt2)
  #Quick look at data
  head(dt2)
  tail(dt2)
  
  ##Histograms and Bar Plots##
  # Loop through each column in the dataframe
  for (colname in names(dt2)) {
    
    # Check if column is numeric
    if (is.numeric(dt2[[colname]])) {
      hist(dt2[[colname]],
           main = paste("Histogram of", colname),
           xlab = colname,
           col = "lightblue",
           border = "white")
      
      # Check if column is categorical (factor or character)
    } else if (is.factor(dt2[[colname]]) || is.character(dt2[[colname]])) {
      barplot(table(dt2[[colname]]),
              main = paste("Bar Chart of", colname),
              xlab = colname,
              ylab = "Frequency",
              col = "salmon",
              las = 2)  # rotate labels for readability
    }
  }
  
  dt2$employer_size[which(dt2$employer_size=='Oct-49')]='10-49'
  # Step 4: Clean Label Inconsistencies
  
  #Células com ? ou "" ou " " outros caracteres estranhos
  
  
  dt2[dt2 == ""] <- NA
  dt2=dt2[,-2]
  
  #Removing Duplicates
  #remove duplicate rows
  unique_values <- unique(dt2)
  
  
  # Step 5: Convert Variable Types
  #Convert categorical vars to factor
  #Keep numeric vars as numeric
  
  
  # Factor conversion
  factors <- c("sex",'prior_experience','university_enrollment','academic_qualification','field_of_study','work_experience_years','employer_size','employer_type','time_since_last_job_change','target')
  for (f in factors) dt2[[f]] <- as.factor(dt2[[f]])
  
  # Numeric conversion
  nums <- c("city_dev_score", "hours_of_training")
  for (n in nums) dt2[[n]] <- as.numeric(dt2[[n]])
  
  
  
  ####RELEVEL#####---------------------------------------------------------------
  levels(dt2$target)  #[1] "class1"  "class2"
  
  
  #ANALISAR TEST
  lapply(dts2,unique)
  summary(dts2)
  str(dts2)
  colSums(is.na(dts2)) #How many missing values per column
  dts2[dts2 == ""] <- NA
  dts2=dts2[,-2]
  dts2$employer_size[which(dts2$employer_size=='Oct-49')]='10-49'
  
  # Factor conversion
  factorss <- c("sex",'prior_experience','university_enrollment','academic_qualification','field_of_study','work_experience_years','employer_size','employer_type','time_since_last_job_change')
  for (f in factorss) dts2[[f]] <- as.factor(dts2[[f]])
  
  # Numeric conversion
  numss <- c("city_dev_score", "hours_of_training")
  for (n in numss) dts2[[n]] <- as.numeric(dts2[[n]])
  
 
  
  
  
  
  #Fazer histograma de novo
  ##Histograms and Bar Plots##
  # Loop through each column in the dataframe
  for (colname in names(dt2)) {
    
    # Check if column is numeric
    if (is.numeric(dt2[[colname]])) {
      hist(dt2[[colname]],
           main = paste("Histogram of", colname),
           xlab = colname,
           col = "lightblue",
           border = "white")
      
      # Check if column is categorical (factor or character)
    } else if (is.factor(dt2[[colname]]) || is.character(dt2[[colname]])) {
      barplot(table(dt2[[colname]]),
              main = paste("Bar Chart of", colname),
              xlab = colname,
              ylab = "Frequency",
              col = "salmon",
              las = 2)  # rotate labels for readability
    }
  }
  

  
############Data Cleaning############

# # Step 6: Outliers
#   ###Outliers - numeric data
#   #Statistical Methods
#   
#   #@@Box Plot
#     
#     # Identify numeric columns
#     numeric_cols <- sapply(dt2, is.numeric)
#     
#     # Initialize summary dataframes
#     outlier_report <- data.frame(
#       Variable = character(),
#       NumOutliers = integer(),
#       PercentOutliers = numeric(),
#       stringsAsFactors = FALSE
#     )
#     
#     outlier_indices <- data.frame(
#       RowIndex = integer(),
#       Column = character(),
#       Value = numeric(),
#       stringsAsFactors = FALSE
#     )
#     
#     n_total <- nrow(dt2)
#     
#     # Loop through numeric columns
#     for (colname in names(dt2)[numeric_cols]) {
#       x <- dt2[[colname]]
#       
#       # Compute Q1, Q3, IQR
#       Q1 <- quantile(x, 0.25, na.rm = TRUE)
#       Q3 <- quantile(x, 0.75, na.rm = TRUE)
#       IQR_val <- Q3 - Q1
#       
#       # Define outliers based on IQR rule
#       is_outlier <- (x < Q1 - 1.5 * IQR_val) | (x > Q3 + 1.5 * IQR_val)
#       
#       # Remove NAs from detection
#       outlier_rows <- which(is_outlier & !is.na(is_outlier))
#       
#       if (length(outlier_rows) > 0) {
#         # Save outlier details
#         outlier_indices <- rbind(outlier_indices, data.frame(
#           RowIndex = outlier_rows,
#           Column = rep(colname, length(outlier_rows)),
#           Value = x[outlier_rows]
#         ))
#         
#         #OPTION 1: Set outlier values to NA
#         #dt2[outlier_rows, colname] <- NA
#       }
#       
#       # Add to outlier report
#       num_out <- length(outlier_rows)
#       perc_out <- round(100 * num_out / n_total, 2)
#       
#       outlier_report <- rbind(outlier_report, data.frame(
#         Variable = colname,
#         NumOutliers = num_out,
#         PercentOutliers = perc_out
#       ))
#       
#       #Plot boxplot for each variable
#       boxplot(x, main = paste("Boxplot of", colname),
#               col = "lightblue", outline = TRUE)
#     }
#     
#    
#     
#     # Show results
#     print(outlier_report)
#     print(outlier_indices)
#     # Order by row index for easier tracking
#     outlier_indices <- outlier_indices[order(outlier_indices$RowIndex), ]
#     print(outlier_indices)
#     
#     #OPTION 2: Remove all rows that had at least one outlier
#     #all_outlier_rows <- unique(outlier_indices$RowIndex)
#     #dt_cleaned <- dt2[-all_outlier_rows, ]
#     #dt_cleaned=dt2[-c(412),]
    
# Step 7: Missing Data
  ###Missing Data
#removemos colunas: employer size e employer type por termos muitos NAs  
colnames(dt2)
dt2=dt2[,-c(9,10)]
dts2=dts2[,-c(9,10)]   

###O MELHOR: utilizando o VIM::kNN -> melhor para substituir variáveis numéricas e categóricas - já faz a normalização dentro da função
col_name=as.vector(colnames(dt2))
columns_to_impute <- col_name[-c(1,11)]  #colocar todas as colunas menos id e y!
data_imputed <- kNN(dt2[, columns_to_impute], k = 5) 
#colocar na dataframe só os outputs da função kNN que interessam
dt2[, columns_to_impute] <- data_imputed[, !grepl("_imp$", names(data_imputed))]
             



######MISSING VALUES TEST######
  # Define columns to impute

  
  # 1. Identify rows in dts2 that have at least one NA
  rows_with_na <- which(apply(dts2[, columns_to_impute], 1, function(x) any(is.na(x))))
  
  # 2. For each row with missing values:
  for (row_idx in rows_with_na) {
    
    # Extract the test row
    test_row <- dts2[row_idx, columns_to_impute, drop = FALSE]
    
    # 3. Temporarily bind to dt2
    temp_data <- rbind(dt2[, columns_to_impute], test_row)
    
    # 4. Apply kNN imputation
    temp_imputed <- kNN(temp_data, k = 5)
    
    # 5. Keep only the original columns (remove "_imp" columns)
    temp_imputed_clean <- temp_imputed[, !grepl("_imp$", names(temp_imputed))]
    
    # 6. Extract the imputed last row
    imputed_row <- temp_imputed_clean[nrow(temp_imputed_clean), columns_to_impute]
    
    # 7. Replace the row in dts2
    dts2[row_idx, columns_to_impute] <- imputed_row
  }
  
  # Final imputed dts2
  print(dts2)  
  
# ######GRAVAR CSV########
# # Gravar o dataset de treino
# write.csv(dt2, file = "train2.csv", row.names = FALSE)
#   
# # Gravar o dataset de teste
# write.csv(dts2, file = "test2.csv", row.names = FALSE)