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
  library(yardstick)
  train_path <- if (file.exists("data/training_data.csv")) "data/training_data.csv" else "../data/training_data.csv"
  test_path <- if (file.exists("data/test_data.csv")) "data/test_data.csv" else "../data/test_data.csv"
  
  ############Data Integration############
  # Step 2: Read the dataset and convert to data frame
  dt2 <- read.csv(train_path, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
  dt2 <- as.data.frame(dt2)
  
  dts2 <- read.csv(test_path, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
  dts2 <- as.data.frame(dts2)
  
# Step 3: Exploring The Data

  str(dt) #reveals the data types of each variable and general shape
  summary(dt) #Min, max, mean, NA count
  lapply(dt,unique) #ver se temos "" ou "?"
  # Seeing missing values  
  colSums(is.na(dt)) #How many missing values per column
  
  #Dimensions of the dataset
  nrow(dt)
  ncol(dt)
  dim(dt) 
  #Names of columns and rows
  colnames(dt)
  rownames(dt)
  #Quick look at data
  head(dt)
  tail(dt)

##Histograms and Bar Plots##
# Loop through each column in the dataframe
for (colname in names(dt)) {
  
  # Check if column is numeric
  if (is.numeric(dt[[colname]])) {
    hist(dt[[colname]],
         main = paste("Histogram of", colname),
         xlab = colname,
         col = "lightblue",
         border = "white")
    
  # Check if column is categorical (factor or character)
  } else if (is.factor(dt[[colname]]) || is.character(dt[[colname]])) {
    barplot(table(dt[[colname]]),
            main = paste("Bar Chart of", colname),
            xlab = colname,
            ylab = "Frequency",
            col = "salmon",
            las = 2)  # rotate labels for readability
  }
}

# Step 4: Clean Label Inconsistencies

  #Células com ? ou "" ou " " outros caracteres estranhos
  
  for (i in colnames(dt)) {
    dt[[i]] <- gsub("\\s+", "", dt[[i]])
  } #retiramos todo o tipo de espaços, tabs,...

  dt[dt == ""] <- NA
  
  #Trocar ordem das colunas
  number_columns = ncol(dt)
  move_column <- function(data, column_name, position) {
    # Remove the column to move from the list of names
    col_names <- setdiff(names(data), column_name)
    
    # Insert the column at the desired position
    new_order <- append(col_names, column_name, after = position - 1)
    
    # Reorder and return the data
    return(data[, new_order])
  }
  # Move column 'id' to position 1
  dt <- move_column(dt, "id", 1)
  # Move column 'y' to last position
  dt <- move_column(dt, "y", number_columns)
  
  #Handling Text Data
  
  #Removing punctuation and converting to lowercase 
  #Punctuation: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
  dt$col <- tolower(gsub("[[:punct:]]", "", dt$col))
  
  #Converting to lowercase and removing extra whitespaces
  dt$col <- tolower(dt$col) #converter para minúsculas
  dt$col <- tolower(trimws(dt$col)) #retira espaços
  
  #Removing special characters from column names
  colnames(dt) <- gsub("[^a-zA-Z0-9]", "_", colnames(dt)) #Replacing everything that is not a letter or number with an underscore (_)
  
  #Splitting parts of a character attribute
  #Split the Title
  dt$title <- str_extract(dt$name, "\\b(Mr|Mrs|Miss|Master|Dr|Rev|Col|Major|Ms|Mlle|Mme|Don|Capt)\\b")
  
  # Split on comma
  split_names <- str_split(dt$Name, ", ") #gives a list
  # Extract components
  dt$LastName <- sapply(split_names, function(x) x[1]) #For each element in split_names (which is a list), extract the first element.
  dt$Rest <- sapply(split_names, function(x) x[2])
  
  # Rename to match
  colnames(dt)[colnames(dt) == "AGE"] <- "age"
  
  dt$col <- as.numeric(substr(dt$col, 2, 2))
  dt$col[dt$col == "a1"] <- "1"
  
  #  Removing column col as it is an extra index column  
  dt$col <- NULL
  
  #Removing Duplicates
  #remove duplicate rows
  dt <- unique(dt)
  
  #remove duplicated rows of specified columns
  dt <- dt[!duplicated(dt[, c("col1", "col2")]), ]
  
  #remove duplicate columns
  dt <- dt[, !duplicated(colnames(dt))]
  
  # Merge DataFrames
  # By common variable
  merged_data <- merge(data1, data2, by = "common_variable")
  
  # Combine by rows
  combined_rows <- rbind(data1, data2)
  
  # Combine by columns
  combined_cols <- cbind(data1, data2)

# Step 5: Convert Variable Types
  #Convert categorical vars to factor
  #Keep numeric vars as numeric
  
  # Convert data types
  dt$col = as.numeric(dt$col)
  
  # Factor conversion
  factors <- c("col1", "col2")
  for (f in factors) dt[[f]] <- as.factor(dt[[f]])
  
  # Numeric conversion
  nums <- c("col1", "col2")
  for (n in nums) dt[[n]] <- as.numeric(dt[[n]])
  
  ####RELEVEL#####---------------------------------------------------------------
  levels(dt$y)  #[1] "class1"  "class2"
  levels(dt$y)=c(1,0) # ver qual quero que seja a classe positiva (neste caso era class1)
  dt$y=relevel(dt$y,ref="0") #reference é o primeiro nível, quero que o segundo nível seja a positive class!
  levels(dt$y) 

  #Dataset principal pode-se dividir em dois: um de dados categóricos e um de dados numéricos
  dt_numeric <- dt[, nums]
  dt_cat <- dt[, factors]
  
############Data Cleaning############

# Step 6: Outliers
  ###Outliers - numeric data
  #Statistical Methods
  
  #@@Normal/Z-score (k=3)
  
    # Identify numeric columns
    numeric_cols <- sapply(dt, is.numeric)
    
    # Initialize summary dataframes
    outlier_report <- data.frame(
      Variable = character(),
      NumOutliers = integer(),
      PercentOutliers = numeric(),
      stringsAsFactors = FALSE
    )
    
    outlier_indices <- data.frame(
      RowIndex = integer(),
      Column = character(),
      Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    n_total <- nrow(dt)
    
    # Loop through numeric columns
    for (colname in names(dt)[numeric_cols]) {
      x <- dt[[colname]]
      
      # Detect outliers
      is_outlier <- (x < mean(x, na.rm = TRUE) - 3 * sd(x, na.rm = TRUE)) |
        (x > mean(x, na.rm = TRUE) + 3 * sd(x, na.rm = TRUE))
      
      # Remove NAs from detection
      outlier_rows <- which(is_outlier & !is.na(is_outlier))
      
      if (length(outlier_rows) > 0) {
        # Save outlier details
        outlier_indices <- rbind(outlier_indices, data.frame(
          RowIndex = outlier_rows,
          Column = rep(colname, length(outlier_rows)),
          Value = x[outlier_rows]
        ))
        
        #OPTION 1: Set outlier values to NA in dt
        #dt[outlier_rows, colname] <- NA
      }
      
      # Add to outlier report
      num_out <- length(outlier_rows)
      perc_out <- round(100 * num_out / n_total, 2)
      
      outlier_report <- rbind(outlier_report, data.frame(
        Variable = colname,
        NumOutliers = num_out,
        PercentOutliers = perc_out
      ))
    }
    
    
    # Show results
    print(outlier_report)
    print(outlier_indices)
    outlier_indices <- outlier_indices[order(outlier_indices$RowIndex), ]
    print(outlier_indices)
    
    #OPTION 2: Remove all rows that had at least one outlier
    #all_outlier_rows <- unique(outlier_indices$RowIndex)
    #dt_cleaned <- dt[-all_outlier_rows, ]
    #dt_cleaned=dt[-c(412),]
  
  
  #@@Box Plot
    
    # Identify numeric columns
    numeric_cols <- sapply(dt, is.numeric)
    
    # Initialize summary dataframes
    outlier_report <- data.frame(
      Variable = character(),
      NumOutliers = integer(),
      PercentOutliers = numeric(),
      stringsAsFactors = FALSE
    )
    
    outlier_indices <- data.frame(
      RowIndex = integer(),
      Column = character(),
      Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    n_total <- nrow(dt)
    
    # Loop through numeric columns
    for (colname in names(dt)[numeric_cols]) {
      x <- dt[[colname]]
      
      # Compute Q1, Q3, IQR
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      
      # Define outliers based on IQR rule
      is_outlier <- (x < Q1 - 1.5 * IQR_val) | (x > Q3 + 1.5 * IQR_val)
      
      # Remove NAs from detection
      outlier_rows <- which(is_outlier & !is.na(is_outlier))
      
      if (length(outlier_rows) > 0) {
        # Save outlier details
        outlier_indices <- rbind(outlier_indices, data.frame(
          RowIndex = outlier_rows,
          Column = rep(colname, length(outlier_rows)),
          Value = x[outlier_rows]
        ))
        
        #OPTION 1: Set outlier values to NA
        #dt[outlier_rows, colname] <- NA
      }
      
      # Add to outlier report
      num_out <- length(outlier_rows)
      perc_out <- round(100 * num_out / n_total, 2)
      
      outlier_report <- rbind(outlier_report, data.frame(
        Variable = colname,
        NumOutliers = num_out,
        PercentOutliers = perc_out
      ))
      
      #Plot boxplot for each variable
      boxplot(x, main = paste("Boxplot of", colname),
              col = "lightblue", outline = TRUE)
    }
    
   
    
    # Show results
    print(outlier_report)
    print(outlier_indices)
    # Order by row index for easier tracking
    outlier_indices <- outlier_indices[order(outlier_indices$RowIndex), ]
    print(outlier_indices)
    
    #OPTION 2: Remove all rows that had at least one outlier
    #all_outlier_rows <- unique(outlier_indices$RowIndex)
    #dt_cleaned <- dt[-all_outlier_rows, ]
    #dt_cleaned=dt[-c(412),]
    
# Step 7: Missing Data
  ###Missing Data
  
  # Seeing missing values  
    #per column
      colSums(is.na(dt))
    #per row
      na_row_summary <- data.frame(
        RowIndex = 1:nrow(dt),
        NumMissing = rowSums(is.na(dt))
      )
      na_row_summary <- na_row_summary[na_row_summary$NumMissing > 0, ]
      
      print("Rows with missing values:")
      print(na_row_summary)
  
  # 1) Remover Observações (eliminar linhas) 
    # Select the observations that contain more than the specified number (6) of missing values
    aux <- apply(dt, 1, function(x) sum(is.na(x))) #number of unknown values in each row
    dt_2 <- dt[-c(which(aux >= 6)), ]
  
  # 2) Filling in the Unknowns with the Most Frequent Values (mean/median) - numeric data
    dt[is.na(dt$col), "col"] <- mean(dt$col, na.rm = TRUE)  # mean
    dt[is.na(dt$col), "col"] <- median(dt$col, na.rm = TRUE)  # median
  
  # 3) Filling in the Unknowns with the Most Frequent Values (mode) - categorical data
    # Step 1: Identify non-numeric columns with missing values
    na_cols <- colnames(dt)[
      !sapply(dt, is.numeric) & colSums(is.na(dt)) >= 1
    ]
    # Step 2: Replace missing values in each column with the mode
    for (col in na_cols) {
      col_data <- dt[[col]]
      mode_value <- as.numeric(names(sort(-table(col_data[!is.na(col_data)])))[1])
      dt[is.na(col_data), col] <- mode_value
    }
  
  # 4) Filling in the Unknown Values by Exploring Correlations (Regression - numeric data)
    # Verificar qual a regression com maior R^2 
    #Loop through the variables with missing values (trestbps, ca)
    for (i in c('col1', 'col2')) {
      for (j in c('col3', 'col4')) {
        regression_results <- lm(dt[, i] ~ dt[, j])
        print(summary(regression_results))
      }
    }
    #best fits:col1~ col3; col2~ col4
    
    #Criar Modelo/s
    model <- lm(y ~ x, data = dt)
    
    #Filling 1 row
    dt[28, "y"] <- model[["coefficients"]][1] + model[["coefficients"]][2] * dt[28, "x"]
    
    #Filling several rows
    filly <- function(x) {
      if (is.na())
        return(NA)
      else
        (return(model[["coefficients"]][1] + model[["coefficients"]][2] * x))
    }
    
    dt[is.na(dt$y), "y"] <- sapply(dt[is.na(dt$y), "x"], filly)
  
  # 5) Filling in the Unknown Values by Exploring Similarities between Cases (KNN) 
    dt <- knnImputation(dt, k = 10) #meth='weighAvg' by default - numeric data
    #or
    dt <- knnImputation(dt, k = 10, meth = "median") #numeric data (median) or factors (most frequent value) 
    
  ###O MELHOR: utilizando o VIM::kNN -> melhor para substituir variáveis numéricas e categóricas - já faz a normalização dentro da função
    columns_to_impute <- c()  #colocar todas as colunas menos id e y!
    data_imputed <- kNN(dt[, columns_to_impute], k = 5) 
    #colocar na dataframe só os outputs da função kNN que interessam
    dt[, columns_to_impute] <- data_imputed[, !grepl("_imp$", names(data_imputed))]
    
    

###Noisy Data - numeric data

  #Binning
    #1)Discretize - equal depth
    dt <- na.omit(dt)
    
    # Create bins: k = √N
    discretize(dt$col, disc = "equalfreq", nbins = sqrt(length(dt$col)))
    dt$bin_2 <- as.numeric(unlist(discretize(dt$col, disc = "equalfreq", nbins = sqrt(length(dt$col)))))
  
  #2)Smooth - bin mean
    # Associate the mean value to each bin
    for (k in 1:sqrt(length(dt$col))) {
      dt$col[dt$bin_2 == k] <- mean(dt$col[dt$bin_2 == k])
    }

############Data Transformation############

###New Features
  #Fazer nova coluna que é a multiplicação das outras 2
  dt$var1 <- dt$var_2 * dt$var_3
  
  #Fazer nova coluna, em que =1 se “column” >150 e =0 caso contrário 
  dt$Cl <- 0
  dt[which(dt$column > 150), 'Cl'] <- 1 
  #or
  dt$Cl <- ifelse(dt$column > 150, 1, 0)
  
###"Other" category (factors)
  freq <- prop.table(table(dt$ColFactor))
  
  # Ver quais têm menos de 10% de frequência ou o que virmos que faz sentido
  names(freq[freq < 0.1])
  
  # Substituir cores raras por "Outra"
  dt$ColFactor <- ifelse(dt$ColFactor %in% names(freq[freq < 0.1]), "Other",dt$ColFactor)
  dt$ColFactor=as.factor(dt$ColFactor)

###Normalization numeric data
  #1) Min-Max
    # Define new desired range
    new_max <- 1
    new_min <- 0
    
    # Define transformation function
    doit <- function(x) {
      (x - min(x, na.rm = TRUE)) /
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) *
        ((new_max - new_min) + new_min)
    }
    
    # Apply to each column of the dataset_a_ser_normalizado (only numeric columns!)
    normed <- apply(dataset_a_ser_normalizado, 2, doit)
  
  #2) Z-score
    z_score_function <- function(x) {
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    }
    
    normed <- apply(dataset_a_ser_normalizado, 2, z_score_function)
  
  #3) Decimal Scaling
    # Get max absolute values
    maxvect <- apply(abs(dataset_a_ser_normalizado), 2, max)
    
    # Calculate scaling factors
    kvector <- ceiling(log10(maxvect))
    scalefactor <- 10^kvector
    
    # Scale using the scale() function
    normed <- scale(dataset_a_ser_normalizado, center = FALSE, scale = scalefactor)

############Data Reduction############
# Step 8: See Correlations

  # Compute correlation matrix
  numeric_cols <- names(dt)[sapply(dt, is.numeric)]
  numeric_data <- dt[, numeric_cols]
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  
  print("Correlation matrix of numeric variables:")
  print(cor_matrix)
  
  # Find highly correlated pairs (above 0.9 or below -0.9)
  threshold <- 0.9
  high_corr_pairs <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
  
  # Avoid duplicate pairs (like [A,B] and [B,A])
  high_corr_pairs <- high_corr_pairs[high_corr_pairs[,1] < high_corr_pairs[,2], ]
  
  # Create a readable list of variable pairs
  if (nrow(high_corr_pairs) > 0) {
    corr_list <- data.frame(
      Var1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
      Var2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
      Correlation = cor_matrix[high_corr_pairs]
    )
    print("Highly correlated variable pairs (|correlation| > threshold):")
    print(corr_list)
  } else {
    print("No variable pairs with |correlation| > threshold found.")
  }
  #Remover colunas? Temos de remover no Test e no Train!
  var_to_remove=c("col1","col2")
  dt <- dt[, !(names(dt) == var_to_remove)]  # Remove the selected variables
  dts <- dts[, !(names(dts) == var_to_remove)] # Remove the selected variables

##########################################################
# Step 9: Imbalanced Dataset?

  # Table of class counts
  table_y <- table(dt$type)
  print(table_y)
  
  # Proportions
  prop_y <- prop.table(table_y)
  print(round(prop_y * 100, 2))
  
  # Set imbalance threshold
  imbalance_threshold <- 0.3
  
  # Logic to detect imbalance
  if (any(prop_y < imbalance_threshold)) {
    cat("The target variable 'y' is UNBALANCED because at least one class represents less than",
        imbalance_threshold * 100, "% of the total.\n")
  } else {
    cat("The target variable 'y' is reasonably balanced.\n")
  }
  
  #Optional: Barplot
  barplot(table_y, col = c("skyblue", "salmon"),
          main = "Class Distribution in Target (y)",
          ylab = "Count", names.arg = names(table_y))

  
######MISSING VALUES TEST######
  # Define columns to impute
  columns_to_impute <- c()  #colocar todas as colunas menos id e y!
  
  # 1. Identify rows in dts that have at least one NA
  rows_with_na <- which(apply(dts[, columns_to_impute], 1, function(x) any(is.na(x))))
  
  # 2. For each row with missing values:
  for (row_idx in rows_with_na) {
    
    # Extract the test row
    test_row <- dts[row_idx, columns_to_impute, drop = FALSE]
    
    # 3. Temporarily bind to dt
    temp_data <- rbind(dt[, columns_to_impute], test_row)
    
    # 4. Apply kNN imputation
    temp_imputed <- kNN(temp_data, k = 5)
    
    # 5. Keep only the original columns (remove "_imp" columns)
    temp_imputed_clean <- temp_imputed[, !grepl("_imp$", names(temp_imputed))]
    
    # 6. Extract the imputed last row
    imputed_row <- temp_imputed_clean[nrow(temp_imputed_clean), columns_to_impute]
    
    # 7. Replace the row in dts
    dts[row_idx, columns_to_impute] <- imputed_row
  }
  
  # Final imputed dts
  print(dts)  
  
# ######GRAVAR CSV########
# # Gravar o dataset de treino
# write.csv(dt, file = "train.csv", row.names = FALSE)
#   
# # Gravar o dataset de teste
# write.csv(dts, file = "test.csv", row.names = FALSE)