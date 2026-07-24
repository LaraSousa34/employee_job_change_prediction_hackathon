# setwd("C:/your/folder/path")

# Step 1: Load Libraries for Data Preprocessing & Modeling

  # # Optional: Install all packages at once
  # install.packages(c("Boruta", "DMwR2", "EnvStats", "FNN", "GGally", "Hmisc", "MASS",
  #                    "NbClust", "OneR", "PresenceAbsence", "RODBC", "ROSE", "Rcpp", "RcppRoll", "Rmisc", "TTR",
  #                    "VIM", "XLConnect", "arules", "arulesViz", "caret", "cluster", "clustertend", "discretization",
  #                    "dplyr", "dtw", "e1071", "factoextra", "forcats", "fpc", "funModeling", "fuzzyjoin", "gapminder",
  #                    "ggTimeSeries", "ggalluvial", "gganimate", "ggforce", "ggmap", "ggparallel", "ggpubr",
  #                    "ggrepel", "ggstatsplot", "hopkins", "infotheo", "lubridate", "mice", "openintro", "openxlsx",
  #                    "outliers", "pROC", "plyr", "pracma", "rCBA", "rattle", "readr", "readxl", "reshape", "reshape2",
  #                    "rpart", "rpart.plot", "sampling", "scales", "smotefamily", "splitTools", "stringdist", "stringi",
  #                    "stringr", "tidyrules", "tidyverse", "wavelets", "writexl", "xlsx", "zoo",
  #                    "markdown", "esquisse", "yardstick"))
  
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
  dt1 <- read.csv(train_path, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
  dt1 <- as.data.frame(dt1)
  
  dts1 <- read.csv(test_path, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
  dts1 <- as.data.frame(dts1)
  
# Step 3: Exploring The Data

  str(dt1) #reveals the data types of each variable and general shape
  summary(dt1) #Min, max, mean, NA count
  lapply(dt1,unique) #ver se temos "" ou "?"
  # Seeing missing value
  
  
  
  
  
  
  summary(dt1) #Min, max, mean, NA count
  #Dimensions of the dataset
  nrow(dt1)
  ncol(dt1)
  dim(dt1) 
  #Names of columns and rows
  colnames(dt1)
  rownames(dt1)
  #Quick look at data
  head(dt1)
  tail(dt1)
  
  ##Histograms and Bar Plots##
  # Loop through each column in the dataframe
  for (colname in names(dt1)) {
    
    # Check if column is numeric
    if (is.numeric(dt1[[colname]])) {
      hist(dt1[[colname]],
           main = paste("Histogram of", colname),
           xlab = colname,
           col = "lightblue",
           border = "white")
      
      # Check if column is categorical (factor or character)
    } else if (is.factor(dt1[[colname]]) || is.character(dt1[[colname]])) {
      barplot(table(dt1[[colname]]),
              main = paste("Bar Chart of", colname),
              xlab = colname,
              ylab = "Frequency",
              col = "salmon",
              las = 2)  # rotate labels for readability
    }
  }
  
  dt1$employer_size[which(dt1$employer_size=='Oct-49')]='10-49'
# Step 4: Clean Label Inconsistencies

  #Células com ? ou "" ou " " outros caracteres estranhos
  

  dt1[dt1 == ""] <- NA
  dt1=dt1[,-2]
 
  #Removing Duplicates
  #remove duplicate rows
  dt2 <- unique(dt1)
  
  
# Step 5: Convert Variable Types
  #Convert categorical vars to factor
  #Keep numeric vars as numeric
  

  # Factor conversion
  factors <- c("location_city", "sex",'prior_experience','university_enrollment','academic_qualification','field_of_study','work_experience_years','employer_size','employer_type','time_since_last_job_change','target')
  for (f in factors) dt1[[f]] <- as.factor(dt1[[f]])
  
  # Numeric conversion
  nums <- c("city_dev_score", "hours_of_training")
  for (n in nums) dt1[[n]] <- as.numeric(dt1[[n]])
  

  
  ####RELEVEL#####---------------------------------------------------------------
  levels(dt1$target)  #[1] "class1"  "class2"
  
  
  #ANALISAR TEST
  lapply(dts1,unique)
  summary(dts1)
  str(dts1)
  colSums(is.na(dts1)) #How many missing values per column
  dts1[dts1 == ""] <- NA
  dts1=dts1[,-2]
  dts1$employer_size[which(dts1$employer_size=='Oct-49')]='10-49'
  
  # Factor conversion
  factorss <- c("location_city", "sex",'prior_experience','university_enrollment','academic_qualification','field_of_study','work_experience_years','employer_size','employer_type','time_since_last_job_change')
  for (f in factorss) dts1[[f]] <- as.factor(dts1[[f]])
  
  # Numeric conversion
  numss <- c("city_dev_score", "hours_of_training")
  for (n in numss) dts1[[n]] <- as.numeric(dts1[[n]])
  
  