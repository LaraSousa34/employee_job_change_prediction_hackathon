# Preprocessing Strategy 3: KNN Imputation (Extended Variable Drop)
# Objective: Prepare data for modeling by dropping high-NA variables ('sex', 'employer_type', 'employer_size') and applying KNN imputation (k=5).

library(tidyverse)
library(VIM)
library(caret)

# Load data
train_path <- file.path("..", "data", "training_data.csv")
test_path  <- file.path("..", "data", "test_data.csv")

if (!file.exists(train_path)) {
  train_path <- file.path("data", "training_data.csv")
  test_path  <- file.path("data", "test_data.csv")
}

dt3 <- read.csv(train_path, stringsAsFactors = FALSE)
dts3 <- read.csv(test_path, stringsAsFactors = FALSE)

# 1. Clean label inconsistencies & missing indicators
dt3[dt3 == ""] <- NA
dts3[dts3 == ""] <- NA

dt3$employer_size[which(dt3$employer_size == 'Oct-49')] <- '10-49'
dts3$employer_size[which(dts3$employer_size == 'Oct-49')] <- '10-49'

# Drop ID column if present (e.g. column 2 or location_city)
if ("location_city" %in% names(dt3)) dt3$location_city <- NULL
if ("location_city" %in% names(dts3)) dts3$location_city <- NULL

# 2. Convert types
factors <- c("prior_experience", "university_enrollment", "academic_qualification", 
             "field_of_study", "work_experience_years", "time_since_last_job_change", "target")
for (f in intersect(factors, names(dt3))) dt3[[f]] <- as.factor(dt3[[f]])

factors_test <- setdiff(factors, "target")
for (f in intersect(factors_test, names(dts3))) dts3[[f]] <- as.factor(dts3[[f]])

nums <- c("city_dev_score", "hours_of_training")
for (n in nums) {
  if (n %in% names(dt3)) dt3[[n]] <- as.numeric(dt3[[n]])
  if (n %in% names(dts3)) dts3[[n]] <- as.numeric(dts3[[n]])
}

# 3. Preprocessing 3 specific drops: sex, employer_type, employer_size
drop_cols <- c("sex", "employer_type", "employer_size")
dt3 <- dt3[, !(names(dt3) %in% drop_cols)]
dts3 <- dts3[, !(names(dts3) %in% drop_cols)]

# 4. KNN Imputation (k = 5) using VIM
columns_to_impute <- setdiff(names(dt3), c("id", "target"))
data_imputed <- kNN(dt3[, columns_to_impute], k = 5)
dt3[, columns_to_impute] <- data_imputed[, !grepl("_imp$", names(data_imputed))]

# 5. Apply KNN Imputation to Test set using train neighbors
rows_with_na <- which(apply(dts3[, columns_to_impute], 1, function(x) any(is.na(x))))
for (row_idx in rows_with_na) {
  test_row <- dts3[row_idx, columns_to_impute, drop = FALSE]
  temp_data <- rbind(dt3[, columns_to_impute], test_row)
  temp_imputed <- kNN(temp_data, k = 5)
  temp_clean <- temp_imputed[, !grepl("_imp$", names(temp_imputed))]
  dts3[row_idx, columns_to_impute] <- temp_clean[nrow(temp_clean), columns_to_impute]
}

cat("Preprocessing Strategy 3 completed successfully.\n")
