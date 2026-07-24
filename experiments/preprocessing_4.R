# Preprocessing Strategy 4: Row Filtering, Binning, and Tree-Ready Setup
# Objective: Remove heavily missing rows (>4 NAs), bin work experience into categories, drop problematic columns, and preserve NAs for Tree models.

library(tidyverse)

# Load data
train_path <- file.path("..", "data", "training_data.csv")
test_path  <- file.path("..", "data", "test_data.csv")

if (!file.exists(train_path)) {
  train_path <- file.path("data", "training_data.csv")
  test_path  <- file.path("data", "test_data.csv")
}

dt4 <- read.csv(train_path, stringsAsFactors = FALSE)
dts4 <- read.csv(test_path, stringsAsFactors = FALSE)

# 1. Clean missing indicators
dt4[dt4 == ""] <- NA
dts4[dts4 == ""] <- NA

# Remove rows with more than 4 missing values in training set
na_counts <- rowSums(is.na(dt4))
dt4 <- dt4[na_counts <= 4, ]

# 2. Drop high NA / redundant columns: location_city, sex, employer_type, employer_size
drop_cols <- c("location_city", "sex", "employer_type", "employer_size")
dt4 <- dt4[, !(names(dt4) %in% drop_cols)]
dts4 <- dts4[, !(names(dts4) %in% drop_cols)]

# 3. Bin work_experience_years into categories (<1, 1-2, 3-5, 6-10, 11-15, >20, Missing)
bin_experience <- function(exp_col) {
  exp_num <- as.numeric(gsub("[^0-9]", "", exp_col))
  case_when(
    is.na(exp_col) ~ "Missing",
    exp_col == "<1" | exp_num < 1 ~ "<1",
    exp_num >= 1 & exp_num <= 2 ~ "1-2",
    exp_num >= 3 & exp_num <= 5 ~ "3-5",
    exp_num >= 6 & exp_num <= 10 ~ "6-10",
    exp_num >= 11 & exp_num <= 15 ~ "11-15",
    exp_col == ">20" | exp_num > 20 ~ ">20",
    TRUE ~ "Other"
  )
}

if ("work_experience_years" %in% names(dt4)) {
  dt4$work_experience_years <- as.factor(bin_experience(dt4$work_experience_years))
}
if ("work_experience_years" %in% names(dts4)) {
  dts4$work_experience_years <- as.factor(bin_experience(dts4$work_experience_years))
}

# 4. Convert categorical columns to factors and target relevel
factors <- c("prior_experience", "university_enrollment", "academic_qualification", 
             "field_of_study", "time_since_last_job_change", "target")
for (f in intersect(factors, names(dt4))) dt4[[f]] <- as.factor(dt4[[f]])

factors_test <- setdiff(factors, "target")
for (f in intersect(factors_test, names(dts4))) dts4[[f]] <- as.factor(dts4[[f]])

cat("Preprocessing Strategy 4 completed successfully (Tree-ready dataset created).\n")
