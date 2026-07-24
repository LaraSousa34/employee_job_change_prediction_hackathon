# Environment & Dependencies

This repository requires **R (v4.0.0 or higher)** along with several R packages for data preparation, missing value imputation, cross-validation, and classification modeling.

## Prerequisites

- **R**: >= 4.0.0
- **RStudio** (recommended) or any R runtime environment.

## Required R Packages

| Package | Purpose |
| :--- | :--- |
| `tidyverse` / `dplyr` | Data manipulation, transformation, and binning |
| `caret` | Cross-validation framework and metric calculation |
| `VIM` | K-Nearest Neighbors (`kNN`) imputation for missing values |
| `ROSE` | Undersampling / oversampling for class imbalance treatment |
| `rpart` / `rpart.plot` | Decision tree modeling and visualizations |
| `OneR` | Rule-based classification algorithm |
| `e1071` | Naive Bayes implementation |
| `pROC` | Receiver Operating Characteristic (ROC) & AUC computation |
| `yardstick` | Model evaluation metrics (Accuracy, F1 score, Brier score) |
| `Boruta` | Feature selection algorithm |
| `FNN` | Fast Nearest Neighbor search for KNN classification |

## Installation Script

You can install all necessary R dependencies by executing the following command in your R console:

```R
install.packages(c(
  "tidyverse", "dplyr", "caret", "VIM", "ROSE", "rpart", "rpart.plot",
  "OneR", "e1071", "pROC", "yardstick", "Boruta", "FNN", "DMwR2", "ggplot2"
))
```
