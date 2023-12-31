## mlbench rpart randomForest class glmnet

## load library
library(caret)
library(tidyverse)
library(mlbench)
library(MLmetrics)

## see available data
data()

## glimpse data
df <- BostonHousing
glimpse(df)

## clustering => segmentation

subset_df <- df %>%
  select(crim, rm, age, lstat, medv) %>%
  as_tibble()

## test different k (k= 2-5)
result <- kmeans(x = subset_df, centers = 3)

## membership [1,2,3]
subset_df$cluster <- result$cluster

## -----------------------------------------
## lm, knn

df <- as_tibble(df)

# 1. split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size=0.8*n)
train_data <- df[id, ]
test_data <- df[-id, ]

# 2. train model
# medv = f(crim, rm, age)
lm_model <- train(medv ~ crim + rm + age,
                  data = train_data,
                  method = "lm",
                  preProcess = c("center", "scale"))

set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

# grid search tune hyperparameters
k_grid <- data.frame(k = c(3,5,7,9,11))

knn_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "Rsquared",
                   tuneGrid = k_grid,
                   preProcess = c("center", "scale"),
                   trControl = ctrl)

# tuneLength random search
(knn_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "Rsquared",
                   tuneLength = 2,
                   preProcess = c("center", "scale"),
                   trControl = ctrl))

# 3. score
p <- predict(knn_model, newdata=test_data)

# 4. evaluate
RMSE(p, test_data$medv)

## --------------------------------------

## classification problem
data("PimaIndiansDiabetes")

df <- PimaIndiansDiabetes

# library(forcats)
df$diabetes <- fct_relevel(df$diabetes, "pos")

# 1. split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size=0.8*n)
train_data <- df[id, ]
test_data <- df[-id, ]

# 2. train model
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

(knn_model <- train(diabetes ~ .,
                   data = train_data,
                   method = "knn",
                   preProcess = c("center", "scale"),
                   metric = "Accuracy", # PR AUC 
                   trControl = ctrl))

# 3. score
p <- predict(knn_model, newdata = test_data)

# 4. evaluate

confusionMatrix(p, test_data$diabetes, 
                positive="pos",
                mode = "prec_recall")


# -------------------------------------------------
## Logistic Regression

set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

(logit_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "glm",
                    metric = "Accuracy", 
                    trControl = ctrl))

# -------------------------------------------------
## Decision Tree (rpart) 

(tree_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy", 
                    trControl = ctrl))

library(rpart.plot)
rpart.plot(tree_model$finalModel)

# -------------------------------------------------
## Random Forest 
## Model accuracy the higest >= 76%

## mtry = number of features used to train model
## bootstrap sampling

## bagging technique
mtry_grid <- data.frame(mtry = 2:8)

(rf_model <- train(diabetes ~ .,
                     data = train_data,
                     method = "rf",
                     metric = "Accuracy", 
                     tuneGrid = mtry_grid,
                     trControl = ctrl))


## ----------------------------------------------
## compare models 

list_models <- list(knn = knn_model,
                    logistic = logit_model,
                    decisionTree = tree_model,
                    randomForest = rf_model)

result <- resamples(list_models)

summary(result)

## --------------------------------------------
## ridge vs. lasso regression
library(glmnet)

# 0=Ridge, 1=Lasso
glmnet_grid <- expand.grid(alpha = 0:1,
                          lambda = c(0.1, 0.2, 0.3))

(glmnet_model <- train(diabetes ~ .,
                   data = train_data,
                   method = "glmnet",
                   metric = "Accuracy", 
                   tuneLength = 10,
                   trControl = ctrl))


