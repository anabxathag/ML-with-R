# caret = Classification And REgression Tree

library(caret)
library(tidyverse)

# train test split
# 1. split data
# 2. train
# 3. score
# 4. evaluate

glimpse(mtcars)

# split data 80%: 20%
train_test_split <- function(data, trainRatio=0.7) {
  set.seed(42)
  n <- nrow(data)
  id <- sample(1:n, size=trainRatio*n)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  
  list(train=train_data, test=test_data) 
}

set.seed(42)
splitData <- train_test_split(mtcars, 0.8)
train_data <- splitData$train
test_data <- splitData$test 

# train model
model <- lm(mpg ~ hp + wt + am, data = train_data)

# score model
mpg_pred <- predict(model, newdata = test_data)

# evaluate model
# MAE, MSE, RMSE

mae_metric <- function(actual, prediction) {
  # mean absolute error
  abs_error <- abs(actual - prediction)
  mean(abs_error)
}

mse_metric <- function(actual, prediction) {
  # mean squared error
  sq_error <- (actual - prediction)**2
  mean(sq_error)
}

rmse_metric <- function(actual, prediction) {
  # root mean squared error
  sq_error <- (actual - prediction)**2
  sqrt(mean(sq_error)) 
}

## CARET = Classification And REgression Tree
## Supervised Learning = Prediction

library(caret)

# 1. split data 70:30
splitData <- train_test_split(mtcars, 0.7)
train_data <- splitData[[1]]
test_data <- splitData[[2]]

# 2. train model
# mpg = f(hp, wt, am)
set.seed(42)

ctrl <- trainControl(
  method = "cv", # k-fold golden standard
  number = 5, # k=5
  verboseIter = TRUE
)

lm_model <- train(mpg ~ hp + wt + am, 
               data = train_data,
               method = "lm", 
               trControl = ctrl) 

rf_model <- train(mpg ~ hp + wt + am, 
                  data = train_data,
                  method = "rf", # random forest
                  trControl = ctrl) 

knn_model <- train(mpg ~ hp + wt + am, 
                  data = train_data,
                  method = "knn", 
                  trControl = ctrl) 

# rf > lm > knn
# experimentation

# 3. score model
p <- predict(model, newdata = test_data)

# 4. evaluate model
rmse_metric(test_data$mpg, p)

# 5. save model
saveRDS(model, "linear_regression_v1.RDS")


## on our friends laptop
## read model into R environment
(new_cars <- data.frame(
  hp = c(150, 200, 250),
  wt = c(1.25, 2.2, 2.5),
  am = c(0, 1, 1)
))

model <- readRDS("linear_regression_v1.RDS")

new_cars$mpg_pred <- predict(model, newdata=new_cars)
View(new_cars)
























