rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(plyr)
library(tidyverse)
library(caret)
library(pROC)
library(Matrix)

# Leitura de dados --------------------------------------------------------

load('produced_data/train_test.RData')
#train <- read.csv("produced_data/train.csv")
#train <- train[sample(nrow(train), 10000),]

# Índices para treinamento -----------------------------------------------
index <- list()
for(i in 1:10) index[[i]] <- which(y_train$fold != i)
#train <- train[,-ncol(train)]

# Criar objeto de validação

tr_control <- trainControl(method = "cv",
                           index = index,
                           classProbs = TRUE,
                           summaryFunction = prSummary,
                           sampling = "smote",
                           verboseIter = TRUE,
                           savePredictions = FALSE)


# Separar target e features -----------------------------------------------
y <- y_train[,"TARGET"]
y <- ifelse(y == 0, "NO", "YES")
y <- factor(y, levels = c("YES", "NO"))
x_train <- data.matrix(x_train)
# x <- train[, -(1:4)]
# rm(train)
# gc()
# x <- apply(x, 2, as.numeric)
# x <- data.frame(x)

# Treinamento -------------------------------------------------------------
set.seed(34839)
xgb_grid <- expand.grid(max_depth = 1:10,
                                   eta = c(0.01, 0.05, 0.1),
                                   nrounds = seq(100, 2000, 100),
                                   subsample = c(0.2, 0.5, 0.8, 1),
                                   colsample_bytree = c(0.1, 0.3, 0.5, 0.7),
                                   min_child_weight = 1,
                                   gamma = 0)
xgb_grid <- xgb_grid[sample(nrow(xgb_grid), 30),]


fit <- train(x = x_train,
             y = y,
             metric = "Precision",
             tuneGrid = xgb_grid,
             method = "xgbTree",
             nthread = 4,
             verbose = 1,
             maximize = TRUE,
             trControl = tr_control)

fit

# Otimização bayesiana
xgb_fit_bayes <- function(max_depth, eta, nrounds,
                          subsample, colsample_bytree
                          ){
  txt <- capture.output(
    fit <- train(x = x_train,
                 y = y,
                 metric = "AUC",
                 tuneGrid = data.frame(
                   max_depth = max_depth,
                   eta = eta,
                   nrounds = nrounds,
                   subsample = subsample,
                   colsample_bytree = colsample_bytree,
                   min_child_weight = 1,
                   gamma = 0
                 ),
                 method = "xgbTree",
                 nthread = 4,
                 verbose = 1,
                 maximize = TRUE,
                 trControl = tr_control)
  )
  
  list(Score = getTrainPerf(fit)[, "TrainAUC"], Pred = 0)
}

# Limites
bounds <- list(
  max_depth = c(1L, 40L),
  eta = c(0.01, 0.1),
  nrounds = c(100L, 4000L),
  subsample = c(0.1, 1),
  colsample_bytree = c(0.3, 1)
)

# Grid inicial
initial_grid <- fit$results[,c("max_depth", "eta", "nrounds",
                               "subsample", "colsample_bytree",
                               "AUC")]
names(initial_grid)[6] <- "Value"

# Otimização
library(rBayesianOptimization)

set.seed(4729)

ba_search <- BayesianOptimization(xgb_fit_bayes,
                                  bounds = bounds,
                                  init_grid_dt = initial_grid,
                                  init_points = 0,
                                  n_iter = 30,
                                  acq = "ucb",
                                  kappa = 1, 
                                  eps = 0.0,
                                  verbose = TRUE)
# Teste -------------------------------------------------------------------
test2 <- readRDS('produced_data/teste.RDS')
for(i in unique(test2$data)){
  print(i)
  test <- test2 %>% filter(data == i)
  pred <- predict(fit, data.frame(test))
  y_test <- test[,"TARGET"]
  y_test <- ifelse(y_test == 0, "NO", "YES")
  y_test <- factor(y_test, levels = c("YES", "NO"))
  confusionMatrix(pred, y_test) %>% print()
}

