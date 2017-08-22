rm(list = ls())
# Pacotes -----------------------------------------------------------------

library(xgboost)
library(tidyverse)
library(DMwR)
library(ModelMetrics)
library(rBayesianOptimization)

# Load Data ---------------------------------------------------------------

load('produced_data/train_test.RData')

# Folds -------------------------------------------------------------------

index <- list()
for(i in 1:10) index[[i]] <- which(y_train$fold != i)

# Função para xgb ---------------------------------------------------------

xgb_fit_bayes <- function(max_depth, eta, nrounds, subsample, colsample_bytree){
  
  f1_score <- vector(mode = "numeric", length = 10)
  for(i in 1:10){
    # Treino
    x_train_tmp <- as.matrix(x_train[index[[i]], ])
    y_train_tmp <- y_train[index[[i]], ] %>% 
      select(APROVOU) %>% 
      mutate(APROVOU = ifelse(APROVOU == 1, "YES", "NO"),
             APROVOU = factor(APROVOU, levels = c("NO", "YES")))
    row.names(y_train_tmp) <- NULL
    row.names(x_train_tmp) <- NULL
    x_train_tmp <- as.data.frame(cbind(y_train_tmp, x_train_tmp))
    data_smote <- SMOTE(APROVOU ~ ., data = x_train_tmp,
                        perc.over = 2000, perc.under = 105)
    table(data_smote$APROVOU)
    rm(x_train_tmp); gc();
    
    dtrain <- xgb.DMatrix(data = data.matrix(data_smote[, -1]),
                          label = as.numeric(data_smote[, "APROVOU"]) - 1)
    # Validação
    x_val <- x_train[-index[[i]], ]
    y_val <- y_train[-index[[i]], ] %>% 
      mutate(APROVOU = ifelse(APROVOU == 1, "YES", "NO"),
             APROVOU = factor(APROVOU, levels = c("NO", "YES"))) %>% 
      .[["APROVOU"]]
    dval <- xgb.DMatrix(data = data.matrix(x_val),
                        label = as.numeric(y_val) - 1)
    
    parametros <- list(
      objective = "reg:logistic",
      max_depth = max_depth,
      eta = eta,
      colsample_bytree = colsample_bytree,
      subsample = subsample,
      eval_metric = "auc"
    )
    
    fit <- xgb.train(params = parametros,
                     data = dtrain,
                     nrounds = nrounds,
                     maximize = TRUE)
    pred <- predict(fit, dval)
    f1_score[i] <- f1Score(as.numeric(y_val) - 1, pred)
    print(precision(as.numeric(y_val) - 1, pred))
    print(recall(as.numeric(y_val) - 1, pred))
    print(f1_score)
  }
  
  list(Score = mean(f1_score), Pred = 0)
  
}


# Limites 
bounds <- list(
  max_depth = c(1L, 40L),
  eta = c(0.01, 0.1),
  nrounds = c(100L, 4000L),
  subsample = c(0.1, 1),
  colsample_bytree = c(0.1, 1)
)

set.seed(43890)

ba_search <- BayesianOptimization(xgb_fit_bayes,
                                  bounds = bounds,
                                  init_points = 20,
                                  n_iter = 20,
                                  acq = "ucb",
                                  kappa = 1, 
                                  eps = 0.0,
                                  verbose = TRUE)

result <- xgb_fit_bayes(max_depth = 22, eta = 0.0393, nrounds = 2937,
              subsample = 0.3230, colsample_bytree = 0.3689)


# Modelo Final ------------------------------------------------------------
xgb_fit_final <- function(max_depth, eta, nrounds, subsample, colsample_bytree){
  
    # Treino
    x_train_tmp <- as.matrix(x_train)
    y_train_tmp <- y_train %>% 
      select(APROVOU) %>% 
      mutate(APROVOU = ifelse(APROVOU == 1, "YES", "NO"),
             APROVOU = factor(APROVOU, levels = c("NO", "YES")))
    row.names(y_train_tmp) <- NULL
    row.names(x_train_tmp) <- NULL
    x_train_tmp <- as.data.frame(cbind(y_train_tmp, x_train_tmp))
    data_smote <- SMOTE(APROVOU ~ ., data = x_train_tmp,
                        perc.over = 2000, perc.under = 105)
    table(data_smote$APROVOU)
    rm(x_train_tmp); gc();
    
    dtrain <- xgb.DMatrix(data = data.matrix(data_smote[, -1]),
                          label = as.numeric(data_smote[, "APROVOU"]) - 1)
 
    # Parâmetros
    parametros <- list(
      objective = "binary:logistic",
      max_depth = max_depth,
      eta = eta,
      colsample_bytree = colsample_bytree,
      subsample = subsample,
      eval_metric = "auc"
    )
    
    fit <- xgb.train(params = parametros,
                     data = dtrain,
                     nrounds = nrounds,
                     maximize = TRUE)
   return(fit)
  
}

set.seed(334342)
fit <- xgb_fit_final(max_depth = 22, eta = 0.0393, nrounds = 1965,
                     subsample = 0.7631, colsample_bytree = 0.2593)
dtest <- xgb.DMatrix(data = x_test)

y_test$pred <- predict(fit, dtest)
y_test %>% 
  group_by(data) %>% 
  summarise(precision = precision(APROVOU, pred, cutoff = 0.5),
            recall = recall(APROVOU, pred, cutoff = 0.5),
            f1score = f1Score(APROVOU, pred, cutoff = 0.5))

results <- data.frame()
for(i in as.character(unique(y_test$data))){
  print(i)
  x_test_tmp <- x_test[which(y_test$data == i),]
  y_test_tmp <- y_test$APROVOU[which(y_test$data == i)]
  dtest <- xgb.DMatrix(data = x_test_tmp)
  pred <- predict(fit, dtest)
  results_tmp <- data.frame(date = i,
                            precision = ModelMetrics::precision(y_test_tmp, pred),
                            recall = ModelMetrics::recall(y_test_tmp, pred),
                            f1score = f1Score(y_test_tmp, pred))
  results <- bind_rows(results, results_tmp)
  rm(results_tmp)
}

results
    