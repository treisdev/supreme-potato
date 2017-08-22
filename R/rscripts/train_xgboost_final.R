rm(list = ls())
# Pacotes -----------------------------------------------------------------

library(xgboost)
library(tidyverse)
library(DMwR)
library(ModelMetrics)
library(rBayesianOptimization)

# Load Data ---------------------------------------------------------------

load('produced_data/final_train.RData')

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

set.seed(44439)
fit <- xgb_fit_final(max_depth = 22, eta = 0.0393, nrounds = 1965,
                     subsample = 0.7631, colsample_bytree = 0.2593)

dfinal <- xgb.DMatrix(data = x_final)
pred <- predict(fit, dfinal)
y_final$chance <- pred
y_final$velocidade <- x_final[,"n_tram_30d"]
y_final$qtd_tramitacoes <- x_final[, "qtd_tram"]

write.csv(y_final, 'produced_data/dados_chance.csv', row.names = FALSE)
