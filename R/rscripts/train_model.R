rm(list = ls())

# Pacotes -----------------------------------------------------------------

library(plyr)
library(tidyverse)
library(caret)
library(pROC)
library(Matrix)

# Leitura de dados --------------------------------------------------------

train <- read.csv('produced_data/train.csv')
#train <- train[sample(nrow(train), 10000),]

# Índices para treinamento -----------------------------------------------
index <- list()
for(i in 1:10) index[[i]] <- which(train$fold != i)
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
y <- train[,"TARGET"]
y <- ifelse(y == 0, "NO", "YES")
y <- factor(y, levels = c("YES", "NO"))
x <- train[, -(1:4)]
x <- apply(x, 2, as.numeric)
x <- data.frame(x)

rm(train)
gc()
# Treinamento -------------------------------------------------------------
set.seed(34839)
fit <- train(x = x,
             y = y,
             metric = "AUC",
             method = "xgbTree",
             nthread = 4,
             verbose = 1,
             maximize = TRUE,
             trControl = tr_control)

fit
