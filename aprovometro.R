library(Matrix)     
library(xgboost)    
library(dplyr)

seed <- 123
set.seed(seed)

setwd('/Users/sauloguerra/Development/desafioleg/')
prop.tram <- read.csv2('data/PROPOSICOES_E_TRAMITACOES.csv')

base <- prop.tram %>% 
  group_by(NOM_PROPOSICAO) %>% 
  mutate(TRAMITACOES = length(ORDEM_TRAM),
         COMISSOES = length(unique(COD_ORGAO))) %>% 
  group_by(NOM_PROPOSICAO, SIG_TIPO_PROPOSICAO, 
           NOM_PARTIDO_POLITICO, SIG_UF, TEX_REGIAO_GEOGRAFICA_AUTOR, COD_SEXO,
           TRAMITACOES, COMISSOES, TARGET) %>% 
  distinct() %>% 
  ungroup() %>% 
  arrange(TARGET) %>% 
  mutate(TRAIN_TEST = ifelse(is.na(TARGET), 0, 1)) #teste 0, treino 1

# View(base)


train <- base %>% filter(!is.na(TARGET_FINAL)) %>% select(TARGET_FINAL)
y_train = train$TARGET_FINAL
train_test = base %>% select(-TARGET_FINAL, -NOM_PROPOSICAO)

nomes <- sapply(train_test, class)
categoricas <- names(nomes[nomes=='factor'|nomes=='character'])
numericas <- names(nomes[nomes=='integer'])

train_test[,categoricas] <- apply(train_test[,categoricas], 2, function(x) as.numeric(as.factor(x)))
train_test[] <- apply(train_test[], 2, function(x) as.numeric(x))
str(train_test)

x_train = train_test %>% filter(TRAIN_TEST == 1) %>% select(-TRAIN_TEST)
x_test = train_test %>% filter(TRAIN_TEST == 0) %>% select(-TRAIN_TEST)

dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))

xgb_params = list(
  seed = seed,
  subsample = 0.70,
  colsample_bytree = 1,
  eta = 0.05,
  objective = 'binary:logistic',
  max_depth = 6,
  min_child_weight = 3,
  metric = 'roc'
)

res = xgb.cv(xgb_params,
             dtrain,
             nrounds=5000,
             nfold = 50,
             early_stopping_rounds=10,
             print_every_n = 50,
             verbose= 1,
             maximize = TRUE)

best_nrounds = res$best_iteration

gbdt = xgb.train(xgb_params, dtrain, best_nrounds)

resultado <- data.frame(TARGET = NA)
resultado = as.data.frame(predict(gbdt, dtest))
resultado <- cbind(x_test, resultado)
resultado.final <- base %>% filter(is.na(TARGET))
View(resultado)
View(cbind(resultado, resultado.final))
