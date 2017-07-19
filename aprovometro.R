
library(dplyr)
library(caret)
library(stringr)
library(tidyr)
library(splitstackshape)
library(pROC)

seed <- 123
set.seed(seed)

setwd('/Users/sauloguerra/Development/desafioleg/')
prop.tram <- read.csv2('data/PROPOSICOES_E_TRAMITACOES.csv')

#faz corte
#detecta status REAL por tramitaçao, ignorando "falsos" arquivamentos
teste <- prop.tram %>%
  mutate(DATA_TRAM = as.Date(DATA_TRAM, format="%d/%m/%Y"),
         CORTE = ifelse(DATA_TRAM <= as.Date('01/01/2016', format="%d/%m/%Y"), 1, 0)) %>% 
  group_by(NOM_PROPOSICAO) %>% 
  distinct() %>% 
  arrange(ORDEM_TRAM) %>% 
  mutate(TEVE_CORTE = length(unique(CORTE)),
         STATUS_REAL = ifelse(str_detect(DES_TRAM, '^Transformado n'), 'APROVADA', NA),
         STATUS_REAL = ifelse((ARQUIVADA == 1) & (ORDEM_TRAM == last(ORDEM_TRAM[str_detect(DES_TRAM, '^Arquivad')])), 'ARQUIVADA', STATUS_REAL),
         STATUS_REAL = ifelse(is.na(STATUS_REAL), 'TRAMITANDO', STATUS_REAL),
         STATUS_REAL = ifelse(ORDEM_TRAM > last(ORDEM_TRAM[STATUS_REAL=='ARQUIVADA'|STATUS_REAL=='APROVADA']), 'DESCARTADO', STATUS_REAL)
  ) %>% 
  ungroup() %>% 
  filter(TEVE_CORTE == 2)



# Cria dummies de comissoes
comissoes <- prop.tram %>% 
  mutate(COMISSAO = word(DES_ORGAO)) %>% 
  select(NOM_PROPOSICAO, COMISSAO, DES_ORGAO) %>% 
  mutate(TIPO = ifelse(startsWith(COMISSAO, 'P'), 1, 0), #COMISSOES ESPECIAIS
         TIPO = ifelse(COMISSAO == 'PLEN', 0, TIPO), 
         TIPO = ifelse(COMISSAO %in% c('MPV00101', 'GTFICHA', 'GTTAXI', 'CPIPETRO'), 1, TIPO), #COMISSOES ESPECIAIS
         COMISSAO = ifelse(TIPO == 1, 'ESPECIAL', COMISSAO)) %>% #COMISSOES ESPECIAIS
  select(NOM_PROPOSICAO, COMISSAO) %>% 
  distinct()
  
comissoes <- dcast.data.table(cSplit(comissoes, "COMISSAO", ",", "long"), 
                              NOM_PROPOSICAO ~ COMISSAO, value.var = "COMISSAO", 
                              fun.aggregate = length)
nome.comissoes <- names(comissoes)
names(comissoes) <- c('NOM_PROPOSICAO', paste0('C', 1:(length(nome.comissoes)-1))) #MUDA OS NOMES
# View(comissoes)
comissoes <- as.data.frame(comissoes)
str(comissoes)

# CRIA DUMMIES DE temas
temas <- prop.tram %>% 
  select(NOM_PROPOSICAO, AREAS_TEMATICAS_APRESENTACAO) %>% 
  distinct()

temas <- dcast.data.table(cSplit(temas, "AREAS_TEMATICAS_APRESENTACAO", ",", "long"), 
                          NOM_PROPOSICAO ~ AREAS_TEMATICAS_APRESENTACAO, value.var = "AREAS_TEMATICAS_APRESENTACAO", 
                          fun.aggregate = length)

nome.temas <- names(temas)
names(temas) <- c('NOM_PROPOSICAO', paste0('T', 1:(length(nome.temas)-1))) # muda os nomes
temas <- as.data.frame(temas)
# View(temas)

detach("package:splitstackshape", unload=TRUE)




base <- prop.tram %>% 
  select(NOM_PROPOSICAO, SIG_TIPO_PROPOSICAO, NOM_PARTIDO_POLITICO, SIG_UF, TEX_REGIAO_GEOGRAFICA_AUTOR,
         COD_SEXO, ORDEM_TRAM, COD_ORGAO, TARGET) %>% 
  group_by(NOM_PROPOSICAO) %>% 
  mutate(TRAMITACOES = length(ORDEM_TRAM),
         COMISSOES = length(unique(COD_ORGAO))
         # NOM_PARTIDO_POLITICO = as.character(as.numeric(NOM_PARTIDO_POLITICO)),
         # TEX_REGIAO_GEOGRAFICA_AUTOR = as.character(as.numeric(TEX_REGIAO_GEOGRAFICA_AUTOR)),
         # SIG_UF = as.character(as.numeric(SIG_UF)),
         # COD_SEXO = as.character(as.numeric(COD_SEXO)),
         # SIG_TIPO_PROPOSICAO = as.character(as.numeric(SIG_TIPO_PROPOSICAO))
         ) %>% 
  select(NOM_PROPOSICAO, SIG_TIPO_PROPOSICAO, NOM_PARTIDO_POLITICO, SIG_UF, TEX_REGIAO_GEOGRAFICA_AUTOR, 
         COD_SEXO, TRAMITACOES, COMISSOES, TARGET) %>% 
  group_by(NOM_PROPOSICAO, SIG_TIPO_PROPOSICAO, NOM_PARTIDO_POLITICO, SIG_UF, TEX_REGIAO_GEOGRAFICA_AUTOR, 
           COD_SEXO, TRAMITACOES, COMISSOES, TARGET) %>% 
  left_join(temas) %>% 
  left_join(comissoes) %>% 
  distinct() %>% 
  arrange(TARGET) %>% 
  ungroup() %>%
  mutate(TRAIN_TEST = ifelse(is.na(TARGET), 0, 1),
         TARGET = ifelse(TARGET == 1, 'YES', 'NO')) %>% 
  mutate(TARGET = as.factor(TARGET))

# View(base)
# dim(base)

base.treino <- base %>% filter(TRAIN_TEST == 1) %>% select(-NOM_PROPOSICAO, -TRAIN_TEST,
                                                           -C40, -C18, -C8) #sem variação
# dim(base.treino)
# View(base.treino)

base.treino[] <- lapply(base.treino, factor)

ctrl_1 <- trainControl(method = "cv",
                     # number = 2,
                     repeats = 2,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     verboseIter = TRUE,
                     sampling = 'smote') #up, down, smote

fit_1 <- train(TARGET ~ .,
                  data = base.treino,
                  method = "xgbTree",
                  verbose = TRUE,
                  metric = "ROC",
                  trControl = ctrl_1)


test_roc <- function(model, data) {
  roc(data$TARGET,
      predict(model, data, type = "prob")[, "YES"])
}

fit_1 %>%
  test_roc(data = base.treino) %>%
  auc()

base.teste <- base %>% filter(TRAIN_TEST == 0) %>% select(-TRAIN_TEST,
                                                           -C40, -C18, -C8)
dim(base.teste)
teste <- fit_1 %>% predict(base.teste, type='prob')
final <- cbind(base.teste, teste)
View(final)

# xgb.grid <- expand.grid(nrounds=1:6 * 10,
#                      max_depth=c(4, 6, 8, 10),
#                      eta=1:3 * 0.1)
# 
# ctrl <- trainControl('cv', number=4, repeats=3,
#                     classProbs=TRUE, summaryFunction=twoClassSummary,
#                     sampling='down', verboseIter=TRUE)
# fit <- train(x=x.train, y=y.train.fact, method='xgbTree', metric='ROC',
#             trControl=ctrl, subsample=0.8, tuneGrid=xgb.grid)

