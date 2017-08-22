rm(list = ls())
options(na.action='na.pass')

# Data: 18/07/2017

# Pacotes -----------------------------------------------------------------
library(text2vec)
library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(tidytext)
library(Matrix)

# Função para criação de dados de treinamento -------------------------------
source('functions/processa_dados.R')

# Leitura de dados --------------------------------------------------------

dados_propostas <- read_csv2('data/PROPOSICOES_E_TRAMITACOES.csv')

# Converter dados de datas ------------------------------------------------

dados_propostas$DATA_TRAM <- as.Date(dados_propostas$DATA_TRAM, "%d/%m/%Y")
dados_propostas$DATAPRESENTACAOPROPOSICAO <- as.Date(dados_propostas$DATAPRESENTACAOPROPOSICAO,
                                                     "%d-%m-%Y")
dados_propostas$DATTRANSFPROPOSICAOLEI <- as.Date(dados_propostas$DATTRANSFPROPOSICAOLEI,
                                                  "%d-%m-%Y")

# Adiciona id
dados_propostas <- dados_propostas %>% 
  mutate(id = 1:n())

# Stopwords ---------------------------------------------------------------

stop_words <- read.table('https://tinyurl.com/y8yzb7jf')
colnames(stop_words) <- 'word'

# Vocabulário para tramitação ---------------------------------------------
it <- dados_propostas %>% 
  filter(DATA_TRAM <= as.Date("2015-12-31")) %>% 
  group_by(NOM_PROPOSICAO) %>% 
  summarise(DES_TRAM = paste0(DES_TRAM, collapse = " ")) %>% 
  arrange(NOM_PROPOSICAO) %>% 
  pull(DES_TRAM) %>%
  str_replace_all(., pattern = "[0-9]", replacement = "") %>% 
  itoken(.,
         preprocessor = tolower,
         tokenizer = word_tokenizer, 
         progressbar = FALSE)
vocab <-  create_vocabulary(it, stopwords = as.character(stop_words$word),
                            ngram = c(ngram_min = 1L,
                                      ngram_max = 2L))
pruned_vocab  <- prune_vocabulary(vocab,
                                  vocab_term_max = 300,
                                  doc_proportion_max = 0.3,
                                  doc_proportion_min = 0.01)
vectorizer <- vocab_vectorizer(pruned_vocab)

dtm_train <- create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_train <- fit_transform(dtm_train, tfidf)

# Dicionário para tramitações -----------------------------------
dados_tramitacoes <- dados_propostas %>% 
  filter(DATA_TRAM <= as.Date("2015-12-31")) %>% 
  select(NOM_PROPOSICAO, DES_TRAM) %>% 
  unnest_tokens(word, DES_TRAM) %>% 
  mutate(word = tolower(word))

dic_tram <- dados_tramitacoes %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  anti_join(stop_words) %>% 
  mutate(nchar = nchar(word)) %>% 
  filter(nchar > 3 & !str_detect(word, "[0-9]")) %>% 
  mutate(rank = rank(-n)) %>% 
  filter(rank <= 200) %>% 
  select(word)

# Vetor de temas -------------------------------------------------------
temas <- dados_propostas %>% 
  filter(DATA_TRAM <= as.Date("2015-12-31")) %>% 
  pull(AREAS_TEMATICAS_APRESENTACAO)
temas <- str_split(sort(unique(temas)),
                   pattern = "(,(?=\\S)|:)") %>%
  unlist() %>%
  unique()

# Vetor de comissões ------------------------------------------------------

comissoes <- dados_propostas %>% 
  filter(DATA_TRAM <= as.Date("2015-12-31")) %>% 
  pull(DES_ORGAO) %>% 
  str_extract(pattern = "(?:^|(?:[.!?]\\s))(\\w+)") %>% 
  table %>% 
  sort(decreasing = TRUE) %>% 
  names %>% .[1:29]

#####################################3

processa_dados <- function(dados_propostas, data, meses = 6){
  data <- as.Date(data)
  data_6m <- data %m+% months(meses)
  dados_data_base <- dados_propostas %>% 
    filter(DATA_TRAM <= data) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVADA = ifelse(grepl('^Transformado n', DES_TRAM), 1, 0),
           ARQUIVADA = ifelse(grepl('^Arquiva|^ARQUIVA', DES_TRAM), 1, 0),
           APENSADA = ifelse(str_detect(pattern = 'apensado ao PL|Apense-se ao P|Apense-se à',
                                   DES_TRAM), 1, 0),
           DESARQUIVADA = ifelse(grepl('^Desarquivad', DES_TRAM), 1, 0),
           ARQUIVADA= ifelse(max(ARQUIVADA * ORDEM_TRAM) > max(DESARQUIVADA * ORDEM_TRAM),
                             1, 0),
           STATUS_REAL = sum(APROVADA + ARQUIVADA + APENSADA, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(STATUS_REAL == 0) %>% 
    select(-STATUS_REAL, -APROVADA)
  
  # Checa quais propostas foram finalizadas entre janeiro e junho de 2015
  dados_data_posterior <- dados_propostas %>% 
    filter(DATA_TRAM > data & DATA_TRAM <= data_6m) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVOU = ifelse(grepl('^Transformado n', DES_TRAM), 1, 0)) %>% 
    filter(APROVOU == 1) %>% 
    select(NOM_PROPOSICAO, APROVOU)
  
  dados_treino <- left_join(dados_data_base, dados_data_posterior,
                            by = "NOM_PROPOSICAO") %>% 
    replace_na(list(APROVOU = 0)) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVOU = max(APROVOU))
  
  # Cria as features 
  
  # Tempo de tramitação em dias
  dados_treino <- dados_treino %>% 
    mutate(dias_tramitando = as.numeric(as.Date(data) -
                                          DATAPRESENTACAOPROPOSICAO))
  
  # Número de tramitações
  dados_treino <- dados_treino %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(qtd_tram = n()) %>% 
    ungroup()
  
  # Número de dias de envio ao senado federal
  dados_senado_federal <- dados_treino %>% 
    filter(str_detect(tolower(DES_TRAM), "senado federal")) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    summarise(dias_senado_federal = max(as.Date(data) -
                                       DATA_TRAM))
  
  dados_treino <- left_join(dados_treino, dados_senado_federal) %>% 
    replace_na(list(dias_senado_federal = 0))
  
  # Comissões
  comissoes_df_tmp <- dados_treino %>% 
    group_by(NOM_PROPOSICAO) %>% 
    summarise(DES_ORGAO = paste0(DES_ORGAO, collapse = " ")) %>% 
    arrange(NOM_PROPOSICAO)
  comissoes_df <- list()
  for(i in comissoes){
    comissoes_df[[i]] <- as(str_count(comissoes_df_tmp$DES_ORGAO, i) * 1,
                 "sparseVector")
  }
  comissoes_df <- lapply(comissoes_df, as, "sparseMatrix")
  comissoes_df <- do.call(cBind, comissoes_df)
  colnames(comissoes_df) <- paste0("comissao_", comissoes)
  comissoes_df <- data.matrix(comissoes_df) %>% data.frame()
  comissoes_df$NOM_PROPOSICAO <- comissoes_df_tmp$NOM_PROPOSICAO
  
  #dados_treino <- left_join(dados_treino, comissao)
  
  # Tipo de Proposição
  tipo_proposicao_df_tmp <- dados_treino %>% 
    select(NOM_PROPOSICAO, SIG_TIPO_PROPOSICAO) %>% 
    distinct() %>% 
    arrange(NOM_PROPOSICAO)
  tipo_proposicao_df <- sparse.model.matrix(~ SIG_TIPO_PROPOSICAO - 1,
                                            data = tipo_proposicao_df_tmp)
  tipo_proposicao_df <- data.matrix(tipo_proposicao_df) %>% data.frame()
  tipo_proposicao_df$NOM_PROPOSICAO <- tipo_proposicao_df_tmp$NOM_PROPOSICAO
  
  # Temas
  tema_df_tmp <- dados_treino %>% 
    select(NOM_PROPOSICAO, AREAS_TEMATICAS_APRESENTACAO) %>% 
    distinct() %>% 
    arrange(NOM_PROPOSICAO)
  tema_df <- list()
  for(i in temas){
    tema_df[[i]] <- as(str_detect(tema_df_tmp$AREAS_TEMATICAS_APRESENTACAO, i) * 1,
                       "sparseVector")
  }
  tema_df <- lapply(tema_df, as, "sparseMatrix")
  tema_df <- do.call(cBind, tema_df)
  colnames(tema_df) <- paste0("tema_", temas)
  tema_df <- data.matrix(tema_df) %>% data.frame()
  tema_df$NOM_PROPOSICAO <- tema_df_tmp$NOM_PROPOSICAO
  
  # bag of words para as tramitações
  tram_df_tmp <- tram_df <- dados_treino %>% 
    group_by(NOM_PROPOSICAO) %>% 
    summarise(DES_TRAM = paste0(DES_TRAM, collapse = " ")) %>% 
    arrange(NOM_PROPOSICAO)
  
  tram_df <- tram_df_tmp %>% 
    pull(DES_TRAM) %>% 
    tolower() %>% 
    word_tokenizer() %>% 
    itoken() %>% 
    create_dtm(., vectorizer) %>% 
    transform(tfidf)
  
  colnames(tram_df) <- paste0("tram_", colnames(tram_df))
  tram_df <- data.matrix(tram_df) %>% data.frame()
  tram_df$NOM_PROPOSICAO <- tram_df_tmp$NOM_PROPOSICAO
  
  # Número de tramitações nos últimos 30 dias
  tram_30_dias <- dados_treino %>% 
    filter(DATA_TRAM >= data - 30  & DATA_TRAM <= data) %>%
    group_by(NOM_PROPOSICAO) %>% 
    summarise(n_tram_30d = n()) %>% 
    replace_na(list(n_tram_30d = 0)) %>%
    ungroup()
  
  dados_treino <- left_join(dados_treino, tram_30_dias)
  
  # Número de tramitações nos últimos 30 dias
  tram_90_dias <- dados_treino %>% 
    filter(DATA_TRAM >= data - 90  & DATA_TRAM <= data) %>%
    group_by(NOM_PROPOSICAO) %>% 
    summarise(n_tram_90d = n()) %>% 
    replace_na(list(n_tram_90d = 0)) %>% 
    ungroup()
  
  dados_treino <- left_join(dados_treino, tram_90_dias)
  
  # Fim
  dados_treino <- dados_treino %>% 
    replace_na(list(APROVOU = 0,
                    n_tram_30d = 0,
                    n_tram_90d = 0)) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVOU = max(APROVOU)) %>% 
    select(NOM_PROPOSICAO, APROVOU, dias_tramitando, qtd_tram,
           n_tram_30d, n_tram_90d, dias_senado_federal) %>% 
    mutate(data = data) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(NOM_PROPOSICAO) %>% 
    left_join(comissoes_df) %>% 
    left_join(tema_df) %>% 
    left_join(tipo_proposicao_df) %>% 
    left_join(tram_df)
   
  
  y <- dados_treino %>% 
    select(APROVOU, NOM_PROPOSICAO, data) %>% 
    data.frame()
  
  x <- dados_treino %>% 
    select(-APROVOU, -NOM_PROPOSICAO, -data)
  x <- sparse.model.matrix(~ . - 1, data = x)
  # x <- cBind(x, tipo_proposicao_df)
  # x <- cBind(x, comissoes_df)
  # x <- cBind(x, tema_df)
  # x <- cBind(x, tram_df)
  
  return(list(x = x, y = y))
}

# Dados treinamento -----------------------------------------------------
seq_datas <- seq(ymd('2014-01-01'),ymd('2016-01-01'), by = '1 month') - 1
dados <- lapply(seq_datas, processa_dados,
                dados_propostas = dados_propostas)
# Combina dados em um único data.frame
x_train <- dados[[1]]$x
for(i in 2:length(dados)){
  x_train <- rbind(x_train,
                   dados[[i]]$x)
}

y_train <- dados[[1]]$y
for(i in 2:length(dados)){
  y_train <- rbind(y_train,
                   dados[[i]]$y)
}
rm(dados); gc();
  
# Criar folds
set.seed(38394)
folds <- y_train %>%
  group_by(NOM_PROPOSICAO) %>%
  summarise(APROVOU = max(APROVOU)) %>% 
  group_by(APROVOU) %>%
  mutate(fold = sample(1:10, size = n(), replace = T)) %>% 
  ungroup() %>% 
  select(NOM_PROPOSICAO, fold)

y_train <- left_join(y_train, folds)

# Dados teste -----------------------------------------------------
seq_datas <- seq(ymd('2016-07-01'),ymd('2017-01-01'), by = '1 month') - 1
dados <- lapply(seq_datas, processa_dados,
                dados_propostas = dados_propostas)
# Combina dados em um único data.frame
x_test <- dados[[1]]$x
for(i in 2:length(dados)){
  x_test <- rbind(x_test,
                   dados[[i]]$x)
}

y_test <- dados[[1]]$y
for(i in 2:length(dados)){
  y_test <- rbind(y_test,
                   dados[[i]]$y)
}
rm(dados); gc();

# Escrever os objetos
save(y_train, x_train, y_test, x_test,
     file = 'produced_data/train_test.RData')

# Dados para treinamento do modelo final -------------------------------
max_data_tram <- max(dados_propostas$DATA_TRAM)

data_final_treino <- max_data_tram %m-% months(6)
data_inicio_treino <- data_final_treino %m-% months(24)

seq_datas <- seq(data_inicio_treino, data_final_treino, by = '1 month')
dados <- lapply(seq_datas, processa_dados,
                dados_propostas = dados_propostas)
# Combina dados em um único data.frame
x_train <- dados[[1]]$x
for(i in 2:length(dados)){
  x_train <- rbind(x_train,
                   dados[[i]]$x)
}

y_train <- dados[[1]]$y
for(i in 2:length(dados)){
  y_train <- rbind(y_train,
                   dados[[i]]$y)
}
rm(dados); gc();

# Dados para predições - período mais recente
dados_final <- processa_dados(dados_propostas, max_data_tram)

x_final <- dados_final$x
y_final <- dados_final$y

# Salvar arquivo final
save(y_train, x_train, y_final, x_final,
     file = 'produced_data/final_train.RData')
