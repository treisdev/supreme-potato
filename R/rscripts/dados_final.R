rm(list = ls())
# Data: 18/07/2017

# Pacotes -----------------------------------------------------------------
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


# Stopwords ---------------------------------------------------------------

stop_words <- read.table('https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt')
colnames(stop_words) <- 'word'

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

# Dicionário para áreas temáticas -----------------------------------
dados_area_tematica <- dados_propostas %>% 
  filter(DATA_TRAM <= as.Date("2015-12-31")) %>% 
  select(NOM_PROPOSICAO, AREAS_TEMATICAS_APRESENTACAO) %>% 
  unnest_tokens(word, AREAS_TEMATICAS_APRESENTACAO) %>% 
  mutate(word = tolower(word))

dic_area_tematica <- dados_area_tematica %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  anti_join(stop_words) %>% 
  mutate(nchar = nchar(word)) %>% 
  filter(nchar > 3 & !str_detect(word, "[0-9]")) %>% 
  mutate(rank = rank(-n)) %>% 
  filter(rank <= 200) %>% 
  select(word)

# Dicionário para comissões -----------------------------------
dados_comissoes <- dados_propostas %>% 
  filter(DATA_TRAM <= as.Date("2015-12-31")) %>% 
  select(NOM_PROPOSICAO, DES_ORGAO) %>% 
  unnest_tokens(word, DES_ORGAO) %>% 
  mutate(word = tolower(word))

dic_comissoes <- dados_comissoes %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  anti_join(stop_words) %>% 
  mutate(nchar = nchar(word)) %>% 
  filter(nchar %in% c(3, 4) & !str_detect(word, "[0-9]")) %>% 
  mutate(rank = rank(-n)) %>% 
  filter(rank <= 20) %>% 
  select(word)

# Adiciona maior tramitação de arquivo ----------------------------------------
dados_propostas <- dados_propostas %>% 
  group_by(NOM_PROPOSICAO) %>% 
  mutate(ORDEM_ARQUIVO = ifelse(grepl('^Arquivad', DES_TRAM), ORDEM_TRAM, NA),
         MAX_ORDEM_ARQUIVO = max(ORDEM_ARQUIVO, na.rm = TRUE)) %>% 
  ungroup()

# Dados Treinamento --------------------------------------------------------
# Cria sequência de dados
seq_datas <- seq(ymd('2015-02-01'),ymd('2017-02-01'), by = '1 month') - 1
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

y_train <- y_train %>% 
  rename(TARGET = APROVOU)

# Dados que irão para o app
data_final <- "2017-08-21"
dados_final <- processa_dados(dados_propostas, data_final)
y_final <- dados_final$y
y_final <- y_final %>% 
  rename(TARGET = APROVOU)
x_final <- dados_final$x

rm(dados_final); gc();

# Escrever os objetos
save(y_train, x_train, y_final, x_final,
     file = 'produced_data/final_train.RData')
