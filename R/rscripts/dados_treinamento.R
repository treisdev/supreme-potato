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

# Escreve sqlite ----------------------------------------------------------

dados_area_tematica_relacional <- dados_propostas %>% 
  # filter(DATA_TRAM <= as.Date("2015-12-31")) %>% 
  select(NOM_PROPOSICAO, AREAS_TEMATICAS_APRESENTACAO) %>% 
  mutate(tema = str_replace(AREAS_TEMATICAS_APRESENTACAO, ', ', '='),
         tema = str_split(tema, ",")) %>% 
  unnest(tema) %>%
  mutate(tema = str_replace(tema, '=', ', ')) %>% 
  distinct() %>% 
  select(NOM_PROPOSICAO, tema)

dados_comissoes_relacional <- dados_propostas %>% 
  mutate(COMISSAO = word(DES_ORGAO)) %>% 
  select(NOM_PROPOSICAO, COMISSAO, DES_ORGAO) %>% 
  mutate(TIPO = ifelse(startsWith(COMISSAO, 'P'), 1, 0), #COMISSOES ESPECIAIS
         TIPO = ifelse(COMISSAO == 'PLEN', 0, TIPO), 
         TIPO = ifelse(COMISSAO %in% c('MPV00101', 'GTFICHA', 'GTTAXI', 'CPIPETRO'), 1, TIPO), #COMISSOES ESPECIAIS
         COMISSAO = ifelse(TIPO == 1, 'ESPECIAL', COMISSAO)) %>% #COMISSOES ESPECIAIS
  select(NOM_PROPOSICAO, COMISSAO) %>% 
  distinct()

base <- dados_propostas %>% 
  select(NOM_PROPOSICAO, SIG_TIPO_PROPOSICAO, ANO_PROPOSICAO, NUM_PROPOSICAO, DATAPRESENTACAOPROPOSICAO) %>% 
  distinct() %>% 
  ### DADOS FAKE
  mutate(chance = round(runif(n = n(), min = 0, max = 1),2),
         qtd_tramitacoes = round(runif(n = n(), min = 1, max = 200)),
         velocidade = round(runif(n = n(), min = 1, max = 50),1)) ### SUPRIMIR

library(dbplyr)

my_db_file <- "base.sqlite"
my_db <- src_sqlite(my_db_file, create = TRUE)

proposicao <- base
proposicao_tema <- dados_area_tematica_relacional
proposicao_comissao <- dados_comissoes_relacional
copy_to(my_db, proposicao, temporary = FALSE)
copy_to(my_db, dados_area_tematica_relacional, temporary = FALSE)
copy_to(my_db, dados_comissoes_relacional, temporary = FALSE)



# Dados Treinamento --------------------------------------------------------
# Cria sequência de dados
seq_datas <- seq(ymd('2014-01-01'),ymd('2016-01-01'), by = '1 month') - 1
dados <- lapply(seq_datas, processa_dados,
                dados_propostas = dados_propostas)
# Combina dados em um único data.frame
nomes_var <- colnames(dados[[1]]$x)
for(i in 2:length(dados)){
  nomes_var <- intersect(nomes_var, colnames(dados[[i]]$x))
}
x_train <- dados[[1]]$x[,nomes_var]
for(i in 2:length(dados)){
  x_train <- rbind(x_train,
                   dados[[i]]$x[, nomes_var])
}

y_train <- dados[[1]]$y
for(i in 2:length(dados)){
  y_train <- rbind(y_train,
                   dados[[i]]$y)
}
rm(dados); gc();

# Dados Teste
seq_datas <- seq(ymd('2016-07-01'),ymd('2017-01-01'), by = '1 month') - 1
dados_teste <- lapply(seq_datas, processa_dados,
                      dados_propostas = dados_propostas)
# Combina dados em um único data.frame
nomes_var <- colnames(dados_teste[[1]]$x)
for(i in 2:length(dados_teste)){
  nomes_var <- intersect(nomes_var, colnames(dados_teste[[i]]$x))
}
x_test <- dados_teste[[1]]$x[,nomes_var]
for(i in 2:length(dados_teste)){
  x_test <- rbind(x_test,
                   dados_teste[[i]]$x[, nomes_var])
}

y_test <- dados_teste[[1]]$y
for(i in 2:length(dados_teste)){
  y_test <- rbind(y_test,
                   dados_teste[[i]]$y)
}
rm(dados_teste); gc();

# Cria os folds
# Separa as propostas entre aquelas aprovadas (em algum momento) e não aprovadas
# Busca que os folds tenham uma proporção de 0 e 1 similar aos dados originais
set.seed(8934843)
propostas <- y_train %>%
  group_by(NOM_PROPOSICAO) %>%
  summarise(APROVOU = max(APROVOU)) %>% 
  group_by(APROVOU) %>%
  mutate(fold = sample(1:10, size = n(), replace = T)) %>% 
  ungroup() %>% 
  select(NOM_PROPOSICAO, fold)

# Final para dados de treinamento
y_train <- left_join(y_train, propostas) %>% 
  rename(TARGET = APROVOU) 

# Final para dados de teste
y_test <- y_test %>% 
  rename(TARGET = APROVOU) 

# Colunas em comum (PRECISA TRABALHAR NISSO)
colunas <- intersect(colnames(x_train), colnames(x_test))
x_train <- x_train[,colunas]
x_test <- x_test[,colunas]

# Escrever os objetos
save(y_train, x_train, y_test, x_test,
     file = 'produced_data/train_test2.RData')

# --------------------------------------------------------
# Escreve dados de treinamento
#write.csv(dados[,c("fold", colunas)], 'produced_data/train.csv', row.names = FALSE)
#saveRDS(dados[,c("fold", colunas)], 'produced_data/train.RDS')
# Escreve dados de teste
#write.csv(dados_teste[, colunas], 'produced_data/teste.csv', row.names = FALSE)
#saveRDS(dados_teste[, colunas], 'produced_data/teste.RDS')
