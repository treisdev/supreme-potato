# Data: 18/07/2017

# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(tidytext)

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

# Adiciona maior tramitação de arquivo ----------------------------------------
dados_propostas <- dados_propostas %>% 
  group_by(NOM_PROPOSICAO) %>% 
  mutate(ORDEM_ARQUIVO = ifelse(grepl('^Arquivad', DES_TRAM), ORDEM_TRAM, NA),
         MAX_ORDEM_ARQUIVO = max(ORDEM_ARQUIVO, na.rm = TRUE)) %>% 
  ungroup()

# Dados Treinamento --------------------------------------------------------
# Cria sequência de dados
seq_datas <- seq(ymd('2013-12-31'),ymd('2015-12-31'), by = '1 month')
dados <- lapply(seq_datas, processa_dados,
                dados_propostas = dados_propostas)
  # Combina dados em um único data.frame
dados <- bind_rows(dados)

# Dados Teste
seq_datas <- seq(ymd('2016-06-30'),ymd('2016-12-31'), by = '1 month')
dados_teste <- lapply(seq_datas, processa_dados,
                      dados_propostas = dados_propostas)
# Combina dados em um único data.frame
dados_teste <- bind_rows(dados_teste)

# Cria os folds
# Separa as propostas entre aquelas aprovadas (em algum momento) e não aprovadas
# Busca que os folds tenham uma proporção de 0 e 1 similar aos dados originais
propostas <- dados %>%
  group_by(NOM_PROPOSICAO) %>%
  summarise(APROVOU = max(APROVOU)) %>% 
  group_by(APROVOU) %>%
  mutate(fold = sample(1:10, size = n(), replace = T)) %>% 
  ungroup() %>% 
  select(NOM_PROPOSICAO, fold)

# Final para dados de treinamento
dados <- left_join(dados, propostas) %>% 
  rename(TARGET = APROVOU) %>% 
  select(NOM_PROPOSICAO, TARGET, fold, data, everything())

# Escreve dados de treinamento
write.csv(dados, 'produced_data/train.csv', row.names = FALSE)

# Final para dados de teste
dados_teste <- dados_teste %>% 
  rename(TARGET = APROVOU) %>% 
  select(NOM_PROPOSICAO, TARGET, data, everything())

# Escreve dados de teste
write.csv(dados_teste, 'produced_data/teste.csv', row.names = FALSE)

