# Função para gerar dados para treinamento e teste
# Os dados devem ter as mesmas colunas do arquivo 
# PROPOSICOES_E_TRAMITACOES.csv
#
# Ver dados_treinamento.R

## Parâmetros
# - dados_propostas: data.frame base
# - data: data de referência
# simula um conjunto de dados observados na data especificada
# olha apenas períodos anteriores para construção das variáveis
# - meses: número de meses à frente para construção do target
# verifica se a proposta foi aprovada nos `meses` subsequentes

## Output
# Cria um data.frame com as features para treinamento
# Além das features, são adicionadas variáveis de identificação
# da proposta e data de referência

processa_dados <- function(dados_propostas, data, meses = 6){
  data <- as.Date(data)
  data_6m <- data %m+% months(meses)
  dados_data_base <- dados_propostas %>% 
    filter(DATA_TRAM <= data) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVADA = ifelse(grepl('^Transformado n', DES_TRAM), 1, 0),
           ARQUIVADA = ifelse(grepl('^Arquivad', DES_TRAM), 1, 0),
           DESARQUIVADA = ifelse(grepl('^Desarquivad', DES_TRAM), 1, 0),
           ARQUIVADA= ifelse(max(ARQUIVADA * ORDEM_TRAM) > max(DESARQUIVADA * ORDEM_TRAM),
                             1, 0),
           STATUS_REAL = sum(APROVADA + ARQUIVADA, na.rm = TRUE)) %>% 
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
  
  # Comissões
  dic_comissao_df <- expand.grid(NOM_PROPOSICAO = unique(dados_tramitacoes$NOM_PROPOSICAO),
                                      word = paste0("comissao_", dic_comissoes$word),
                                      stringsAsFactors = FALSE)
  
  comissao <- dados_treino %>% 
    ungroup() %>%
    select(NOM_PROPOSICAO, DES_ORGAO) %>% distinct()
  comissao <- unnest_tokens(comissao, word, DES_ORGAO) %>% 
    anti_join(stop_words)
  comissao <- comissao %>% 
    mutate(word = paste0("comissao_", word)) %>%
    count(NOM_PROPOSICAO, word, sort = TRUE) %>% 
    right_join(dic_comissao_df) %>% 
    spread(key = word, value = n, fill = 0)
  
  dados_treino <- left_join(dados_treino, comissao)
  
  # Tipo de Proposição
  dummies_proposicao <- dummyVars('~ SIG_TIPO_PROPOSICAO', data = dados_treino)
  dummies_proposicao <- predict(dummies_proposicao, dados_treino) %>% 
    data.frame()
  dados_treino <- bind_cols(dados_treino, dummies_proposicao)
  
  # Temas
  dic_area_tematica_df <- expand.grid(NOM_PROPOSICAO = unique(dados_tramitacoes$NOM_PROPOSICAO),
                             word = paste0("tema_", dic_area_tematica$word),
                             stringsAsFactors = FALSE)
  
  area_tematica <- dados_treino %>% 
    ungroup() %>%
    select(NOM_PROPOSICAO, AREAS_TEMATICAS_APRESENTACAO) %>% distinct()
  area_tematica <- unnest_tokens(area_tematica, word, AREAS_TEMATICAS_APRESENTACAO) %>% 
    anti_join(stop_words)
  area_tematica <- area_tematica %>% 
    mutate(word = paste0("tema_", word)) %>%
    count(NOM_PROPOSICAO, word, sort = TRUE) %>% 
    bind_tf_idf(word, NOM_PROPOSICAO, n) %>% 
    select(NOM_PROPOSICAO, word, tf_idf) %>% 
    right_join(dic_area_tematica_df) %>% 
    spread(key = word, value = tf_idf, fill = 0)
  dados_treino <- left_join(dados_treino, area_tematica)
  
  # bag of words para as tramitações
  dados_tramitacoes <- dados_treino %>% 
    select(NOM_PROPOSICAO, DES_TRAM) %>% 
    unnest_tokens(word, DES_TRAM)
  
  dic_tram_df <- expand.grid(NOM_PROPOSICAO = unique(dados_tramitacoes$NOM_PROPOSICAO),
                                     word = paste0("tram_", dic_tram$word),
                                     stringsAsFactors = FALSE)
    
  dados_tramitacoes <- dados_tramitacoes %>%
    mutate(word = paste0("tram_", word)) %>%
    count(NOM_PROPOSICAO, word, sort = TRUE) %>% 
    right_join(dic_tram_df) %>% 
    spread(key = word, value = n, fill = 0)

  dados_treino <- left_join(dados_treino, dados_tramitacoes)
  
  # Número de tramitações nos últimos 30 dias
  tram_30_dias <- dados_treino %>% 
    filter(DATA_TRAM >= data - 30  & DATA_TRAM <= data) %>%
    group_by(NOM_PROPOSICAO) %>% 
    summarise(n_tram_30d = n()) %>% 
    ungroup()
  
  dados_treino <- left_join(dados_treino, tram_30_dias)
  
  # Número de tramitações nos últimos 30 dias
  tram_90_dias <- dados_treino %>% 
    filter(DATA_TRAM >= data - 90  & DATA_TRAM <= data) %>%
    group_by(NOM_PROPOSICAO) %>% 
    summarise(n_tram_90d = n()) %>% 
    ungroup()
  
  dados_treino <- left_join(dados_treino, tram_90_dias)
  
  # Fim
  dados_treino <- dados_treino %>% 
    replace_na(list(APROVOU = 0)) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVOU = max(APROVOU)) %>% 
    filter(ORDEM_TRAM == max(ORDEM_TRAM)) %>% 
    mutate(id = 1:n()) %>% 
    filter(id == 1) %>% 
    select(APROVOU, NOM_PROPOSICAO, 26:ncol(.)) %>% 
    ungroup() %>% 
    distinct() %>% 
    mutate(data = data) %>% 
    select(APROVOU, NOM_PROPOSICAO, data,
           everything(), -id) %>% 
    data.frame()
  
    y <- dados_treino %>% 
      select(APROVOU, NOM_PROPOSICAO, data) %>% 
      data.frame()
    
    x <- dados_treino %>% 
      select(-APROVOU, -NOM_PROPOSICAO, -data) %>% 
      data.frame() %>% 
      replace(is.na(.), 0)
  
    x <- sparse.model.matrix(~ . -1, data = x)
    
    return(list(x = x, y = y))
}
