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
  dados_treino <- dados_treino %>% 
    mutate(COMISSAO = word(DES_ORGAO)) %>% 
    mutate(TIPO = ifelse(startsWith(COMISSAO, 'P'), 1, 0), #COMISSOES ESPECIAIS
           TIPO = ifelse(COMISSAO == 'PLEN', 0, TIPO), 
           TIPO = ifelse(COMISSAO %in% c('MPV00101', 'GTFICHA', 'GTTAXI', 'CPIPETRO'), 1, TIPO), #COMISSOES ESPECIAIS
           COMISSAO = ifelse(TIPO == 1, 'ESPECIAL', COMISSAO)) #COMISSOES ESPECIAIS
  dummies_comissao <- dados_treino %>% 
    select(COMISSAO) 
  one_comissao <- dummyVars('~ COMISSAO', data = dummies_comissao) 
  dummies_comissao <- predict(one_comissao, newdata = dummies_comissao) %>% 
    data.frame()
  dados_treino <- bind_cols(dados_treino, dummies_comissao) %>% 
    select(-COMISSAO)
  
  # Tipo de Proposição
  dummies_proposicao <- dummyVars('~ SIG_TIPO_PROPOSICAO', data = dados_treino)
  dummies_proposicao <- predict(dummies_proposicao, dados_treino) %>% 
    data.frame()
  dados_treino <- bind_cols(dados_treino, dummies_proposicao)
  
  # Temas
  area_tematica <- dados_treino %>% 
    ungroup() %>%
    select(NOM_PROPOSICAO, AREAS_TEMATICAS_APRESENTACAO) %>% distinct()
  area_tematica <- unnest_tokens(area_tematica, word, AREAS_TEMATICAS_APRESENTACAO) %>% 
    anti_join(stop_words)
  area_tematica <- area_tematica %>% 
    count(NOM_PROPOSICAO, word, sort = TRUE) %>% 
    bind_tf_idf(word, NOM_PROPOSICAO, n) %>% 
    select(NOM_PROPOSICAO, word, tf_idf) %>% 
    spread(key = word, value = tf_idf, fill = 0)
  dados_treino <- left_join(dados_treino, area_tematica)
  
  
  # Fim
  dados_treino <- dados_treino %>% 
    replace_na(list(APROVOU = 0)) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVOU = max(APROVOU)) %>% 
    filter(ORDEM_TRAM == max(ORDEM_TRAM)) %>% 
    mutate(id = 1:n()) %>% 
    filter(id == 1) %>% 
    select(APROVOU, NOM_PROPOSICAO, 26:ncol(.), -TIPO) %>% 
    ungroup() %>% 
    distinct() %>% 
    mutate(data = data) %>% 
    select(APROVOU, NOM_PROPOSICAO, data,
           everything(), -id)
  
  dados_treino
}
