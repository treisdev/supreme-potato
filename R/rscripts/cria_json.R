library(tidyverse)
library(dbplyr)

conexao <- src_sqlite('~/Downloads/base.sqlite', create = FALSE)

db_list_tables(conexao$con)

dados_area_tematica_relacional <- tbl(conexao, "dados_area_tematica_relacional") %>% collect()
dados_comissoes_relacional <- tbl(conexao, "dados_comissoes_relacional") %>% collect()
proposicao <- tbl(conexao, "proposicao") %>% collect()

head(proposicao)
head(dados_comissoes_relacional)
head(dados_area_tematica_relacional)


teste_prop <- unique(proposicao$NOM_PROPOSICAO)
teste_prop

proposicao <- proposicao %>% 
  filter(NOM_PROPOSICAO %in% teste_prop)

lista_tema <- vector("list", length(teste_prop))
lista_comissao <- vector("list", length(teste_prop))
j <- 1
for(i in teste_prop){
  print(j)
  lista_tema[[j]] <- dados_area_tematica_relacional %>% 
    filter(NOM_PROPOSICAO == i) %>% 
    select(tema)
  
  lista_comissao[[j]] <- dados_comissoes_relacional %>% 
    filter(NOM_PROPOSICAO == i) %>% 
    select(COMISSAO)
  
  j <- j + 1
}


proposicao %>% 
  tbl_df %>% 
  add_column(tag_temas = lista_tema,
             tag_comissoes = lista_comissao) %>% 
  toJSON(pretty = FALSE) %>% 
  writeLines('~/Downloads/aprovometro.json')
