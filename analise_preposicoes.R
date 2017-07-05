library(dplyr)
library(stringr)
library(XML)
library(httr)

setwd('/Users/sauloguerra/Development/desafioleg/')

# extrai node com hierarquia 1:1 e transforma em dataframe
xml_df <-function(doc, path){
  dumFun <- function(x){
    xname <- xmlName(x)
    c(sapply(xmlChildren(x), xmlValue))
  }
  as.data.frame(t(xpathSApply(doc, path, dumFun)), stringsAsFactors = FALSE)
}

# pega dados complementares da proposicao
pega_proposicao <- function(sigla, numero, ano) {
  print(paste('sigla', s, 'numero', n, 'ano', a))
  url <- paste0('www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterAndamento?sigla=',sigla,
                '&numero=',numero,
                '&ano=',ano,
                '&dataIni=&codOrgao=')
  
  doc <- xmlParse(content(GET(url), type="text", encoding = 'UTF-8'))
  root <- xmlRoot(doc)
  
  id <- as.data.frame(t(xmlAttrs(root))) # CABECALHO
  ultima <- xml_df(doc, "//*/ultimaAcao/tramitacao") # TRAMITACAO UNICA
  tramitacoes <- xml_df(doc, "//*/andamento/tramitacao") #CADEIA DE TRAMITACAO
  
  if(ncol(tramitacoes)==0) {
    final <- merge(id, ultima, all=TRUE)
  } else {
    final <- merge(id, rbind(ultima, tramitacoes %>% select(-codReuniao, -inteiroTeor)), all=TRUE) 
  }
  final
}

# LISTA DO SITE E INCOMPLETA!!!
# # extraçao: http://www.camara.leg.br/buscaProposicoesWeb/pesquisaSimplificada
# # apenas PEC, PLP e PL.
# arquivos <- list.files()
# arquivos <- arquivos[str_detect(arquivos, 'relatorioPesquisa')]
# lista.completa <- NULL
# for(i in arquivos) {
#   lista <- read.csv2(i, header = FALSE, encoding = 'latin1')
#   lista <- lista[4:(dim(lista)[1]-3), c(1, 8, 9)]
#   if(is.null(lista.completa)) {
#     lista.completa <- lista
#   } else {
#     lista.completa <- rbind(lista.completa, lista)
#   }
# }
# names(lista.completa) <- c('PROPOSICAO', 'ULTIMO_STATUS', 'LINK')
# lista.completa$ID <- str_split(lista.completa$LINK, pattern = '=', simplify = TRUE)[,2]
# lista.completa <- data.frame(lapply(lista.completa, as.character), stringsAsFactors=FALSE)


# extraçao: http://www.camara.gov.br/internet/arquivosDadosAbertos/proposicoes.csv
# em: https://dadosabertos.camara.leg.br/swagger/api.html
dados <- read.csv2('proposicoes.csv')

lista.proposicoes <- dados %>% 
  select(NOM_PROPOSICAO, NOM_PARTIDO_POLITICO, DES_SITUACAO_PROPOSICAO, ANO_PROPOSICAO,
         NUM_PROPOSICAO, SIG_TIPO_PROPOSICAO, SIG_NORMA_JURIDICA, 
         DATAPRESENTACAOPROPOSICAO, DATTRANSFPROPOSICAOLEI) %>% 
  filter(SIG_TIPO_PROPOSICAO %in% c('PEC','PLP','PL'), ANO_PROPOSICAO >= 2008) #

# teste <- dados %>% filter(ANO_PROPOSICAO == '2015', SIG_TIPO_PROPOSICAO == 'PLP')
# View(teste)

prop.detalhe <- NULL
for(i in lista.proposicoes$NOM_PROPOSICAO) {
  s <- strsplit(i, ' |/')[[1]][1]  
  n <- strsplit(i, ' |/')[[1]][2]
  a <- strsplit(i, ' |/')[[1]][3]
  
  aux <- pega_proposicao(s, n, a)
  if(is.null(prop.detalhe)) {
    prop.detalhe <- aux
  } else {
    prop.detalhe <- rbind(prop.detalhe, aux)
  }
}

## TRACEBACK - ERRO DESCONHECIDO NA OCORRENCIA "sigla PL numero 3840 ano 2012"
# curl::curl_fetch_memory(url, handle = handle) : 
#   Timeout was reached 
# curl::curl_fetch_memory(url, handle = handle) 
# request_fetch.write_memory(req$output, req$url, handle) 
# request_fetch(req$output, req$url, handle) 
# request_perform(req, hu$handle$handle) 
# GET(url) 
# inherits(x, "response") 
# is.response(x) 
# stopifnot(is.response(x)) 
# content(GET(url), type = "text", encoding = "UTF-8") 
# xmlParse(content(GET(url), type = "text", encoding = "UTF-8")) 
# pega_proposicao(s, n, a) 

# dim(prop.detalhe)
# dim(lista.proposicoes)
# write.csv2(prop.detalhe, "parcial1.csv", row.names = FALSE)

# parcial <- read.csv2('parcial1.csv')
# dim(parcial)
# View(parcial)


# proposicoes$ENCERRADA_REPROVADA <- grepl(x = proposicoes$ULTIMO_STATUS, pattern = 'Arquivada|Vetado')
# proposicoes$APROVADA <- grepl(x = proposicoes$ULTIMO_STATUS, pattern = 'Transformado em Norma Jurídica')

