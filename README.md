## Estrutura

analise_proposicoes.R = montagem e cruzamento das bases de tramitações e base com informações gerais

aprovometro.R = experimentos com modelos para prever probabilidade

## Arquivos base: 

http://www.camara.gov.br/internet/arquivosDadosAbertos/proposicoes.csv

Filtro do teste inicial para obtenção: 

```
dados <- read.csv2('proposicoes.csv')

lista.proposicoes <- dados %>% 
  select(NOM_PROPOSICAO, NOM_PARTIDO_POLITICO, DES_SITUACAO_PROPOSICAO, ANO_PROPOSICAO,
         NUM_PROPOSICAO, SIG_TIPO_PROPOSICAO, SIG_NORMA_JURIDICA, 
         DATAPRESENTACAOPROPOSICAO, DATTRANSFPROPOSICAOLEI, 
         SIG_UF, TEX_REGIAO_GEOGRAFICA_AUTOR, AREAS_TEMATICAS_APRESENTACAO, COD_SEXO) %>% 
  filter(SIG_TIPO_PROPOSICAO %in% c('PEC','PLP','PL'), ANO_PROPOSICAO >= 2008) %>% # FILTRO INICIAL A PARTIR DE 2008
  mutate(NOM_PROPOSICAO = paste0(SIG_TIPO_PROPOSICAO, ' ', NUM_PROPOSICAO, '/', ANO_PROPOSICAO)) # TEM PROPOSICAO COM O NOME QUEBRADO

```


Montagem da base de tramitações:

```
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
  situacao <- xml_df(doc, "//*/situacao")
  id <- cbind(id, situacao)
  ultima <- xml_df(doc, "//*/ultimaAcao/tramitacao") # TRAMITACAO UNICA
  tramitacoes <- xml_df(doc, "//*/andamento/tramitacao") #CADEIA DE TRAMITACAO
  
  if(ncol(tramitacoes)==0) {
    final <- merge(id, ultima, all=TRUE)
  } else {
    final <- merge(id, rbind(ultima, tramitacoes %>% select(-codReuniao, -inteiroTeor)), all=TRUE) 
  }
  final
}
### Captura os dados apenas uma vez
prop.detalhe <- NULL
loop <- 1
for(i in lista.proposicoes$NOM_PROPOSICAO) {
  s <- strsplit(i, ' |/')[[1]][1]  
  n <- strsplit(i, ' |/')[[1]][2]
  a <- strsplit(i, ' |/')[[1]][3]
  
  aux <- pega_proposicao(s, n, a)
  print(loop)
  if(is.null(prop.detalhe)) {
    prop.detalhe <- aux
  } else {
    prop.detalhe <- rbind(prop.detalhe, aux)
  }
  loop <- loop + 1
}
write.csv2(prop.detalhe, "TRAMITACOES.csv", row.names = FALSE)

tramitacoes <- read.csv2("TRAMITACOES.csv", stringsAsFactors = FALSE)
```
