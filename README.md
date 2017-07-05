## Estrutura

analise_preposicoes.R deveria se chamar analise_proposicoes.R :thinking_face:

Arquivo base: 

http://www.camara.gov.br/internet/arquivosDadosAbertos/proposicoes.csv

Filtro do teste inicial para obtenção: 

```filter(SIG_TIPO_PROPOSICAO %in% c('PEC','PLP','PL'), ANO_PROPOSICAO >= 2008)```

Falha de timeout:

```
"sigla PL numero 3840 ano 2012"
 curl::curl_fetch_memory(url, handle = handle) : 
   Timeout was reached 
 curl::curl_fetch_memory(url, handle = handle) 
 request_fetch.write_memory(req$output, req$url, handle) 
 request_fetch(req$output, req$url, handle) 
 request_perform(req, hu$handle$handle) 
 GET(url) 
 inherits(x, "response") 
 is.response(x) 
 stopifnot(is.response(x)) 
 content(GET(url), type = "text", encoding = "UTF-8") 
 ```
