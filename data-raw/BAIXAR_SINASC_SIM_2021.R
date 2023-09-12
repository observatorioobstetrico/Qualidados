library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(dplyr)
library(coro)
library(readr)


# Token da PCDaS
token = getPass()

# Função para converter os resultados das consultas para data.frame
convertRequestToDF <- function(request, column_names = c()){
  if("RequestError" %in% names(content(request))) stop(content(request)$RequestError)
  variables = unlist(content(request)$columns)
  variables = variables[names(variables) == "name"]
  if (!length(column_names)){
    column_names <- unname(variables)
  }
  values = content(request,)$rows
  df <- as.data.frame(do.call(rbind,lapply(values,function(r) {
    row <- r
    row[sapply(row, is.null)] <- NA
    rbind(unlist(row))
  } )))
  names(df) <- column_names
  return(df)
}

query_with_cursor <- generator(function(sql_query, token, nrows){
  tryCatch({
    json_api <- paste0('{"token": {"token": "',token,'"}, "sql": {"sql": {"query":"',sql_query,'", "fetch_size":"',nrows,'" }}}')
    response <- POST(url = "https://bigdata-api.fiocruz.br/sql_query/", body = json_api, encode = "json")
    df <- convertRequestToDF(response)
    col_names <- colnames(df)
    yield(df)
    while(TRUE){
      json_api <- paste0('{"token": {"token": "',token,'"}, "sql": {"sql": {"cursor":"',content(response)$cursor,'" }}}')
      response <- POST(url = "https://bigdata-api.fiocruz.br/sql_query/", body = json_api, encode = "json")
      if(length(content(response)$rows)>0){
        yield(convertRequestToDF(response,col_names))
      }
      else return(NULL)
    }
  }, error=function(cond) message(paste0(cond,"\n",content(response))) )
})

convertColTypeToNum <- function(df, colname){
  df[,colname] <- as.numeric(as.character(df[,colname]))
  return(df)
}

estados <- c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')
df_total_max3 <- data.frame()
for(i in estados){

  query <- paste0('SELECT (*)',
                  ' FROM \\"datasus-sinasc\\" WHERE (ano_nasc = 2021  AND res_SIGLA_UF = \'',i,'\')')

  df_total <- data.frame()
  loop(for (df in query_with_cursor(query, token, nrows=10000)) {
    print(paste0('Número de registros recuperados a cada iteração: ', nrow(df)))
    df_total <- rbind(df_total,df)
  })

  df_total_max3 <- rbind(df_total,df_total_max3)
}

SINASC_2021 <- df_total_max3
write_rds(SINASC_2021,file = 'data1/SINASC2021.rds')
#SIM

estados <- c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')
df_total_max3 <- data.frame()
for(i in estados){

  query <- paste0('SELECT (*)',
                  ' FROM \\"datasus-sim\\" WHERE (ano_obito = 2021  AND res_SIGLA_UF = \'',i,'\')')

  df_total <- data.frame()
  loop(for (df in query_with_cursor(query, token, nrows=10000)) {
    print(paste0('Número de registros recuperados a cada iteração: ', nrow(df)))
    df_total <- rbind(df_total,df)
  })

  df_total_max3 <- rbind(df_total,df_total_max3)
}
