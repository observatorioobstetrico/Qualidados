#pacotes
library(rjson)
library(readr)
library(dplyr)
#INCOMPLETUDE
regras_sinasc <- c(fromJSON(file = 'data1/SINASC_Incompletude_Regras.json'))
Sinasc_incom <- read_csv("data1/SINASC_Incompletude_v2.csv")
tabela_muni <- read_csv("data1/municipios_SINASC.csv")
vars_incom_sinasc <- unique(Sinasc_incom$VARIAVEL)
#ACRESCENTAR A COLUNA DE MUNICIPIOS
# Sinasc_incom$CODMUNNASC <- Sinasc_incom$CODMUNNASC %>%
#   as.character() %>% stringr::str_sub(1,6)
# tabela_muni$cod <- tabela_muni$cod %>% as.character()
# dado_aux <- Sinasc_incom %>%
#   rename(cod = CODMUNNASC ) %>%
#   left_join(tabela_muni,by='cod')
