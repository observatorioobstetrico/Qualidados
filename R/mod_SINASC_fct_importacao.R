#pacotes
library(rjson)
library(readr)
library(dplyr)
############## INCOMPLETUDE ################################################

regras_sinasc_incom <- c(fromJSON(file = 'data1/SINASC_Incompletude_Regras.json'))
Sinasc_incom <- read_csv("data1/SINASC_Incompletude_v2.csv")
vars_incom_sinasc <- unique(Sinasc_incom$VARIAVEL)

#ACRESCENTAR A COLUNA DE MUNICIPIOS E MUNICIPIOS
aux_muni2 <- abjData::muni %>%
  dplyr::select(muni_id,
                muni_nm_clean,
                uf_sigla) %>%
  mutate_at("muni_id", as.character)  %>%
  mutate(cod_mun = stringr::str_sub(muni_id, 1, 7))
aux_muni3 <- aux_muni2
aux_muni3$cod_mun <- aux_muni3$cod_mun |> stringr::str_sub(1,6)
aux_muni2 <- rbind(aux_muni2,aux_muni3)

Sinasc_incom$CODMUNNASC <- Sinasc_incom$CODMUNNASC |> as.character()

dado_aux <- Sinasc_incom %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun')
Sinasc_incom$CODMUNNASC <- dado_aux$muni_nm_clean
Sinasc_incom$ESTADO <- dado_aux$uf_sigla

############### IMPLAUSIBILIDADE ############################################

regras_sinasc_implau <- c(fromJSON(file = 'data1/SINASC_Implausibilidade_Regras.json'))
Sinasc_implau <- read_csv('data1/SINASC_Implausibilidade_v2.csv')
vars_implau_sinasc <- unique(Sinasc_implau$VARIAVEL)
vars_implau_sinasc <- vars_implau_sinasc |>
  stringr::str_split("_IMPLAUSIVEL") |> unlist()
vars_implau_sinasc <- vars_implau_sinasc[stringr::str_detect(vars_implau_sinasc,'')]

#ACRESCENTAR A COLUNA DE MUNICIPIOS E MUNICIPIOS

Sinasc_implau$CODMUNNASC <- Sinasc_implau$CODMUNNASC |> as.character()

dado_aux <- Sinasc_implau %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun')
Sinasc_implau$CODMUNNASC <- dado_aux$muni_nm_clean
Sinasc_implau$ESTADO <- dado_aux$uf_sigla
