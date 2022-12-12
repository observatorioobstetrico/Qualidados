#pacotes
library(rjson)
library(readr)
library(dplyr)
############## INCOMPLETUDE ################################################

regras_sinasc_incom <- c(fromJSON(file = 'data1/SINASC_Incompletude_Regras.json'))
Sinasc_incom <- read_csv("data1/SINASC_Incompletude_v2.csv")
vars_incom_sinasc <- unique(Sinasc_incom$VARIAVEL)
municipios_SINASC <- read_csv("data1/municipios_SINASC.csv")
municipios_SINASC$cod <- municipios_SINASC$cod |> as.character()
municipios_SINASC[municipios_SINASC$cod == 29000,]
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
aux_muni2[aux_muni2$cod_mun == '290000',]
Sinasc_incom$CODMUNNASC <- Sinasc_incom$CODMUNNASC |> as.character()

Sinasc_incom <- Sinasc_incom %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun')
Sinasc_incom$CODMUNNASC <- Sinasc_incom$muni_nm_clean
Sinasc_incom$ESTADO <- Sinasc_incom$uf_sigla
TESTE <- Sinasc_incom[is.na(Sinasc_incom$muni_nm_clean) == T,]

############### IMPLAUSIBILIDADE ############################################

regras_sinasc_implau <- c(fromJSON(file = 'data1/SINASC_Implausibilidade_Regras.json'))
Sinasc_implau <- read_csv('data1/SINASC_Implausibilidade_v2.csv')
vars_implau_sinasc <- unique(Sinasc_implau$VARIAVEL)
vars_implau_sinasc <- vars_implau_sinasc |>
  stringr::str_split("_IMPLAUSIVEL") |> unlist()
vars_implau_sinasc <- vars_implau_sinasc[stringr::str_detect(vars_implau_sinasc,'')]

#ACRESCENTAR A COLUNA DE MUNICIPIOS E MUNICIPIOS

Sinasc_implau$CODMUNNASC <- Sinasc_implau$CODMUNNASC |> as.character()

Sinasc_implau <- Sinasc_implau %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun')
Sinasc_implau$CODMUNNASC <- Sinasc_implau$muni_nm_clean
Sinasc_implau$ESTADO <- Sinasc_implau$uf_sigla
