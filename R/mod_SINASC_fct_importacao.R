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
#ACRESCENTAR A COLUNA DE MUNICIPIOS E MUNICIPIOS

aux_muni2 <- abjData::muni %>%
  dplyr::select(uf_id,
                muni_id,
                muni_nm_clean,
                uf_sigla) %>%
  mutate_at("muni_id", as.character)  %>%
  mutate(cod_mun = stringr::str_sub(muni_id, 1, 7))

aux_muni2 <- rbind(aux_muni2,aux_muni2|>
                     mutate(cod_mun = stringr::str_sub(muni_id, 1, 6)))

Sinasc_incom$CODMUNNASC <- Sinasc_incom$CODMUNNASC |> as.character()

Sinasc_incom <- Sinasc_incom %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun')

Sinasc_incom[,c('muni_id','uf_id')] <- NULL
aux <- Sinasc_incom[is.na(Sinasc_incom$muni_nm_clean) ==T,]
table( aux$ANO)
Sinasc_incom <- Sinasc_incom |>
  mutate(uf_id = stringr::str_sub(cod_mun,1,2))


Sinasc_incom[is.na(Sinasc_incom$uf_sigla)==T,'uf_sigla']<-
  Sinasc_incom[is.na(Sinasc_incom$uf_sigla)==T,]|>
  left_join(unique(aux_muni2[,c('uf_id','uf_sigla')]),by = 'uf_id') |> dplyr::select(uf_sigla.y)

Sinasc_incom[is.na(Sinasc_incom$muni_nm_clean)==T,'muni_nm_clean'] <- 'Não informado'

Sinasc_incom$CODMUNNASC <- Sinasc_incom$muni_nm_clean
Sinasc_incom$ESTADO <- Sinasc_incom$uf_sigla
teste <- Sinasc_incom[Sinasc_incom$muni_nm_clean == 'Não informado',]
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
