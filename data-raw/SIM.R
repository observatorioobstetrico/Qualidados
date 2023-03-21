## code to prepare `SIM` dataset goes here
library(rjson)
library(readr)
library(dplyr)
library(readxl)
SIM_dic <- read_excel("data1/dicionarios.xlsx", sheet = "SIM")
usethis::use_data(SIM_dic,overwrite = T)
############## INCOMPLETUDE ################################################

regras_sim_incom <- c(fromJSON(file = 'data1/SIM_Incompletude_Regras.json'))
SIM_Incom <- read_csv("data1/SIM_Incompletude_v2.csv",show_col_types = FALSE )

#ACRESCENTAR A COLUNA DE MUNICIPIOS E MUNICIPIOS
####################################################################################
aux_muni2 <- abjData::muni %>%
  dplyr::select(uf_id,
                muni_id,
                muni_nm_clean,
                uf_sigla) %>%
  mutate_at("muni_id", as.character)  %>%
  mutate(cod_mun = stringr::str_sub(muni_id, 1, 7))

aux_muni2 <- rbind(aux_muni2,aux_muni2|>
                     mutate(cod_mun = stringr::str_sub(muni_id, 1, 6)))
####################################################################################

SIM_Incom$CODMUNOCOR  <- as.character(format(SIM_Incom$CODMUNOCOR  , scientific = FALSE))
SIM_Incom$CODMUNOCOR  <- gsub(' ','',SIM_Incom$CODMUNOCOR)

SIM_Incom <- SIM_Incom %>%
  rename(cod_mun = CODMUNOCOR ) %>%
  left_join(aux_muni2 ,by='cod_mun')

SIM_Incom[,c('muni_id','uf_id')] <- NULL

SIM_Incom <- SIM_Incom |>
  mutate(uf_id = stringr::str_sub(cod_mun,1,2))


SIM_Incom[is.na(SIM_Incom$uf_sigla)==T,'uf_sigla']<-
  SIM_Incom[is.na(SIM_Incom$uf_sigla)==T,]|>
  left_join(unique(aux_muni2[,c('uf_id','uf_sigla')]),by = 'uf_id') |> dplyr::select(uf_sigla.y)

SIM_Incom[is.na(SIM_Incom$muni_nm_clean)==T,'muni_nm_clean'] <- 'Não informado'

SIM_Incom$CODMUNNASC <- SIM_Incom$muni_nm_clean
SIM_Incom$ESTADO <- SIM_Incom$uf_sigla
SIM_Incom[,c('cod_mun','uf_id','uf_sigla','muni_nm_clean')] <- NULL
var_sim_tirar <- c('CODBAIOCOR',
                   'CODCART',
                   'CODMUNCART',
                   'CONTADOR',
                   'DTREGCART',
                   'EXPDIFDATA',
                   'NUMREGCART',
                   'UFINFORM',
                   'ALTCAUSA',
                   'DTCADINF',
                   'DTCADINV',
                   'DTCONCASO',
                   'DTCONINV',
                   'ESTABDESCR',
                   'FONTES',
                   'FONTESINF',
                   'MORTEPARTO',
                   'NUDIASINF',
                   'NUDIASOBCO',
                   'NUDIASOBIN',
                   'ORIGEM',
                   'TPNIVELINV',
                   'TPOBITOCOR',
                   'TPRESGINFO')
SIM_Incom <- SIM_Incom[!(SIM_Incom$VARIAVEL %in% var_sim_tirar),]
SIM_Incom <- merge(SIM_Incom, SIM_dic[,c("Codigo Qualidados", "Codigo SIM") ], by.x="VARIAVEL", by.y="Codigo SIM", all=TRUE)
SIM_Incom$VARIAVEL <- SIM_Incom$`Codigo Qualidados`
SIM_Incom$`Codigo Qualidados` <- NULL
vars_incom_sim<- unique(SIM_Incom$VARIAVEL)
################# REGRAS
df_aux <- regras_sim_incom |> as.data.frame() |> t() |> as.data.frame()
df_aux<-  cbind(row.names(df_aux),df_aux)
df_aux |> row.names() <- NULL
df_aux$`row.names(df_aux)` <- df_aux$`row.names(df_aux)` |> gsub(pattern = 'IGNORADOS_', replacement = '')
colnames(df_aux) <- c('Variável','Regra')
regras_sim_incom <- df_aux

usethis::use_data(SIM_Incom, overwrite = TRUE)
usethis::use_data(vars_incom_sim, overwrite = TRUE)

############# IMPLAUSIBILIDADE ####################################################

regras_sim_implau <- c(fromJSON(file = 'data1/SIM_Implausibilidade_Regras.json'))
SIM_Implau <- read_csv("data1/SIM_Implausibilidade_v2.csv",show_col_types = FALSE )
SIM_Implau$VARIAVEL <- SIM_Implau$VARIAVEL |> gsub(pattern = '_IMPLAUSIVEL',replacement = '')



SIM_Implau$CODMUNOCOR  <- as.character(format(SIM_Implau$CODMUNOCOR  , scientific = FALSE))
SIM_Implau$CODMUNOCOR  <- gsub(' ','',SIM_Implau$CODMUNOCOR)

SIM_Implau <- SIM_Implau %>%
  rename(cod_mun = CODMUNOCOR ) %>%
  left_join(aux_muni2 ,by='cod_mun')

SIM_Implau[,c('muni_id','uf_id')] <- NULL

SIM_Implau <- SIM_Implau |>
  mutate(uf_id = stringr::str_sub(cod_mun,1,2))


SIM_Implau[is.na(SIM_Implau$uf_sigla)==T,'uf_sigla']<-
  SIM_Implau[is.na(SIM_Implau$uf_sigla)==T,]|>
  left_join(unique(aux_muni2[,c('uf_id','uf_sigla')]),by = 'uf_id') |> dplyr::select(uf_sigla.y)

SIM_Implau[is.na(SIM_Implau$muni_nm_clean)==T,'muni_nm_clean'] <- 'Não informado'

SIM_Implau$CODMUNNASC <- SIM_Implau$muni_nm_clean
SIM_Implau$ESTADO <- SIM_Implau$uf_sigla
SIM_Implau[,c('cod_mun','uf_id','uf_sigla','muni_nm_clean')] <- NULL
SIM_Implau <- SIM_Implau[!(SIM_Implau$VARIAVEL %in% var_sim_tirar),]
# SIM_Implau <- merge(SIM_Implau, SIM_dic[,c("Codigo Qualidados", "Codigo SIM") ], by.x="VARIAVEL", by.y="Codigo SIM", all=TRUE)
# SIM_Implau$VARIAVEL <- SIM_Implau$`Codigo Qualidados`
# SIM_Implau$`Codigo Qualidados` <- NULL
vars_implau_sim<- unique(SIM_Implau$VARIAVEL)
################# REGRAS
df_aux <- regras_sim_implau |> as.data.frame() |> t() |> as.data.frame()
df_aux<-  cbind(row.names(df_aux),df_aux)
df_aux |> row.names() <- NULL
colnames(df_aux) <- c('Variável','Regra')
regras_sim_implau <- df_aux


usethis::use_data(SIM_Implau, overwrite = TRUE)
usethis::use_data(vars_implau_sim, overwrite = TRUE)

################################ Inconsistencia
regras_sim_incon <- c(fromJSON(file = 'data1/SIM_Inconsistencia_Regras.json'))
SIM_Incon <- read_csv("data1/SIM_Inconsistencia_v2.csv",show_col_types = FALSE )
SIM_Incon$VARIAVEL <- SIM_Incon$VARIAVEL |> gsub(pattern = '_INCONSISTENTES',replacement = '')
SIM_Incon$VARIAVEL <- SIM_Incon$VARIAVEL |> gsub(pattern = '_',replacement = ' ')


SIM_Incon$CODMUNOCOR  <- as.character(format(SIM_Incon$CODMUNOCOR  , scientific = FALSE))
SIM_Incon$CODMUNOCOR  <- gsub(' ','',SIM_Incon$CODMUNOCOR)

SIM_Incon <- SIM_Incon %>%
  rename(cod_mun = CODMUNOCOR ) %>%
  left_join(aux_muni2 ,by='cod_mun')

SIM_Incon[,c('muni_id','uf_id')] <- NULL

SIM_Incon <- SIM_Incon |>
  mutate(uf_id = stringr::str_sub(cod_mun,1,2))


SIM_Incon[is.na(SIM_Incon$uf_sigla)==T,'uf_sigla']<-
  SIM_Incon[is.na(SIM_Incon$uf_sigla)==T,]|>
  left_join(unique(aux_muni2[,c('uf_id','uf_sigla')]),by = 'uf_id') |> dplyr::select(uf_sigla.y)

SIM_Incon[is.na(SIM_Incon$muni_nm_clean)==T,'muni_nm_clean'] <- 'Não informado'

SIM_Incon$CODMUNNASC <- SIM_Incon$muni_nm_clean
SIM_Incon$ESTADO <- SIM_Incon$uf_sigla
SIM_Incon[,c('cod_mun','uf_id','uf_sigla','muni_nm_clean')] <- NULL
SIM_Incon <- SIM_Incon[!(SIM_Incon$VARIAVEL %in% SIM_Incon),]
vars_incon_sim<- unique(SIM_Incon$VARIAVEL)
################# REGRAS
df_aux <- regras_sim_incon |> as.data.frame() |> t() |> as.data.frame()
df_aux<-  cbind(row.names(df_aux),df_aux)
df_aux |> row.names() <- NULL
df_aux$`row.names(df_aux)` <- df_aux$`row.names(df_aux)` |> gsub(pattern = '_', replacement = ' ')
colnames(df_aux) <- c('Variável','Regra')
regras_sim_incon <- df_aux

usethis::use_data(SIM_Incon, overwrite = TRUE)
usethis::use_data(vars_incon_sim, overwrite = TRUE)

########### REGRAS ##############################
regras_sim_implau$Indicador <- 'Implausibilidade'
regras_sim_incom$Indicador <- 'Incompletude'
regras_sim_incon$Indicador <- 'Inconsistência'
regras_sim <- rbind(regras_sim_implau,regras_sim_incom,regras_sim_incon)
usethis::use_data(regras_sim, overwrite = TRUE)
