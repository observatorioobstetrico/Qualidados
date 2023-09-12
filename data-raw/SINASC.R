#pacotes
library(rjson)
library(readr)
library(dplyr)
library(readxl)
SINASC_dic <- read_excel("data1/dicionarios.xlsx", sheet = "SINASC")
usethis::use_data(SINASC_dic,overwrite = T)
############## INCOMPLETUDE ################################################

regras_sinasc_incom <- c(fromJSON(file = 'data1/SINASC_Incompletude_Regras.json'))
Sinasc_incom <- read_csv("data1/SINASC_Incompletude_v2.csv",show_col_types = FALSE )
S2021 <- read_csv("data1/SINASC_INCOMPLETUDE_2021.csv",show_col_types = FALSE )
s2022 <- read_csv("data1/SINASC_2022_incompletude.csv",show_col_types = FALSE )
Sinasc_incom <- rbind(Sinasc_incom,S2021,s2022)
#FILTRAR APENAS PARA VARIAVEIS PRESENTES NO DICIONARIO
vars <- SINASC_dic$`Codigo SINASC` %>% unique()
Sinasc_incom$VARIAVEL %>% unique() %>% setdiff(vars)
Sinasc_incom <- Sinasc_incom[Sinasc_incom$VARIAVEL %in% vars,]

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


Sinasc_incom$CODMUNNASC <- as.character(format(Sinasc_incom$CODMUNNASC , scientific = FALSE))
Sinasc_incom$CODMUNNASC <- gsub(' ','',Sinasc_incom$CODMUNNASC)

Sinasc_incom <- Sinasc_incom %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun') %>%
  select(-muni_id,-uf_id) %>%
  mutate(uf_id = stringr::str_sub(cod_mun,1,2))


Sinasc_incom[is.na(Sinasc_incom$uf_sigla)==T,'uf_sigla']<-
  Sinasc_incom[is.na(Sinasc_incom$uf_sigla)==T,]|>
  left_join(unique(aux_muni2[,c('uf_id','uf_sigla')]),by = 'uf_id') |> dplyr::select(uf_sigla.y)

Sinasc_incom[is.na(Sinasc_incom$muni_nm_clean)==T,'muni_nm_clean'] <- 'Não informado'

Sinasc_incom$CODMUNNASC <- Sinasc_incom$muni_nm_clean
Sinasc_incom$ESTADO <- Sinasc_incom$uf_sigla
Sinasc_incom[,c('cod_mun','uf_id','uf_sigla','muni_nm_clean')] <- NULL
Sinasc_incom[is.na(Sinasc_incom$ESTADO),"ESTADO"] <- 'Não Informado'

regras_sinasc_incom <-regras_sinasc_incom |> as.data.frame()  |> t() |> as.data.frame()
regras_sinasc_incom <- cbind(regras_sinasc_incom|> row.names(), regras_sinasc_incom)
regras_sinasc_incom |> row.names() <- NULL
regras_sinasc_incom |> colnames() <- c('Variável','Regra')
regras_sinasc_incom$Variável <- regras_sinasc_incom$Variável |> gsub(pattern = 'IGNORADOS_', replacement = '')
regras_sinasc_incom$Regra <- regras_sinasc_incom$Regra |> gsub(pattern = 'estiver', replacement = ' estiver')

var_aux <- Sinasc_incom$VARIAVEL |> unique()
Sinasc_incom <- merge(Sinasc_incom, SINASC_dic[,c("Codigo Qualidados", "Codigo SINASC") ], by.x="VARIAVEL", by.y="Codigo SINASC", all=TRUE)
Sinasc_incom <- Sinasc_incom[Sinasc_incom$VARIAVEL %in% var_aux,]
Sinasc_incom$VARIAVEL <- Sinasc_incom$`Codigo Qualidados`
Sinasc_incom$`Codigo Qualidados` <- NULL
vars_incom_sinasc <- unique(Sinasc_incom$VARIAVEL)

############### IMPLAUSIBILIDADE ############################################

regras_sinasc_implau <- c(fromJSON(file = 'data1/SINASC_Implausibilidade_Regras.json'))
Sinasc_implau <- read_csv('data1/SINASC_Implausibilidade_v2.csv',show_col_types = FALSE)
S2021 <- read_csv("data1/SINASC_Implausibilidade_2021.csv",show_col_types = FALSE )
S2022 <- read_csv("data1/SINASC_Implausibilidade_2022.csv",show_col_types = FALSE )
S2021$ESTADO <- NA
S2022$ESTADO <- NA

Sinasc_implau <- rbind(Sinasc_implau,S2021,S2022)
Sinasc_implau$VARIAVEL <- Sinasc_implau$VARIAVEL |>
  gsub(pattern = "_IMPLAUSIVEL", replacement = '')
Sinasc_implau$VARIAVEL %>% unique()

#FILTRAR APENAS PARA VARIAVEIS PRESENTES NO DICIONARIO

Sinasc_implau$VARIAVEL %>% unique() %>% setdiff(vars)
Sinasc_implau <- Sinasc_implau[Sinasc_implau$VARIAVEL %in% vars,]

#ACRESCENTAR A COLUNA DE MUNICIPIOS E MUNICIPIOS

Sinasc_implau$CODMUNNASC <- as.character(format(Sinasc_implau$CODMUNNASC , scientific = FALSE))
Sinasc_implau$CODMUNNASC <- gsub(' ','',Sinasc_implau$CODMUNNASC)

Sinasc_implau <- Sinasc_implau %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun') %>% select(-muni_id,-uf_id)

Sinasc_implau <- Sinasc_implau |>
  mutate(uf_id = stringr::str_sub(cod_mun,1,2))


Sinasc_implau[is.na(Sinasc_implau$uf_sigla)==T,'uf_sigla']<-
  Sinasc_implau[is.na(Sinasc_implau$uf_sigla)==T,]|>
  left_join(unique(aux_muni2[,c('uf_id','uf_sigla')]),by = 'uf_id') |> dplyr::select(uf_sigla.y)

Sinasc_implau[is.na(Sinasc_implau$muni_nm_clean)==T,'muni_nm_clean'] <- 'Não informado'

Sinasc_implau$CODMUNNASC <- Sinasc_implau$muni_nm_clean
Sinasc_implau$ESTADO <- Sinasc_implau$uf_sigla
Sinasc_implau[is.na(Sinasc_implau$ESTADO),'ESTADO'] <- 'Não informado'

Sinasc_implau[,c('cod_mun','uf_id','uf_sigla','muni_nm_clean')] <- NULL

regras_sinasc_implau <-regras_sinasc_implau |> as.data.frame()  |> t() |> as.data.frame()
regras_sinasc_implau <- cbind(regras_sinasc_implau|> row.names(), regras_sinasc_implau)
regras_sinasc_implau |> row.names() <- NULL
regras_sinasc_implau |> colnames() <- c('Variável','Regra')
regras_sinasc_implau$Regra <- regras_sinasc_implau$Regra |> gsub(pattern = 'não',replacement = ' não')

var_aux <- Sinasc_implau$VARIAVEL |> unique()
Sinasc_implau <- merge(Sinasc_implau, SINASC_dic[,c("Codigo Qualidados", "Codigo SINASC") ], by.x="VARIAVEL", by.y="Codigo SINASC", all=TRUE)
Sinasc_implau <- Sinasc_implau[Sinasc_implau$VARIAVEL %in% var_aux,]
Sinasc_implau$VARIAVEL <- Sinasc_implau$`Codigo Qualidados`
Sinasc_implau$`Codigo Qualidados` <- NULL
vars_implau_sinasc <- unique(Sinasc_implau$VARIAVEL)

###################################### INCONSISTÊNCIA ###########################

Sinasc_incon<- read_csv("data1/SINASC_Inconsistencia_v2.csv")
S2021 <- read_csv("data1/SINASC_Inconsistencia_2021.csv",show_col_types = FALSE )

S2022 <- read_csv("data1/SINASC_Inconsistencia_2022.csv",show_col_types = FALSE )
Sinasc_incon <- rbind(Sinasc_incon,S2021,S2022)
regras_sinasc_incon <-  c(fromJSON(file = 'data1/SINASC_Inconsistencias_Regras.json'))
var_incon_sinasc <-Sinasc_incon$VARIAVEL |>
  stringr::str_sub(1,nchar(Sinasc_incon$VARIAVEL)-15) |>
  unique() |>
  gsub(pattern = '_', replacement = ' ')
nomes_incon <- Sinasc_incon$VARIAVEL |> unique()
var_incon_sinasc |> names() <- nomes_incon
# ACRESCENTAR MUNICIPIO
Sinasc_incon$CODMUNNASC <- as.character(format(Sinasc_incon$CODMUNNASC , scientific = FALSE))
Sinasc_incon$CODMUNNASC <- gsub(' ','',Sinasc_incon$CODMUNNASC)
Sinasc_incon <- Sinasc_incon %>%
  rename(cod_mun = CODMUNNASC ) %>%
  left_join(aux_muni2 ,by='cod_mun')

Sinasc_incon[,c('muni_id','uf_id')] <- NULL

Sinasc_incon <- Sinasc_incon |>
  mutate(uf_id = stringr::str_sub(cod_mun,1,2))


Sinasc_incon[is.na(Sinasc_incon$uf_sigla)==T,'uf_sigla']<-
  Sinasc_incon[is.na(Sinasc_incon$uf_sigla)==T,]|>
  left_join(unique(aux_muni2[,c('uf_id','uf_sigla')]),by = 'uf_id') |> dplyr::select(uf_sigla.y)

Sinasc_incon[is.na(Sinasc_incon$muni_nm_clean)==T,'muni_nm_clean'] <- 'Não informado'

Sinasc_incon$CODMUNNASC <- Sinasc_incon$muni_nm_clean
Sinasc_incon$ESTADO <- Sinasc_incon$uf_sigla
Sinasc_incon[,c('cod_mun','uf_id','uf_sigla','muni_nm_clean')] <- NULL
Sinasc_incon[is.na(Sinasc_incon$ESTADO) == T,'ESTADO'] <- 'Não informado'

regras_sinasc_incon <-regras_sinasc_incon |> as.data.frame()  |> t() |> as.data.frame()
regras_sinasc_incon <- cbind(regras_sinasc_incon|> row.names(), regras_sinasc_incon)
regras_sinasc_incon |> row.names() <- NULL
regras_sinasc_incon |> colnames() <- c('Variável','Regra')
regras_sinasc_incon$Variável <- regras_sinasc_incon$Variável |> gsub(pattern = '_',replacement = ' ')


###############################################  EXPORTACAO ##################
usethis::use_data(Sinasc_implau, overwrite = TRUE)
usethis::use_data(vars_implau_sinasc, overwrite = TRUE)
usethis::use_data(Sinasc_incom, overwrite = TRUE)
usethis::use_data(vars_incom_sinasc, overwrite = TRUE)
usethis::use_data(Sinasc_incon, overwrite = TRUE)
usethis::use_data(var_incon_sinasc, overwrite = TRUE)


######## REGRAS ####################
regras_sinasc_implau$Indicador <- 'Implausibilidade'
regras_sinasc_incon$Indicador <- 'Inconsistência'
regras_sinasc_incom$Indicador <- 'Incompletude'



for(i in seq_along(SINASC_dic$`Codigo SINASC`)) {
  for(j in 1:ncol(regras_sinasc_implau)){
    regras_sinasc_implau[,j] <- gsub(SINASC_dic$`Codigo SINASC`[i],
                                     SINASC_dic$`Codigo Qualidados`[i],
                                  regras_sinasc_implau[,j])
  }
}
regras_sinasc_implau <- regras_sinasc_implau[regras_sinasc_implau$Variável %in% vars_implau_sinasc,]
for(i in seq_along(SINASC_dic$`Codigo SINASC`)) {
  for(j in 1:ncol(regras_sinasc_incom)){
    regras_sinasc_incom[,j] <- gsub(SINASC_dic$`Codigo SINASC`[i],
                                    SINASC_dic$`Codigo Qualidados`[i],
                                 regras_sinasc_incom[,j])
  }
}

regras_sinasc_incom <- regras_sinasc_incom[regras_sinasc_incom$Variável %in% vars_incom_sinasc,]
regras_sinasc <- rbind(regras_sinasc_implau,regras_sinasc_incom,regras_sinasc_incon)

usethis::use_data(regras_sinasc, overwrite = TRUE)
