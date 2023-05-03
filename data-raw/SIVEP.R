# library(readr)
# library(readxl)
# library(dplyr)
# library(DT)
# library(shinyjs)
# library(plotly)
# library(shinydashboard)
# library(ggridges)
# library(ggplot2)
# library(tidyr)
# library(forcats)
# library(rjson)
# library(kableExtra)
# library(shinyalert)
# #PRIMEIRA IMPORTAÇÃO -----------------------------
# SIVEP_dic <- read_excel("data1/dicionarios.xlsx", sheet = "SIVEP")
# usethis::use_data(SIVEP_dic,overwrite = T)
# #Tem que rever essa parte, ela nao parece muito util
#
# jsonfile <- c(fromJSON(file = "data1/data_values.json"))
# fields <- jsonfile$fields
#
# #MOSTRAR NOME MAIS DESCRICAO
# relatorio_df <-  readRDS("data1/dados_relatorio_sivep.rds") %>% data.frame()
#
# var_names_join <- list()
# var_names_join <- append(var_names_join,paste(names(fields[1]), "(", fields[[1]], ")"))
# for (field in 2:length(fields)){
#   if((names(fields[field]) %in% names(relatorio_df)) && !(names(fields[field]) %in% jsonfile$ignore))
#     var_names_join <- append(var_names_join, paste(names(fields[field]), "(", fields[[field]], ")"))
# }
#
# for(f in 1:length(names(relatorio_df))){
#   if(names(relatorio_df[f]) %in% jsonfile$datetime){
#     relatorio_df[[f]] <- substring(relatorio_df[[f]], 1, nchar(relatorio_df[[f]]))
#     relatorio_df[[f]] <- as.Date(x = relatorio_df[[f]], tryFormats = c("%d/%m/%y"))
#   }
# }
# #DADOS DE INCOMPLETUDE ------------------------
# dados_incom <- readRDS("data1/dados_incompletude.rds")
# for (variavel in jsonfile$variaveis_tabela) {
#   dados_incom[[variavel]][dados_incom[[variavel]] == "Ignorado"] <-
#     "Dados ignorados"
#   dados_incom[[variavel]][dados_incom[[variavel]] == "na"] <-
#     "Dados em branco"
#   dados_incom[[variavel]][dados_incom[[variavel]] == "não"] <-
#     "Dados válidos"
# }
#
# dados_incom[['ID_MUNICIP']] <- dados_incom$muni_nm_clean %>%
#   purrr::map_chr(function(x) stringr::str_split(x, " -")[[1]][1])
#
#
# #DADOS DE IMPLAUSIBILIDADE ------------
#
# jsonfile_puerp<- c(fromJSON(file = "data1/implausibilidade_puerperas.json"))
# jsonfile_gest<- c(fromJSON(file = "data1/implausibilidade_gestantes.json"))
# implau_gest <- readRDS("data1/implausibilidade_gestantes.rds") %>%
#   select(-PUERPERA_IMPOSSIVEL)
# implau_puerp <- readRDS("data1/implausibilidade_puerperas.rds") %>%
#   select(-CS_GESTANT_IMPOSSIVEL)
#
# dados_implau <- bind_rows(implau_gest, implau_puerp)
#
# dados_implau <- dados_implau %>%
#   mutate(
#     classi_gesta_puerp = case_when(
#       CS_GESTANT == 1  ~ "1tri",
#       CS_GESTANT == 2  ~ "2tri",
#       CS_GESTANT == 3  ~ "3tri",
#       CS_GESTANT == 4  ~ "IG_ig",
#       CS_GESTANT == 5 &
#         PUERPERA == 1 ~ "puerp",
#       CS_GESTANT == 9 & PUERPERA == 1 ~ "puerp",
#       TRUE ~ "não"
#     ),
#     dt_sint = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
#     dt_nasc = as.Date(DT_NASC, format = "%d/%m/%Y"),
#     ano = lubridate::year(dt_sint),
#     muni_nm_clean = paste(ID_MUNICIP, "-", SG_UF_NOT)
#   )
#
# var_dados_implau <- names(dados_implau)[stringr::str_detect(names(dados_implau), "IMPROVAVEL|IMPOSSIVEL")]
#
# dados_implau[, var_dados_implau] <- apply(dados_implau[, var_dados_implau], 2, function(x) as.logical(x))
#
# var_dados_implau <- purrr::map_chr(var_dados_implau, function(x) stringr::str_split(x, "_IMP")[[1]][1]) %>%
#   unique()
#
#
#
# #DADOS DE INCONSISTENCIA -----------------------------
#
# json_incon <- c(fromJSON(file = 'data1/SIVEP_Inconsistencias_Regras.json'))
# dados_incon  <- read_csv("data1/SIVEP_Inconsistencias.csv")
# dados_incon[['ID_MUNICIP']] <- dados_incon$ID_REGIONA
# dados_incon <- dados_incon %>%
#   mutate(
#     classi_gesta_puerp = case_when(
#       CS_GESTANT == 1  ~ "1tri",
#       CS_GESTANT == 2  ~ "2tri",
#       CS_GESTANT == 3  ~ "3tri",
#       CS_GESTANT == 4  ~ "IG_ig",
#       CS_GESTANT == 5 &
#         PUERPERA == 1 ~ "puerp",
#       CS_GESTANT == 9 & PUERPERA == 1 ~ "puerp",
#       TRUE ~ "não"),
#     dt_sint = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
#     dt_nasc = as.Date(DT_NASC, format = "%d/%m/%Y"),
#     ano = lubridate::year(dt_sint)
#   )
# #acertando dados de municipios
# aux_muni <- abjData::muni %>%
#   dplyr::select(muni_id,
#                 muni_nm_clean,
#                 uf_nm,
#                 uf_sigla,
#                 regiao_nm) %>%
#   mutate_at("muni_id", as.character)  %>%
#   mutate(cod_mun = stringr::str_sub(muni_id, 1, 6))
#
# dados_aux <- dados_incon %>%
#   dplyr::mutate_at("CO_MUN_RES", as.character) %>%
#   dplyr::rename(cod_mun = CO_MUN_RES) %>%
#   dplyr::left_join(aux_muni, by = "cod_mun")
#
# dados_aux <- dados_aux %>%
#   mutate(muni_nm_clean = ifelse(is.na(muni_nm_clean), ID_MN_RESI, muni_nm_clean))
# dados_aux <- dados_aux %>%
#   mutate(muni_nm_clean = paste(muni_nm_clean, "-", SG_UF))
# dados_incon <- dados_aux
#
#
# #-----------------------
#
# #ESTADOS DISPONIVEIS PARA FILTRAGEM ------------
# # estadosChoices <- c(
# #   "AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
# #   "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
# #   "RO","RR","RS","SC","SE","SP","TO")
#
# #VARIAVEIS DISPONIVEIS PARA INCOMPLETUDE -------------------
#
# variaveis_incom <- names(dados_incom)[stringr::str_detect(names(dados_incom), "^f_")]
# variaveis_incom_nomes <- c('RACA','ESCOLARIDADE',"ZONA","HIST_VIAGEM",
#                            "SURTO_SG","SRAG_POS","AVES_SUINOS","VACINA",
#                            "ANTIVIRAL","FEBRE","TOSSE","GARGANTA","DISPENEIA","DESC_RESPIRATORIO",
#                            "SATURAÇÃO","DIARREIA","VOMITO","DOR_ABD","FADIGA","PERDA_OLFT",
#                            "PERDA_PALADAR","CARDIOPATI","HEMATOLOGI","HEPÁTICA","ASMA","DIABETES",
#                            "NEUROLÓGICA","PNEUMOPATIA","IMUNODEPRESSAO","RENAL_CRON","OBESIDADE","UTI",
#                            "INTERNACAO","SUPORT_VENT","EVOLUCAO")
# variaveis_incom_nomes_ori <- c('CS_RACA','CS_ESCOL_N','CS_ZONA','HISTO_VGM','SURTO_SG',
#                                'NOSOCOMIAL','AVE_SUINO','VACINA','ANTIVIRAL','FEBRE',
#                                'TOSSE','GARGANTA','DISPNEIA','DESC_RESP','SATURACAO','DIARREIA',
#                                'VOMITO','DOR_ABD','FADIGA','PERD_OLFT','PERD_PALA','CARDIOPATI',
#                                'HEMATOLOGI','HEPATICA','ASMA','DIABETES','NEUROLOGIC','PNEUMOPATI',
#                                'IMUNODEPRE','RENAL','OBESIDADE','UTI','HOSPITAL','SUPORT_VEN','EVOLUCAO')
#
#
# #RELACAO ENTRE OS NOMES CERTOS E OS NOMES NO BD
#
# variaveis_relacao <- variaveis_incom_nomes
# names(variaveis_relacao) <- variaveis_incom
#
# #tem que mudar esse nome aqui, por algum motivo nao tava gerando a variavel dentro
# #do render plot
#
# variaveis <- NA
# var_names <- NA
#
# #DESCRICOES -------------------
# desc_incom <- 'análise das informações que estão faltando na base de dados, seja porque não foram preenchidas (“dados em branco”) ou porque a resposta era desconhecida (“dados ignorados”).'
# desc_implau <- "análise das informações que são improváveis e/ou dificilmente possam ser consideradas aceitáveis dadas as características de sua natureza."
# desc_incon <- "informações que parecem ilógicas e/ou incompatíveis a partir da análise da combinação dos dados informados em dois ou mais campos do formulário."
#
#
#
# #VARIAVEIS DISPONIVEIS PARA INCONSISTENCIA
#
# vars_incon <- gsub('_e_',' e ',colnames(dados_incon[,167:(ncol(dados_incon)-5)]))
# vars_incon <- gsub('_INCONSISTENTES','',vars_incon)
# names(vars_incon) <- colnames(dados_incon[,167:(ncol(dados_incon)-5)])
#
#
# Var_micro_incon <- dados_incon[,c(1:166,185,188)] %>% colnames()
# Var_micro_incon<-Var_micro_incon[stringr::str_detect('SG_UF|ID_MUNICIP',Var_micro_incon) == F]
#
#
# json_incon <- json_incon |> as.data.frame() |> t() |> as.data.frame()
# json_incon <- cbind(json_incon |> row.names(),json_incon)
# json_incon |> row.names() <- NULL
# json_incon |> colnames() <- c('Variavel','Regra')
# json_incon$Variavel <- json_incon$Variavel |> gsub(pattern = '_e_', replacement = ' e ')
# json_incon$Indicador <- 'Inconsistência'
#
# jsonfile_gest <- jsonfile_gest |> as.data.frame() |> t() |> as.data.frame()
# jsonfile_gest <- cbind(jsonfile_gest |> row.names(),jsonfile_gest)
# jsonfile_gest |> row.names() <- NULL
# jsonfile_gest |> colnames() <- c('Variavel','Regra')
# jsonfile_gest$Variavel <- jsonfile_gest$Variavel |> gsub(pattern = '_IMPOSSIVEL', replacement = '')
# jsonfile_gest$Regra <- jsonfile_gest$Regra |> gsub(pattern = 'de gestantes ', replacement = '')
# jsonfile_gest$Regra <- jsonfile_gest$Regra |> gsub(pattern = 'Gestantes ', replacement = 'Gestantes e puérperas ')
# jsonfile_gest$Regra[4] <- 'Gestantes e puérperas ao mesmo tempo'
#
# jsonfile_gest$Indicador <- 'Implausiblidade'
# regras_sivep <- rbind(json_incon,jsonfile_gest)
# ####################################### EXPORTAÇÃO
#
# usethis::use_data(regras_sivep, overwrite = TRUE)
#
# usethis::use_data(var_names_join, overwrite = TRUE)
# usethis::use_data(dados_incom, overwrite = TRUE)
# usethis::use_data(jsonfile_puerp, overwrite = TRUE)
# usethis::use_data(jsonfile_gest, overwrite = TRUE)
# usethis::use_data(dados_implau, overwrite = TRUE)
# usethis::use_data(var_dados_implau, overwrite = TRUE)
# usethis::use_data(json_incon, overwrite = TRUE)
# usethis::use_data(dados_incon, overwrite = TRUE)
# usethis::use_data(variaveis_incom, overwrite = TRUE)
# usethis::use_data(variaveis_incom_nomes, overwrite = TRUE)
# usethis::use_data(variaveis_incom_nomes_ori, overwrite = TRUE)
# usethis::use_data(variaveis_relacao, overwrite = TRUE)
# usethis::use_data(desc_incom, overwrite = TRUE)
# usethis::use_data(desc_implau, overwrite = TRUE)
# usethis::use_data(desc_incon, overwrite = TRUE)
# usethis::use_data(vars_incon, overwrite = TRUE)
# usethis::use_data(Var_micro_incon, overwrite = TRUE)
# usethis::use_data(variaveis, overwrite = TRUE)
# usethis::use_data(var_names, overwrite = TRUE)
