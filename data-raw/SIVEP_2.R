library(readr)
library(magrittr)
library(dplyr)
library(jsonlite)
library(readxl)

SIVEP_dic <- read_excel("data1/dicionarios.xlsx", sheet = "SIVEP")
usethis::use_data(SIVEP_dic,overwrite = T)
df <- readRDS("data1/Sivep_2009-2022.rds")

variaveis_dic <- c('CS_SEXO','NU_IDADE_N','TP_IDADE','CS_RACA','CS_ESCOL_N','CS_ZONA','SURTO_SG','NOSOCOMIAL','AVE_SUINO','FEBRE','TOSSE','GARGANTA','DISPNEIA','DESC_RESP','SATURACAO','DIARREIA',
               'VOMITO','OUTRO_SIN','FATOR_RISC','CARDIOPATI','HEMATOLOGI','SIND_DOWN','HEPATICA','ASMA','DIABETES','NEUROLOGIC','PNEUMOPATI','IMUNODEPRE','RENAL','OBESIDADE','OUT_MORBI',
               'MAE_VAC','M_AMAMENTA','ANTIVIRAL','TP_ANTIVIR','HOSPITAL','DT_INTERNA','UTI','SUPORT_VEN','AMOSTRA','DT_COLETA','POS_PCRFLU','POS_PCROUT','EVOLUCAO','HISTO_VGM','DOR_ABD',
               'FADIGA','PERD_OLFT','PERD_PALA','POS_AN_FLU','POS_AN_OUT','CS_GESTANT','DT_UT_DOSE','DT_VAC_MAE','DT_DOSEUNI','DT_ENTUTI','RAIOX_RES','DT_RAIOX','TOMO_RES','DT_TOMO',
               'TP_TES_AN','DT_RES_AN','VACINA_COV','VACINA','TP_AM_SOR','PUERPERA')

#CRIANDO CLASSIFICACAO DE GESTANTE E PUERP
df_gest <- df %>%
  mutate(
    classi_gesta_puerp = case_when(
      CS_GESTANT == 1 | CS_GESTANT == 1.0 | CS_GESTANT == '1' | CS_GESTANT == '1.0' ~ "1tri",
      CS_GESTANT == 2 | CS_GESTANT == 2.0 | CS_GESTANT == '2' | CS_GESTANT == '2.0' ~ "2tri",
      CS_GESTANT == 3 | CS_GESTANT == 3.0 | CS_GESTANT == '3' | CS_GESTANT == '3.0'  ~ "3tri",
      CS_GESTANT == 4 | CS_GESTANT == 4.0 | CS_GESTANT == '4' | CS_GESTANT == '4.0' ~ "IG_ig",
     ( CS_GESTANT == 5  &
        PUERPERA == 1 )| ( CS_GESTANT == 5.0  &
                             PUERPERA == 1.0 )| ( CS_GESTANT == '5.0'  &
                                                    PUERPERA == '1.0' )~ "puerp",
      (CS_GESTANT == 9 & PUERPERA == 1) |(CS_GESTANT == 9.0 & PUERPERA == 1.0)|(CS_GESTANT == '9.0' & PUERPERA == '1.0')~ "puerp",
      TRUE ~ "não"
    ),
    dt_sint = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
    dt_nasc = as.Date(DT_NASC, format = "%d/%m/%Y"),
    ano = lubridate::year(dt_sint),
    muni_nm_clean = paste(ID_MUNICIP, "-", SG_UF_NOT)
  )

df_gest <- df_gest %>% mutate_if(~ !is.character(.), as.character)
df_gest <- data.frame(lapply(df_gest, function(x) ifelse(x == "1.0", '1',
                                                        ifelse(x == '2.0','2',
                                                               ifelse(x == '3.0','3',
                                                                      ifelse(x == '4.0','4',
                                                                             ifelse(x == '5.0','5',
                                                                                    ifelse(x == '6.0','6',
                                                                                           ifelse(x == '7.0','7',
                                                                                                  ifelse(x == '8.0','8',
                                                                                    ifelse(x == '9.0','9',x)))))))))))

df_gest %>% nrow()
#filtrando gravidas e puerperas
df_gest <- df_gest[df_gest$classi_gesta_puerp != 'não',]

# df_gest[df_gest$ano == '2013' & (is.na(df_gest$OBESIDADE) |df_gest$OBESIDADE== '9.0'),'OBESIDADE']
# df_gest$OBESIDADE %>% unique()
# df_gest[df_gest$ano == '2013' & (df_gest$OBESIDADE %in% c(3,4,5)),'OBESIDADE']
# df_gest[df_gest$ano == '2013' & (df_gest$OBESIDADE %in% c('1.0','2.0')),'OBESIDADE']
# df_gest[df_gest$ano == '2013','OBESIDADE']  %>% length()
# df_gest[df_gest$ano == '2013','OBESIDADE']  %>% unique()

sivep2 <- df_gest
# Usando mutate_if para transformar todas as colunas não-char em char

# INCOMPLETUDE ------------------------------------------------------------
regras_incom <- fromJSON('data1/incompletude_sivep.json')

#VARIAVEIS DO DICIONARIO + VARIAVEIS PARA FILTRAGEM
df_gest2 <- df_gest[,c(variaveis_dic,'classi_gesta_puerp','ano','muni_nm_clean','SG_UF_NOT','CLASSI_FIN')]

#VARIAVEIS EM QUE O VALOR 9 E O VALOR IGNORADO:
variaveis_ign <- c('CS_SEXO','CS_RACA','CS_ESCOL_N','CS_ZONA','NOSOCOMIAL','AVE_SUINO','FEBRE','TOSSE','GARGANTA','DISPNEIA',
                   'DESC_RESP','SATURACAO','DIARREIA','VOMITO','OUTRO_SIN','FATOR_RISC','CARDIOPATI','HEMATOLOGI','SIND_DOWN',
                   'HEPATICA','ASMA','DIABETES','NEUROLOGIC','PNEUMOPATI','IMUNODEPRE','RENAL','OBESIDADE','OUT_MORBI',
                   'MAE_VAC','M_AMAMENTA','ANTIVIRAL','HOSPITAL','UTI','SUPORT_VEN','AMOSTRA','POS_PCRFLU','POS_PCROUT',
                   'EVOLUCAO','DOR_ABD','FADIGA','PERD_OLFT','PERD_PALA','POS_AN_FLU','POS_AN_OUT','CS_GESTANT','RAIOX_RES',
                   'TOMO_RES','VACINA_COV','VACINA','PUERPERA','CLASSI_FIN')
setdiff(variaveis_dic,variaveis_ign)

#SUBSTITUIR VALORES NA POR EM BRANCO
sivep <- replace(df_gest2,is.na(df_gest2) ,"Em Branco")

#SUBSTITUIR VALORES 9 POR IGNORADO
sivep[, variaveis_ign] <- lapply(sivep[, variaveis_ign], function(x) ifelse(x == '9'|x == '9.0', "Ignorado", x))

# Calcular as porcentagens de valores 'Ignorados' e 'Em branco' por coluna
ignored_pct <- colMeans(sivep == "Ignorado", na.rm = TRUE) * 100
blank_pct <- colMeans(sivep == "Em Branco", na.rm = TRUE) * 100

# IMPLAUSIBILIDADE --------------------------------------------------------

regras_implau <- fromJSON('data1/implausibilidade_gestantes.json')
regras_implau2 <- fromJSON('data1/implausibilidade_puerperas.json')

# Criando vetores de variáveis improváveis e impossíveis
improvavel <- grep("_IMPROVAVEL", names(regras_implau), value = TRUE)
impossivel <- grep("_IMPOSSIVEL", names(regras_implau), value = TRUE)
impossivel2 <- grep("_IMPOSSIVEL", names(regras_implau2), value = TRUE)
impossivel <- c(impossivel2, setdiff(impossivel, impossivel2))
# Criando um data.frame com as variáveis improváveis
df_improvavel <- data.frame(
  variavel = gsub(improvavel,pattern = '_IMPROVAVEL',replacement = ''))

# Criando um data.frame com as variáveis impossíveis
df_impossivel <- data.frame(
  variavel =  gsub(impossivel,pattern = '_IMPOSSIVEL',replacement = ''))

# Trocando regras em string por booleanos
df_impossivel <- df_impossivel %>%
  mutate(condicao = case_when(
    grepl("CS_SEXO", variavel) ~ "CS_SEXO != 'F'",
    grepl("NU_IDADE_N", variavel) ~ "as.integer(NU_IDADE_N) < 0 | as.integer(NU_IDADE_N) > 90",
    grepl("CS_GESTANT", variavel) ~ "CS_GESTANT %in% c('1','2','3','4') & PUERPERA == '1' ",
    grepl("DT_INTERNA", variavel) ~ "lubridate::year(as.Date(DT_INTERNA,format = '%d/%m/%Y')) < 2019 & !(is.na(lubridate::year(as.Date(DT_INTERNA,format = '%d/%m/%Y'))))",
    grepl("DT_COLETA", variavel) ~ "lubridate::year(as.Date(DT_COLETA,format = '%d/%m/%Y')) < 2019 & !(is.na(lubridate::year(as.Date(DT_COLETA,format = '%d/%m/%Y'))))",
    grepl("TP_IDADE", variavel) ~ "TP_IDADE != '1' & TP_IDADE != '2' & TP_IDADE != '3'",
    grepl("TP_ANTIVIR", variavel) ~ "TP_ANTIVIR != '1' & TP_ANTIVIR != '2' & TP_ANTIVIR != '3'",
    grepl("SURTO_SG", variavel) ~ "SURTO_SG != '1' & SURTO_SG != '2' & SURTO_SG != 'Ignorado'",
    grepl("NOSOCOMIAL", variavel) ~ "NOSOCOMIAL != '1' & NOSOCOMIAL != '2' & NOSOCOMIAL != 'Ignorado'",
    grepl("AVE_SUINO", variavel) ~ "AVE_SUINO != '1' & AVE_SUINO != '2' & AVE_SUINO != 'Ignorado'",
    grepl("FEBRE", variavel) ~ "FEBRE != '1' & FEBRE != '2' & FEBRE != 'Ignorado'",
    grepl("TOSSE", variavel) ~ "TOSSE != '1' & TOSSE != '2' & TOSSE != 'Ignorado'",
    grepl("GARGANTA", variavel) ~ "GARGANTA != '1' & GARGANTA != '2' & GARGANTA != 'Ignorado'",
    grepl("DISPNEIA", variavel) ~ "DISPNEIA != '1' & DISPNEIA != '2' & DISPNEIA != 'Ignorado'",
    grepl("DESC_RESP", variavel) ~ "DESC_RESP != '1' & DESC_RESP !='2' & DESC_RESP != 'Ignorado'",
    grepl("SATURACAO", variavel) ~ "SATURACAO != '1'  & SATURACAO != '2' & SATURACAO != 'Ignorado'",
    grepl("DIARREIA", variavel) ~ "DIARREIA != '1' & DIARREIA != '2' & DIARREIA != 'Ignorado'",
    grepl("VOMITO", variavel) ~ "VOMITO != '1' & VOMITO != '2' & VOMITO != 'Ignorado'",
    grepl("OUTRO_SIN", variavel) ~ "OUTRO_SIN != '1' & OUTRO_SIN != '2' & OUTRO_SIN != 'Ignorado'",
    grepl("FATOR_RISC", variavel) ~ "FATOR_RISC != '1' & FATOR_RISC != '2' & FATOR_RISC != 'Ignorado'",
    grepl("CARDIOPATI", variavel) ~ "CARDIOPATI != '1' & CARDIOPATI != '2' & CARDIOPATI != 'Ignorado'",
    grepl("HEMATOLOGI", variavel) ~ "HEMATOLOGI != '1' & HEMATOLOGI != '2' & HEMATOLOGI != 'Ignorado'",
    grepl("SIND_DOWN", variavel) ~ "SIND_DOWN != '1' & SIND_DOWN != '2' & SIND_DOWN != 'Ignorado'",
    grepl("HEPATICA", variavel) ~ "HEPATICA != '1' & HEPATICA != '2' & HEPATICA != 'Ignorado'",
    grepl("ASMA", variavel) ~ "ASMA != '1' & ASMA != '2' & ASMA != 'Ignorado'",
    grepl("DIABETES", variavel) ~ "DIABETES != '1' & DIABETES != '2' & DIABETES != 'Ignorado'",
    grepl("NEUROLOGIC", variavel) ~ "NEUROLOGIC != '1' & NEUROLOGIC != '2' & NEUROLOGIC != 'Ignorado'",
    grepl("PNEUMOPATI", variavel) ~ "PNEUMOPATI != '1' & PNEUMOPATI != '2' & PNEUMOPATI != 'Ignorado'",
    grepl("IMUNODEPRE", variavel) ~ "IMUNODEPRE != '1 ' & IMUNODEPRE != '2' & IMUNODEPRE != 'Ignorado'",
    grepl("RENAL", variavel) ~ "RENAL != '1' & RENAL != '2' & RENAL != 'Ignorado'",
    grepl("OBESIDADE", variavel) ~ "OBESIDADE != '1' & OBESIDADE != '2' & OBESIDADE != 'Ignorado'",
    grepl("OUT_MORBI", variavel) ~ "OUT_MORBI != '1' & OUT_MORBI != '2' & OUT_MORBI != 'Ignorado'",
    grepl("VACINA", variavel) ~ "VACINA != '1' & VACINA != '2' & VACINA != 'Ignorado'",
    grepl("MAE_VAC", variavel) ~ "MAE_VAC != '1' & MAE_VAC != '2' & MAE_VAC != 'Ignorado'",
    grepl("M_AMAMENTA", variavel) ~ "M_AMAMENTA != '1' & M_AMAMENTA != '2' & M_AMAMENTA != 'Ignorado'",
    grepl("ANTIVIRAL", variavel) ~ "ANTIVIRAL != '1' & ANTIVIRAL != '2' & ANTIVIRAL != 'Ignorado'",
    grepl("HOSPITAL", variavel) ~ "HOSPITAL != '1' & HOSPITAL != '2' & HOSPITAL != 'Ignorado'",
    grepl("UTI", variavel) ~ "UTI != '1' & UTI != '2' & UTI != 'Ignorado'",
    grepl("AMOSTRA", variavel) ~ "AMOSTRA != '1' & AMOSTRA != '2' & AMOSTRA != 'Ignorado'",
    grepl("POS_PCRFLU", variavel) ~ "POS_PCRFLU != '1' & POS_PCRFLU != '2' & POS_PCRFLU != 'Ignorado'",
    grepl("POS_PCROUT", variavel) ~ "POS_PCROUT != '1' & POS_PCROUT != '2' & POS_PCROUT != 'Ignorado'",
    grepl("HISTO_VGM", variavel) ~ "HISTO_VGM != '1' & HISTO_VGM != '2' & HISTO_VGM != 'Ignorado'",
    grepl("DOR_ABD", variavel) ~ "DOR_ABD != '1' & DOR_ABD != '2' & DOR_ABD != 'Ignorado'",
    grepl("FADIGA", variavel) ~ "FADIGA != '1' & FADIGA != '2' & FADIGA != 'Ignorado'",
    grepl("PERD_OLFT", variavel) ~ "PERD_OLFT != '1' & PERD_OLFT != '2' & PERD_OLFT != 'Ignorado'",
    grepl("PERD_PALA", variavel) ~ "PERD_PALA != '1' & PERD_PALA != '2' & PERD_PALA != 'Ignorado'",
    grepl("POS_AN_FLU", variavel) ~ "POS_AN_FLU != '1' & POS_AN_FLU != '2' & POS_AN_FLU != 'Ignorado'",
    grepl("POS_AN_OUT", variavel) ~ "POS_AN_OUT != '1' & POS_AN_OUT != '2' & POS_AN_OUT != 'Ignorado'",
    grepl("TP_AM_SOR", variavel) ~ "TP_AM_SOR != '1' & TP_AM_SOR != '2' & TP_AM_SOR != 'Ignorado'",
    grepl("PUERPERA", variavel) ~ "(PUERPERA %in% c('1')) & (CS_GESTANT %in% c('1','2','3','4'))"
    ))
    df_improvavel <- df_improvavel %>%
  mutate(condicao = case_when(
    grepl("NU_IDADE_N", variavel) ~ "as.integer(NU_IDADE_N) < 10 | as.integer(NU_IDADE_N) > 55"))

#Substituindo os valores do banco sivep por improvavel e impossivel
attach(sivep)
for(i in 1:nrow(df_impossivel)){
  var <- df_impossivel$variavel[i]
  cond <- df_impossivel$condicao[i]
  sivep[eval(parse(text = paste0(cond," & ",var," != 'Em Branco'"))),var]<- 'Impossivel'
}
for(i in 1:nrow(df_improvavel)){
  var <- df_improvavel$variavel[i]
  cond <- df_improvavel$condicao[i]
  sivep[eval(parse(text = paste0("(",cond,") & (",var," != 'Em branco' &",var," != 'Ignorado') "))),var] <- 'Improvavel'
}
sivep_ic_ip <- sivep

# INCONSISTENCIA ----------------------------------------------------------

incon <- fromJSON('data1/SIVEP_Inconsistencias_Regras.json')
df_gest$POS_AN_FLU %>% unique()
# Criando um data.frame com as variáveis improváveis
df_inconsistencia <- data.frame(
  variavel = names(incon) %>% gsub(pattern = '_e_', replacement = ' e '))
# Trocando regras em string por booleanos
df_inconsistencia <- df_inconsistencia %>%
  mutate(condicao = case_when(
    grepl("CS_SEXO e CS_GESTANT", variavel) ~ "(df_gest_aux$CS_SEXO %in% c('M', 'I')) & (df_gest_aux$CS_GESTANT %in% c('1','2','3','4'))",
    grepl('FATOR_RISC e COMORBIDADES', variavel) ~ "((df_gest_aux$FATOR_RISC == '2' | df_gest_aux$FATOR_RISC == '9') & (df_gest_aux$CARDIOPATI == '1' | df_gest_aux$HEMATOLOGI == '1' | df_gest_aux$SIND_DOWN == '1' | df_gest_aux$HEPATICA == '1' | df_gest_aux$ASMA == '1' | df_gest_aux$DIABETES == '1' |  df_gest_aux$NEUROLOGIC == '1' | df_gest_aux$PNEUMOPATI == '1' | df_gest_aux$IMUNODEPRE == '1' | df_gest_aux$RENAL == '1' | df_gest_aux$OBESIDADE == '1' | df_gest_aux$OBES_IMC == '1' | df_gest_aux$OUT_MORBI == '1')) | ((df_gest_aux$FATOR_RISC == '1') & (df_gest_aux$CARDIOPATI != '1' & df_gest_aux$HEMATOLOGI != '1' & df_gest_aux$SIND_DOWN != '1' & df_gest_aux$HEPATICA != '1' & df_gest_aux$ASMA != '1' & df_gest_aux$DIABETES != '1' &  df_gest_aux$NEUROLOGIC != '1' & df_gest_aux$PNEUMOPATI != '1' & df_gest_aux$IMUNODEPRE != '1' & df_gest_aux$RENAL != '1' & df_gest_aux$OBESIDADE != '1' & df_gest_aux$OBES_IMC != '1' & df_gest_aux$OUT_MORBI != '1'))",
    grepl("VACINA e DT_UT_DOSE", variavel) ~ "df_gest_aux$VACINA %in% c('2', '9') & (df_gest_aux$DT_UT_DOSE != 'Em Branco')",
    grepl("MAE_VAC e DT_VAC_MAE", variavel) ~ "df_gest_aux$MAE_VAC %in% c('2', '9') & (df_gest_aux$DT_VAC_MAE != 'Em Branco')",
    grepl("DT_DOSEUNI e NU_IDADE_N", variavel) ~ "(df_gest_aux$DT_DOSEUNI != 'Em Branco') & (as.integer(df_gest_aux$NU_IDADE_N) <= '6' | as.integer(df_gest_aux$NU_IDADE_N) >= '8')",
    grepl("ANTIVIRAL e TP_ANTIVIR", variavel) ~ "df_gest_aux$ANTIVIRAL %in% c('2', '9') & df_gest_aux$TP_ANTIVIR %in% c('1', '2', '3')",
    grepl("HOSPITAL e DT_INTERNA", variavel) ~ "df_gest_aux$HOSPITAL %in% c('2', '9') & (df_gest_aux$DT_INTERNA != 'Em Branco')",
    grepl("UTI e DT_ENTUTI", variavel) ~ "(df_gest_aux$UTI == '2' | df_gest_aux$UTI == '9') & (df_gest_aux$DT_ENTUTI != 'Em Branco') | (df_gest_aux$HOSPITAL == '2' | df_gest_aux$HOSPITAL == '9') & df_gest_aux$UTI == '1'",
    grepl("RAIOX_RES e DT_RAIOX", variavel) ~ "(df_gest_aux$RAIOX_RES == '6' | df_gest_aux$RAIOX_RES == '9') & (df_gest_aux$DT_RAIOX!= 'Em Branco')",
    grepl("AMOSTRA e DT_COLETA", variavel) ~ "(df_gest_aux$AMOSTRA == '6' | df_gest_aux$AMOSTRA == '9') & (df_gest_aux$DT_COLETA != 'Em Branco')",
    grepl("HISTO_VGM e Campos_VGMs", variavel) ~ "(df_gest_aux$HISTO_VGM == '2' | df_gest_aux$HISTO_VGM == '9') & (df_gest_aux$LO_PS_VGM != 'Em Branco') & (df_gest_aux$DT_VGM != 'Em Branco') & (df_gest_aux$DT_RT_VGM != 'Em Branco')",
    grepl("TOMO_RES e DT_TOMO", variavel) ~ "(df_gest_aux$TOMO_RES == '6' | df_gest_aux$TOMO_RES == '9') & (df_gest_aux$DT_TOMO != 'Em Branco')",
    grepl("TP_TES_AN e DT_RES_AN", variavel) ~ "((df_gest_aux$RES_AN == '4') & (df_gest_aux$TP_TES_AN %in% c('1', '2'))) | ((df_gest_aux$RES_AN == '4') & (df_gest_aux$DT_RES_AN != 'Em Branco'))",
    grepl("VACINA_COV e DOSES", variavel) ~ "(df_gest_aux$VACINA_COV %in% c('2', '9')) & ((df_gest_aux$DOSE_1_COV != 'Em Branco') | (df_gest_aux$DOSE_2_COV!= 'Em Branco'))",
    grepl("CLASSI_FIN_SRAG_INFLUENZA", variavel) ~ "df_gest_aux$CLASSI_FIN == '1' & df_gest_aux$POS_PCRFLU %in% c('2', '9') & df_gest_aux$POS_AN_FLU %in% c('2', '9')",
    grepl("CLASSI_FIN_SRAG_OUTROS_VIRUS", variavel) ~ "df_gest_aux$CLASSI_FIN == '1' & df_gest_aux$PCR_OUTRO %in% c('2', '9') & df_gest_aux$AN_OUTRO %in% c('2 ', '9')"
  ))
df_inconsistencia <- head(df_inconsistencia, -2)
# Criando colunas de inconsistencia no df_gest
df_gest_aux <- df_gest
#SUBSTITUIR VALORES NA POR EM BRANCO
df_gest_aux <- data.frame(lapply(df_gest_aux, function(x) ifelse(is.na(x), "Em Branco", x)))
for(i in 1:nrow(df_inconsistencia)){
  df_gest_aux[[df_inconsistencia$variavel[i]]] <- 'Nao'
}
df_gest_aux %>% colnames()
# Verificando a condição de inconsistência para cada variável do Inconsistencias_df

for(i in 1:(nrow(df_inconsistencia))){
  var <- df_inconsistencia$variavel[i]
  cond <- df_inconsistencia$condicao[i]
  df_gest_aux[eval(parse(text = paste0(cond))) ,var] <- 'Inconsistencia'
}
n <- nrow(df_inconsistencia)
maxi <- ncol(df_gest_aux)

# CONCATENANDO E MUDANDO NOME DAS COLUNAS ---------------------------------

sivep <- cbind(sivep_ic_ip,df_gest_aux[,(maxi - n + 1):maxi])

#RENOMEANDO AS COLUNAS COM BASE NO DICIONARIO

nomes_colunas <- colnames(sivep)

# Substituindo os nomes originais pelos novos
for(i in seq_along(SIVEP_dic$`Codigo SIVEP`)) {
  nomes_colunas <- gsub(SIVEP_dic$`Codigo SIVEP`[i],
                        SIVEP_dic$`Codigo Qualidados`[i],
                        nomes_colunas)
}

# Atribuindo os novos nomes de colunas ao dataframe
colnames(sivep) <- nomes_colunas
sivep_dados <- sivep
regras_implau

# CRIAR VETOR DE NOME DE VARIAVEIS ----------------------------------------
var_sivep_incom <- names(sivep_dados)[apply(sivep_dados == "Em Branco" | sivep_dados == 'Ignorado', 2, any)]
var_sivep_implau <- names(sivep_dados)[apply(sivep_dados == "Impossivel" | sivep_dados == 'Improvavel', 2, any)]
var_sivep_incon <- names(sivep_dados)[apply(sivep_dados == "Inconsistencia" , 2, any)]
var_sivep_incom <- var_sivep_incom[(var_sivep_incom  %in% SIVEP_dic$`Codigo Qualidados`)]
var_sivep_implau <- var_sivep_implau[(var_sivep_implau  %in% SIVEP_dic$`Codigo Qualidados`)]

sivep_dados[apply(sivep_dados == "Inconsistencia" , 2, any)] %>% nrow()

# CRIAR REGRAS DO SIVEP ---------------------------------------------------
#inconsistencia
incon <- incon |> as.data.frame() |> t() |> as.data.frame()
incon <- cbind(incon |> row.names(),incon)
incon |> row.names() <- NULL
incon |> colnames() <- c('Variavel','Regra')
incon$Variavel <- incon$Variavel |> gsub(pattern = '_e_', replacement = ' e ')
incon$Indicador <- 'Inconsistência'

#implausibilidade
regras_implau <- regras_implau |> as.data.frame() |> t() |> as.data.frame()
regras_implau <- cbind(regras_implau |> row.names(),regras_implau)
regras_implau |> row.names() <- NULL
regras_implau |> colnames() <- c('Variavel','Regra')
regras_implau$Variavel <- regras_implau$Variavel |> gsub(pattern = '_IMPOSSIVEL', replacement = '')
regras_implau$Regra <- regras_implau$Regra |> gsub(pattern = 'de gestantes ', replacement = '')
regras_implau$Regra <- regras_implau$Regra |> gsub(pattern = 'Gestantes ', replacement = 'Gestantes e puérperas ')
regras_implau$Regra[4] <- 'Gestantes e puérperas ao mesmo tempo'

#incompletude
regras_incom <- regras_incom |> as.data.frame() |> t() |> as.data.frame()
regras_incom <- cbind(regras_incom |> row.names(),regras_incom)
regras_incom |> row.names() <- NULL
regras_incom |> colnames() <- c('Variavel','Regra')
regras_incom$Indicador <- 'Incompletude'


regras_implau$Indicador <- 'Implausiblidade'
regras_sivep <- rbind(incon,regras_implau,regras_incom)
desc_incom <- 'análise das informações que estão faltando na base de dados, seja porque não foram preenchidas (“dados em branco”) ou porque a resposta era desconhecida (“dados ignorados”).'
 desc_implau <- "análise das informações que são improváveis e/ou dificilmente possam ser consideradas aceitáveis dadas as características de sua natureza."
 desc_incon <- "informações que parecem ilógicas e/ou incompatíveis a partir da análise da combinação dos dados informados em dois ou mais campos do formulário."
usethis::use_data(regras_sivep,overwrite = T)
usethis::use_data(sivep_dados,overwrite = T)
usethis::use_data(var_sivep_incom,overwrite = T)
usethis::use_data(var_sivep_implau,overwrite = T)
usethis::use_data(var_sivep_incon,overwrite = T)
usethis::use_data(desc_incom, overwrite = TRUE)
usethis::use_data(desc_implau, overwrite = TRUE)
usethis::use_data(desc_incon, overwrite = TRUE)
