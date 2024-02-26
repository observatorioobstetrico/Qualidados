library(readr)
library(magrittr)
library(dplyr)
library(jsonlite)
library(readxl)

SIVEP_dic <- read_excel("data1/dicionarios.xlsx", sheet = "SIVEP")
df <- readRDS("data1/Sivep_2009-2022.rds")
df1 <- readRDS("data1/Sivep_2020-2023.rds")
variaveis_dic <- SIVEP_dic$`Codigo SIVEP`
df <- df[!(( as.Date(df$DT_SIN_PRI, format = "%d/%m/%Y") %>%lubridate::year() ) %in% c(2020,2021,2022)),]
df1 <- df1 %>%
  mutate_all(as.character)
df<-bind_rows(df1, df)
#BANCO AUXILIAR PARA CORRECAO DOS MUNICIPIOS
aux_muni2 <- abjData::muni %>%
  dplyr::select(uf_id,
                muni_id,
                muni_nm_clean,
                uf_sigla) %>%
  mutate_at("muni_id", as.character)  %>%
  mutate(cod_mun = stringr::str_sub(muni_id, 1, 6))

#CRIANDO CLASSIFICACAO DE GESTANTE E PUERP E CORRIGINDO OS MUNICIPIOS
df_gest <- df %>%
  #CORRECAO MUNICIPIOS
  left_join(aux_muni2, by = c("ID_MUNICIP" = "cod_mun")) %>%
  mutate(SG_UF_NOT = ifelse(is.na(muni_nm_clean), SG_UF_NOT, uf_sigla),
         ID_MUNICIP = ifelse(is.na(muni_nm_clean), ID_MUNICIP, muni_nm_clean)) %>%
  mutate(
    #DATA DO PRIMEIRO SINTOMA
    dt_sint = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
    #DATA DO NASCIMENTO
    dt_nasc = as.Date(DT_NASC, format = "%d/%m/%Y"),
    #ANO, BASEADO NA DATA DO PRIMEIRO SINTOMA
    ANO = lubridate::year(dt_sint),
    #MUNICIPIO
    MUNICIPIO = paste(ID_MUNICIP, "-", SG_UF_NOT)
  ) %>% select(-muni_nm_clean, -uf_sigla)

# CORRECAO DO ERRO QUE A FALTA DE PADRONIZACAO DOS DADOS OCASIONOU
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

df_gest %>% nrow()#CONFERINDO SE VOLTOU TUDO

sivep2 <- df_gest

#CRIANDO O DICIONARIO DE CONTINUIDADE
continuidade <- SIVEP_dic |>
  mutate(
    `Anos Válidos` = strsplit(`Anos Válidos`,',')
  ) |>
  select(`Codigo Qualidados`,`Anos Válidos`)

# INCOMPLETUDE ------------------------------------------------------------
regras_incom <- fromJSON('data1/incompletude_sivep.json')

#VARIAVEIS DO DICIONARIO + VARIAVEIS PARA FILTRAGEM
df_gest2 <- df_gest[,c(variaveis_dic,'ANO','MUNICIPIO','SG_UF_NOT','CLASSI_FIN')]

#VARIAVEIS EM QUE O VALOR 9 E O VALOR IGNORADO:
variaveis_ign <- c('CS_SEXO','CS_RACA','CS_ESCOL_N','CS_ZONA','NOSOCOMIAL','AVE_SUINO','FEBRE','TOSSE','GARGANTA','DISPNEIA',
                   'DESC_RESP','SATURACAO','DIARREIA','VOMITO','OUTRO_SIN','FATOR_RISC','CARDIOPATI','HEMATOLOGI','SIND_DOWN',
                   'HEPATICA','ASMA','DIABETES','NEUROLOGIC','PNEUMOPATI','IMUNODEPRE','RENAL','OBESIDADE','OUT_MORBI',
                   'MAE_VAC','M_AMAMENTA','ANTIVIRAL','HOSPITAL','UTI','SUPORT_VEN','AMOSTRA','POS_PCRFLU','POS_PCROUT',
                   'EVOLUCAO','DOR_ABD','FADIGA','PERD_OLFT','PERD_PALA','POS_AN_FLU','POS_AN_OUT','CS_GESTANT',
                   'TOMO_RES','VACINA_COV','VACINA','PUERPERA','CLASSI_FIN',"RAIOX_RES" )
setdiff(variaveis_dic,variaveis_ign)
#SUBSTITUIR VALORES NA POR EM BRANCO
sivep <- replace(df_gest2,is.na(df_gest2) ,"Em Branco")

#SUBSTITUIR VALORES 9 POR IGNORADO
sivep[, variaveis_ign] <- lapply(sivep[, variaveis_ign], function(x) ifelse((x == '9'|x == '9.0'), "Ignorado", x))

# Calcular as porcentagens de valores 'Ignorados' e 'Em branco' por coluna so para ver se funcionou
 colMeans(sivep == "Ignorado", na.rm = TRUE) * 100
 colMeans(sivep == "Em Branco", na.rm = TRUE) * 100

# IMPLAUSIBILIDADE --------------------------------------------------------

regras_implau <- fromJSON('data1/implausibilidade_gestantes.json')
regras_implau2 <- fromJSON('data1/implausibilidade_puerperas.json')

# Criando vetores de variáveis improváveis e impossíveis
improvavel <- grep("_IMPROVAVEL", names(regras_implau), value = TRUE)
impossivel <- grep("_IMPOSSIVEL", names(regras_implau), value = TRUE)
impossivel2 <- grep("_IMPOSSIVEL", names(regras_implau2), value = TRUE)
impossivel <- c(impossivel2,impossivel) %>% unique()

# Criando um data.frame com as variáveis improváveis
df_improvavel <- data.frame(
  variavel = gsub(improvavel,pattern = '_IMPROVAVEL',replacement = ''))

# Criando um data.frame com as variáveis impossíveis
df_impossivel <- data.frame(
  variavel =  gsub(impossivel,pattern = '_IMPOSSIVEL',replacement = ''))

# Trocando regras em string por booleANOs
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
    grepl("NU_IDADE_N", variavel) ~ "(as.integer(NU_IDADE_N) < 10 & as.integer(NU_IDADE_N) >= 0) | (as.integer(NU_IDADE_N) > 55 & as.integer(NU_IDADE_N) <= 90)"))

#Substituindo os valores do banco sivep por improvavel e impossivel
attach(sivep)
for(i in 1:nrow(df_impossivel)){
  var <- df_impossivel$variavel[i]
  cond <- df_impossivel$condicao[i]
  sivep[eval(parse(text = paste0(cond," & (",var," != 'Em Branco')"))),var]<- 'Impossivel'
}
for(i in 1:nrow(df_improvavel)){
  var <- df_improvavel$variavel[i]
  cond <- df_improvavel$condicao[i]
  sivep[eval(parse(text = paste0("(",cond,") & (",var," != 'Em branco' &",var," != 'Ignorado') "))),var] <- 'Improvavel'
}
detach(sivep)
sivep_ic_ip <- sivep

# INCONSISTENCIA ----------------------------------------------------------

regras_incon <- fromJSON('data1/SIVEP_Inconsistencias_Regras.json')

# Criando um data.frame com as variáveis improváveis
df_inconsistencia <- data.frame(
  variavel = names(regras_incon) %>% gsub(pattern = '_e_', replacement = ' e '))

# Trocando regras em string por booleANOs
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
    grepl("RAIOX_RES e DT_RAIOX", variavel) ~ "(df_gest_aux$RAIOX_RES == '6' | df_gest_aux$RAIOX_RES == '9') & (df_gest_aux$DT_RAIOX != 'Em Branco')",
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
df_gest_aux %>% colnames() #VENDO SE DEU CERTO

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

# CRIAR REGRAS DO SIVEP ---------------------------------------------------
#inconsistencia
regras_incon <- regras_incon |> as.data.frame() |> t() |> as.data.frame()
regras_incon <- cbind(regras_incon |> row.names(),regras_incon)
regras_incon |> row.names() <- NULL
regras_incon |> colnames() <- c('Variavel','Regra')
regras_incon$Variavel <- regras_incon$Variavel |> gsub(pattern = '_e_', replacement = ' e ')
regras_incon$Indicador <- 'Inconsistência'
regras_incon <- regras_incon[-c(17,18),]

#implausibilidade
regras_implau <- regras_implau |> as.data.frame() |> t() |> as.data.frame()
regras_implau <- cbind(regras_implau |> row.names(),regras_implau)
regras_implau |> row.names() <- NULL
regras_implau |> colnames() <- c('Variavel','Regra')
regras_implau$Variavel <- regras_implau$Variavel |> gsub(pattern = '_IMPOSSIVEL', replacement = '')
regras_implau$Regra <- regras_implau$Regra |> gsub(pattern = 'de gestantes ', replacement = '')
regras_implau$Regra <- regras_implau$Regra |> gsub(pattern = 'Gestantes ', replacement = 'Gestantes e puérperas ')
regras_implau$Regra[4] <- 'Gestantes e puérperas ao mesmo tempo'
regras_implau$Indicador <- 'Implausiblidade'

#incompletude
regras_incom <- regras_incom |> as.data.frame() |> t() |> as.data.frame()
regras_incom <- cbind(regras_incom |> row.names(),regras_incom)
regras_incom |> row.names() <- NULL
regras_incom |> colnames() <- c('Variavel','Regra')
regras_incom$Indicador <- 'Incompletude'

#CORRECAO PARA CODIGO DO QUALIDADOS
regras_sivep <- rbind(regras_incon,regras_implau,regras_incom)
for(i in seq_along(SIVEP_dic$`Codigo SIVEP`)) {
  for(j in 1:ncol(regras_sivep)){
  regras_sivep[,j] <- gsub(SIVEP_dic$`Codigo SIVEP`[i],
                           SIVEP_dic$`Codigo Qualidados`[i],
                       regras_sivep[,j])
  }
}

regras_sivep$Variavel <- regras_sivep$Variavel %>% gsub(pattern = '_IMPROVAVEL',replacement = '')

#DESCRICAO DOS INDICADORES
desc_incom <- 'análise das informações que estão faltando na base de dados, seja porque não foram preenchidas (“dados em branco”) ou porque a resposta era desconhecida (“dados ignorados”).'
desc_implau <- "análise das informações que são improváveis e/ou dificilmente possam ser consideradas aceitáveis dadas as características de sua natureza."
desc_incon <- "informações que parecem ilógicas e/ou incompatíveis a partir da análise da combinação dos dados informados em dois ou mais campos do formulário."

var_sivep_incon <- regras_sivep[regras_sivep$Indicador=='Inconsistência','Variavel']
#VARIAVEIS AUXILIARES PARA INCONSISTENCIA
Var_incon_relacao <- list(
  c('SEXO','IDADE_GEST'),
  c('FATOR_RISCO','CARDIOPATI', 'HEMATOLOGI', 'SIND_DOWN', 'HEPÁTICA', 'ASMA', 'DIABETES',
    'NEUROLÓGICA', 'PNEUMOPATIA', 'IMUNODEPRESSAO', 'RENAL_CRON', 'OBESIDADE',
    'OBES_IMC', 'OUT_FATOR_RISCO'),
  c('VACINA','DT_VACINA_GRIPE'),
  c('MAE_VACINA' ,'DT_VACINA_MAE' ),
  c('DT_DOSE_UNICA','IDADE'),
  c('ANTIVIRAL','TIPO_ANTIVIRAL'),
  c('INTERNACAO','DT_INTERNACAO'),
  c('UTI' ,'DT_UTI' ,'INTERNACAO'),
  c('RESULT_RAIOX' ,'DT_RAIOX' ),
  c('AMOSTRA_DIAG' ,'DT_COLETA_AMO' ),
  c('HIST_VIAGEM','LO_PS_VGM', 'DT_VGM', 'DT_RT_VGM'),
  c('RESULT_TOMOGR' ,'DT_TOMOGRAFIA' ),
  c('RES_AN' ,'TIPO_ANTIGENICO' ,'DT_RES_ANTIGENICO' ),
  c('VACINA_COVID' ,'DOSE_1_COV' ,'DOSE_2_COV' ),
  c('CLASSI_FIN' ,'PCR_INFLU' ,'ANTIGENICO_INFLU' ),
  c('CLASSI_FIN' ,'PCR_OUTRO' ,'AN_OUTRO')
)
names(Var_incon_relacao) <- regras_sivep[regras_sivep$Indicador == 'Inconsistência','Variavel']
Var_incon_relacao <- Var_incon_relacao[var_sivep_incon] %>% unlist() %>% unname()
Var_incon_relacao <- Var_incon_relacao[Var_incon_relacao %in% colnames(sivep)]

#VARIAVEIS PARA FILTRO
var_sivep_implau <- regras_sivep$Variavel[regras_sivep$Indicador == 'Implausiblidade'] %>% unique()
var_sivep_incom <- regras_sivep$Variavel[regras_sivep$Indicador == 'Incompletude'] %>% unique()
dados_oobr_qualidados_SIVEP_2009_2023 <- sivep

dados_oobr_qualidados_SIVEP_2009_2023
continuidade[continuidade$`Codigo Qualidados` == 'DT_RAIOX','Anos Válidos'] |> unlist()

dados_oobr_qualidados_SIVEP_2009_2023$`RESULT_RAIOX e DT_RAIOX`[dados_oobr_qualidados_SIVEP_2009_2023$ANO == '2018'] |>
  unique()

# for(var in continuidade$`Codigo Qualidados`){
#   anos <- continuidade$`Anos Válidos`[continuidade$`Codigo Qualidados` == var] |> unlist()
#   dados_oobr_qualidados_SIVEP_2009_2023[!(dados_oobr_qualidados_SIVEP_2009_2023$ANO %in% anos)
#                                           ,var] <- NA
#}
aux <-list(
  "SEXO e IDADE_GEST"  = c('SEXO','IDADE_GEST'),
  "FATOR_RISCO e COMORBIDADES"   = c('FATOR_RISCO','CARDIOPATI', 'HEMATOLOGI', 'SIND_DOWN', 'HEPÁTICA', 'ASMA', 'DIABETES',
                                     'NEUROLÓGICA', 'PNEUMOPATIA', 'IMUNODEPRESSAO', 'RENAL_CRON', 'OBESIDADE',
                                     'OBES_IMC', 'OUT_FATOR_RISCO'),
  "VACINA e DT_VACINA_GRIPE" = c('VACINA','DT_VACINA_GRIPE'),
  "MAE_VACINA e DT_VACINA_MAE"= c('MAE_VACINA' ,'DT_VACINA_MAE' ),
  "DT_DOSE_UNICA e IDADE" =c('DT_DOSE_UNICA','IDADE'),
  "ANTIVIRAL e TIPO_ANTIVIRAL"=c('ANTIVIRAL','TIPO_ANTIVIRAL'),
  "INTERNACAO e DT_INTERNACAO" =c('INTERNACAO','DT_INTERNACAO'),
  "UTI e DT_UTI" =  c('UTI' ,'DT_UTI' ,'INTERNACAO'),
  "RESULT_RAIOX e DT_RAIOX"  =c('RESULT_RAIOX' ,'DT_RAIOX' ),
  "AMOSTRA_DIAG e DT_COLETA_AMO" =c('AMOSTRA_DIAG' ,'DT_COLETA_AMO' ),
  "HIST_VIAGEM e Campos_VGMs"  = c('HIST_VIAGEM','LOCAL_VIAGEM', 'DT_VIAGEM', 'DT_RETORNO_VIAGEM'),
  "RESULT_TOMOGR e DT_TOMOGRAFIA" = c('RESULT_TOMOGR' ,'DT_TOMOGRAFIA' ),
  "TIPO_ANTIGENICO e DT_RES_ANTIGENICO"=  c('RESULT_ANT' ,'TIPO_ANTIGENICO' ,'DT_RES_ANTIGENICO' ),
  "VACINA_COVID e DOSES"= c('VACINA_COVID' ,'DOSE1_COVID' ,'DOSE2_COVID' ),
  "CLASSI_FIN_SRAG_INFLUENZA" =  c('CLASSI_FIN' ,'PCR_INFLU' ,'ANTIGENICO_INFLU' ),
  "CLASSI_FIN_SRAG_OUTROS_VIRUS"  =c('CLASSI_FIN' ,'PCR_OUTRO' ,'AN_OUTRO')
)

dados_oobr_qualidados_SIVEP_2009_2023
for(var in names(aux)){
  variaveis <- aux[[var]]
  if(length(variaveis) > 4){
    a1 <- continuidade$`Anos Válidos`[continuidade$`Codigo Qualidados` == 'FATOR_RISCO']  |> unlist()
    a2 <-  continuidade$`Anos Válidos`[continuidade$`Codigo Qualidados` %in% variaveis] |> unlist()
    anos <- table(a2)[table(a2) >= 2] |> names()
    anos <- anos[anos %in% a1]
  }else{
    anos <- continuidade$`Anos Válidos`[continuidade$`Codigo Qualidados` %in% variaveis] |> unlist()
    anos <- table(anos)[table(anos) >= length(variaveis)] |> names()
  }
  dados_oobr_qualidados_SIVEP_2009_2023[!(dados_oobr_qualidados_SIVEP_2009_2023$ANO %in% anos),var] <- NA
}
dados_oobr_qualidados_SIVEP_2009_2023$`HIST_VIAGEM e Campos_VGMs`[dados_oobr_qualidados_SIVEP_2009_2023$ANO == '2018' ] |> table()
dados_oobr_qualidados_SIVEP_2009_2023$`HIST_VIAGEM e Campos_VGMs`|>
  table(useNA = 'always')


#DADOS
usethis::use_data(dados_oobr_qualidados_SIVEP_2009_2023,overwrite = T)
#VARIVEIS PARA FILTRO
usethis::use_data(Var_incon_relacao,overwrite = T)
usethis::use_data(var_sivep_incom,overwrite = T)
usethis::use_data(var_sivep_implau,overwrite = T)
usethis::use_data(var_sivep_incon,overwrite = T)
#DESCRICAO
usethis::use_data(desc_incom, overwrite = T)
usethis::use_data(desc_implau, overwrite = T)
usethis::use_data(desc_incon, overwrite = T)
#DICIONARIO
usethis::use_data(SIVEP_dic,overwrite = T)
usethis::use_data(regras_sivep,overwrite = T)

