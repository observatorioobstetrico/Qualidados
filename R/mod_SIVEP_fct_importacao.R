library(readr)
library(readxl)
library(dplyr)
library(DT)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(ggridges)
library(ggplot2)
library(tidyr)
library(forcats)
library(rjson)
#PRIMEIRA IMPORTAÇÃO -----------------------------
#ESTADOS DISPONIVEIS PARA FILTRAGEM
estadosChoices <- c(
    "AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
    "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
    "RO","RR","RS","SC","SE","SP","TO")
jsonfile <- c(fromJSON(file = "data1/data_values.json"))
jsonfile_gest <- c(fromJSON(file = "data1/implausibilidade_puerperas.json"))
jsonfile_puerp <- c(fromJSON(file = "data1/implausibilidade_gestantes.json"))
fields <- jsonfile$fields
#MOSTRAR NOME MAIS DESCRICAO
relatorio_df <-  readRDS("data1/dados_relatorio_sivep.rds") %>% data.frame()

var_names_join <- list()
var_names_join <- append(var_names_join,paste(names(fields[1]), "(", fields[[1]], ")"))
for (field in 2:length(fields)){
  if((names(fields[field]) %in% names(relatorio_df)) && !(names(fields[field]) %in% jsonfile$ignore))
  var_names_join <- append(var_names_join, paste(names(fields[field]), "(", fields[[field]], ")"))
  }

for(f in 1:length(names(relatorio_df))){
  if(names(relatorio_df[f]) %in% jsonfile$datetime){
  relatorio_df[[f]] <- substring(relatorio_df[[f]], 1, nchar(relatorio_df[[f]]))
  relatorio_df[[f]] <- as.Date(x = relatorio_df[[f]], tryFormats = c("%d/%m/%y"))
    }
  }
#DADOS DE INCOMPLETUDE ------------------------
dados_incom <- readRDS("data1/dados_incompletude.rds")
for (variavel in jsonfile$variaveis_tabela) {
  dados_incom[[variavel]][dados_incom[[variavel]] == "Ignorado"] <-
    "Dados ignorados"
  dados_incom[[variavel]][dados_incom[[variavel]] == "na"] <-
    "Dados em branco"
  dados_incom[[variavel]][dados_incom[[variavel]] == "não"] <-
    "Dados válidos"
  }
#VARIAVEIS DISPONIVEIS
variaveis_incon <- names(dados_incom)[stringr::str_detect(names(dados_incom), "f_")]
variaveis_incon_nomes <- c('Raça','Escolaridade',"Zona de Residência","Histórico de Viagem",
                           "SG","Infecção Hospitalar","Contato com aves ou suínos","Vacina",
                           "Antiviral","Febre","Tosse","Garganta","Dispneia","Desc. Resp.",
                           "Saturação","Diarreia","Vômito","Dor Abdominal","Fadiga","Perda de Olfato",
                           "Perda paladar","Cardiopatia","Hematologia","Hepática","Asma","Diabetes",
                           "Neuro","Pneumopatia","Imunodepressores","Renal","Obesidade","UTI",
                           "Hospitalização","Suporte\nVentilatório","Evolução")
#RELACAO ENTRE OS NOMES CERTOS E OS NOMES NO BD
variaveis_relacao <- variaveis_incon_nomes
names(variaveis_relacao) <- variaveis_incon
#tem que mudar esse nome aqui, por algum motivo nao tava gerando a variavel dentro
#do render plot
variaveis <- NA
var_names <- NA
