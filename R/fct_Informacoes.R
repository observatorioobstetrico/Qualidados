#ESTADOS DISPONIVEIS PARA FILTRAGEM
estadosChoices <- c(
  "AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
  "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
  "RO","RR","RS","SC","SE","SP","TO")

#VARIAVEIS DISPONIVEIS PARA INCOMPLETUDE

variaveis_incom <- names(dados_incom)[stringr::str_detect(names(dados_incom), "^f_")]
variaveis_incom_nomes <- c('Raça','Escolaridade',"Zona de Residência","Histórico de Viagem",
                           "SG","Infecção Hospitalar","Contato com aves ou suínos","Vacina",
                           "Antiviral","Febre","Tosse","Garganta","Dispneia","Desc. Resp.",
                           "Saturação","Diarreia","Vômito","Dor Abdominal","Fadiga","Perda de Olfato",
                           "Perda paladar","Cardiopatia","Hematologia","Hepática","Asma","Diabetes",
                           "Neuro","Pneumopatia","Imunodepressores","Renal","Obesidade","UTI",
                           "Hospitalização","Suporte\nVentilatório","Evolução")

#RELACAO ENTRE OS NOMES CERTOS E OS NOMES NO BD

variaveis_relacao <- variaveis_incom_nomes
names(variaveis_relacao) <- variaveis_incom

#tem que mudar esse nome aqui, por algum motivo nao tava gerando a variavel dentro
#do render plot

variaveis <- NA
var_names <- NA

#DESCRICOES -------------------
desc_incom <- 'análise das informações que estão faltando na base de dados, seja porque não foram preenchidas (“dados em branco”) ou porque a resposta era desconhecida (“dados ignorados”).'
desc_implau <- "análise das informações que são improváveis e/ou dificilmente possam ser consideradas aceitáveis dadas as características de sua natureza."
desc_incon <- "informações que parecem ilógicas e/ou incompatíveis a partir da análise da combinação dos dados informados em dois ou mais campos do formulário."

