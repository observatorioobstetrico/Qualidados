library(readxl)
library(dplyr)
sim_dic <- read_excel("data1/dicionario_SIM.xlsx")
sivep_dic <-  read_excel("data1/Dicionario_SIVEP_Valores.xlsx")
sinasc_dic <-  read_excel("data1/dicionario_SINASC.xlsx")
usethis::use_data(sinasc_dic,overwrite = T)

# SIM ##########################
sim_dic <- sim_dic[,c(3,4,6)] |> na.omit()
usethis::use_data(sim_dic,overwrite = T)

###### SIVEP ###################
sivep_dic <- sivep_dic[,c('...3','...7','Descrição')]
colnames(sivep_dic) <- c('Coluna','Tipo','Descrição')

usethis::use_data(sivep_dic,overwrite = T)
