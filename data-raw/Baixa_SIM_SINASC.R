#carregar pacotes
.rs.restartR()
rm(list=ls())
loadlibrary <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = T)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}

packages <-
  c(
    "readr",
    "readxl",
    "janitor",
    "dplyr",
    "forcats",
    "stringr",
    "lubridate",
    "summarytools",
    "magrittr",
    "questionr",
    "knitr",
    "data.table",
    "writexl",
    "modelsummary",
    'coro',
    'getPass','httr'
  )
lapply(packages, loadlibrary)
memory.limit(9999999)
ckanr::ckanr_setup("https://opendatasus.saude.gov.br")
# SIM -------------------------------------------
#2022
arqs <- ckanr::package_search("SIM")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~ .x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck,44, "url")
#2021
arqs1 <- ckanr::package_search("SIM")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~ .x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck,43, "url")

#1996 - 2020

# # Loop de 1996 a 2020
# for (ano in 1996:2020) {
#
#   # Crie o nome da variável para buscar o recurso correspondente
#   var_name <- paste0("arqs", ano - 1995)
#
#   # Pesquise o recurso e obtenha o URL
#   url <- ckanr::package_search("SIM")$results %>%
#     purrr::map("resources") %>%
#     purrr::map(purrr::keep, ~ .x$mimetype == "text/csv") %>%
#     purrr::map_chr(purrr::pluck, 42 - (ano - 1996), "url")
#   # Baixe os dados e armazene-os na lista
#   df <- fread(url, sep = ";")
#   # Transforme a coluna HORAOBITO em character
#   df <- df %>%
#     filter(
#       (OBITOGRAV == 1 | OBITOPUERP == 1 | OBITOPUERP == 2)
#     )
#   write.csv(df,file = paste0('data1/dado_1996-2020/sim_',ano), row.names = F)
# }


dados_a <- fread(arqs, sep = ";")

dados_b <- fread(arqs1, sep = ";")


dados_total <- full_join(dados_a, dados_b)
dados_total <- dados_total %>%
  filter(
    (OBITOGRAV == 1 | OBITOPUERP == 1 | OBITOPUERP == 2)
  )

dados_total <-as.data.frame(dados_total)
write.csv(dados_total,file = 'data1/sim2021-2022.csv', row.names = F)

dados_total
# SINASC -------------------

arqs <- ckanr::package_search("SINASC")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~ .x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck,2, "url")
arqs1 <- ckanr::package_search("SINASC")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~ .x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck,3, "url")


dados_a <- fread(arqs, sep = ";")

dados_b <- fread(arqs1, sep = ";")


dados_total <- full_join(dados_a, dados_b)

write.csv(dados_total,file = 'data1/sinasc2021-2022.csv' ,row.names = F)
