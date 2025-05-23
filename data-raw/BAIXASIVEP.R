
### Carregando pacotes

loadlibrary <- function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = T)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}

packages <- c("readr",
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
              'getPass',
              'httr')

lapply(packages, loadlibrary)

### Parte do Web scrapping

# Usando o ckanr para obter as url's

ckanr::ckanr_setup("https://opendatasus.saude.gov.br")

arqs <- ckanr::package_search("srag 2020")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~ .x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck, 1, "url") #2021

arqs2 <- ckanr::package_search("srag 2021")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~.x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck, 2, "url") #2022

arqs3 <- ckanr::package_search("srag 2021")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~.x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck, 3, "url") #2023

arqs4 <- ckanr::package_search("srag 2024")$results %>%
  purrr::map("resources") %>%
  purrr::map(purrr::keep, ~.x$mimetype == "text/csv") %>%
  purrr::map_chr(purrr::pluck, 4, "url") #2024

# Baixando as bases

dados_a <- fread(arqs[1], sep = ";")
dados_b <- fread(arqs[2], sep = ";")
dados_c <- fread(arqs2[1], sep = ";")
dados_d <- fread(arqs3[1], sep= ";")
dados_e <- fread(arqs4, sep= ";")

### Trabalhando com as bases

# Alterando as bases

class(dados_e$COD_IDADE) = "character"

dados_a$FATOR_RISC <- dados_a$FATOR_RISC %>% as.character()
dados_b$FATOR_RISC <- dados_b$FATOR_RISC %>% as.character()
dados_c$FATOR_RISC <- dados_c$FATOR_RISC %>% as.character()
dados_d$FATOR_RISC <- dados_d$FATOR_RISC %>% as.character()
dados_e$FATOR_RISC <- dados_e$FATOR_RISC %>% as.character()

# Unindo as bases

colunas_comuns <- intersect(colnames(dados_a), colnames(dados_b))
for (coluna in colunas_comuns) {
  if (is.numeric(dados_a[[coluna]]) && !(is.numeric(dados_b[[coluna]]))) {
    dados_b[[coluna]] <- as.numeric(dados_b[[coluna]])
  }
}
colunas_comuns <- intersect(colnames(dados_a), colnames(dados_c))
for (coluna in colunas_comuns) {
  if (is.numeric(dados_a[[coluna]]) && !(is.numeric(dados_c[[coluna]]))) {
    dados_c[[coluna]] <- as.numeric(dados_c[[coluna]])
  }
}
colunas_comuns <- intersect(colnames(dados_a), colnames(dados_d))
for (coluna in colunas_comuns) {
  if (is.numeric(dados_a[[coluna]]) && !(is.numeric(dados_d[[coluna]]))) {
    dados_d[[coluna]] <- as.numeric(dados_d[[coluna]])
  }
}
colunas_comuns <- intersect(colnames(dados_a), colnames(dados_e))
for (coluna in colunas_comuns) {
  if (is.numeric(dados_a[[coluna]]) && !(is.numeric(dados_e[[coluna]]))) {
    dados_e[[coluna]] <- as.numeric(dados_e[[coluna]])
  }
}

dados_total <- full_join(dados_a, dados_b) %>%
               full_join(dados_c) %>%
               full_join(dados_d) %>%
               full_join(dados_e)

# Filtrando a base

dados_total <- dados_total %>%
  filter((CS_GESTANT == 1 | CS_GESTANT == 1.0 | CS_GESTANT == '1' | CS_GESTANT == '1.0' |
          CS_GESTANT == 2 | CS_GESTANT == 2.0 | CS_GESTANT == '2' | CS_GESTANT == '2.0' |
          CS_GESTANT == 3 | CS_GESTANT == 3.0 | CS_GESTANT == '3' | CS_GESTANT == '3.0' |
          CS_GESTANT == 4 | CS_GESTANT == 4.0 | CS_GESTANT == '4' | CS_GESTANT == '4.0' |
            PUERPERA == 1 |   PUERPERA == 1.0 |   PUERPERA == '1' |   PUERPERA == '1.0'
          ))

### Salvando a base como .RDS

write_rds(dados_total,file = 'data1/Sivep_2020-2024.rds')
