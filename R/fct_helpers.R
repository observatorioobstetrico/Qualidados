#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

escolher_banco_dados <- function(SIM, indicador) {

  # criar uma lista com os dataframes disponíveis
  lista_dados <- list(
    Sinasc_incom = dados_oobr_qualidados_SINASC_Incompletude_1996_2022,
    Sinasc_implau = dados_oobr_qualidados_SINASC_Implausibilidade_1996_2022,
    Sinasc_incon = dados_oobr_qualidados_SINASC_Inconsistencia_1996_2022,
    SIM_Incom = dados_oobr_qualidados_SIM_Incompletude_1996_2022,
    SIM_Implau = dados_oobr_qualidados_SIM_Implausibilidade_1996_2022,
    SIM_Incon = dados_oobr_qualidados_SIM_Inconsistencia_1996_2022
  )

  # selecionar o dataframe correto usando um switch
  switch(paste(SIM, indicador),
         "FALSE incom" = lista_dados$Sinasc_incom,
         "FALSE implau" = lista_dados$Sinasc_implau,
         "FALSE incon" = lista_dados$Sinasc_incon,
         "TRUE incom" = lista_dados$SIM_Incom,
         "TRUE implau" = lista_dados$SIM_Implau,
         "TRUE incon" = lista_dados$SIM_Incon,
         NA)
}
