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
    Sinasc_incom = Sinasc_incom,
    Sinasc_implau = Sinasc_implau,
    Sinasc_incon = Sinasc_incon,
    SIM_Incom = SIM_Incom,
    SIM_Implau = SIM_Implau,
    SIM_Incon = SIM_Incon
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
