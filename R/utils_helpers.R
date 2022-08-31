#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_input_name <- function(input){
  split <- (strsplit(input, split=' '))[[1]]
  return(split[1])
}

Plot_tabela_incom <- function(var_name){
  var <- variaveis_relacao[var_name]
  return(renderText({
    kableExtra::kable(
      questionr::freq(
        selectData()$var,
        cum = FALSE,
        total = TRUE,
        na.last = FALSE,
        valid = FALSE
      ),
      caption = paste("Dados faltantes para ", var_name),
      digits = 2
    ) %>%
      kableExtra::kable_styling()
  }))
}
