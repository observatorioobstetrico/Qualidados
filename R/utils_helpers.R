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


