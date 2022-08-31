get_input_name <- function(input){
  split <- (strsplit(input, split=' '))[[1]]
  return(split[1])
}
