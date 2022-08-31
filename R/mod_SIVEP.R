#' SIVEP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SIVEP_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' SIVEP Server Functions
#'
#' @noRd 
mod_SIVEP_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_SIVEP_ui("SIVEP_1")
    
## To be copied in the server
# mod_SIVEP_server("SIVEP_1")
