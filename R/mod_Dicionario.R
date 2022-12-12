#' Dicionario UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Dicionario_ui <- function(id,tabname){
  ns <- NS(id)
  library(shiny)
  library(shinydashboard)
  useShinyjs()
  tabItem(tabname = tabname

  )
}

#' Dicionario Server Functions
#'
#' @noRd
mod_Dicionario_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Dicionario_ui("Dicionario_1")

## To be copied in the server
# mod_Dicionario_server("Dicionario_1")
