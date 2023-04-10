#' construcao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_construcao_ui <- function(id,tabname){
  ns <- NS(id)
  shinyjs::useShinyjs()
    shinydashboard::tabItem(tabName = tabname,

                            fluidRow(
                              shinydashboard::box(width = 12,
                                                  shiny::includeMarkdown('construcao.md'))))
}

#' construcao Server Functions
#'
#' @noRd
mod_construcao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_construcao_ui("construcao_1")

## To be copied in the server
# mod_construcao_server("construcao_1")
