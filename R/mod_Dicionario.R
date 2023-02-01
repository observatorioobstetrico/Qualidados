#' Dicionario UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Dicionario_ui <- function(id, tabname ){
  ns <- NS(id)
  shinyjs::useShinyjs()
  shinydashboard::tabItem(tabName = tabname,
    shiny::fluidRow(
      shinydashboard::box(collapsible = TRUE,
                          width = 12,
                          title = "Regras de agrupamento dos indicadores do banco de dados",
                          status = "primary",
                          solidHeader = FALSE,
                          shiny::div(shiny::tabsetPanel(
                            shiny::tabPanel("Incompletude",
                                            shiny::tableOutput(ns('INCOM'))),
                            shiny::tabPanel('Implausibilidade',
                                            shiny::tableOutput(ns('IMPLAU'))),
                            shiny::tabPanel("Inconsistência",
                                            shiny::tableOutput(ns('INCON')))
                          ))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(collapsible = TRUE,
                          width = 12,
                          title = "Dicionário de variáveis disponíveis no banco de dados",
                          status = "primary",
                          solidHeader = FALSE,
                          shiny::tableOutput(ns("dicionario"))))

  )
}

#' Dicionario Server Functions
#'
#' @noRd
mod_Dicionario_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$dicionario <-  shiny::renderText({
      x <- c('Coluna','Tipo','Descrição')
      df <- df[!is.na(df$Descrição),x]
      knitr::kable(df) |>
        kableExtra::kable_styling("striped", full_width = F)
    })
  })
}

## To be copied in the UI
# mod_Dicionario_ui("Dicionario_1")

## To be copied in the server
# mod_Dicionario_server("Dicionario_1")
