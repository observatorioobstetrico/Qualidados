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
                          reactable::reactableOutput(ns("regras"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(collapsible = TRUE,
                          width = 12,
                          title = "Dicionário de variáveis disponíveis no banco de dados",
                          status = "primary",
                          solidHeader = FALSE,
                          reactable::reactableOutput(ns("dicionario"))))

  )
}

#' Dicionario Server Functions
#'
#' @noRd
mod_Dicionario_server <- function(id, df, regra){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$dicionario <-  reactable::renderReactable({
      reactable::reactable(df,
                           filterable = TRUE,
                           showSortable = TRUE,
                           searchable = TRUE,
                           showPageSizeOptions = TRUE,
                           pageSizeOptions = c(10, 15, 27),
                           defaultPageSize = 27,
                           striped = TRUE,
                           highlight = TRUE,
                           theme = reactable::reactableTheme(
                             color = "#000000",
                             borderColor = "#dfe2e5",
                             stripedColor = "#f6f8fa",
                             highlightColor = "#f0f5f9",
                             cellPadding = "8px 12px",
                             style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                             searchInputStyle = list(width = "100%")))
    })
    output$regras <-  reactable::renderReactable({
      reactable::reactable(regra, groupBy = c("Indicador"),
                         filterable = TRUE,
                         showSortable = TRUE,
                         searchable = TRUE,
                         showPageSizeOptions = TRUE,
                         pageSizeOptions = c(10, 15, 27),
                         defaultPageSize = 27,
                         striped = TRUE,
                         highlight = TRUE,
                         theme = reactable::reactableTheme(
                           color = "#000000",
                           borderColor = "#dfe2e5",
                           stripedColor = "#f6f8fa",
                           highlightColor = "#f0f5f9",
                           cellPadding = "8px 12px",
                           style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                           searchInputStyle = list(width = "100%")))
    })
})
}

## To be copied in the UI
# mod_Dicionario_ui("Dicionario_1")

## To be copied in the server
# mod_Dicionario_server("Dicionario_1")
