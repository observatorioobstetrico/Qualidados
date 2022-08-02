#' SIVEP_incompletude UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SIVEP_incompletude_ui <- function(id,tabname,vars_incon,estadosChoices){
  ns <- NS(id)
  library(shiny)
  library(shinydashboard)
  tagList(useShinyjs(),
    tabItems(
      tabItem(tabName = tabname,
              fluidRow(
                box(
                  width = 12,
                  title = "Descrição",
                  status = "primary",
                  solidHeader = FALSE,
                  span('Beijo te pela ultima vez, meus olhos e vos, meus bracos dai lhe o
                       vosso ultimo abraço, vos meus labios, q sois a porta de respiracao, selai
                       com um ultimo beijo o pacto eterno com a morte voraz, vem amago fatal guia
                       vem piloto sem esperanca',
                    style = "color:black"
                  )
                )
              ),
              fluidRow(
                box(collapsible = TRUE,
                    width = 2,
                    title = "Campos",
                    status = "primary",
                    solidHeader = FALSE,
                    shinyWidgets::pickerInput(
                      inputId = ns("Graf_Variaveis_Incon"),
                      label = "Variaveis",
                      choices = vars_incon,
                      selected = c("CS_SEXO", "NU_IDADE_N"),
                      options = list(`actions-box` = TRUE),
                      multiple = T),

                    checkboxGroupInput(
                      inputId = ns("Graf_DiagonisticoSRAG_Implau"),
                      label = "Diagnóstico de SRAG",
                      choices = c(
                        "COVID-19" = "5",
                        "Não especificado" = "4",
                        "Não respondido" = "9",
                        "Influenza" = "1",
                        "Outro vírus" = "2",
                        "Outro agente" = "3"),
                      selected = c("5")),

                    checkboxGroupInput(
                      inputId = ns("Graf_Condicao_Incon"),
                      label = "Filtrar Condição",
                      choices = c(
                        "Grávidas 1º Trimestre" = "1tri",
                        "Grávidas 2º Trimestre" = "2tri",
                        "Grávidas 3º Trimestre" = "3tri",
                        "Puérperas" = "puerp"),
                      selected = c("1tri", "2tri", "3tri", "puerp")),

                    checkboxGroupInput(
                      inputId = ns("Exib_Finalizados"),
                      label = "Casos Finalizados:",
                      choices = c("Exibir casos finalizados" = "cf")),

                    tippy::tippy_this(
                      elementId = ns("Exib_Finalizados"),
                      tooltip = "Casos em que se tem informação sobre a evolução.",
                      placement = "right"),

                    checkboxGroupInput(
                      inputId = ns("Exib_Dados"),
                      label = "Exibir dados:",
                      choices = c("Ignorados" = "ignore",
                                  "Em branco" = "na"),
                      selected = c("ignore", "na")),

                    tippy::tippy_this(
                      elementId = ns("Exib_Dados"),
                      tooltip = "Dados que não foram preenchidos ('dados em branco') ou são desconhecidos ('dados ignorados')",
                      placement = "right"),

                    selectInput(
                      "Graf_OpcaoLocalidade",
                      "Dados por localidade:",
                      c(
                        "Brasil" = "br",
                        "Estado" = "est",
                        "Município" = "muni"
                      ),
                      selected = c("br")),

                    conditionalPanel(
                      condition = "input.Graf_OpcaoLocalidade == 'est'",
                      selectInput(
                        ns("Graf_Estado"),
                        "Selecione o estado",
                        choices = estadosChoices,
                        selected = c("ES")))
                ),
                  box(
                    title = "Visualização",
                    status = "primary",
                    width = 1))
    )))
}

#' SIVEP_incompletude Server Functions
#'
#' @noRd
mod_SIVEP_incompletude_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_SIVEP_incompletude_ui("SIVEP_incompletude_1")

## To be copied in the server
# mod_SIVEP_incompletude_server("SIVEP_incompletude_1")
