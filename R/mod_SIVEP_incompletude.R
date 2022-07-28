#' SIVEP_incompletude UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SIVEP_incompletude_ui <- function(id,nome_item,descricao){
  ns <- NS(id)
  library(shiny)
  library(shinydashboard)
  tagList(tabItem(tabName = nome_item,
                  fluidRow(
                    box(
                      width = 12,
                      title = "Descrição",
                      status = "primary",
                      solidHeader = FALSE,
                      span(descricao,
                        style = "color:black"
                      )
                    )
                  ),
                  fluidRow(
                    box(
                      collapsible = TRUE,
                      width = 2,
                      title = "Filtros",
                      status = "primary",
                      solidHeader = FALSE,
                      checkboxGroupInput(
                        inputId = ns("Graf_DiagonisticoSRAG"),
                        label = "Diagnóstico de SRAG",
                        choices = c(
                          "COVID-19" = "5",
                          "Não especificado" = "4",
                          "Não respondido" = "9",
                          "Influenza" = "1",
                          "Outro vírus" = "2",
                          "Outro agente" = "3"
                        ),
                        selected = c("5")
                      ),
                      checkboxGroupInput(
                        inputId = ns("Graf_Condicao"),
                        label = "Filtrar Condição",
                        choices = c(
                          "Grávidas 1º Trimestre" = "1tri" ,
                          "Grávidas 2º Trimestre" = "2tri",
                          "Grávidas 3º Trimestre" = "3tri",
                          "Puérperas" = "puerp"
                        ),
                        selected = c("1tri", "2tri", "3tri", "puerp")
                      ),
                      selectInput(
                        inputId = ns("Graf_CjtoVariaveis"),
                        label = "Conjunto de variáveis",
                        choices = c(
                          "Caracterização" = "caracterizacao",
                          "Comorbidades" = "comorbidades",
                          "Desfecho" = "desfecho",
                          "Sintomas" = "sintomas"
                        ),
                        selected = c("desfecho")
                      ),
                      checkboxGroupInput(
                        inputId = ns("Exib_Finalizados"),
                        label = "Casos Finalizados:",
                        choices = c("Exibir casos finalizados" = "cf")
                      ),
                      tippy::tippy_this(
                        elementId = ns("Exib_Finalizados"),
                        tooltip = "Casos em que se tem informação sobre a evolução.",
                        placement = "right"
                      ),
                      checkboxGroupInput(
                        inputId = ns("Exib_Dados"),
                        label = "Exibir dados:",
                        choices = c("Ignorados" = "ignore",
                                    "Em branco" = "na"),
                        selected = c("ignore", "na")
                      ),
                      tippy::tippy_this(
                        elementId = ns("Exib_Dados"),
                        tooltip = "Dados que não foram preenchidos ('dados em branco') ou são desconhecidos ('dados ignorados')",
                        placement = "right"
                      ),
                      selectInput(ns("Graf_OpcaoLocalidade"),"Dados por localidade:",
                        c(
                          "Brasil" = "br",
                          "Estado" = "est",
                          "Município" = "muni"
                        ),
                        selected = c("br")
                      ),
                      conditionalPanel(
                        condition = ns("input.Graf_OpcaoLocalidade == 'est'"),
                        selectInput(ns("Graf_Estado"),"Selecione o estado",
                          choices = estadosChoices,
                          selected = c("ES"))
                      ),
                      conditionalPanel(
                        condition = ns("input.Graf_OpcaoLocalidade == 'muni'"),
                        selectInput(ns("Graf_Municipio"), "Selecione o município:",
                                    c(unique(
                                      dados_incom$muni_nm_clean)))
                      ),
                      selectInput(
                        ns("Graf_OpcaoComparar"),
                        "Fazer comparação?",
                        c(
                          "Não" = "br",
                          "Sim, com estado" = "est",
                          "Sim, com município" = "muni"
                        ),
                        selected = c("br")
                      ),
                      tippy::tippy_this(
                        elementId = ns("Graf_OpcaoComparar"),
                        tooltip = "Disponível apenas para visualização gráfica.",
                        placement = "right"
                      ),
                      conditionalPanel(
                        condition = ns("input.Graf_OpcaoComparar == 'est'"),
                        selectInput(
                          ns("Graf_CompararEstado"),
                          "Estado de comparação",
                          choices = estadosChoices,
                          selected = "ES"
                        )
                      ),
                      conditionalPanel(
                        condition = ns("input.Graf_OpcaoComparar == 'muni'"),
                        selectInput(
                          "Graf_CompararMunicipio",
                          "Município de comparação",
                          c(unique(dados_incom$muni_nm_clean))
                        )
                      )
                    ),
                    box(
                      title = "Visualização",
                      status = "primary",
                      width = 10,
                      div(tabsetPanel(
                        tabPanel(
                          "Gráficos",
                          plotlyOutput(ns("graficoCompleteness"), height =
                                         'auto')
                        ),
                        tabPanel(
                          "Tabelas",
                          conditionalPanel(
                            condition = ns("input.Graf_CjtoVariaveis == 'desfecho'"),
                            tabBox(
                              width = 24,
                              tabPanel(htmlOutput("print32")),
                              tabPanel(htmlOutput("print33")),
                              tabPanel(htmlOutput("print34")),
                              tabPanel(htmlOutput("print35"))
                            )
                          ),
                          conditionalPanel(
                            condition = ns("input.Graf_CjtoVariaveis == 'sintomas'"),
                            tabBox(
                              width = 24,
                              tabPanel(htmlOutput("print10")),
                              tabPanel(htmlOutput("print11")),
                              tabPanel(htmlOutput("print12")),
                              tabPanel(htmlOutput("print13")),
                              tabPanel(htmlOutput("print14")),
                              tabPanel(htmlOutput("print15")),
                              tabPanel(htmlOutput("print16")),
                              tabPanel(htmlOutput("print17")),
                              tabPanel(htmlOutput("print18")),
                              tabPanel(htmlOutput("print19")),
                              tabPanel(htmlOutput("print20")),
                              tabPanel(htmlOutput("print21"))
                            )
                          ),
                          conditionalPanel(
                            condition = ns("input.Graf_CjtoVariaveis == 'comorbidades'"),
                            tabBox(
                              width = 24,
                              tabPanel(htmlOutput("print22")),
                              tabPanel(htmlOutput("print23")),
                              tabPanel(htmlOutput("print24")),
                              tabPanel(htmlOutput("print25")),
                              tabPanel(htmlOutput("print26")),
                              tabPanel(htmlOutput("print27")),
                              tabPanel(htmlOutput("print28")),
                              tabPanel(htmlOutput("print29")),
                              tabPanel(htmlOutput("print30")),
                              tabPanel(htmlOutput("print31"))
                            )
                          ),
                          conditionalPanel(
                            condition = ns("input.Graf_CjtoVariaveis == 'caracterizacao'"),
                            tabBox(
                              width = 24,
                              tabPanel(htmlOutput("print1")),
                              tabPanel(htmlOutput("print2")),
                              tabPanel(htmlOutput("print3")),
                              tabPanel(htmlOutput("print4")),
                              tabPanel(htmlOutput("print5")),
                              tabPanel(htmlOutput("print6")),
                              tabPanel(htmlOutput("print7")),
                              tabPanel(htmlOutput("print8")),
                              tabPanel(htmlOutput("print9"))
                            )
                          )
                        )

  ))))))
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
