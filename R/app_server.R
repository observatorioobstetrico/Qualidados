#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_SIVEP_server("SIVEP_incompletude", indicador = 'incom')
  mod_SIVEP_server("SIVEP_implausibilidade", indicador = 'implau')
  mod_SIVEP_server("SIVEP_inconsistencia", indicador = 'incon')
  mod_SINASC_server("SINASC_incompletude",indicador = 'incom')
  mod_SINASC_server("SINASC_implausibilidade",indicador = 'implau')
  mod_SINASC_server("SINASC_inconsistencia",indicador = 'incon')
  mod_SINASC_server("SIM_incompletude",indicador = 'incom', SIM = TRUE)
  mod_SINASC_server("SIM_implausibilidade",indicador = 'implau', SIM = TRUE)
  mod_SINASC_server("SIM_inconsistencia",indicador = 'incon', SIM = TRUE)
  mod_Dicionario_server('dicsivep',df=SIVEP_dic,regra = regras_sivep)
  mod_Dicionario_server("dicsinasc",df=SINASC_dic,regra = regras_sinasc)
  mod_Dicionario_server("dicsim",df=SIM_dic,regra = regras_sim)
  observeEvent(input$help, {
    showModal(modalDialog(title = "Tutorial Uso Qualidados",
                          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/9nRmnLDh128" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')

    )
    )
  })
}

