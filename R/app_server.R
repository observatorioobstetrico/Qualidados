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
    # Show a modal when the button is pressed
    shinyalert::shinyalert("Ajuda",html =T,text = tagList("Funcionamento do Painel",
      tags$video(type ='video/mp4',src = 'www/hello-there.mp4', width = '100%',
                                  controls = 'controls')))
  })
}

