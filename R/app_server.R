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
  mod_SINASC_server("SIM_inconsistencia",indicador = 'incom', SIM = TRUE)
  observeEvent(input$help, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert("Ajuda",html =T,text = tagList("TEXTO AJUDA",
      tags$video(type ='video/mp4',src = 'www/hello-there.mp4', width = '100%',
                                  controls = 'controls')))
  })
}

