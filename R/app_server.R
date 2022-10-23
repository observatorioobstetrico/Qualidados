#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_SIVEP_incompletude_server("SIVEP_incompletude", indicador = 'incom')
  mod_SIVEP_incompletude_server("SIVEP_implausibilidade", indicador = 'implau')
  mod_SIVEP_incompletude_server("SIVEP_inconsistencia", indicador = 'incon')
  observeEvent(input$help, {
    # Show a modal when the button is pressed
    shinyalert("Ajuda",html =T,text = tagList("TEXTO AJUDA",
      tags$video(type ='video/mp4',src = 'www/hello-there.mp4', width = '100%',
                                  controls = 'controls')))
  })
}

