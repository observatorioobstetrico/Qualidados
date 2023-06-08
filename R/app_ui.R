#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
#'
library(markdown)
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinyjs::useShinyjs(),
            shinydashboard::dashboardPage(title="Qualidados",
              shinydashboard::dashboardHeader( title = span(tagList(img(src = "www/qualidadoslogo.png",width = '70'),'Qualidados')), titleWidth = 130),
              shinydashboard::dashboardSidebar(
                width = 174,
                shinydashboard::sidebarMenu(
                  style = " overflow: visible;",
                  shinydashboard::menuItem('Início',
                    tabName = 'main',
                    icon = icon('house'),
                    startExpanded = TRUE),
                  shinydashboard::menuItem(
                    "SIVEP-GRIPE" ,
                    tabname = "sivep",
                    icon = icon("table"),
                    shinydashboard::menuSubItem("Incompletude",
                                tabName = "incom_sivep"),
                    shinydashboard::menuSubItem("Implausibilidade",
                                tabName = "implau_sivep"),
                    shinydashboard::menuSubItem("Inconsist\u00eancia",
                                tabName = "incons_sivep"),
                    shinydashboard::menuSubItem("Dicionário",
                                tabName = "dic-sivep")
                  ),
                  shinydashboard::menuItem(
                    "SINASC" ,
                    tabname = "sinasc",
                    icon = icon("table"),
                    shinydashboard::menuSubItem("Incompletude",
                                tabName = "incom_sinasc"),
                    shinydashboard::menuSubItem("Implausibilidade",
                                tabName = "implau_sinasc"),
                    shinydashboard::menuSubItem("Inconsist\u00eancia",
                                tabName = "incons_sinasc"),
                    shinydashboard::menuSubItem("Dicionário",
                                                tabName = "dic-sinasc")

                  ),
                  shinydashboard::menuItem(
                    "SIM" ,
                    tabname = "sim",
                    icon = icon("table"),
                    shinydashboard::menuSubItem("Incompletude",
                                tabName = "incom_sim"),

                    shinydashboard::menuSubItem("Implausibilidade",
                                tabName = "implau_sim"),
                    shinydashboard::menuSubItem("Inconsist\u00eancia",
                                tabName = "incon_sim"),
                    shinydashboard::menuSubItem("Dicionário",
                                                tabName = "dic-sim")

                  ),
                  shiny::actionButton('help','Ajuda',icon('question',lib="font-awesome"),
                    style =  "color: #0A1E3;
                              background-color: white;
                              border-color: #0A1E3")
                )
              ),
              shinydashboard::dashboardBody(shinydashboard::tabItems(
              mod_SIVEP_ui(id = "SIVEP_incompletude",
                                        vars_incon = sort(var_sivep_incom),
                                        tabname ="incom_sivep",
                                        descricao =  desc_incom ,indicador = 'incom',
                                        selecionadas = c('RACA','ESCOLARIDADE')),

              mod_SIVEP_ui(id = "SIVEP_implausibilidade",
                                        vars_incon = sort(var_sivep_implau),
                                        tabname ="implau_sivep",
                                        descricao = desc_implau,indicador = 'implau',
                                        selecionadas = c('IDADE','SEXO')),

              mod_SIVEP_ui(id = "SIVEP_inconsistencia",
                                        vars_incon = sort(var_sivep_incon),
                                        tabname ="incons_sivep",
                                        descricao = desc_incon,indicador = 'incon',
                                        selecionadas = sort(var_sivep_incon)[c(1,2)]),
              mod_SINASC_ui(id = "SINASC_incompletude",
                                        tabname = "incom_sinasc",
                                        indicador = 'incom',
                                        descricao = desc_incom,
                                        vars = vars_incom_sinasc,
                                        estados = sort(unique(Sinasc_incom$ESTADO)),
                            selecionadas = c('RACA','ESCOLARIDADE')),
              mod_SINASC_ui(id = "SINASC_implausibilidade",
                                        tabname = "implau_sinasc",
                                        indicador = 'implau',
                                        descricao = desc_implau,
                                        vars = vars_implau_sinasc,
                                        estados = sort(unique(Sinasc_implau$ESTADO)),
                            selecionadas = c('RACA','SEXO')),
              mod_SINASC_ui(id = "SINASC_inconsistencia",
                            tabname = "incons_sinasc",
                            indicador = 'incon',
                            descricao = desc_incon,
                            vars = unname(var_incon_sinasc),
                            estados = sort(unique(Sinasc_incon$ESTADO)),
                            selecionadas = unname(var_incon_sinasc)[c(1,2)]),
              mod_SINASC_ui(id = "SIM_incompletude",
                            tabname = "incom_sim",
                            indicador = 'incom',
                            SIM = TRUE,
                            descricao = desc_incom,
                            vars = unname(vars_incom_sim),
                            estados = sort(unique(SIM_Incom$ESTADO)),
                            selecionadas =c('RACA','ESCOLARIDADE') ),
              mod_SINASC_ui(id = "SIM_implausibilidade",
                            tabname = "implau_sim",
                            indicador = 'implau',
                            SIM = TRUE,
                            descricao = desc_implau,
                            vars = unname(vars_implau_sim),
                            estados = sort(unique(SIM_Implau$ESTADO)),
                            selecionadas = c('RACA','ESCOLARIDADE')),
              mod_SINASC_ui(id = "SIM_inconsistencia",
                            tabname = "incon_sim",
                            indicador = 'incon',
                            SIM = TRUE,
                            descricao = desc_incon,
                            vars = unname(vars_incon_sim),
                            estados = sort(unique(SIM_Incon$ESTADO)),
                            selecionadas = unname(vars_incon_sim)[c(1,2)]),
              mod_Dicionario_ui("dicsivep","dic-sivep"),
              mod_Dicionario_ui("dicsinasc","dic-sinasc"),
              mod_Dicionario_ui("dicsim",'dic-sim'),
              shinydashboard::tabItem(tabName = 'main',
                                      shinyjs::useShinyjs(),
                                      fluidRow(
                                      shinydashboard::box(width = 12,
                                      shiny::includeMarkdown('inicio.md')
                                      )))
            )
                  )
                )
              )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$script(HTML("$('body').addClass('fixed');"))
  tags$head(tags$style(
    HTML(
      '
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0A1E3C;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0A1E3C;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0A1E3C;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0A1E3C;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #32A0FF;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0A1E3C;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #32A0FF;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #32A0FF;
                              }
                              '
    ),
    htmltools::HTML("hr {border-top: 1px solid #0A1E3C;}")
  ))
  }
