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
                  shinydashboard::menuItem('Documentação',
                                           tabName = 'documentacao',
                                           icon = icon('book'),
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
                                        estados = sort(unique(dados_oobr_qualidados_SINASC_Incompletude_1996_2022$ESTADO)),
                            selecionadas = c('RACA','ESCOLARIDADE')),
              mod_SINASC_ui(id = "SINASC_implausibilidade",
                                        tabname = "implau_sinasc",
                                        indicador = 'implau',
                                        descricao = desc_implau,
                                        vars = vars_implau_sinasc,
                                        estados = sort(unique(dados_oobr_qualidados_SINASC_Implausibilidade_1996_2022$ESTADO)),
                            selecionadas = c('RACA','SEXO')),
              mod_SINASC_ui(id = "SINASC_inconsistencia",
                            tabname = "incons_sinasc",
                            indicador = 'incon',
                            descricao = desc_incon,
                            vars = unname(var_incon_sinasc),
                            estados = sort(unique(dados_oobr_qualidados_SINASC_Inconsistencia_1996_2022$ESTADO)),
                            selecionadas = unname(var_incon_sinasc)[c(1,2)]),
              mod_SINASC_ui(id = "SIM_incompletude",
                            tabname = "incom_sim",
                            indicador = 'incom',
                            SIM = TRUE,
                            descricao = desc_incom,
                            vars = unname(vars_incom_sim),
                            estados = sort(unique(dados_oobr_qualidados_SIM_Incompletude_1996_2022$ESTADO)),
                            selecionadas =c('RACA','ESCOLARIDADE') ),
              mod_SINASC_ui(id = "SIM_implausibilidade",
                            tabname = "implau_sim",
                            indicador = 'implau',
                            SIM = TRUE,
                            descricao = desc_implau,
                            vars = unname(vars_implau_sim),
                            estados = sort(unique(dados_oobr_qualidados_SIM_Implausibilidade_1996_2022$ESTADO)),
                            selecionadas = c('RACA','ESCOLARIDADE')),
              mod_SINASC_ui(id = "SIM_inconsistencia",
                            tabname = "incon_sim",
                            indicador = 'incon',
                            SIM = TRUE,
                            descricao = desc_incon,
                            vars = unname(vars_incon_sim),
                            estados = sort(unique(dados_oobr_qualidados_SIM_Inconsistencia_1996_2022$ESTADO)),
                            selecionadas = unname(vars_incon_sim)[c(1,2)]),
              mod_Dicionario_ui("dicsivep","dic-sivep"),
              mod_Dicionario_ui("dicsinasc","dic-sinasc"),
              mod_Dicionario_ui("dicsim",'dic-sim'),
              shinydashboard::tabItem(tabName = 'main',
                                      shinyjs::useShinyjs(),
                                      fluidRow(
                                      shinydashboard::box(width = 12,
                                      shiny::includeMarkdown('inicio.md')
                                      ))),

              shinydashboard:: tabItem(
                tabName = "documentacao",
                h1(strong("Fontes")),
                h2(strong(
                  "Dados SIVEP-GRIPE"
                )),
                (
                  "Utilizamos os registros das notificações de Síndrome Respiratória Aguda Grave (SRAG) na base SIVEP Gripe (Sistema de Informação da Vigilância Epidemiológica da Gripe).
       "
                ),
                br(),
                br(),
                (
                  "A atualização desta base é disponibilizada pelo Ministério da Saúde pelo portal"
                ),
                a(" Open Data SUS.", href = "https://opendatasus.saude.gov.br/organization/ministerio-da-saude"),
                br(),
                br(),
                (
                  "A última atualização foi realizada em 29/02/2024, aqui constam as bases do SIVEP-Gripe de 2009 à 2023."
                ),
                br(),
                br(),
                p(
                  "São disponibilizados aqui os indicadores de qualidade para os casos definidos como gestante (qualquer trimestre gestacional ou
        idade gestacional ignorada) ou puérpera."
                ),
                p(
                  "Para a identificação de gestante, há a variável CS_GESTANT. Essa variável assume os valores: 1-1º Trimestre; 2-2º Trimestre; 3-3º Trimestre; 4-Idade Gestacional Ignorada; 5-Não; 6-Não se aplica; 9-Ignorado.
        Consideramos aqui como gestante se CS_GESTANT for 1 ou 2 ou 3 ou 4."
                ),
                p(
                  "Para a identificação de puérpera, há a variável PUERPERA, com 1-sim e 2-não.
        Consideramos como puérpera os casos que PUERPERA=1 e CS_GESTANT=5 ou PUERPERA=1 e CS_GESTANT=9."
                ),
                br(),
                br(),
                h2(strong('Sistema de Informações sobre Nascidos Vivos (SINASC).')),
                (
                  'O SINASC é gerenciado pelo Ministério da Saúde em parceria com as Secretarias Estaduais e Municipais de Saúde. Seu objetivo principal é subsidiar a formulação, implementação e avaliação de políticas públicas relacionadas à saúde materno-infantil.'
                ),
                br(),
                br(),
                ('São disponibilizados aqui os indicadores de qualidade para os casos definidos como gestante (qualquer trimestre gestacional ou
        idade gestacional ignorada) ou puérpera.'),
                br(),
                br(),
                ('Os dados são obtidos via API da PCDas, em https://pcdas.icict.fiocruz.br/, uma plataforma de ciência de dados aplicada a saúde proporcionada pela fundação Fiocruz'),
                br(),
                br(),
                ('Os dados aqui apresentados possuem atualização em 22/02/2024 para dados do SINASC de 2009 a 2022, com os dados preliminares de 2023.'),
                br(),
                br(),
                h2(strong('Sistema de Informação sobre Mortalidade (SIM) .')),
                (
                  'O Sistema de Informação Sobre Mortalidade (SIM), desenvolvido pelo Ministério da Saúde em 1975, é resultado da integração de mais de quarenta modelos de instrumentos utilizados ao longo dos anos para coletar dados sobre mortalidade no país.'
                ),
                br(),
                br(),
                ('São disponibilizados aqui os indicadores de qualidade para os casos definidos como gestante (qualquer trimestre gestacional ou
        idade gestacional ignorada) ou puérpera.'),
                br(),
                br(),
                ('Os dados são obtidos via API da PCDas, em https://pcdas.icict.fiocruz.br/, uma plataforma de ciência de dados aplicada a saúde proporcionada pela fundação Fiocruz'),
                br(),
                br(),
                ('Os dados aqui apresentados possuem atualização em  22/02/2024 para dados do SIM de 2009 a 2022, com os dados preliminares de 2023.'),
                br(),
                br(),
                actionButton("generate", "Gerar pdf da documentação"),
                uiOutput("pdfview"),
                br()
              )
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
