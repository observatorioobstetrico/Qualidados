#' SIVEP_incompletude UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SIVEP_incompletude_ui <- function(id, tabname, vars_incon = variaveis_incon_nomes, descricao){
  ns <- NS(id)
  library(shiny)
  library(shinydashboard)
  library(plotly)
  tagList(useShinyjs(),
    tabItems(
      tabItem(tabName = tabname,
              #Descricao -----------
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
              #Caixa com filtros -----------------
              fluidRow(
                box(collapsible = TRUE,
                    width = 2,
                    title = "Campos",
                    status = "primary",
                    solidHeader = FALSE,
                    #VARIAVEIS DISPONIVEIS PARA PLOTAGEM
                    shinyWidgets::pickerInput(
                      inputId = ns("Graf_Variaveis_Incon"),
                      label = "Variaveis",
                      choices = vars_incon,
                      selected = vars_incon[c(1,2)],
                      options = list(`actions-box` = TRUE),
                      multiple = T),
                    #FILTRO DE DIAGNOSTICO DE SRAG
                    checkboxGroupInput(
                      inputId = ns("Graf_DiagonisticoSRAG_Incon"),
                      label = "Diagnóstico de SRAG",
                      choices = c(
                        "COVID-19" = "5",
                        "Não especificado" = "4",
                        "Não respondido" = "9",
                        "Influenza" = "1",
                        "Outro vírus" = "2",
                        "Outro agente" = "3"),
                      selected = c("5")),
                    #FILTRO DE CONDICAO
                    checkboxGroupInput(
                      inputId = ns("Graf_Condicao_Incon"),
                      label = "Filtrar Condição",
                      choices = c(
                        "Grávidas 1º Trimestre" = "1tri",
                        "Grávidas 2º Trimestre" = "2tri",
                        "Grávidas 3º Trimestre" = "3tri",
                        "Puérperas" = "puerp"),
                      selected = c("1tri", "2tri", "3tri", "puerp")),
                    #FILTRO DE EXIBICAO OU NAO DE CASOS FINALIZADOS
                    checkboxGroupInput(
                      inputId = ns("Exib_Finalizados"),
                      label = "Casos Finalizados:",
                      choices = c("Exibir casos finalizados" = "cf")),
                    #DESCRICAO EXIB_FINALIZADOS
                    tippy::tippy_this(
                      elementId = ns("Exib_Finalizados"),
                      tooltip = "Casos em que se tem informação sobre a evolução.",
                      placement = "right"),
                    #FILTRO PARA TIPOS DE DADOS, IGNORADOS OU EM BRANCO
                    checkboxGroupInput(
                      inputId = ns("Exib_Dados"),
                      label = "Exibir dados:",
                      choices = c("Ignorados" = "ignore",
                                  "Em branco" = "na"),
                      selected = c("ignore", "na")),
                    #DESCRICAO EXIB_DADOS
                    tippy::tippy_this(
                      elementId = ns("Exib_Dados"),
                      tooltip = "Dados que não foram preenchidos ('dados em branco') ou são desconhecidos ('dados ignorados')",
                      placement = "right"),
                    #FILTRO POR LOCALIDADE
                    selectInput(
                      ns("Graf_OpcaoLocalidade"),
                      "Dados por localidade:",
                      c(
                        "Brasil" = "br",
                        "Estado" = "est",
                        "Município" = "muni"
                      ),
                      selected = c("br")),
                    #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE SE POR ESTADO
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'est'",ns("Graf_OpcaoLocalidade")),
                      selectInput(
                        ns("Graf_Estado"),
                        "Selecione o estado",
                        choices = estadosChoices,
                        selected = c("ES"))),
                    #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE POR MUNICIPIO
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'muni'",ns("Graf_OpcaoLocalidade")),
                      selectInput(
                        ns("Graf_muni"),
                        "Selecione o município",
                        choices = c(unique(dados_incom$muni_nm_clean)))),
                    #FAZER COMPARACAO
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
                      condition = sprintf("input['%s'] == 'est'",ns("Graf_OpcaoComparar")),
                      selectInput(
                        "Graf_CompararEstado",
                        "Estado de comparação",
                        choices = estadosChoices,
                        selected = "ES"
                      )
                    ),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'muni'",ns("Graf_OpcaoComparar")),
                      selectInput(
                        "Graf_CompararMunicipio",
                        "Município de comparação",
                        c(unique(dados_incom$muni_nm_clean))
                      )
                    )
                ),
                  #AREA DOS GRAFICOS, TABELAS QUANT E EXPLICATIVAS ----------------
                  box(
                    title = "Visualização",
                    status = "primary",
                    width = 10,
                    div(tabsetPanel(
                          #GRAFICOS
                          tabPanel(
                            "Gráficos",
                            plotlyOutput(ns("graficoCompleteness"),
                                                height = 'auto')),
                          #TABELAS QUANT
                           tabPanel(
                             "Tabelas",
                               tabBox(
                                 width = 50,
                                   tabPanel(htmlOutput(ns("print1")))
                           )
                          ),
                          #TABELAS EXPLICATIVAS
                          tabPanel("Microdados",
                                 reactable::reactableOutput(ns("table_incom")))
                          ))
                    )
))))
}

#' SIVEP_incompletude Server Functions
#'
#' @noRd
mod_SIVEP_incompletude_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #GRAFICO IMCOMPLETUDE ------------
    output$graficoCompleteness <- renderPlotly({
    #FILTRAGEM PELOS FILTROS SELECIONADOS ------------
        #VARIAVEIS SELECIONADAS NO FILTRO
        for(var in input$Graf_Variaveis_Incon){
            variaveis <- c(variaveis,names(variaveis_relacao[variaveis_relacao == var]))
            var_names <- c(var_names,variaveis_relacao[variaveis_relacao == var])
        }
        #TIRAR OS VALORES NA INICIAIS
      var_labeller <- function(variable, value){
        return(var_names[value])
      }
        variaveis <- variaveis[!is.na(variaveis)]
        var_names <- var_names[!is.na(var_names)]
        #FILTRAR POR CLASSI_FIN E CLASSE DE GESTANTE SELECIONADA
        Dados_GraficoIncompletudeIniciais <- dados_incom %>%
          dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG_Incon)
        #CRIAR ANO E DATA E FILTRAR DATAS APOS 2020-3
        Dados_GraficoIncompletudeIniciais$data <- with(Dados_GraficoIncompletudeIniciais,
                                                       sprintf("%d-%02d", ano, mes))
        Dados_GraficoIncompletudeIniciais <- Dados_GraficoIncompletudeIniciais %>%
          filter(data >= '2020-03')
        #FILTRAR POR CASOS FINALIZADOS
        if("cf" %in% input$Exib_Finalizados)
        {
          Dados_GraficoIncompletudeIniciais <- Dados_GraficoIncompletudeIniciais %>%
            filter(f_evolucao == 'Dados válidos')
        }
    #CONVERTER VARIAVEIS SELECIONADAS PARA BINARIO PARA CALCULO DE PORCENTAGEM --------------
        VarSelecionadas = Dados_GraficoIncompletudeIniciais[variaveis]

        VarSelecionadas[VarSelecionadas == 'Dados válidos'] <- "0"

        if(!("ignore" %in% input$Exib_Dados))
        {
          VarSelecionadas[VarSelecionadas == 'Dados ignorados'] <- "0"
        }
        else
        {
          VarSelecionadas[VarSelecionadas == 'Dados ignorados'] <- "1"
        }

        if(!("na" %in% input$Exib_Dados))
        {
          VarSelecionadas[VarSelecionadas == 'Dados em branco'] <- "0"
        }
        else
        {
          VarSelecionadas[VarSelecionadas == 'Dados em branco'] <- "1"
        }

        VarSelecionadas <- lapply(VarSelecionadas, as.numeric)

        Dados_GraficoIncompletudeIniciais[variaveis] <- VarSelecionadas

        Dados_GraficoIncompletudeIniciais <- Dados_GraficoIncompletudeIniciais %>%
          tidyr::pivot_longer(
            cols = all_of(variaveis),
            names_to = "variable",
            values_to = "value"
          )
    #FILTRAGEM E FINALIZACAO POR TIPO DE LOCALIDADE ---------------
        if (input$Graf_OpcaoLocalidade == 'br') {
          Dados_GraficoIncompletude <- Dados_GraficoIncompletudeIniciais[c('variable', 'value', 'data')]

        } else {
          Dados_GraficoIncompletude <- Dados_GraficoIncompletudeIniciais[c('variable', 'value','SG_UF', 'muni_nm_clean', 'data')]

          if (input$Graf_OpcaoLocalidade == "muni") {
            Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
              filter(muni_nm_clean == input$Graf_Municipio)

          } else {
            Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
              filter(SG_UF == input$Graf_Estado)

          }

        }

        if (input$Graf_OpcaoLocalidade == 'br') {
          Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
            group_by(data, variable) %>%
            summarize(value = mean(value))
          Dados_GraficoIncompletude$localidade <- 'BR'
        } else {

          if (input$Graf_OpcaoLocalidade == "muni") {
            Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))
            Dados_GraficoIncompletude$localidade <- input$Graf_Municipio
            print(input$Graf_Municipio)
          } else {
            Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))
            Dados_GraficoIncompletude$localidade <- input$Graf_Estado
          }
        }

        if (input$Graf_OpcaoComparar != 'br') {
          Dados_GraficoIncompletude2 <- Dados_GraficoIncompletudeIniciais[c('variable', 'value','SG_UF', 'muni_nm_clean', 'data')]

          if (input$Graf_OpcaoComparar == "muni") {
            Dados_GraficoIncompletude2 <- Dados_GraficoIncompletudeIniciais %>%
              filter(muni_nm_clean == input$Graf_CompararMunicipio) %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))

            Dados_GraficoIncompletude2$localidade <- input$Graf_CompararMunicipio
            print(input$Graf_CompararMunicipio)
          } else {
            print("else")
            Dados_GraficoIncompletude2 <- Dados_GraficoIncompletudeIniciais %>%
              filter(SG_UF == input$Graf_CompararEstado) %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))
            Dados_GraficoIncompletude2$localidade <- input$Graf_CompararEstado
          }
          Dados_GraficoIncompletude <- rbind( Dados_GraficoIncompletude,Dados_GraficoIncompletude2)

        }

        Dados_GraficoIncompletude$value <- round(Dados_GraficoIncompletude$value * 100, 2)
    #FINALIZACAO COM GGPLOT -------------------------
        g <- ggplot(data = Dados_GraficoIncompletude,
                    aes(y=value, x=data , fill = localidade)) +
          geom_bar(position="dodge", stat="identity") +
          facet_grid(rows = vars(variable), labeller=var_labeller)

        g <- g + labs(x = NULL) +
          labs(y="Incompletude (%)", fill = "Localidade") +
          scale_y_continuous(breaks=seq(0,100,20), limits = c(0, 100)) +
          scale_fill_viridis_d() +
          theme_bw() +
          theme(axis.text.x = element_text(face="bold", color="#000000",
                                           size=9, angle=45))

        ggplotly(g, height=length(variaveis)*125) %>% layout(legend = list(orientation = "h", y = 20))
    })
    #TABELAS IMCOPLETUDE ( dados filtrados ) ----------------------
    selectData <- reactive({
      g <- dados_incom %>%
        dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG_Incon) %>%
        dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
        dplyr::filter(if (input$Graf_OpcaoLocalidade == "est")
          SG_UF == input$estado
          else
            is.na(SG_UF) | !is.na(SG_UF)) %>%
        dplyr::filter(if (input$Graf_OpcaoLocalidade == "muni")
          muni_nm_clean == input$selectmuni
          else
            is.na(muni_nm_clean) | !is.na(muni_nm_clean))

      if("cf" %in% input$Exib_Finalizados)
      {
        g <- g %>%
          filter(f_evolucao == 'Dados válidos')
      } else{
        g
      }
    })
    #TABELA -----------------
        output$print1 <- renderTable(table(head(dados_incom[input$Graf_Variaveis_Incon])))
    #MICRODADOS ---------------------

  })
}



## To be copied in the UI
# mod_SIVEP_incompletude_ui("SIVEP_incompletude_1")

## To be copied in the server
# mod_SIVEP_incompletude_server("SIVEP_incompletude_1")
