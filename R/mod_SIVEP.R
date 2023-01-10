#' SIVEP_incompletude UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SIVEP_ui <- function(id, tabname, vars_incon , descricao,
                                      indicador,estados){
    ns <- NS(id)

    shinyjs::useShinyjs()
    shinydashboard::tabItem(tabName = tabname,
              #DESCRICAO -----------
              shiny::fluidRow(
                shinydashboard::box(
                  width = 12,
                  title = "Descri\u00e7\u00e3o",
                  status = "primary",
                  solidHeader = FALSE,
                  span(descricao,
                    style = "color:black"
                  )
                )
              ),
              #CAIXA COM FILTROS -----------------
              shiny::fluidRow(
                shinydashboard::box(collapsible = TRUE,
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
                    shiny::checkboxGroupInput(
                      inputId = ns("Graf_DiagonisticoSRAG"),
                      label = "Diagn\u00f3stico de SRAG",
                      choices = c(
                        "COVID-19" = "5",
                        "N\u00e3o especificado" = "4",
                        "N\u00e3o respondido" = "9",
                        "Influenza" = "1",
                        "Outro v\u00edrus" = "2",
                        "Outro agente" = "3"),
                      selected = c("5")),
                    #FILTRO DE CONDICAO
                    tippy::tippy_this(
                      elementId = ns("Graf_DiagonisticoSRAG"),
                      tooltip = "Causa da Sindrome Respirat\u00f3ria Aguda Grave (SRAG).",
                      placement = "right"),
                    shiny::checkboxGroupInput(
                      inputId = ns("Graf_Condicao_Incon"),
                      label = "Filtrar Condi\u00e7\u00e3o",
                      choices = c(
                        "Gr\u00e1vidas 1\u00ba Trimestre" = "1tri",
                        "Gr\u00e1vidas 2\u00ba Trimestre" = "2tri",
                        "Gr\u00e1vidas 3\u00ba Trimestre" = "3tri",
                        "Pu\u00e9rperas" = "puerp"),
                      selected = c("1tri", "2tri", "3tri", "puerp")),
                    #FILTRO DE EXIBICAO OU NAO DE CASOS FINALIZADOS
                    if(indicador=='incom'){
                      shiny::checkboxGroupInput(
                      inputId = ns("Exib_Finalizados"),
                      label = "Casos Finalizados:",
                      choices = c("Exibir casos finalizados" = "cf"))},
                    #DESCRICAO EXIB_FINALIZADOS
                    if(indicador =='incom'){
                    tippy::tippy_this(
                      elementId = ns("Exib_Finalizados"),
                      tooltip = "Casos em que se tem informa\u00e7\u00e3o sobre a evolu\u00e7\u00e3o.",
                      placement = "right")},
                    #FILTRO PARA TIPOS DE DADOS, IGNORADOS OU EM BRANCO
                    if(indicador == 'incom'){
                      shiny::checkboxGroupInput(
                      inputId = ns("Exib_Dados"),
                      label = "Exibir dados:",
                      choices = c("Ignorados" = "ignore",
                                  "Em branco" = "na"),
                      selected = c("ignore", "na"))},
                    if(indicador == 'incom'){
                    tippy::tippy_this(
                      elementId = ns("Exib_Dados"),
                      tooltip = "Dados que n\u00e3o foram preenchidos ('dados em branco') ou s\u00e3o desconhecidos ('dados ignorados')",
                      placement = "right")},
                    if(indicador == 'implau'){
                      shiny::checkboxGroupInput(
                      inputId = ns("Exib_Dados2"),
                      label = "Exibir dados:",
                      choices = c("Dado Improv\u00e1vel",
                                    "Dado Imposs\u00edvel"),
                      selected = c("Dado Improv\u00e1vel",
                                "Dado Imposs\u00edvel"))},
                    if(indicador =='implau'){
                      tippy::tippy_this(
                        elementId = ns('Exib_Dados2'),
                        tooltip = 'Tipos de implausibilidades, a op\u00e7\u00e3o Dado Plaus\u00edvel so pode ser utilizada na sess\u00e3o de tabelas',
                        placement = 'right'
                      )
                    },
                    #FILTRO POR LOCALIDADE
                    shiny::selectInput(
                      ns("Graf_OpcaoLocalidade"),
                      "Dados por localidade:",
                      c(
                        "Brasil" = "br",
                        "Estado" = "est",
                        "Munic\u00edpio" = "muni"
                      ),
                      selected = c("br")),
                    #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE SE POR ESTADO
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] != 'br'",ns("Graf_OpcaoLocalidade")),
                      shiny::selectInput(
                        ns("Graf_Estado"),
                        "Selecione o estado",
                        choices = estados,
                        selected = estados[1])),
                    #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE POR MUNICIPIO
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] == 'muni'",ns("Graf_OpcaoLocalidade")),
                      shiny::selectInput(
                        ns("Graf_muni"),
                        "Selecione o munic\u00edpio",
                        choices = 'municipios')),
                    #FAZER COMPARACAO
                    shiny::selectInput(
                      ns("Graf_OpcaoComparar"),
                      "Fazer compara\u00e7\u00e3o?",
                      c(
                        "N\u00e3o" = "br",
                        "Sim, com estado" = "est",
                        "Sim, com munic\u00edpio" = "muni"
                      ),
                      selected = c("br")
                    ),
                    tippy::tippy_this(
                      elementId = ns("Graf_OpcaoComparar"),
                      tooltip = "Dispon\u00edvel apenas para visualiza\u00e7\u00e3o gr\u00e1fica.",
                      placement = "right"
                    ),
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] != 'br'",ns("Graf_OpcaoLocalidade")),
                      shiny::selectInput(
                        ns("Graf_CompararEstado"),
                        "Estado de compara\u00e7\u00e3o",
                        choices = estados,
                        selected = estados[1]
                      )),
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] == 'muni'",ns("Graf_OpcaoComparar")),
                      selectInput(
                        ns("Graf_CompararMunicipio"),
                        "Munic\u00edpio de compara\u00e7\u00e3o",
                        ('municipios')
                      )
                    )
                ),
              #AREA DOS GRAFICOS, TABELAS QUANT E EXPLICATIVAS ----------------
                  shinydashboard::box(
                    title = "Visualiza\u00e7\u00e3o",
                    status = "primary",
                    width = 10,
                    shiny::div(tabsetPanel(
                          #GRAFICOS
                          shiny::tabPanel(
                            "Gr\u00e1ficos",
                            plotly::plotlyOutput(ns("graficoCompleteness"),
                                                height = 'auto')),

                          #TABELAS QUANT
                           shiny::tabPanel(
                             "Tabelas",
                             shinydashboard::tabBox(
                               width = 24,
                               shiny::tabPanel(shiny::htmlOutput(ns('print1'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print2'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print3'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print4'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print5'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print6'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print7'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print8'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print9'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print10'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print11'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print12'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print13'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print14'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print15'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print16'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print17'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print18'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print19'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print20'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print21'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print22'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print23'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print24'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print25'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print26'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print27'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print28'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print29'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print30'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print31'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print32'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print33'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print34'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print35')))
                                )
                          ),
                          #TABELAS EXPLICATIVAS
                          shiny::tabPanel("Microdados",
                                    if(indicador == 'incon'){
                                      shinyWidgets::pickerInput(
                                        inputId = ns("Vars_microdados_incon"),
                                        label = "Variaveis",
                                        choices = sort(Var_micro_incon),
                                        selected = Var_micro_incon[1:3],
                                        options = list(`actions-box` = TRUE),
                                        multiple = T)
                                     },
                                 reactable::reactableOutput(ns("table_incom")))
                          )))))

    }

#' SIVEP_incompletude Server Functions
#'
#' @noRd
mod_SIVEP_server <- function(id, indicador){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #municipio
    shiny::observe({
      x <- input$Graf_Estado
      y <- input$Graf_CompararEstado
      if(indicador == 'incom')dado <- dados_incom
      if(indicador == 'implau')dado <- dados_implau
      if(indicador == 'incon')dado <- dados_incon
      dados_aux <- dado[dado$SG_UF %in% x,c('muni_nm_clean','SG_UF')]
      dados_compara <- dado[dado$SG_UF %in% y,c('muni_nm_clean','SG_UF')]

      shiny::updateSelectInput(session,("Graf_muni"),
                        choices = sort(unique(dados_aux$muni_nm_clean)),
                        selected = unique(dados_aux$muni_nm_clean)[1])

      shiny::updateSelectInput(session,("Graf_CompararMunicipio"),
                        choices = sort(unique(dados_compara$muni_nm_clean)),
                        selected = unique(dados_compara$muni_nm_clean)[1])


    })

    #GRAFICO INCOMPLETUDE ------------
    if(indicador == 'incom'){


      output$graficoCompleteness <- plotly::renderPlotly({
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
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG)
        #CRIAR ANO E DATA E FILTRAR DATAS APOS 2020-3
        Dados_GraficoIncompletudeIniciais$data <- with(Dados_GraficoIncompletudeIniciais,
                                                       sprintf("%d-%02d", ano, mes))
        Dados_GraficoIncompletudeIniciais <- Dados_GraficoIncompletudeIniciais %>%
          dplyr::filter(data >= '2020-03')
        #FILTRAR POR CASOS FINALIZADOS
        if("cf" %in% input$Exib_Finalizados){
          Dados_GraficoIncompletudeIniciais <- Dados_GraficoIncompletudeIniciais %>%
            dplyr::filter(f_evolucao == 'Dados v\u00e1lidos')
        }
    #CONVERTER VARIAVEIS SELECIONADAS PARA BINARIO PARA CALCULO DE PORCENTAGEM --------------
        VarSelecionadas = Dados_GraficoIncompletudeIniciais[variaveis]

        VarSelecionadas[VarSelecionadas == 'Dados v\u00e1lidos'] <- "0"

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
              dplyr::filter(muni_nm_clean == input$Graf_muni)

          } else {
            Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
              dplyr::filter(SG_UF == input$Graf_Estado)

          }

        }

        if (input$Graf_OpcaoLocalidade == 'br') {
          Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
            dplyr::group_by(data, variable) %>%
            dplyr::summarize(value = mean(value))
          Dados_GraficoIncompletude$localidade <- 'BR'
        } else {

          if (input$Graf_OpcaoLocalidade == "muni") {
            Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))
            Dados_GraficoIncompletude$localidade <- input$Graf_muni
            print(input$Graf_muni)
          } else {
            Dados_GraficoIncompletude <- Dados_GraficoIncompletude %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))
            Dados_GraficoIncompletude$localidade <- input$Graf_Estado
          }
        }

        if (input$Graf_OpcaoComparar != 'br') {
          Dados_GraficoIncompletude2 <- Dados_GraficoIncompletudeIniciais[c('variable', 'value','SG_UF', 'muni_nm_clean', 'data')]

          if (input$Graf_OpcaoComparar == "muni") {
            Dados_GraficoIncompletude2 <- Dados_GraficoIncompletudeIniciais %>%
              dplyr::filter(muni_nm_clean == input$Graf_CompararMunicipio) %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))

            Dados_GraficoIncompletude2$localidade <- input$Graf_CompararMunicipio
            print(input$Graf_CompararMunicipio)
          } else {
            Dados_GraficoIncompletude2 <- Dados_GraficoIncompletudeIniciais %>%
              dplyr::filter(SG_UF == input$Graf_CompararEstado) %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))
            Dados_GraficoIncompletude2$localidade <- input$Graf_CompararEstado
          }
          Dados_GraficoIncompletude <- rbind( Dados_GraficoIncompletude,Dados_GraficoIncompletude2)

        }

        Dados_GraficoIncompletude$value <- round(Dados_GraficoIncompletude$value * 100, 2)
    #FINALIZACAO COM GGPLOT -------------------------
        g <- ggplot2::ggplot(data = Dados_GraficoIncompletude,
                    ggplot2::aes(y=value, x=data , fill = localidade)) +
          ggplot2::geom_bar(position="dodge", stat="identity") +
          ggplot2::facet_grid(rows = ggplot2::vars(variable))#, labeller=var_labeller)

        g <- g + ggplot2::labs(x = NULL) +
          ggplot2::labs(y = "Incompletude (%)", fill = "Localidade") +
          ggplot2::scale_y_continuous(breaks = seq(0,100,20), limits = c(0, 100)) +
          ggplot2::scale_fill_viridis_d() +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            face="bold",
            color="#000000",
            size=9,
            angle=45
          ))

        plotly::ggplotly(g, height=(length(variaveis)*125 + 125)) %>% plotly::layout(legend = list(orientation = "h", y = 20))
    })


      selectData <- shiny::reactive({
        df <- dados_incom %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG) %>%
          dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
          dplyr::filter(if(input$Graf_OpcaoLocalidade == "est")
            SG_UF == input$Graf_Estado
            else
              is.na(SG_UF) | !is.na(SG_UF)) %>%
          dplyr::filter(if (input$Graf_OpcaoLocalidade == "muni")
            muni_nm_clean == input$Graf_muni
            else
              is.na(muni_nm_clean) | !is.na(muni_nm_clean))

        if("cf" %in% input$Exib_Finalizados)
        {
          df <- df %>%
            dplyr::filter(f_evolucao == 'Dados v\u00e1lidos')
        } else{
          df
        }

      })


      for(i in 1:35){
        local({
          my_i <- i
          output[[paste('print',i,sep='')]] <- shiny::renderText({
            if(variaveis_incom_nomes[my_i] %in% input$Graf_Variaveis_Incon){
              kableExtra::kable(
                questionr::freq(
                  selectData()[[variaveis_incom[my_i]]],
                  cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE
                ),
                caption = paste0("Dados faltantes para ",variaveis_incom_nomes[my_i]),
                digits = 2
              ) %>%
                kableExtra::kable_styling()}
          })
        })
      }


      output$table_incom <- reactable::renderReactable({
        variaveis <- NA
        var_names <- NA
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
        Dados_microdados <-  dados_incom %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG) %>%
          dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon)
        if("cf" %in% input$Exib_Finalizados)
        {
          Dados_microdados  <- Dados_microdados %>%
            dplyr::filter(f_evolucao == 'Dados v\u00e1lidos')
        }
        if(input$Graf_OpcaoLocalidade == 'est'){
          Dados_microdados <- Dados_microdados[Dados_microdados$SG_UF == input$Graf_Estado,]
        } else if(input$Graf_OpcaoLocalidade == 'muni'){
          Dados_microdados <- Dados_microdados[Dados_microdados$muni_nm_clean == input$Graf_muni,]
        }
        #cria coluna de ano data para o grafico
        Dados_microdados$data <-
          with(
            Dados_microdados,
            format(Dados_microdados$dt_sint, "%Y-%m")
          )
        Dados_microdados <- Dados_microdados %>%
          dplyr::filter(as.character(data) >= '2020-03')

        Dados_microdados <-
          Dados_microdados %>%
          tidyr::pivot_longer(cols = all_of(variaveis),
                              names_to = "variable",
                              values_to = "value")

        columns <- unique(c("SG_UF", "ID_MUNICIP",variaveis))

        Dados_microdados <- Dados_microdados %>%
          dplyr::filter(variable %in% columns) %>%
          dplyr::arrange(SG_UF)

        Dados_microdados <- Dados_microdados[,c("SG_UF", "ID_MUNICIP",'variable','value')]
        Dados_microdados <- Dados_microdados %>% dplyr::count(value,SG_UF,ID_MUNICIP,variable)
        for(i in variaveis){
          Dados_microdados[Dados_microdados$variable == i,"variable"] <- variaveis_relacao[i]
        }
        names(Dados_microdados)[c(1,4,5)] <- c('Incompletude','Vari\u00e1vel','Frequ\u00eancia')
        if('ignore' %in%  input$Exib_Dados & 'na' %in%  input$Exib_Dados){
          Dados_microdados <-
            Dados_microdados[Dados_microdados$Incompletude !='Dados v\u00e1lidos',]
        }else if('ignore' %in% input$Exib_Dados){
          Dados_microdados <-
            Dados_microdados[Dados_microdados$Incompletude=='Dados ignorados',]
        }else if('na' %in% input$Exib_Dados){
          Dados_microdados <-
            Dados_microdados[Dados_microdados$Incompletude=='Dados em branco',]
        }else{
          Dados_microdados <- NULL
        }

        reactable::reactable(Dados_microdados, groupBy = c("Incompletude",'Vari\u00e1vel','SG_UF'),
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


    }
    #GRAFICO IMPLAUSIBILIDADE --------
    if(indicador == 'implau'){


      output$graficoCompleteness <- plotly::renderPlotly({
           variaveis <- vector()
           for(var in input$Graf_Variaveis_Incon) {
             if (var == "NU_IDADE_N") {
               variaveis <-
                 union(variaveis,
                       paste0(var, "_IMPROVAVEL"))
               variaveis <-
                 union(variaveis,
                       paste0(var, "_IMPOSSIVEL"))
             } else{
               variaveis <-
                 union(variaveis,
                       paste0(var, "_IMPOSSIVEL"))
             }
           }
           variaveis <- variaveis[!is.na(variaveis)]
           Dados_GraficoImplauIniciais <- dados_implau %>%
             dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
             dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG)

           # cria coluna de ano data para o grafico
           Dados_GraficoImplauIniciais$data <-
             with(
               Dados_GraficoImplauIniciais,
               format(Dados_GraficoImplauIniciais$dt_sint, "%Y-%m")
             )

           Dados_GraficoImplauIniciais <- Dados_GraficoImplauIniciais %>%
             dplyr::filter(as.character(data) >= '2020-03')
           VarSelecionadas <- Dados_GraficoImplauIniciais[variaveis]

           Dados_GraficoImplauIniciais <-
             Dados_GraficoImplauIniciais %>%
             tidyr::pivot_longer(cols = all_of(variaveis),
                                 names_to = "variable",
                                 values_to = "value")

           Dados_GraficoImplauIniciais <- Dados_GraficoImplauIniciais %>%
             dplyr::mutate(imps = dplyr::case_when(
               stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Imposs\u00edvel",
               stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improv\u00e1vel",
               TRUE ~ as.character(variable)
             ),
             variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1])
             )

           Dados_TabelaImplau <- Dados_GraficoImplauIniciais

           Dados_GraficoImplauIniciais <- Dados_GraficoImplauIniciais %>%
             dplyr::filter(imps %in% input$Exib_Dados2)

           # filtra apenas as variaveis escolhidas pelo usuario

           if (input$Graf_OpcaoLocalidade == 'br') {
             Dados_GraficoImplau <-
               Dados_GraficoImplauIniciais[c('variavel', 'value', 'data')]

           } else {
             Dados_GraficoImplau <-
               Dados_GraficoImplauIniciais[c('variavel', 'value', 'SG_UF', 'muni_nm_clean', 'data')]

             if (input$Graf_OpcaoLocalidade == "muni") {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 dplyr::filter(muni_nm_clean == input$Graf_muni)

             } else {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 dplyr::filter(SG_UF == input$Graf_Estado)

             }

           }

           if (input$Graf_OpcaoLocalidade== 'br') {
             Dados_GraficoImplau <- Dados_GraficoImplau %>%
               dplyr::group_by(data, variavel) %>%
               dplyr::summarize(value = mean(value))
             Dados_GraficoImplau$localidade <- 'BR'
           } else {
             if (input$Graf_OpcaoLocalidade == "muni") {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 dplyr::group_by(data, variavel) %>%
                 dplyr::summarize(value = mean(value))
               Dados_GraficoImplau$localidade <- input$Graf_muni
               print(input$Graf_muni)
             } else {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 dplyr::group_by(data, variavel) %>%
                 dplyr::summarize(value = mean(value))
               Dados_GraficoImplau$localidade <- input$Graf_Estado
             }
           }

           if (input$Graf_OpcaoComparar != 'br') {
             Dados_GraficoImplau2 <-
               Dados_GraficoImplauIniciais[c('variavel', 'value', 'SG_UF', 'muni_nm_clean', 'data')]

             if (input$Graf_OpcaoComparar== "muni") {
               Dados_GraficoImplau2 <- Dados_GraficoImplauIniciais %>%
                 dplyr::filter(muni_nm_clean == input$Graf_CompararMunicipio) %>%
                 dplyr::group_by(data, variavel) %>%
                 dplyr::summarize(value = mean(value))

               Dados_GraficoImplau2$localidade <-
                 input$Graf_CompararMunicipio
               print(input$Graf_CompararMunicipio)
             } else {
               print("else")
               Dados_GraficoImplau2 <-
                 Dados_GraficoImplauIniciais %>%
                 dplyr::filter(SG_UF == input$Graf_CompararEstado) %>%
                 dplyr::group_by(data, variavel) %>%
                 dplyr::summarize(value = mean(value))
               Dados_GraficoImplau2$localidade <-
                 input$Graf_CompararEstado
             }
             Dados_GraficoImplau <-
               rbind(Dados_GraficoImplau, Dados_GraficoImplau2)

           }

           Dados_GraficoImplau$value <-
             round(Dados_GraficoImplau$value * 100, 2)

      g <- ggplot2::ggplot(data = Dados_GraficoImplau,
                           ggplot2::aes(y = value, x = data, fill = localidade)) +
        ggplot2::geom_bar(position = "dodge", stat = "identity") +
        ggplot2::facet_grid(rows = ggplot2::vars(variavel))

      g <- g + ggplot2::labs(x = NULL) +
        ggplot2::labs(y = "Implausibilidade (%)", fill = "Localidade") +
        ggplot2::scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x =ggplot2::element_text(
          face = "bold",
          color = "#000000",
          size = 9,
          angle = 45
        ))
      if('NU_IDADE_N' %in% input$Graf_Variaveis_Incon){
      plotly::ggplotly(g, height = length(variaveis) * 125) %>%
          plotly::layout(legend = list(orientation = "h", y = 20))}
      else{plotly::ggplotly(g, height = (length(variaveis) * 125 + 125)) %>%
          plotly::layout(legend = list(orientation = "h", y = 20))

      }})


      selectDataAux <- shiny::reactive({
        variaveis_tab <- vector()
        var_valida <- vector()
        for(var_tab in input$Graf_Variaveis_Incon) {
          if (var_tab == "NU_IDADE_N") {
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPROVAVEL"))
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPOSSIVEL"))
          } else{
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPOSSIVEL"))
          }
          var_valida <- union(var_tab,var_valida)
        }


        Dados_TabelaImplau <- dados_implau %>%
          dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG)

        # cria coluna de ano data para o grafico
        Dados_TabelaImplau$data <-
          with(
            Dados_TabelaImplau,
            format(Dados_TabelaImplau$dt_sint, "%Y-%m")
          )

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          dplyr::filter(as.character(data) >= '2020-03')
        if(input$Graf_OpcaoLocalidade == 'est'){
          Dados_TabelaImplau <- Dados_TabelaImplau[Dados_TabelaImplau$SG_UF == input$Graf_Estado,]
        } else if(input$Graf_OpcaoLocalidade == 'muni'){
          Dados_TabelaImplau <- Dados_TabelaImplau[Dados_TabelaImplau$muni_nm_clean == input$Graf_muni,]
        }
        Dados_TabelaImplau
      })


      selectData <- shiny::reactive({
        variaveis_tab <- vector()
        var_valida <- vector()
        for(var_tab in input$Graf_Variaveis_Incon) {
          if (var_tab == "NU_IDADE_N") {
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPROVAVEL"))
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPOSSIVEL"))
          } else{
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPOSSIVEL"))
          }
          var_valida <- union(var_tab,var_valida)
        }

        variaveis_tab <- sort(variaveis_tab)

        Dados_TabelaImplau <- selectDataAux()
        Dados_TabelaImplau <-
          Dados_TabelaImplau %>%
          tidyr::pivot_longer(cols = all_of(variaveis_tab),
                              names_to = "variable",
                              values_to = "value")

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          dplyr::filter(value)

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          dplyr::mutate(imps = dplyr::case_when(
            stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Imposs\u00edvel",
            stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improv\u00e1vel",
            TRUE ~ as.character(variable)
          ),
          variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1]),
          motivo = purrr::map_chr(variable, function(x) jsonfile_gest[[x]])
          )

        columns <- unique(c("SG_UF", "ID_MUNICIP", input$Graf_Variaveis_Incon, "motivo"))

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          dplyr::filter(variavel %in% columns) %>%
          dplyr::arrange(SG_UF)

        Dados_TabelaImplau <- Dados_TabelaImplau[, columns]
      })


      selectData2 <- shiny::reactive({
        variaveis_tab <- vector()
        var_valida <- vector()
      for(var_tab in input$Graf_Variaveis_Incon) {
        if (var_tab == "NU_IDADE_N") {
          variaveis_tab <-
            union(variaveis_tab,
                  paste0(var_tab, "_IMPROVAVEL"))
          variaveis_tab <-
            union(variaveis_tab,
                  paste0(var_tab, "_IMPOSSIVEL"))
        } else{
          variaveis_tab <-
            union(variaveis_tab,
                  paste0(var_tab, "_IMPOSSIVEL"))
        }
        var_valida <- union(var_tab,var_valida)
      }

      Dados_TabelaImplau <- selectDataAux()
      for(var in var_valida){
        Dados_TabelaImplau[[paste0(var,'_IMPLAUSIVEL')]] <- TRUE
        Dados_TabelaImplau[Dados_TabelaImplau[[paste0(var,'_IMPOSSIVEL')]] == TRUE,paste0(var,'_IMPLAUSIVEL')] <- FALSE
        if(var == 'NU_IDADE_N'){
          Dados_TabelaImplau[Dados_TabelaImplau[[paste0(var,'_IMPROVAVEL')]] == TRUE,paste0(var,'_IMPLAUSIVEL')] <- FALSE
        }
      }

      variaveis_tab <- union(variaveis_tab,paste0(var_valida,'_IMPLAUSIVEL')) %>% sort()
      Dados_TabelaImplau <-
        Dados_TabelaImplau %>%
        tidyr::pivot_longer(cols = all_of(variaveis_tab),
                            names_to = "variable",
                            values_to = "value")

      Dados_TabelaImplau <- Dados_TabelaImplau %>%
        dplyr::filter(value)
      Dados_TabelaImplau <- Dados_TabelaImplau %>%
        dplyr::mutate(Dado = dplyr::case_when(
          stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Imposs\u00edvel",
          stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improv\u00e1vel",
          stringr::str_detect(variable, "PLAUSIVEL") ~ "Dado Plaus\u00edvel",
          TRUE ~ as.character(variable)
        ),
        variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1])#,
        )

        Dados_TabelaImplau  <- Dados_TabelaImplau %>%
               dplyr::count(variavel,Dado)
        Dados_TabelaImplau[['%']] <- NA
        for(var in var_valida){
          Dados_TabelaImplau[Dados_TabelaImplau$variavel == var, 4] <- 100 *
            Dados_TabelaImplau[Dados_TabelaImplau$variavel == var,3]/sum(Dados_TabelaImplau[Dados_TabelaImplau$variavel == var,3])
        }
        Dados_TabelaImplau[['%']] <-Dados_TabelaImplau[['%']] %>%  round(2)
        Dados_TabelaImplau
      })

       for(i in 1:35){
         local({
           my_i <- i
           output[[paste('print',i,sep='')]] <- shiny::renderText({
             if(var_dados_implau[my_i] %in% input$Graf_Variaveis_Incon){
               dados <-  selectData2()
               dados <- dados[dados$variavel == var_dados_implau[my_i],]
               total <- c('Total', 'Total',sum(dados$n),sum(dados[['%']]))
               dados <- rbind(dados,total)
               kableExtra::kable(dados[,c(2:4)],
                 caption = paste0(var_dados_implau[my_i]),
                digits  = 2
               ) %>%
                 kableExtra::kable_styling()}
           })
         })
       }

      output$table_incom <- reactable::renderReactable({
        variaveis_tab <- vector()
        var_valida <- vector()
        for(var_tab in input$Graf_Variaveis_Incon) {
          if (var_tab == "NU_IDADE_N") {
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPROVAVEL"))
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPOSSIVEL"))
          } else{
            variaveis_tab <-
              union(variaveis_tab,
                    paste0(var_tab, "_IMPOSSIVEL"))
          }
          var_valida <- union(var_tab,var_valida)
        }

        variaveis_tab <- sort(variaveis_tab)

        Dados_TabelaImplau <- selectDataAux()
        Dados_TabelaImplau <-
          Dados_TabelaImplau %>%
          tidyr::pivot_longer(cols = all_of(variaveis_tab),
                              names_to = "variable",
                              values_to = "value")

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          dplyr::filter(value)

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          dplyr::mutate(imps = dplyr::case_when(
            stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Imposs\u00edvel",
            stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improv\u00e1vel",
            TRUE ~ as.character(variable)
          ),
          variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1]),
          motivo = purrr::map_chr(variable, function(x) jsonfile_gest[[x]])
          )  %>%
          dplyr::filter(imps %in% input$Exib_Dados2)

        columns <- unique(c("SG_UF", "ID_MUNICIP", input$Graf_Variaveis_Incon, "motivo"))

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          dplyr::filter(variavel %in% columns) %>%
          dplyr::arrange(SG_UF)

        Dados_TabelaImplau <- Dados_TabelaImplau[, columns]

      reactable::reactable(Dados_TabelaImplau, groupBy = c("motivo", "SG_UF"),
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
    }
    #GRAFICO INCONSISTENCIA ----------
    if(indicador == 'incon'){

      output$graficoCompleteness <- plotly::renderPlotly({
        #FILTRAGEM PELOS FILTROS SELECIONADOS ------------
        #VARIAVEIS SELECIONADAS NO FILTRO
        variaveis <- NA
        var_names <- NA
        for(var in input$Graf_Variaveis_Incon){
          variaveis <- c(variaveis,names(vars_incon[vars_incon == var]))
          var_names <- c(var_names,vars_incon[vars_incon == var])
        }
        #TIRAR OS VALORES NA INICIAIS
        var_labeller <- function(variable, value){
          return(var_names[value])
        }
        variaveis <- variaveis[!is.na(variaveis)]
        var_names <- var_names[!is.na(var_names)]
        #FILTRAR POR CLASSI_FIN E CLASSE DE GESTANTE SELECIONADA
        Dados_GraficoIncon <- dados_incon %>%
          dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG)
        #CRIAR ANO E DATA E FILTRAR DATAS APOS 2020-3
        Dados_GraficoIncon$data <-
          with(
            Dados_GraficoIncon,
            format(Dados_GraficoIncon$dt_sint, "%Y-%m")
          )

        Dados_GraficoIncon <- Dados_GraficoIncon %>%
         dplyr::filter(as.character(data) >= '2020-03')

        #CONVERTER VARIAVEIS SELECIONADAS PARA BINARIO PARA CALCULO DE PORCENTAGEM --------------
        Dados_GraficoIncon[variaveis] <- sapply(Dados_GraficoIncon[variaveis], as.numeric)

        Dados_GraficoIncon <- Dados_GraficoIncon %>%
          tidyr::pivot_longer(
            cols = all_of(variaveis),
            names_to = "variable",
            values_to = "value"
          )
        #FILTRAGEM E FINALIZACAO POR TIPO DE LOCALIDADE ---------------
        if (input$Graf_OpcaoLocalidade == 'br') {
          Dados_GraficoIncon1 <- Dados_GraficoIncon[c('variable', 'value', 'data')]

        } else {
          Dados_GraficoIncon1 <- Dados_GraficoIncon[c('variable', 'value','SG_UF', 'muni_nm_clean', 'data')]

          if (input$Graf_OpcaoLocalidade == "muni") {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
             dplyr::filter(muni_nm_clean == input$Graf_muni)

          } else {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              dplyr::filter(SG_UF == input$Graf_Estado)

          }

        }

        if (input$Graf_OpcaoLocalidade == 'br') {
          Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
            dplyr::group_by(data, variable) %>%
            dplyr::summarize(value = mean(value))
          Dados_GraficoIncon1$localidade <- 'BR'
        } else {

          if (input$Graf_OpcaoLocalidade == "muni") {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))
            Dados_GraficoIncon1$localidade <- input$Graf_muni
            print(input$Graf_muni)
          } else {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))
            Dados_GraficoIncon1$localidade <- input$Graf_Estado
          }
        }

        if (input$Graf_OpcaoComparar != 'br') {
          Dados_GraficoIncon2 <- Dados_GraficoIncon[c('variable', 'value','SG_UF', 'muni_nm_clean', 'data')]

          if (input$Graf_OpcaoComparar == "muni") {
            Dados_GraficoIncon2 <- Dados_GraficoIncon %>%
              dplyr::filter(muni_nm_clean == input$Graf_CompararMunicipio) %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))

            Dados_GraficoIncon2$localidade <- input$Graf_CompararMunicipio
            print(input$Graf_CompararMunicipio)
          } else {
            print("else")
            Dados_GraficoIncon2 <- Dados_GraficoIncon %>%
              dplyr::filter(SG_UF == input$Graf_CompararEstado) %>%
              dplyr::group_by(data, variable) %>%
              dplyr::summarize(value = mean(value))
            Dados_GraficoIncon2$localidade <- input$Graf_CompararEstado
          }
          Dados_GraficoIncon1 <- rbind( Dados_GraficoIncon1,Dados_GraficoIncon2)

        }
        Dados_GraficoIncon1$value <- round(Dados_GraficoIncon1$value * 100, 2)
        #FINALIZACAO COM GGPLOT -------------------------
        g <- ggplot2::ggplot(data = Dados_GraficoIncon1,
                    ggplot2::aes(y = value, x = data, fill = localidade)) +
          ggplot2::geom_bar(position = "dodge", stat = "identity") +
          ggplot2::facet_grid(rows = ggplot2::vars(variable))

        g <- g + ggplot2::labs(x = NULL) +
          ggplot2::labs(y = "Inconsist\u00eancia (%)", fill = "Localidade") +
          ggplot2::scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
          ggplot2::scale_fill_viridis_d() +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            face = "bold",
            color = "#000000",
            size = 9,
            angle = 45
          ))

        plotly::ggplotly(g, height = c(length(var_names) * 125 + 125)) %>%
        plotly::layout(legend = list(orientation = "h", y = 20))})

      selectDataFiltro <- reactive({
        variaveis <- NA
        var_names <- NA
        for(var in input$Graf_Variaveis_Incon){
          variaveis <- c(variaveis,names(vars_incon[vars_incon == var]))
          var_names <- c(var_names,vars_incon[vars_incon == var])
        }
        variaveis <- variaveis[!is.na(variaveis)]
        var_names <- var_names[!is.na(var_names)]
        #FILTRAR POR CLASSI_FIN E CLASSE DE GESTANTE SELECIONADA
        Dados_GraficoIncon <- dados_incon %>%
          dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG)
        Dados_GraficoIncon$data <-
          with(
            Dados_GraficoIncon,
            format(Dados_GraficoIncon$dt_sint, "%Y-%m")
          )

        Dados_GraficoIncon <- Dados_GraficoIncon %>%
          dplyr::filter(as.character(data) >= '2020-03')
        VarSelecionadas <- Dados_GraficoIncon[variaveis]

        Dados_GraficoIncon <- Dados_GraficoIncon %>%
          tidyr::pivot_longer(
            cols = all_of(variaveis),
            names_to = "variable",
            values_to = "value"
          )
        #FILTRAGEM E FINALIZACAO POR TIPO DE LOCALIDADE ---------------
        if (input$Graf_OpcaoLocalidade == 'br') {
          Dados_GraficoIncon1 <- Dados_GraficoIncon[c('variable', 'value', 'data')]

        } else {
          Dados_GraficoIncon1 <- Dados_GraficoIncon[c('variable', 'value','SG_UF', 'muni_nm_clean', 'data')]

          if (input$Graf_OpcaoLocalidade == "muni") {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              dplyr::filter(muni_nm_clean == input$Graf_muni)

          } else {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              dplyr::filter(SG_UF == input$Graf_Estado)

          }

        }

        Dados_GraficoIncon1
      })

      selectDataTable <- reactive({
        data <- selectDataFiltro()
        data <- data %>%
          dplyr::mutate(Dado = dplyr::case_when(
            stringr::str_detect(value, "FALSE") ~ "Dado V\u00e1lido",
            stringr::str_detect(value, "TRUE") ~ "Dado Inconsistente"
          ),
          variavel = gsub('_e_',' e ',variable)
          ) %>%
          dplyr::filter(Dado %in% input$Exib_Dados)
        data <- data[4:ncol(data)]
        data <- data  %>% group_by(variavel,Dado)%>% dplyr::count()
        data[['variavel']] <- gsub('_INCONSISTENTES','', data[['variavel']])
        data
      })

      for(i in 1:35){
        local({
          my_i <- i
          output[[paste('print',i,sep='')]] <- shiny::renderText({
            if(unname(vars_incon)[my_i] %in% input$Graf_Variaveis_Incon){
              dados <-  selectDataTable()
              dados <- dados[dados$variavel == unname(vars_incon[my_i]),]
              dados[['%']] <- round(((dados$n*100)/sum(dados$n)),2)
              total <- data.frame(variavel = 'Total',
                                  Dado = 'Total',
                                  n = sum(dados$n))
              total[['%']] <- sum(dados[['%']])
              dados <- rbind(dados,total)

              kableExtra::kable(dados[,c(2:4)],
                                caption = paste0(unname(vars_incon)[my_i]),
                                digits  = 2
              ) %>%
                kableExtra::kable_styling()}
          })
        })
      }

      selectDataMicro <- shiny::reactive({
          variaveis <- NA
          var_names <- NA
          for(var in input$Graf_Variaveis_Incon){
            variaveis <- c(variaveis,names(vars_incon[vars_incon == var]))
            var_names <- c(var_names,vars_incon[vars_incon == var])
          }
          #TIRAR OS VALORES NA INICIAIS
          var_labeller <- function(variable, value){
            return(var_names[value])
          }
          variaveis <- variaveis[!is.na(variaveis)]
          var_names <- var_names[!is.na(var_names)]
          #FILTRAR POR CLASSI_FIN E CLASSE DE GESTANTE SELECIONADA
          Dados_GraficoIncon <- dados_incon %>%
            dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
            dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG)
          #CRIAR ANO E DATA E FILTRAR DATAS APOS 2020-3
          Dados_GraficoIncon$data <-
            with(
              Dados_GraficoIncon,
              format(Dados_GraficoIncon$dt_sint, "%Y-%m")
            )
          Dados_GraficoIncon <- Dados_GraficoIncon %>%
            dplyr::filter(as.character(data) >= '2020-03')
          VarSelecionadas <- Dados_GraficoIncon[variaveis]
          if(input$Graf_OpcaoLocalidade == 'est'){
            Dados_GraficoIncon <- Dados_GraficoIncon[Dados_GraficoIncon$SG_UF == input$Graf_Estado,]
          } else if(input$Graf_OpcaoLocalidade == 'muni'){
            Dados_GraficoIncon <- Dados_GraficoIncon[Dados_GraficoIncon$muni_nm_clean == input$Graf_muni,]
          }
          Dados_GraficoIncon <- Dados_GraficoIncon[is.na(Dados_GraficoIncon$SG_UF) == F,]
          Dados_GraficoIncon <- Dados_GraficoIncon %>%
            tidyr::pivot_longer(
              cols = all_of(variaveis),
              names_to = "variable",
              values_to = "value"
            )
          colunas <- input$Vars_microdados_incon
          Dados_GraficoIncon1 <- Dados_GraficoIncon[,c('variable', 'value','SG_UF','muni_nm_clean',colunas)]
          Dados_GraficoIncon1$variable <-   gsub('_e_',' e ',Dados_GraficoIncon1$variable)
          Dados_GraficoIncon1$variable <-   gsub('_INCONSISTENTES','',Dados_GraficoIncon1$variable)
          names(Dados_GraficoIncon1)[c(1,4)] <- c('Inconsist\u00eancia','MUNICIPIO')
          Dados_GraficoIncon2 <- Dados_GraficoIncon1[Dados_GraficoIncon1$value==TRUE,c(1,3:ncol(Dados_GraficoIncon1))]
          })

      output$table_incom <- reactable::renderReactable({
        Dados_TabelaIncon <- selectDataMicro()
        reactable::reactable(Dados_TabelaIncon, groupBy = c('Inconsist\u00eancia',"SG_UF", "MUNICIPIO"),
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

    }
  })
}

## To be copied in the UI
# mod_SIVEP_incompletude_ui("SIVEP_incompletude_1")

## To be copied in the server
# mod_SIVEP_incompletude_server("SIVEP_incompletude_1")
