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
    library(shiny)
    library(shinydashboard)
    shinyjs::useShinyjs()
    tabItem(tabName = tabname,
              #DESCRICAO -----------
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
              #CAIXA COM FILTROS -----------------
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
                      inputId = ns("Graf_DiagonisticoSRAG"),
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
                    tippy::tippy_this(
                      elementId = ns("Graf_DiagonisticoSRAG"),
                      tooltip = "Causa da Sindrome Respiratória Aguda Grave (SRAG).",
                      placement = "right"),
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
                    if(indicador=='incom'){
                    checkboxGroupInput(
                      inputId = ns("Exib_Finalizados"),
                      label = "Casos Finalizados:",
                      choices = c("Exibir casos finalizados" = "cf"))},
                    #DESCRICAO EXIB_FINALIZADOS
                    if(indicador =='incom'){
                    tippy::tippy_this(
                      elementId = ns("Exib_Finalizados"),
                      tooltip = "Casos em que se tem informação sobre a evolução.",
                      placement = "right")},
                    #FILTRO PARA TIPOS DE DADOS, IGNORADOS OU EM BRANCO
                    if(indicador == 'incom'){
                      checkboxGroupInput(
                      inputId = ns("Exib_Dados"),
                      label = "Exibir dados:",
                      choices = c("Ignorados" = "ignore",
                                  "Em branco" = "na"),
                      selected = c("ignore", "na"))},
                    if(indicador == 'incom'){
                    tippy::tippy_this(
                      elementId = ns("Exib_Dados"),
                      tooltip = "Dados que não foram preenchidos ('dados em branco') ou são desconhecidos ('dados ignorados')",
                      placement = "right")},
                    if(indicador == 'implau'){
                     checkboxGroupInput(
                      inputId = ns("Exib_Dados2"),
                      label = "Exibir dados:",
                      choices = c("Dado Improvável",
                                    "Dado Impossível"),
                      selected = c("Dado Improvável",
                                "Dado Impossível"))},
                    if(indicador =='implau'){
                      tippy::tippy_this(
                        elementId = ns('Exib_Dados2'),
                        tooltip = 'Tipos de implausibilidades, a opção Dado Plausível so pode ser utilizada na sessão de tabelas',
                        placement = 'right'
                      )
                    },
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
                      condition = sprintf("input['%s'] != 'br'",ns("Graf_OpcaoLocalidade")),
                      selectInput(
                        ns("Graf_Estado"),
                        "Selecione o estado",
                        choices = estados,
                        selected = estados[1])),
                    #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE POR MUNICIPIO
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'muni'",ns("Graf_OpcaoLocalidade")),
                      selectInput(
                        ns("Graf_muni"),
                        "Selecione o município",
                        choices = 'municipios')),
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
                      condition = sprintf("input['%s'] != 'br'",ns("Graf_OpcaoLocalidade")),
                      selectInput(
                        ns("Graf_CompararEstado"),
                        "Estado de comparação",
                        choices = estados,
                        selected = estados[1]
                      )),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'muni'",ns("Graf_OpcaoComparar")),
                      selectInput(
                        ns("Graf_CompararMunicipio"),
                        "Município de comparação",
                        ('municipios')
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
                            plotly::plotlyOutput(ns("graficoCompleteness"),
                                                height = 'auto')),

                          #TABELAS QUANT
                           tabPanel(
                             "Tabelas",
                             tabBox(
                               width = 24,
                               tabPanel(htmlOutput(ns('print1'))),
                               tabPanel(htmlOutput(ns('print2'))),
                               tabPanel(htmlOutput(ns('print3'))),
                               tabPanel(htmlOutput(ns('print4'))),
                               tabPanel(htmlOutput(ns('print5'))),
                               tabPanel(htmlOutput(ns('print6'))),
                               tabPanel(htmlOutput(ns('print7'))),
                               tabPanel(htmlOutput(ns('print8'))),
                               tabPanel(htmlOutput(ns('print9'))),
                               tabPanel(htmlOutput(ns('print10'))),
                               tabPanel(htmlOutput(ns('print11'))),
                               tabPanel(htmlOutput(ns('print12'))),
                               tabPanel(htmlOutput(ns('print13'))),
                               tabPanel(htmlOutput(ns('print14'))),
                               tabPanel(htmlOutput(ns('print15'))),
                               tabPanel(htmlOutput(ns('print16'))),
                               tabPanel(htmlOutput(ns('print17'))),
                               tabPanel(htmlOutput(ns('print18'))),
                               tabPanel(htmlOutput(ns('print19'))),
                               tabPanel(htmlOutput(ns('print20'))),
                               tabPanel(htmlOutput(ns('print21'))),
                               tabPanel(htmlOutput(ns('print22'))),
                               tabPanel(htmlOutput(ns('print23'))),
                               tabPanel(htmlOutput(ns('print24'))),
                               tabPanel(htmlOutput(ns('print25'))),
                               tabPanel(htmlOutput(ns('print26'))),
                               tabPanel(htmlOutput(ns('print27'))),
                               tabPanel(htmlOutput(ns('print28'))),
                               tabPanel(htmlOutput(ns('print29'))),
                               tabPanel(htmlOutput(ns('print30'))),
                               tabPanel(htmlOutput(ns('print31'))),
                               tabPanel(htmlOutput(ns('print32'))),
                               tabPanel(htmlOutput(ns('print33'))),
                               tabPanel(htmlOutput(ns('print34'))),
                               tabPanel(htmlOutput(ns('print35')))
                                )
                          ),
                          #TABELAS EXPLICATIVAS
                          tabPanel("Microdados",
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
    observe({
      x <- input$Graf_Estado
      y <- input$Graf_CompararEstado
      if(indicador == 'incom')dado <- dados_incom
      if(indicador == 'implau')dado <- dados_implau
      if(indicador == 'incon')dado <- dados_incon
      dados_aux <- dado[dado$SG_UF %in% x,c('muni_nm_clean','SG_UF')]
      dados_compara <- dado[dado$SG_UF %in% y,c('muni_nm_clean','SG_UF')]

      updateSelectInput(session,("Graf_muni"),
                        choices = sort(unique(dados_aux$muni_nm_clean)),
                        selected = unique(dados_aux$muni_nm_clean)[1])

      updateSelectInput(session,("Graf_CompararMunicipio"),
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
              filter(muni_nm_clean == input$Graf_muni)

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
            Dados_GraficoIncompletude$localidade <- input$Graf_muni
            print(input$Graf_muni)
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
          labs(y = "Incompletude (%)", fill = "Localidade") +
          scale_y_continuous(breaks = seq(0,100,20), limits = c(0, 100)) +
          scale_fill_viridis_d() +
          theme_bw() +
          theme(axis.text.x = element_text(
            face="bold",
            color="#000000",
            size=9,
            angle=45
          ))

        plotly::ggplotly(g, height=(length(variaveis)*125 + 125)) %>% layout(legend = list(orientation = "h", y = 20))
    })


      selectData <- reactive({
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
            filter(f_evolucao == 'Dados válidos')
        } else{
          df
        }

      })


      for(i in 1:35){
        local({
          my_i <- i
          output[[paste('print',i,sep='')]] <- renderText({
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
            filter(f_evolucao == 'Dados válidos')
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
          filter(as.character(data) >= '2020-03')

        Dados_microdados <-
          Dados_microdados %>%
          tidyr::pivot_longer(cols = all_of(variaveis),
                              names_to = "variable",
                              values_to = "value")

        columns <- unique(c("SG_UF", "ID_MUNICIP",variaveis))

        Dados_microdados <- Dados_microdados %>%
          filter(variable %in% columns) %>%
          arrange(SG_UF)

        Dados_microdados <- Dados_microdados[,c("SG_UF", "ID_MUNICIP",'variable','value')]
        Dados_microdados <- Dados_microdados %>% count(value,SG_UF,ID_MUNICIP,variable)
        for(i in variaveis){
          Dados_microdados[Dados_microdados$variable == i,"variable"] <- variaveis_relacao[i]
        }
        names(Dados_microdados)[c(1,4,5)] <- c('Incompletude','Variável','Frequência')
        if('ignore' %in%  input$Exib_Dados & 'na' %in%  input$Exib_Dados){
          Dados_microdados <-
            Dados_microdados[Dados_microdados$Incompletude !='Dados válidos',]
        }else if('ignore' %in% input$Exib_Dados){
          Dados_microdados <-
            Dados_microdados[Dados_microdados$Incompletude=='Dados ignorados',]
        }else if('na' %in% input$Exib_Dados){
          Dados_microdados <-
            Dados_microdados[Dados_microdados$Incompletude=='Dados em branco',]
        }else{
          Dados_microdados <- NULL
        }

        reactable::reactable(Dados_microdados, groupBy = c("Incompletude",'Variável','SG_UF'),
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
             filter(as.character(data) >= '2020-03')
           VarSelecionadas <- Dados_GraficoImplauIniciais[variaveis]

           Dados_GraficoImplauIniciais <-
             Dados_GraficoImplauIniciais %>%
             tidyr::pivot_longer(cols = all_of(variaveis),
                                 names_to = "variable",
                                 values_to = "value")

           Dados_GraficoImplauIniciais <- Dados_GraficoImplauIniciais %>%
             mutate(imps = case_when(
               stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Impossível",
               stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improvável",
               TRUE ~ as.character(variable)
             ),
             variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1])
             )

           Dados_TabelaImplau <- Dados_GraficoImplauIniciais

           Dados_GraficoImplauIniciais <- Dados_GraficoImplauIniciais %>%
             filter(imps %in% input$Exib_Dados2)

           # filtra apenas as variaveis escolhidas pelo usuario

           if (input$Graf_OpcaoLocalidade == 'br') {
             Dados_GraficoImplau <-
               Dados_GraficoImplauIniciais[c('variavel', 'value', 'data')]

           } else {
             Dados_GraficoImplau <-
               Dados_GraficoImplauIniciais[c('variavel', 'value', 'SG_UF', 'muni_nm_clean', 'data')]

             if (input$Graf_OpcaoLocalidade == "muni") {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 filter(muni_nm_clean == input$Graf_muni)

             } else {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 filter(SG_UF == input$Graf_Estado)

             }

           }

           if (input$Graf_OpcaoLocalidade== 'br') {
             Dados_GraficoImplau <- Dados_GraficoImplau %>%
               group_by(data, variavel) %>%
               summarize(value = mean(value))
             Dados_GraficoImplau$localidade <- 'BR'
           } else {
             if (input$Graf_OpcaoLocalidade == "muni") {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 group_by(data, variavel) %>%
                 summarize(value = mean(value))
               Dados_GraficoImplau$localidade <- input$Graf_muni
               print(input$Graf_muni)
             } else {
               Dados_GraficoImplau <- Dados_GraficoImplau %>%
                 group_by(data, variavel) %>%
                 summarize(value = mean(value))
               Dados_GraficoImplau$localidade <- input$Graf_Estado
             }
           }

           if (input$Graf_OpcaoComparar != 'br') {
             Dados_GraficoImplau2 <-
               Dados_GraficoImplauIniciais[c('variavel', 'value', 'SG_UF', 'muni_nm_clean', 'data')]

             if (input$Graf_OpcaoComparar== "muni") {
               Dados_GraficoImplau2 <- Dados_GraficoImplauIniciais %>%
                 filter(muni_nm_clean == input$Graf_CompararMunicipio) %>%
                 group_by(data, variavel) %>%
                 summarize(value = mean(value))

               Dados_GraficoImplau2$localidade <-
                 input$Graf_CompararMunicipio
               print(input$Graf_CompararMunicipio)
             } else {
               print("else")
               Dados_GraficoImplau2 <-
                 Dados_GraficoImplauIniciais %>%
                 filter(SG_UF == input$Graf_CompararEstado) %>%
                 group_by(data, variavel) %>%
                 summarize(value = mean(value))
               Dados_GraficoImplau2$localidade <-
                 input$Graf_CompararEstado
             }
             Dados_GraficoImplau <-
               rbind(Dados_GraficoImplau, Dados_GraficoImplau2)

           }

           Dados_GraficoImplau$value <-
             round(Dados_GraficoImplau$value * 100, 2)

      g <- ggplot(data = Dados_GraficoImplau,
                  aes(y = value, x = data, fill = localidade)) +
        geom_bar(position = "dodge", stat = "identity") +
        facet_grid(rows = vars(variavel))

      g <- g + labs(x = NULL) +
        labs(y = "Implausibilidade (%)", fill = "Localidade") +
        scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
        scale_fill_viridis_d() +
        theme_bw() +
        theme(axis.text.x = element_text(
          face = "bold",
          color = "#000000",
          size = 9,
          angle = 45
        ))
      if('NU_IDADE_N' %in% input$Graf_Variaveis_Incon){
      plotly::ggplotly(g, height = length(variaveis) * 125) %>%
        layout(legend = list(orientation = "h", y = 20))}
      else{plotly::ggplotly(g, height = (length(variaveis) * 125 + 125)) %>%
          layout(legend = list(orientation = "h", y = 20))

      }})


      selectDataAux <- reactive({
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
          filter(as.character(data) >= '2020-03')
        if(input$Graf_OpcaoLocalidade == 'est'){
          Dados_TabelaImplau <- Dados_TabelaImplau[Dados_TabelaImplau$SG_UF == input$Graf_Estado,]
        } else if(input$Graf_OpcaoLocalidade == 'muni'){
          Dados_TabelaImplau <- Dados_TabelaImplau[Dados_TabelaImplau$muni_nm_clean == input$Graf_muni,]
        }
        Dados_TabelaImplau
      })


      selectData <- reactive({
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
          filter(value)

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          mutate(imps = case_when(
            stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Impossível",
            stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improvável",
            TRUE ~ as.character(variable)
          ),
          variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1]),
          motivo = purrr::map_chr(variable, function(x) jsonfile_gest[[x]])
          )

        columns <- unique(c("SG_UF", "ID_MUNICIP", input$Graf_Variaveis_Incon, "motivo"))

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          filter(variavel %in% columns) %>%
          arrange(SG_UF)

        Dados_TabelaImplau <- Dados_TabelaImplau[, columns]
      })


      selectData2 <- reactive({
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
        filter(value)
      Dados_TabelaImplau <- Dados_TabelaImplau %>%
        mutate(Dado = case_when(
          stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Impossível",
          stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improvável",
          stringr::str_detect(variable, "PLAUSIVEL") ~ "Dado Plausível",
          TRUE ~ as.character(variable)
        ),
        variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1])#,
        )

        Dados_TabelaImplau  <- Dados_TabelaImplau %>%
               count(variavel,Dado)
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
           output[[paste('print',i,sep='')]] <- renderText({
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
          filter(value)

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          mutate(imps = case_when(
            stringr::str_detect(variable, "IMPOSSIVEL") ~ "Dado Impossível",
            stringr::str_detect(variable, "IMPROVAVEL") ~ "Dado Improvável",
            TRUE ~ as.character(variable)
          ),
          variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1]),
          motivo = purrr::map_chr(variable, function(x) jsonfile_gest[[x]])
          )  %>%
          filter(imps %in% input$Exib_Dados2)

        columns <- unique(c("SG_UF", "ID_MUNICIP", input$Graf_Variaveis_Incon, "motivo"))

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          filter(variavel %in% columns) %>%
          arrange(SG_UF)

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
         filter(as.character(data) >= '2020-03')

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
              filter(muni_nm_clean == input$Graf_muni)

          } else {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              filter(SG_UF == input$Graf_Estado)

          }

        }

        if (input$Graf_OpcaoLocalidade == 'br') {
          Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
            group_by(data, variable) %>%
            summarize(value = mean(value))
          Dados_GraficoIncon1$localidade <- 'BR'
        } else {

          if (input$Graf_OpcaoLocalidade == "muni") {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))
            Dados_GraficoIncon1$localidade <- input$Graf_muni
            print(input$Graf_muni)
          } else {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))
            Dados_GraficoIncon1$localidade <- input$Graf_Estado
          }
        }

        if (input$Graf_OpcaoComparar != 'br') {
          Dados_GraficoIncon2 <- Dados_GraficoIncon[c('variable', 'value','SG_UF', 'muni_nm_clean', 'data')]

          if (input$Graf_OpcaoComparar == "muni") {
            Dados_GraficoIncon2 <- Dados_GraficoIncon %>%
              filter(muni_nm_clean == input$Graf_CompararMunicipio) %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))

            Dados_GraficoIncon2$localidade <- input$Graf_CompararMunicipio
            print(input$Graf_CompararMunicipio)
          } else {
            print("else")
            Dados_GraficoIncon2 <- Dados_GraficoIncon %>%
              filter(SG_UF == input$Graf_CompararEstado) %>%
              group_by(data, variable) %>%
              summarize(value = mean(value))
            Dados_GraficoIncon2$localidade <- input$Graf_CompararEstado
          }
          Dados_GraficoIncon1 <- rbind( Dados_GraficoIncon1,Dados_GraficoIncon2)

        }
        Dados_GraficoIncon1$value <- round(Dados_GraficoIncon1$value * 100, 2)
        #FINALIZACAO COM GGPLOT -------------------------
        g <- ggplot(data = Dados_GraficoIncon1,
                    aes(y = value, x = data, fill = localidade)) +
          geom_bar(position = "dodge", stat = "identity") +
          facet_grid(rows = vars(variable))

        g <- g + labs(x = NULL) +
          labs(y = "Inconsistência (%)", fill = "Localidade") +
          scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
          scale_fill_viridis_d() +
          theme_bw() +
          theme(axis.text.x = element_text(
            face = "bold",
            color = "#000000",
            size = 9,
            angle = 45
          ))

        plotly::ggplotly(g, height = c(length(var_names) * 125 + 125)) %>%
          layout(legend = list(orientation = "h", y = 20))})

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
          filter(as.character(data) >= '2020-03')
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
              filter(muni_nm_clean == input$Graf_muni)

          } else {
            Dados_GraficoIncon1 <- Dados_GraficoIncon1 %>%
              filter(SG_UF == input$Graf_Estado)

          }

        }

        Dados_GraficoIncon1
      })

      selectDataTable <- reactive({
        data <- selectDataFiltro()
        data <- data %>%
          mutate(Dado = case_when(
            stringr::str_detect(value, "FALSE") ~ "Dado Válido",
            stringr::str_detect(value, "TRUE") ~ "Dado Inconsistente"
          ),
          variavel = gsub('_e_',' e ',variable)
          ) %>%
          filter(Dado %in% input$Exib_Dados)
        data <- data[4:ncol(data)]
        data <- data  %>% group_by(variavel,Dado)%>% count()
        data[['variavel']] <- gsub('_INCONSISTENTES','', data[['variavel']])
        data
      })

      for(i in 1:35){
        local({
          my_i <- i
          output[[paste('print',i,sep='')]] <- renderText({
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

      selectDataMicro <- reactive({
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
            filter(as.character(data) >= '2020-03')
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
          names(Dados_GraficoIncon1)[c(1,4)] <- c('Inconsistência','MUNICIPIO')
          Dados_GraficoIncon2 <- Dados_GraficoIncon1[Dados_GraficoIncon1$value==TRUE,c(1,3:ncol(Dados_GraficoIncon1))]
          })

      output$table_incom <- reactable::renderReactable({
        Dados_TabelaIncon <- selectDataMicro()
        reactable::reactable(Dados_TabelaIncon, groupBy = c('Inconsistência',"SG_UF", "MUNICIPIO"),
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
