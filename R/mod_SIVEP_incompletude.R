#' SIVEP_incompletude UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SIVEP_incompletude_ui <- function(id, tabname, vars_incon = variaveis_incon_nomes, descricao,
                                      indicador){
  ns <- NS(id)
  library(shiny)
  library(shinydashboard)
  library(plotly)
  useShinyjs()
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
                    tippy::tippy_this(
                      elementId = ns("Graf_DiagonisticoSRAG_Incon"),
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
                      choices = c("Improvável",
                                    "Impossível"),
                      selected = c("Improvável",
                                "Impossível"))},
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
                      condition = sprintf("input['%s'] == 'est'",ns("Graf_OpcaoLocalidade")),
                      selectInput(
                        ns("Graf_CompararEstado"),
                        "Estado de comparação",
                        choices = estadosChoices,
                        selected = "ES"
                      )),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'muni'",ns("Graf_OpcaoComparar")),
                      selectInput(
                        ns("Graf_CompararMunicipio"),
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
                                 reactable::reactableOutput(ns("table_incom")))
                          )))))

    }

#' SIVEP_incompletude Server Functions
#'
#' @noRd
mod_SIVEP_incompletude_server <- function(id, indicador){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #INCOMPLETUDE ------------
    if(indicador == 'incom'){
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

        ggplotly(g, height=length(variaveis)*125) %>% layout(legend = list(orientation = "h", y = 20))
    })
      #DADOS
      selectData <- reactive({
        df <- dados_incom %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG_Incon) %>%
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

      #TABELAS Incompletude
      for(i in 1:35){
        local({
          my_i <- i
          output[[paste('print',i,sep='')]] <- renderText({
            if(variaveis_incon_nomes[my_i] %in% input$Graf_Variaveis_Incon){
              kableExtra::kable(
                questionr::freq(
                  selectData()[[variaveis_incon[my_i]]],
                  cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE
                ),
                caption = paste0("Dados faltantes para ",variaveis_incon_nomes[my_i]),
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
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG_Incon) %>%
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
    #GRAFICO IMPLAUSIBILIDADE -----------
    if(indicador == 'implau'){
      output$graficoCompleteness <- renderPlotly({
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
             dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG_Incon)

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
               stringr::str_detect(variable, "IMPOSSIVEL") ~ "Impossível",
               stringr::str_detect(variable, "IMPROVAVEL") ~ "Improvável",
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

      ggplotly(g, height = length(variaveis) * 125) %>% layout(legend = list(orientation = "h", y = 20))})
      selectData <- reactive({
        variaveis_tab <- vector()

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
        }

        variaveis_tab <- sort(variaveis_tab)

        Dados_TabelaImplau <- dados_implau %>%
          dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
          dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG_Incon)

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

        #Filtra os casos finalizados
        # if ("cf" %in% input$Exib_Finalizados)
        # {
        #   Dados_GraficoIncompletudeIniciais <-
        #     Dados_GraficoIncompletudeIniciais %>%
        #     filter(f_evolucao == 'Dados válidos')
        # }

        Dados_TabelaImplau <-
          Dados_TabelaImplau %>%
          tidyr::pivot_longer(cols = all_of(variaveis_tab),
                              names_to = "variable",
                              values_to = "value")

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          filter(value)

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          mutate(imps = case_when(
            stringr::str_detect(variable, "IMPOSSIVEL") ~ "Impossível",
            stringr::str_detect(variable, "IMPROVAVEL") ~ "Improvável",
            TRUE ~ as.character(variable)
          ),
          variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1]),
          motivo = purrr::map_chr(variable, function(x) jsonfile_gest[[x]])
          ) %>%
          filter(imps %in% input$Exib_Dados2)

        columns <- unique(c("SG_UF", "ID_MUNICIP", input$Graf_Variaveis_Incon, "motivo"))

        Dados_TabelaImplau <- Dados_TabelaImplau %>%
          filter(variavel %in% columns) %>%
          arrange(SG_UF)

        Dados_TabelaImplau <- Dados_TabelaImplau[, columns]
      })
      selectData2 <- reactive({variaveis_tab <- vector()

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
      }

      variaveis_tab <- sort(variaveis_tab)

      Dados_TabelaImplau <- dados_implau %>%
        dplyr::filter(classi_gesta_puerp %in% input$Graf_Condicao_Incon) %>%
        dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG_Incon)

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

      #Filtra os casos finalizados
      # if ("cf" %in% input$Exib_Finalizados)
      # {
      #   Dados_GraficoIncompletudeIniciais <-
      #     Dados_GraficoIncompletudeIniciais %>%
      #     filter(f_evolucao == 'Dados válidos')
      # }

      Dados_TabelaImplau <-
        Dados_TabelaImplau %>%
        tidyr::pivot_longer(cols = all_of(variaveis_tab),
                            names_to = "variable",
                            values_to = "value")

      Dados_TabelaImplau <- Dados_TabelaImplau %>%
        filter(value)
      Dados_TabelaImplau <- Dados_TabelaImplau %>%
        mutate(imps = case_when(
          stringr::str_detect(variable, "IMPOSSIVEL") ~ "Impossível",
          stringr::str_detect(variable, "IMPROVAVEL") ~ "Improvável",
          TRUE ~ as.character(variable)
        ),
        variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, "_IMP")[[1]][1]),
        motivo = purrr::map_chr(variable, function(x) jsonfile_gest[[x]])
        ) %>%
        filter(imps %in% input$Exib_Dados2)
        Dados_TabelaImplau  <- Dados_TabelaImplau %>%
               count(variavel,imps)
        teste <- NA
        for(i in 1:nrow(Dados_TabelaImplau)){
          for(j in 1:as.numeric(Dados_TabelaImplau[i,3]))
          teste <- rbind(teste,c(unname(Dados_TabelaImplau[i,c(1,2)])))
        }
        teste <- teste[!is.na(teste[,1]) ,]
        database <- data.frame(variaveis = unlist(teste[,1]),
                               imps = unlist(teste[,2]))
      })
       for(i in 1:35){
         local({
           my_i <- i
           output[[paste('print',i,sep='')]] <- renderText({
             if(var_dados_implau[my_i] %in% input$Graf_Variaveis_Incon){
               dados <- selectData2()
               dados <- dados[dados$variaveis == var_dados_implau[my_i],]
               kableExtra::kable(
                 questionr::freq(
                   dados[['imps']],
                   cum = FALSE,
                   total = TRUE,
                   na.last = FALSE,
                   valid = FALSE
                 ),
                 caption = paste0(var_dados_implau[my_i] ),
                 digits = 2
               ) %>%
                 kableExtra::kable_styling()}
           })
         })
       }

      output$table_incom <- reactable::renderReactable({
        Dados_TabelaImplau <- selectData()

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
  })
}

## To be copied in the UI
# mod_SIVEP_incompletude_ui("SIVEP_incompletude_1")

## To be copied in the server
# mod_SIVEP_incompletude_server("SIVEP_incompletude_1")
