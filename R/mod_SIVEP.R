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
                                      indicador,estados,selecionadas){
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
                      inputId = ns("vars_select"),
                      label = "Vari\u00e1veis",
                      choices = vars_incon,
                      selected = selecionadas,
                      options = list(`actions-box` = TRUE),
                      multiple = T),
                    #tempo
                    shiny::sliderInput(
                      ns('filtro_tempo'),
                      'Selecione a janela de tempo:',
                      min = 2009,
                      max = 2022,
                      value = c(2005,2022),
                      round = T,
                      sep=''),
                    #FILTRO DE DIAGNOSTICO DE SRAG
                    shiny::checkboxGroupInput(
                      inputId = ns("Graf_DiagonisticoSRAG"),
                      label = "Diagn\u00f3stico de SRAG",
                      choices = c(
                        "COVID-19" = "5",
                        "N\u00e3o especificado" = "4",
                        "N\u00e3o respondido" = "Em Branco",
                        "Influenza" = "1",
                        "Outro v\u00edrus" = "2",
                        "Outro agente" = "3"),
                      selected = c("5",'3')),
                    tippy::tippy_this(
                      elementId = ns("Graf_DiagonisticoSRAG"),
                      tooltip = "Causa da Sindrome Respirat\u00f3ria Aguda Grave (SRAG).",
                      placement = "right"),
                    #FILTRO DE EXIBICAO OU NAO DE CASOS FINALIZADOS
                      shiny::checkboxGroupInput(
                      inputId = ns("Exib_Finalizados"),
                      label = "Casos Finalizados:",
                      choices = c("Exibir casos finalizados" = "cf")),
                    # #DESCRICAO EXIB_FINALIZADOS
                    tippy::tippy_this(
                      elementId = ns("Exib_Finalizados"),
                      tooltip = "Casos em que se tem informa\u00e7\u00e3o sobre a evolu\u00e7\u00e3o.",
                      placement = "right"),
                    #FILTRO PARA TIPOS DE DADOS, IGNORADOS OU EM BRANCO
                    if(indicador == 'incom'){
                      shiny::checkboxGroupInput(
                      inputId = ns("Exib_Dados2"),
                      label = "Exibir dados:",
                      choices = c("Ignorado" = "Ignorado",
                                  "Em Branco" = "Em Branco"),
                      selected = c("Ignorado", "Em Branco"))},
                    if(indicador == 'incom'){
                    tippy::tippy_this(
                      elementId = ns("Exib_Dados2"),
                      tooltip = "Dados que n\u00e3o foram preenchidos ('dados em branco') ou s\u00e3o desconhecidos ('dados ignorados')",
                      placement = "right")},
                    if(indicador == 'implau'){
                      shiny::checkboxGroupInput(
                      inputId = ns("Exib_Dados2"),
                      label = "Exibir dados:",
                      choices = c("Dado Improv\u00e1vel" = 'Improvavel',
                                    "Dado Imposs\u00edvel" = 'Impossivel'),
                      selected = c("Improvavel",
                                "Impossivel"))},
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
                        choices = 'AC')),
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
                      condition = sprintf("input['%s'] != 'br'",ns("Graf_OpcaoComparar")),
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
                    shiny::div(shiny::tabsetPanel(
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
                               shiny::tabPanel(shiny::htmlOutput(ns('print35'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print36'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print37'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print38'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print39'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print40'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print41'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print42'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print43'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print44'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print45'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print46'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print47'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print48'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print49'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print50'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print51'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print52'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print53'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print54'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print55'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print56'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print57'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print58'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print59'))),
                               shiny::tabPanel(shiny::htmlOutput(ns('print60')))
                                ))
                          # ),
                          # #TABELAS EXPLICATIVAS
                          # shiny::tabPanel("Microdados",
                          #           if(indicador == 'incon'){
                          #             shinyWidgets::pickerInput(
                          #               inputId = ns("Vars_microdados_incon"),
                          #               label = "Variaveis",
                          #               choices = sort(vars_incon),
                          #               selected = vars_incon[1:3],
                          #               options = list(`actions-box` = TRUE),
                          #               multiple = T)
                          #            },
                          #        reactable::reactableOutput(ns("table_incom")))
                          )))))

    }

#' SIVEP_incompletude Server Functions
#'
#' @noRd
mod_SIVEP_server <- function(id, indicador){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #FILTRO DE TEMPO E VARIAVEL PARA PODER ATUALIZAR O FILTRO DE ESTADO
    data_inicio <- shiny::reactive({
      var_value <- input$vars_select
      #BANCO DE DADO
      dados <- sivep_dados[c(var_value,'muni_nm_clean','CLASSI_FIN','SG_UF_NOT','EVOLUCAO','ano')] %>%
        dplyr::filter(as.integer(ano) >= input$filtro_tempo[1] & as.integer(ano) <= input$filtro_tempo[2]) %>%
        dplyr::filter(CLASSI_FIN %in% input$Graf_DiagonisticoSRAG )
      #FILTRAR POR CASOS FINALIZADOS
      if("cf" %in% input$Exib_Finalizados){
        dados <- dados %>%
          dplyr::filter(EVOLUCAO != 'Em Branco')
      }

      dados
    })

    #ATUALIZACAO DO FILTRO DE ESTADO E MUNICIPIO
    shiny::observe({
      # Obtem os valores dos inputs
      x <- input$Graf_Estado
      y <- input$Graf_CompararEstado

      # Carrega o dataframe
      dado <- data_inicio()

      # Filtra as observações pelos estados selecionados e obtém os municípios únicos
      muni <- unique(dado$muni_nm_clean [dado$SG_UF_NOT == x])
      muni_comp <- unique(dado$muni_nm_clean [dado$SG_UF_NOT == y])

      # Ordena os municípios alfabeticamente
      muni <- sort(muni)
      muni_comp <- sort(muni_comp)

      # Atualiza os valores dos selectInputs
      shiny::updateSelectInput(session, "Graf_muni", choices = muni, selected = muni[1])
      shiny::updateSelectInput(session, "Graf_CompararMunicipio", choices = muni_comp, selected = muni_comp[1])
    })

    #FILTRO E REFORMULACAO DA DISPOSICAO DO DATAFRAME
    filtragem <-  shiny::reactive({
      if(indicador == 'implau'){
        leg <- 'Implausibilidade'
        x <- c('Impossivel','Improvavel')
      }else{
        if(indicador == 'incom'){
          leg <- 'Incompletude'
          x <- c('Em Branco','Ignorado')
        }else{
          leg <- 'Inconsistencia'
          x <- c('Inconsistencia','nada')
        }
      }
      dados <- data_inicio()
      #FILTRO DE LOCALIDADE
      if(input$Graf_OpcaoLocalidade != 'br'){
        if(input$Graf_OpcaoLocalidade == 'est'){
          dados <- dados[dados$SG_UF_NOT == input$Graf_Estado,]
          localidade <- input$Graf_Estado
        }else{
          dados <- dados[dados$muni_nm_clean == input$Graf_muni,]
          localidade <-  input$Graf_muni
        }
      } else{
        localidade <- 'BR'
      }
      dados1 <- dados[,c(input$vars_select,'ano')]
      # Criar um vetor com os nomes das colunas
      colunas <- colnames(dados1)

      # Criar um dataframe vazio para armazenar os resultados
      resultados <- data.frame(Variavel = character(), Ano = integer(), "x1" = numeric(), "x2" = numeric(), Total = numeric(), stringsAsFactors = FALSE)

      # Iterar sobre as colunas e contar os valores em branco ou ignorados por ano
      for (coluna in colunas) {
        for (ano in unique(dados1$ano)) {
          cond1 <- sum(dados1[[coluna]][dados1$ano == ano] == x[1])
          cond2 <- sum(dados1[[coluna]][dados1$ano == ano] == x[2])
          total <- sum(dados1$ano == ano)
          resultados <- rbind(resultados, data.frame(Variavel = coluna, Ano = ano, "x1" = cond1, "x2" = cond2, Total = total, stringsAsFactors = FALSE))
        }
      }

      # Ordenar os resultados por ano e por variável
      resultados <- resultados %>%
        dplyr::mutate(
          Localidade = localidade,
          compara = 1
        )
      colnames(resultados)[c(3,4)] <- c(paste0('Dados ',x[1]),paste0('Dados ',x[2]))
      #FILTRO DE COMPARACAO
      if(input$Graf_OpcaoComparar != 'br'){
        dados2 <- dados
        if(input$Graf_OpcaoComparar == 'est'){
          dados2 <- dados[dados2$SG_UF_NOT == input$Graf_CompararEstado,]
          localidade <- input$Graf_CompararEstado
        }else{
          dados2 <- dados[dados2$muni_nm_clean == input$Graf_CompararMunicipio,]
          localidade <-  input$Graf_CompararMunicipio
        }
        dados2 <- dados[,input$vars_select]
        # Criar um vetor com os nomes das colunas
        colunas <- colnames(dados2 )

        # Criar um dataframe vazio para armazenar os resultados
        resultados2 <- data.frame(Variavel = character(), Ano = integer(), "x1" = numeric(), "x2" = numeric(), Total = numeric(), stringsAsFactors = FALSE)

        # Iterar sobre as colunas e contar os valores em branco ou ignorados por ano
        for (coluna in colunas) {
          for (ano in unique(dados2$ano)) {
            cond1 <- sum(dados2[[coluna]][dados2$ano == ano] == x[1])
            cond2 <- sum(dados2[[coluna]][dados2$ano == ano] == x[2])
            total <- sum(dados2$ano == ano)
            resultados2 <- rbind(resultados, data.frame(Variavel = coluna, Ano = ano, "x1" = cond1, "x2" = cond2, Total = total, stringsAsFactors = FALSE))
          }
        }

        # Ordenar os resultados por ano e por variável
        resultados2 <- resultados2 %>%
          dplyr::mutate(
            Localidade = localidade,
            compara = 1
          )
        colnames(resultados2)[c(3,4)] <- c(paste0('Dados ',x[1]),paste0('Dados ',x[2]))

        resultados <- rbind(resultados,resultado2)
      }
      resultados <- resultados[resultados$Variavel != 'ano',]
      resultados$Ano <- resultados$Ano %>% as.numeric()

      if((indicador %in% c('implau','incom')) & (length(input$Exib_Dados2) != 2)){
        y <- x[!(x %in% input$Exib_Dados2)]

          if(length(y) == 1){
            resultados['Total'] <- resultados['Total'] - (resultados[paste0('Dados ',y[1])])
            resultados[paste0('Dados ',y[1])] <- 0
           }else{
             resultados['Total'] <- resultados['Total'] - (resultados[paste0('Dados ',y[1])])- (resultados[paste0('Dados ',y[2])])
            resultados[paste0('Dados ',y[1])] <- 0
            resultados[paste0('Dados ',y[2])] <- 0
          }
      }
      resultados['value'] <- as.vector(((resultados[,3] + resultados[,4])*100)/resultados['Total']) %>% unlist()
      resultados
    })

    #GRAFICO
    output$graficoCompleteness <- plotly::renderPlotly({
      if(indicador == 'implau'){
        leg <- 'Implausibilidade'
      }else{
        if(indicador == 'incom'){
          leg <- 'Incompletude'
        }else{
          leg <- 'Inconsistencia'
        }
      }
     dados <- filtragem()
     h_plot <- length(input$vars_select) * 150

    g <- ggplot2::ggplot(data = dados,
                           ggplot2::aes(y = value, x = as.factor(Ano), fill = Localidade)) +
        ggplot2::geom_bar(position = "dodge", stat = "identity") +
        ggplot2::facet_grid(rows = ggplot2::vars(dados$Variavel))


    # Personalizando o gráfico
    g <- g + ggplot2::labs(x = NULL) +
      ggplot2::labs(y = paste0(leg," (%)")) +
      ggplot2::scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(
        face = "bold",
        color = "#000000",
        size = 9,
        angle = 45
      ))

    # Convertendo o gráfico ggplot2 para plotly e ajustando a layout
    plotly::ggplotly(g, height = (h_plot + 125)) %>%
      plotly::layout(legend = list(orientation = "h", y = 20))
    })

    #TABELAS
      if(indicador == 'incom' | indicador == 'implau'){
        #IMPLAUSIBILIDADE E INCOMPLETEUDE
      if(indicador == 'incom'){
        vars_reais <- sort(var_sivep_incom)
        leg <- 'Incompletude'
      }else{
          vars_reais <- sort(var_sivep_implau)
          leg <- 'implausibilidade'
      }
      for(i in 1:60) {
        local({
          my_i <- i
          output[[paste('print',i,sep='')]]  <- shiny::renderText({
            # verifica se a variável atual está selecionada pelo usuário
            if (vars_reais[my_i] %in% input$vars_select) {
              # filtra a tabela de dados
              dados <- filtragem()
              dados1 <- dados[dados$Variavel == vars_reais[my_i] & dados$compara == 1,] %>%
                dplyr::select(-c(value,Ano,Localidade,Variavel,compara)) %>%
                dplyr::summarise_at(dplyr::vars(1:3), sum)
              dados1 <- dados1 %>%
                dplyr::mutate(
                  'Dados Validos' = dados1[,3] -( dados1[,1] + dados1[,2])
                ) %>%  t() %>% as.data.frame()

              colnames(dados1) <- 'n'
              dados1<- dados1%>% dplyr::arrange(row.names(dados1))
              dados1[['%']] <- ((100*dados1$n)/dados1$n[4]) %>% round(digits = 2)
              # gera tabela formatada
              kableExtra::kable(dados1,
                                caption = paste0('Valores de ',leg, ' para ',vars_reais[my_i]),
                                digits  = 2) %>%
                kableExtra::kable_styling()
            }
          })
        })
      }

        }else{
        #INCONSISTENCIA
        vars_reais <- var_sivep_incon
        for(i in 1:35) {
          local({
            my_i <- i
            output[[paste('print',i,sep='')]]  <- shiny::renderText({
              # verifica se a variável atual está selecionada pelo usuário
              if (vars_reais[my_i] %in% input$vars_select) {
                # filtra a tabela de dados
                dados <- filtragem()
                dados1 <- dados[dados$Variavel == vars_reais[my_i] & dados$compara == 1,] %>%
                  dplyr::select(-c(value,Ano,Localidade,Variavel,compara,`Dados nada`)) %>%
                  dplyr::summarise_at(dplyr::vars(1:2), sum)
                dados1 <- dados1 %>%
                  dplyr::mutate(
                    'Dados Validos' = dados1[,2] -( dados1[,1])
                  ) %>%  t() %>% as.data.frame()

                colnames(dados1) <- 'n'
                dados1<- dados1%>% dplyr::arrange(row.names(dados1))
                dados1[['%']] <-((100* dados1$n)/dados1$n[3]) %>% round(digits = 2)
                # gera tabela formatada
                kableExtra::kable(dados1,
                                  caption = paste0('Valores de Inconsistencia para ',vars_reais[my_i]),
                                  digits  = 2) %>%
                  kableExtra::kable_styling()
              }
            })})

    }
  }
})}

## To be copied in the UI
# mod_SIVEP_incompletude_ui("SIVEP_incompletude_1")

## To be copied in the server
# mod_SIVEP_incompletude_server("SIVEP_incompletude_1")
