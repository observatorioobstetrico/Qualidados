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
                                      indicador,selecionadas){
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
                      max = 2023,
                      value = c(2005,2023),
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
                        choices = sort(unique(dados_oobr_qualidados_SIVEP_2009_2023$SG_UF_NOT)),
                        selected = sort(unique(dados_oobr_qualidados_SIVEP_2009_2023$SG_UF_NOT))[1])),
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
                        choices = sort(unique(dados_oobr_qualidados_SIVEP_2009_2023$SG_UF_NOT)),
                        selected = sort(unique(dados_oobr_qualidados_SIVEP_2009_2023$SG_UF_NOT))[1]
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
                                )),
                          #TABELAS EXPLICATIVAS
                          shiny::tabPanel("Microdados",
                                          if(indicador == 'incon'){
                                            shinyWidgets::pickerInput(
                                              inputId = ns("var_extra"),
                                              label = "Vari\u00e1veis para filtragem",
                                              choices = Var_incon_relacao,
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

    #FILTRO DE TEMPO E VARIAVEL PARA PODER ATUALIZAR O FILTRO DE ESTADO
    data_inicio <- shiny::reactive({
      var_value <- input$vars_select
      #BANCO DE DADO
      if(indicador == 'incon'){
        aux <- input$var_extra
        var_value <- c(var_value,aux)
      }

      dados <- dados_oobr_qualidados_SIVEP_2009_2023[c(var_value,'MUNICIPIO','CLASSI_FIN','SG_UF_NOT','EVOLUCAO','ANO') %>% unique()] %>%
        dplyr::filter(as.integer(ANO) >= input$filtro_tempo[1] & as.integer(ANO) <= input$filtro_tempo[2]) %>%
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
      muni <- unique(dado$MUNICIPIO [dado$SG_UF_NOT == x])
      muni_comp <- unique(dado$MUNICIPIO [dado$SG_UF_NOT == y])

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
          dados1 <- dados[dados$SG_UF_NOT == input$Graf_Estado,]
          localidade <- input$Graf_Estado
        }else{
          dados1 <- dados[dados$MUNICIPIO == input$Graf_muni,]
          localidade <-  input$Graf_muni
        }
      } else{
        dados1 <- dados
        localidade <- 'BR'
      }
      dados1 <- dados1[,c(input$vars_select,'ANO')]
      # Criar um vetor com os nomes das colunas
      colunas <- colnames(dados1)

      # Criar um dataframe vazio para armazenar os resultados
      resultados <- data.frame(Variavel = character(), ANO = integer(), "x1" = numeric(), "x2" = numeric(), Total = numeric(), stringsAsFactors = FALSE)

      # Iterar sobre as colunas e contar os valores em branco ou ignorados por ANO
      for (coluna in colunas) {
        for (ANO in unique(dados1$ANO)) {
          cond1 <- sum(dados1[[coluna]][dados1$ANO == ANO] == x[1])
          cond2 <- sum(dados1[[coluna]][dados1$ANO == ANO] == x[2])
          total <- sum(dados1$ANO == ANO)
          resultados <- rbind(resultados, data.frame(Variavel = coluna, ANO = ANO, "x1" = cond1, "x2" = cond2, Total = total, stringsAsFactors = FALSE))
        }
      }

      # Ordenar os resultados por ANO e por variável
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
          dados2 <- dados2[dados2$MUNICIPIO == input$Graf_CompararMunicipio,]
          localidade <-  input$Graf_CompararMunicipio
        }
        dados2 <- dados2[,c(input$vars_select,"ANO")]
        # Criar um vetor com os nomes das colunas
        colunas <- colnames(dados2 )

        # Criar um dataframe vazio para armazenar os resultados
        resultados2 <- data.frame(Variavel = character(), ANO = integer(), "x1" = numeric(), "x2" = numeric(), Total = numeric(), stringsAsFactors = FALSE)

        # Iterar sobre as colunas e contar os valores em branco ou ignorados por ANO
        for (coluna in colunas) {
          for (ANO in unique(dados2$ANO)) {
            cond1 <- sum(dados2[[coluna]][dados2$ANO == ANO] == x[1])
            cond2 <- sum(dados2[[coluna]][dados2$ANO == ANO] == x[2])
            total <- sum(dados2$ANO == ANO)
            resultados2 <- rbind(resultados2, data.frame(Variavel = coluna, ANO = ANO, "x1" = cond1, "x2" = cond2, Total = total, stringsAsFactors = FALSE))
          }
        }

        # Ordenar os resultados por ANO e por variável
        resultados2 <- resultados2 %>%
          dplyr::mutate(
            Localidade = localidade,
            compara = 0
          )
        colnames(resultados2)[c(3,4)] <- c(paste0('Dados ',x[1]),paste0('Dados ',x[2]))

        resultados <- rbind(resultados,resultados2)
      }
      resultados <- resultados[resultados$Variavel != 'ANO',]
      resultados$ANO <- resultados$ANO %>% as.factor()

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
     if(indicador == 'incon'){
       h_plot <- length(input$vars_select) * 245
     }else{
     h_plot <- length(input$vars_select) * 200}
    dados$value <- dados$value %>% round(2)
    g <- ggplot2::ggplot(data = dados,
                           ggplot2::aes(y = value, x = ANO, fill = Localidade)) +
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
                dplyr::select(-c(value,ANO,Localidade,Variavel,compara)) %>%
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
                  dplyr::select(-c(value,ANO,Localidade,Variavel,compara,`Dados nada`)) %>%
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
    output$table_incom  <- reactable::renderReactable({
      if(indicador == 'incom'){
        X <- c('Em Branco','Ignorado')
        regra <- regras_sivep[regras_sivep$Indicador == 'Incompletude',]
      }else{
        if(indicador == 'implau'){
          regra <- regras_sivep[regras_sivep$Indicador == 'Implausiblidade',]
          X <- c('Impossivel','Improvavel')

        }else{
          regra <- regras_sivep[regras_sivep$Indicador == 'Inconsistência',]
          X <- c('Inconsistencia','NADA')
        }
      }

      variaveis_tab <- input$vars_select
      variaveis_tab <- sort(variaveis_tab)

      Dados_TabelaImplau <- data_inicio()
      aux <- data.frame(x1 =  Dados_TabelaImplau[,variaveis_tab] %>%
                          lapply(function(x) ifelse(x == X[1], TRUE, FALSE)),
                        x2 = Dados_TabelaImplau[,variaveis_tab] %>%
                          lapply(function(x) ifelse(x == X[2], TRUE, FALSE)))

      colnames(aux) <- gsub(colnames(aux),pattern = 'x1.',replacement = paste0(X[1],'xx_'))
      colnames(aux) <- gsub(colnames(aux),pattern = 'x2.',replacement = paste0(X[2],'xx_'))
      Dados_TabelaImplau <- cbind(Dados_TabelaImplau,aux)
      Dados_TabelaImplau <-
        Dados_TabelaImplau %>%
        tidyr::pivot_longer(cols = all_of(colnames(aux)),
                            names_to = "variable",
                            values_to = "value")
      Dados_TabelaImplau <- Dados_TabelaImplau %>% dplyr::filter(value)

      Dados_TabelaImplau <- Dados_TabelaImplau %>%
        dplyr::mutate(erros = dplyr::case_when(
          stringr::str_detect(variable, X[1]) ~ X[1],
          stringr::str_detect(variable, X[2]) ~ X[2],
          TRUE ~ as.character(variable)
        ),
        Variavel = purrr::map_chr(variable, function(x) stringr::str_split(x, 'xx_')[[1]][2])
        ) %>% dplyr::mutate(
          Variavel = gsub(Variavel,pattern = '.e.',replacement =' e ')
        ) %>%
        dplyr::left_join(regra %>% dplyr::select(Variavel,Regra) , by = 'Variavel')
      if(indicador != 'incon'){
        Dados_TabelaImplau <-  Dados_TabelaImplau  %>%
          dplyr::filter(erros %in% input$Exib_Dados2)
      }


      columns <- unique(c("SG_UF_NOT", "MUNICIPIO",variaveis_tab, "Regra"))
      Dados_TabelaImplau <- Dados_TabelaImplau %>%
        dplyr::filter(Variavel %in% columns) %>%
        dplyr::arrange(SG_UF_NOT )
      if(indicador == 'incon'){
        aux <- input$var_extra
        Dados_TabelaImplau <- Dados_TabelaImplau[, c("SG_UF_NOT", "MUNICIPIO",aux,"Regra") %>% unique()]
      }else{
        Dados_TabelaImplau <- Dados_TabelaImplau[, columns]
      }
      reactable::reactable(Dados_TabelaImplau, groupBy = c("Regra", "SG_UF_NOT"),
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
})}

## To be copied in the UI
# mod_SIVEP_incompletude_ui("SIVEP_incompletude_1")

## To be copied in the server
# mod_SIVEP_incompletude_server("SIVEP_incompletude_1")
