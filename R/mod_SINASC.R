#' mod_SINASC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SINASC_ui <- function(id, tabname, indicador, descricao, vars,selecionadas,estados,SIM= FALSE){
  ns <- NS(id)
    shinyjs::useShinyjs()
    shinydashboard::tabItem(tabName = tabname,
            #DESCRICAO
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Descri\u00e7\u00e3o",
                status = "primary",
                solidHeader = FALSE,
                shiny::span(descricao,
                     style = "color:black")
                )
              ),
            #CAIXA COM FILTROS E GRAFICOS
            shiny::fluidRow(
              #FILTROS
              shinydashboard::box(collapsible = TRUE,
                          width = 2,
                          title = "Campos",
                          status = "primary",
                          solidHeader = FALSE,
                          #VARIAVEIS DISPONIVEIS PARA PLOTAGEM
                          shinyWidgets::pickerInput(
                            inputId = ns("vars_select"),
                            label = "Variaveis",
                            choices = vars,
                            selected =selecionadas,
                            options = list(`actions-box` = TRUE),
                            multiple = T),
                          #TEMPO
                          shiny::sliderInput(
                            ns('filtro_tempo'),
                            'Selecione a janela de tempo:',
                            min = 1996,
                            max = 2022,
                            value = c(2000,2018),
                            round = T,
                            sep=''),
                          #LOCALIDADE
                          shiny::selectInput(
                            ns("filtro_loc"),
                            "Dados por localidade:",
                            c("Brasil" = 'br',
                              "Estado" = 'est',
                              "Municipio" = 'muni'
                            ), selected = 'br'),
                          #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE SE POR ESTADO
                          shiny::conditionalPanel(
                            condition = sprintf("input['%s'] != 'br'",ns("filtro_loc")),
                            shiny::selectInput(
                              ns("filtro_loc_est"),
                              "Selecione o estado",
                              choices = estados,
                              selected = estados[1])),
                          #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE POR MUNICIPIO
                          shiny::conditionalPanel(
                            condition = sprintf("input['%s'] == 'muni'",ns("filtro_loc")),
                            shiny::selectInput(
                              ns("filtro_loc_muni"),
                              "Selecione o munic\u00edpio",
                              choices = ('municipios'))),
                          shiny::selectInput(
                            ns('filtro_compara'),
                            "Fazer compara\u00e7\u00e3o?",
                            c(
                              "N\u00e3o" = "br",
                              "Sim, com estado" = "est",
                              "Sim, com munic\u00edpio" = "muni"
                            ),
                            selected = c("br")
                          ),
                          tippy::tippy_this(
                            elementId = ns("filtro_compara"),
                            tooltip = "Dispon\u00edvel apenas para visualiza\u00e7\u00e3o gr\u00e1fica.",
                            placement = "right"
                          ),
                          shiny::conditionalPanel(
                            condition = sprintf("input['%s'] != 'br'",ns("filtro_compara")),
                            shiny::selectInput(
                              ns("compara_est"),
                              "Estado de compara\u00e7\u00e3o",
                              choices = estados,
                              selected = estados[1]
                            )),
                          shiny::conditionalPanel(
                            condition = sprintf("input['%s'] == 'muni'",ns("filtro_compara")),
                            shiny::selectInput(
                              ns("compara_muni"),
                              "Munic\u00edpio de compara\u00e7\u00e3o",
                              ('municipios')))
                  ),
              #VISUALIZACAO
              shinydashboard::box(title = 'Visualiza\u00e7\u00e3o',
                                  status = 'primary',
                                  width = 10,
                                  shiny::div(shiny::tabsetPanel(
                                    shiny::tabPanel("Gr\u00e1ficos",
                                                    plotly::plotlyOutput(ns("Grafico"), height = 'auto')),
                                    shiny::tabPanel("Tabelas",
                                                    shinydashboard::tabBox(
                                                      width = 25,
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',1)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',2)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',3)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',4)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',5)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',6)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',7)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',8)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',9)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',10)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',11)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',12)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',13)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',14)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',15)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',16)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',17)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',18)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',19)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',20)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',21)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',22)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',23)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',24)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',25)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',26)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',27)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',28)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',29)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',30)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',31)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',32)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',33)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',34)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',35)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',36)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',37)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',38)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',39)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',40)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',41)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',42)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',43)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',44)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',45)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',46)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',47)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',48)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',49)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',50)))),
                                                      shiny::tabPanel(shiny::htmlOutput(ns(paste0('print',51))))
                                                    ))
                                  )

            ))))

}

#' mod_SINASC Server Functions
#'
#' @noRd
mod_SINASC_server <- function(id,indicador,SIM = FALSE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
#BANCO DE DADOS
  dado_aba <- escolher_banco_dados(SIM , indicador)

#FILTRO DE TEMPO E VARIAVEL PARA PODER ATUALIZAR O FILTRO DE ESTADO
  data_inicio <- shiny::reactive({
      var_value <- input$vars_select

      if(SIM == F & indicador =='incon')var_value <- names(var_incon_sinasc[var_incon_sinasc == var_value])
      # Filtrando a variável e período de tempo
      dados <- dado_aba %>%
        dplyr::filter(VARIAVEL %in% var_value) %>%
        dplyr::filter(ANO >= input$filtro_tempo[1] & ANO <= input$filtro_tempo[2])
      dados
      })

#ATUALIZACAO DO FILTRO DE ESTADO E MUNICIPIO
  shiny::observe({
    # Obtem os valores dos inputs
    x <- input$filtro_loc_est
    y <- input$compara_est

    # Carrega o dataframe
    dado <- data_inicio()

    # Filtra as observações pelos estados selecionados e obtém os municípios únicos
    muni <- unique(dado$CODMUNNASC[dado$ESTADO == x])
    muni_comp <- unique(dado$CODMUNNASC[dado$ESTADO == y])

    # Ordena os municípios alfabeticamente
    muni <- sort(muni)
    muni_comp <- sort(muni_comp)

    # Atualiza os valores dos selectInputs
    shiny::updateSelectInput(session, "filtro_loc_muni", choices = muni, selected = muni[1])
    shiny::updateSelectInput(session, "compara_muni", choices = muni_comp, selected = muni_comp[1])
  })


# FILTRAR O BANCO DE DADOS POR MUNICIPIO OU ESTADO
  data_filtrada <- reactive({
      dado <- data_inicio()
      # Filtrando por localidade
      if(input$filtro_loc == 'est'){
        dados_filtrados <- dado[dado$ESTADO %in% input$filtro_loc_est,]
        dados_filtrados$LOCALIDADE <- input$filtro_loc_est
      } else if(input$filtro_loc == 'muni'){
        dados_filtrados <- dado[dado$CODMUNNASC == input$filtro_loc_muni,]
        dados_filtrados$LOCALIDADE <- input$filtro_loc_muni
      } else {
        dados_filtrados <- dado
        dados_filtrados$LOCALIDADE <- 'BR'
      }

      # Agrupando e sumarizando dados
      dados_filtrados <- plyr::ddply(dados_filtrados, c("ANO", "LOCALIDADE", "VARIAVEL"), plyr::numcolwise(sum))
      dados_filtrados$compara <- 1

      # Adicionando dados para comparação
      if(input$filtro_compara != 'br'){
        if(input$filtro_compara == 'muni'){
          dados_compara <- dado[dado$CODMUNNASC == input$compara_muni,]
          dados_compara$LOCALIDADE <- input$compara_muni
        } else {

          dados_compara <- dado[dado$ESTADO == input$compara_est,]
          dados_compara$LOCALIDADE <- input$compara_est
        }

        dados_compara <- plyr::ddply(dados_compara, c("ANO", "LOCALIDADE", "VARIAVEL"), plyr::numcolwise(sum))
        dados_compara$compara <- 0

        dados_filtrados <- rbind(dados_compara, dados_filtrados)
      }
      dados_filtrados$VARIAVEL %>% unique()
      dados_filtrados
    })


  output$Grafico <- plotly::renderPlotly({
    dados <- data_filtrada()
    dados$ANO <- dados$ANO %>%  as.factor()
    # Definindo a altura do gráfico
    h_plot <- input$vars_select %>% unique() %>% length()
    h_plot <- h_plot * 200

    # Definindo o valor da variável de acordo com o indicador selecionado
    if(indicador == 'incom') {
      dados$value <- dados$NULOS + dados$IGNORADOS
      dados$value <- round((dados$value/dados$TOTAIS)*100,2)
      leg <- 'Incompletude'
    }
    if(indicador == 'implau') {
      dados$value <- round((dados$IMPLAUSIVEIS/dados$TOTAIS)*100,2)
      leg <- 'Implausibilidade'
    }
    if(indicador == 'incon') {
      dados$value <- round((dados$INCONSISTENTES/dados$TOTAIS)*100,2)
      leg <- 'Inconsist\u00eancia'
    }

    # Criando o gráfico com ggplot2
    if(indicador == 'incon' && SIM == F) {
      dados$VARIAVEL <- dados$VARIAVEL |> gsub(pattern = '_INCONSISTENTE',replacement = '')
      dados$VARIAVEL <- dados$VARIAVEL |> gsub(pattern = '_e_',replacement = ' e ')
      g <- ggplot2::ggplot(data = dados,
                           ggplot2::aes(y = value, x = ANO, fill = LOCALIDADE)) +
        ggplot2::geom_bar(position = "dodge", stat = "identity") +
        ggplot2::facet_grid(rows = ggplot2::vars(VARIAVEL))
    } else {
      g <- ggplot2::ggplot(data = dados,
                           ggplot2::aes(y = value, x = ANO, fill = LOCALIDADE)) +
        ggplot2::geom_bar(position = "dodge", stat = "identity") +
        ggplot2::facet_grid(rows = ggplot2::vars(VARIAVEL))
    }

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
  if(indicador == 'incom') {
    data_filtro_tab <- reactive({
      # filtra os dados de acordo com a seleção do usuário
      dados <- data_filtrada()
      dados <- dados[dados$compara == 1,c('VARIAVEL','NULOS','IGNORADOS','TOTAIS')]
      # adiciona coluna com quantidade de valores válidos
      dados$VALIDOS <- dados$TOTAIS - (dados$NULOS + dados$IGNORADOS)
      # seleciona as colunas de interesse e agrupa por variável
      dados <- dados[,c('VARIAVEL','NULOS','IGNORADOS','VALIDOS','TOTAIS')]
      dados <- dados %>%
        plyr::ddply('VARIAVEL',plyr::numcolwise(sum))
      # transpõe a tabela
      teste <- as.data.frame(t(dados))
      # renomeia a coluna 'newColName' para 'DADOS'
      colnames(teste) <- teste[1,]
      teste
    })
    if (SIM == FALSE) {
      vars_reais <- vars_incom_sinasc
    } else {
      vars_reais <- vars_incom_sim
    }
    # cria uma lista de outputs
    for(i in 1:72) {
      local({
        my_i <- i
        output[[paste('print',i,sep='')]] <- shiny::renderText({
          # verifica se a variável atual está selecionada pelo usuário
          if (vars_reais[my_i] %in% input$vars_select) {
            # filtra a tabela de dados
            dados <- data_filtro_tab()
            dados <- as.data.frame(dados[-1,vars_reais[my_i]])
            # adiciona coluna com a porcentagem de valores faltantes
            rownames(dados) <- c('Dados em branco','Dados ignorados','Dados v\u00e1lidos','Total')
            dados <- dados %>% dplyr::rename(
                                  n = `dados[-1, vars_reais[my_i]]`
                                ) %>% dplyr::mutate(
                                         '%' = round(100*as.numeric(n)/max(as.numeric(n)),2)
                                       )
            # gera tabela formatada
            kableExtra::kable(dados,
                              caption = paste0('Valores faltantes para ',vars_reais[my_i]),
                              digits  = 2) %>%
              kableExtra::kable_styling()
          }
        })
      })
    }
  }

  if(indicador == 'implau'){
        data_filtro_tab <- reactive({
            # filtra os dados de acordo com a seleção do usuário
            dados <- data_filtrada()
            dados <- dados[dados$compara == 1,c('VARIAVEL','IMPLAUSIVEIS','TOTAIS')]
            # adiciona coluna com quantidade de valores válidos
            dados$VALIDOS <- dados$TOTAIS - (dados$IMPLAUSIVEIS)
            # seleciona as colunas de interesse e agrupa por variável
            dados <- dados[,c('VARIAVEL','IMPLAUSIVEIS','VALIDOS','TOTAIS')]
            dados <- dados %>%
              plyr::ddply('VARIAVEL',plyr::numcolwise(sum))
            # transpõe a tabela
            teste <- as.data.frame(t(dados))
            # renomeia a coluna 'newColName' para 'DADOS'
            colnames(teste) <- teste[1,]
            teste
        })

        if (SIM == FALSE) {
          vars_reais <- vars_implau_sinasc
        } else {
          vars_reais <- vars_implau_sim
        }
        for(i in 1:72) {
          local({
            my_i <- i
            output[[paste('print',i,sep='')]]  <- shiny::renderText({
              # verifica se a variável atual está selecionada pelo usuário
              if (vars_reais[my_i] %in% input$vars_select) {
                # filtra a tabela de dados
                dados <- data_filtro_tab()
                dados <- as.data.frame(dados[-1,vars_reais[my_i]])
                # adiciona coluna com a porcentagem de valores faltantes
                rownames(dados) <- c('Dados Implausiveis','Dados v\u00e1lidos','Total')
                dados <- dados %>% dplyr::rename(
                  n = `dados[-1, vars_reais[my_i]]`
                ) %>% dplyr::mutate(
                  '%' = round(100*as.numeric(n)/max(as.numeric(n)),2)
                )
                # gera tabela formatada
                kableExtra::kable(dados,
                                  caption = paste0('Valores implausiveis para ',vars_reais[my_i]),
                                  digits  = 2) %>%
                  kableExtra::kable_styling()
              }
            })
          })
        }

      }

  if(indicador == 'incon'){
    data_filtro_tab <- reactive({
      # filtra os dados de acordo com a seleção do usuário
      dados <- data_filtrada()
      dados <- dados[dados$compara == 1,c('VARIAVEL','INCONSISTENTES','TOTAIS')]
      # adiciona coluna com quantidade de valores válidos
      dados$VALIDOS <- dados$TOTAIS - (dados$INCONSISTENTES)
      # seleciona as colunas de interesse e agrupa por variável
      dados <- dados[,c('VARIAVEL','INCONSISTENTES','VALIDOS','TOTAIS')]
      dados <- dados %>%
        plyr::ddply('VARIAVEL',plyr::numcolwise(sum))
      # transpõe a tabela
      teste <- as.data.frame(t(dados))
      # renomeia a coluna 'newColName' para 'DADOS'
      colnames(teste) <- teste[1,]
      teste
    })
    for(i in 1:72) {
      local({
        my_i <- i
        output[[paste('print',i,sep='')]]  <- shiny::renderText({
          if (SIM == FALSE) {
            vars_reais <- var_incon_sinasc
          } else {
            vars_reais <- vars_incon_sim
          }
          # verifica se a variável atual está selecionada pelo usuário
          if (vars_reais[my_i] %in% input$vars_select) {
            # filtra a tabela de dados
            dados <- data_filtro_tab()
            dados <- as.data.frame(dados[-1,vars_reais[my_i]])
            # adiciona coluna com a porcentagem de valores faltantes
            rownames(dados) <- c('Dados Inconsistentes','Dados v\u00e1lidos','Total')
            dados <- dados %>% dplyr::rename(
              n = `dados[-1, vars_reais[my_i]]`
            ) %>% dplyr::mutate(
              '%' = round(100*as.numeric(n)/max(as.numeric(n)),2)
            )
            # gera tabela formatada
            kableExtra::kable(dados,
                              caption = paste0('Valores inconsistentes para ',vars_reais[my_i]),
                              digits  = 2) %>%
              kableExtra::kable_styling()
          }
        })
      })
    }

  }

})}


## To be copied in the UI
# mod_SINASC_ui("mod_SINASC_1")

## To be copied in the server
# mod_SINASC_server("mod_SINASC_1")
