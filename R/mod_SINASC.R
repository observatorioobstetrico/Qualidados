#' mod_SINASC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SINASC_ui <- function(id, tabname, indicador, descricao, vars,estados){
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
                            selected = vars[c(1,2)],
                            options = list(`actions-box` = TRUE),
                            multiple = T),
                          #LOCALIDADE
                          shiny::sliderInput(
                            ns('filtro_tempo'),
                            'Selecione a janela de tempo:',
                            min = 1996,
                            max = 2020,
                            value = c(2000,2018),
                            round = T,
                            sep=''),
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
                              choices = 'estados')),
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
                              choices = 'estados'
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
                             plotly::plotlyOutput(ns("Grafico"))),
                    shiny::tabPanel("Tabelas",
                             shinydashboard::tabBox(
                               width = 24,
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
                  ))
              ))
            )

}

#' mod_SINASC Server Functions
#'
#' @noRd
mod_SINASC_server <- function(id,indicador){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      #GRAFICOS
    #municipios
    data_inicio <- shiny::reactive({var_value <- input$vars_select
    if(indicador == 'incom')dado <- Sinasc_incom
    if(indicador == 'implau'){
      dado <- Sinasc_implau
      var_value <- paste0(var_value,'_IMPLAUSIVEL')
    }
    if(indicador == 'incon'){
      dado <- Sinasc_incon
      var_value <- var_incon_sinasc[var_incon_sinasc == var_value] |>
        names()
    }
    dado <- dado %>%
      dplyr::filter(VARIAVEL %in% var_value) %>%
      dplyr::filter(ANO >= input$filtro_tempo[1] & ANO <= input$filtro_tempo[2])
    shiny::updateSelectInput(session,("filtro_loc_est"),
                             choices = unique(dado$ESTADO),
                             selected = unique(dado$ESTADO)[1])

    shiny::updateSelectInput(session,("compara_est"),
                             choices = unique(dado$ESTADO),
                             selected = unique(dado$ESTADO)[1])
    dado
    })


    shiny::observe({
      x <- input$filtro_loc_est
      y <- input$compara_est
      dado <- data_inicio()
      muni <- dado[dado$ESTADO == x,'CODMUNNASC' ] %>%unlist()%>% as.vector()
      muni_comp <- dado[dado$ESTADO == y,'CODMUNNASC' ] %>%unlist()%>% as.vector()

      shiny::updateSelectInput(session,("filtro_loc_muni"),
                        choices = muni,
                        selected = muni[1])

      shiny::updateSelectInput(session,("compara_muni"),
                        choices = muni_comp,
                        selected = muni_comp[1])

    })
      data_filtro <- shiny::reactive({
        dados_inicio <- data_inicio()
        if(input$filtro_loc == 'est'){
          dados <- dados_inicio[dados_inicio$ESTADO == input$filtro_loc_est,]
          dados$LOCALIDADE <- input$filtro_loc_est
        } else if(input$filtro_loc == 'muni'){
          dados <- dados_inicio[dados_inicio$CODMUNNASC == input$filtro_loc_muni,]
          dados$LOCALIDADE <- input$filtro_loc_muni
        } else{
          dados <- dados_inicio
          dados$LOCALIDADE <- 'BR'
        }
        dados <- plyr::ddply(dados,c("ANO","LOCALIDADE","VARIAVEL"), plyr::numcolwise(sum))
        dados$compara <- 1
        if(input$filtro_compara != 'br'){
          dados2 <- dados_inicio
          if(input$filtro_compara == 'muni'){
            dados2 <- dados2 %>%
              dplyr::filter(CODMUNNASC == input$compara_muni)
            dados2$LOCALIDADE <- input$compara_muni
          } else{
            dados2 <- dados2 %>%
              dplyr::filter(ESTADO == input$compara_est)
            dados2$LOCALIDADE <- input$compara_est
          }
          dados2 <- plyr::ddply(dados2,c("ANO","LOCALIDADE","VARIAVEL"), plyr::numcolwise(sum))
          dados2$compara <- 0
          dados <- rbind(dados2,dados)
        }
        dados
      })

      output$Grafico <- plotly::renderPlotly({
        dados <- data_filtro()
        h_plot <- input$vars_select %>% unique() %>% length()
        h_plot <- h_plot * 125
        if(indicador=='incom'){
        dados$value <- dados$NULOS + dados$IGNORADOS
        dados$value <- round((dados$value/dados$TOTAIS)*100,2)
        leg <- 'Incompletude'
        }
        if(indicador == 'implau'){
        dados$value <- round((dados$IMPLAUSIVEIS/dados$TOTAIS)*100,2)
        leg <- 'Impalusibilidade'
        }
        if(indicador == 'incon'){
        dados$value <- round((dados$INCONSISTENTES/dados$TOTAIS)*100,2)
        leg <- 'Inconsist\u00eancia'
        }

        #FINALIZACAO COM GGPLOT -------------------------
        g <- ggplot2::ggplot(data = dados,
                             ggplot2::aes(y = value, x = ANO, fill = LOCALIDADE)) +
          ggplot2::geom_bar(position = "dodge", stat = "identity") +
          ggplot2::facet_grid(rows = ggplot2::vars(VARIAVEL))

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
         plotly::ggplotly(g, height = (h_plot + 125)) %>%
           plotly::layout(legend = list(orientation = "h", y = 20))

        })

      #TABELAS
      if(indicador=='incom'){
      data_filtro_tab <- reactive({
        dados <- data_filtro()
        dados <- dados[dados$compara == 1,c('VARIAVEL','NULOS','IGNORADOS','TOTAIS')]
        dados$VALIDOS <- dados$TOTAIS - (dados$NULOS + dados$IGNORADOS)
        dados <- dados[,c('VARIAVEL','NULOS','IGNORADOS','VALIDOS','TOTAIS')]
        dados <- dados %>%
          plyr::ddply('VARIAVEL',plyr::numcolwise(sum))
        teste <-as.data.frame(t(dados))
        names(teste) <- teste[1,]
        teste <- teste[-1,]
        teste <- cbind(newColName = rownames(teste), teste)
        colnames(teste)[1] <- 'DADOS'
        rownames(teste) <- 1:nrow(teste)
        teste$DADOS <- c('Dados em branco','Dados ignorados','Dados v\u00e1lidos','Total')
        teste
      })

      for(i in 1:72){
        local({
          my_i <- i
          output[[paste('print',i,sep='')]] <- shiny::renderText({
            if(vars_incom_sinasc[my_i] %in% input$vars_select){
              dados <- data_filtro_tab()
              dados <- dados[,c('DADOS',paste0(vars_incom_sinasc[my_i]))]
              dados[['%']] <- (100*as.numeric(
                dados[[paste0(vars_incom_sinasc[my_i])]]))/as.numeric(dados[4,2])
              colnames(dados) <- c('','n','%')
              kableExtra::kable(dados,
                                caption = paste0('Valores faltantes para ',vars_incom_sinasc[my_i]),
                                digits  = 2
              ) %>%
                kableExtra::kable_styling()}
        })})
      }}
      if(indicador == 'implau'){
        data_filtro_tab <- reactive({
          dados <- data_filtro()
          dados <- dados[dados$compara == 1,c('VARIAVEL','IMPLAUSIVEIS','TOTAIS')]
          dados$VALIDOS <- dados$TOTAIS - (dados$IMPLAUSIVEIS)
          dados <- dados[,c('VARIAVEL','IMPLAUSIVEIS','VALIDOS','TOTAIS')]
          dados <- dados %>%
            plyr::ddply('VARIAVEL',plyr::numcolwise(sum))
          teste <-as.data.frame(t(dados))
          names(teste) <- teste[1,]
          teste <- teste[-1,]
          teste <- cbind(newColName = rownames(teste), teste)
          colnames(teste)[1] <- 'DADOS'
          rownames(teste) <- 1:nrow(teste)
          teste$DADOS <- c('Dados implausiveis','Dados v\u00e1lidos','Total')
          names(teste)<-names(teste) %>% substr(1,nchar( names(teste))-12)
          names(teste)[1] <- 'DADOS'
          teste
        })
        for(i in 1:72){
          local({
            my_i <- i
            output[[paste('print',i,sep='')]] <- shiny::renderText({
              if(vars_implau_sinasc[my_i] %in% input$vars_select){
                dados <- data_filtro_tab()
                dados <- dados[,c('DADOS',paste0(vars_implau_sinasc[my_i]))]
                dados[['%']] <- (100*as.numeric(
                  dados[[paste0(vars_implau_sinasc[my_i])]]))/as.numeric(dados[4,2])
                colnames(dados) <- c('','n','%')
                kableExtra::kable(dados,
                                  caption = paste0('Valores implausiveis para ',vars_implau_sinasc[my_i]),
                                  digits  = 2
                ) %>%
                  kableExtra::kable_styling()}
            })})
        }

      }
      if(indicador == 'incon'){
        data_filtro_tab <- reactive({
          dados <- data_filtro()
          dados <- dados[dados$compara == 1,c('VARIAVEL','INCONSISTENTES','TOTAIS')]
          dados$VALIDOS <- dados$TOTAIS - (dados$INCONSISTENTES)
          dados <- dados[,c('VARIAVEL','INCONSISTENTES','VALIDOS','TOTAIS')]
          dados <- dados %>%
            plyr::ddply('VARIAVEL',plyr::numcolwise(sum))
          teste <-as.data.frame(t(dados))
          names(teste) <- teste[1,]
          names(teste) <- var_incon_sinasc[names(teste)] |> unname()
          teste <- cbind(newColName = rownames(teste), teste)
          teste <- teste[-1,]
          colnames(teste)[1] <- 'DADOS'
          rownames(teste) <- 1:nrow(teste)
          teste$DADOS <- c('Dados inconsist\u00eantes','Dados v\u00e1lidos','Total')
          teste
        })

        for(i in 1:72){
          local({
            my_i <- i
            output[[paste('print',i,sep='')]] <- shiny::renderText({
              if(var_incon_sinasc[my_i] %in% input$vars_select){
                dados <- data_filtro_tab()
                dados <- dados[,c('DADOS',(var_incon_sinasc[my_i]))]
                dados[['%']] <- (100*as.numeric(
                  dados[[paste0(var_incon_sinasc[my_i])]]))/as.numeric(dados[3,2])
                colnames(dados) <- c('','n','%')
                kableExtra::kable(dados,
                                  caption = paste0('Valores inconsist\u00eantes para ',var_incon_sinasc[my_i]),
                                  digits  = 2
                ) %>%
                  kableExtra::kable_styling()}
            })})
        }}

  })
}


## To be copied in the UI
# mod_SINASC_ui("mod_SINASC_1")

## To be copied in the server
# mod_SINASC_server("mod_SINASC_1")
