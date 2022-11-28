#' mod_SINASC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SINASC_ui <- function(id, tabname, indicador, descricao, vars, municipios, estados){
  ns <- NS(id)
    library(shiny)
    library(shinydashboard)
    shinyjs::useShinyjs()
    tabItem(tabName = tabname,
            #DESCRICAO
            fluidRow(
              box(
                width = 12,
                title = "Descrição",
                status = "primary",
                solidHeader = FALSE,
                span(descricao,
                     style = "color:black")
                )
              ),
            #CAIXA COM FILTROS E GRAFICOS
            fluidRow(
              #FILTROS
              box(collapsible = TRUE,
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
                          sliderInput(
                            ns('filtro_tempo'),
                            'Selecione a janela de tempo:',
                            min = 1996,
                            max = 2020,
                            value = c(2000,2018),
                            round = T,
                            sep=''),
                          selectInput(
                            ns("filtro_loc"),
                            "Dados por localidade:",
                            c("Brasil" = 'br',
                              "Estado" = 'est',
                              "Municipio" = 'muni'
                            ), selected = 'br'),
                          #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE SE POR ESTADO
                          conditionalPanel(
                            condition = sprintf("input['%s'] != 'br'",ns("filtro_loc")),
                            selectInput(
                              ns("filtro_loc_est"),
                              "Selecione o estado",
                              choices = estados,
                              selected = estados[1])),
                          #PAINEL CONDICIONADO AO TIPO DE LOCALIDADE POR MUNICIPIO
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'muni'",ns("filtro_loc")),
                            selectInput(
                              ns("filtro_loc_muni"),
                              "Selecione o município",
                              choices = sort(unique(municipios)))),
                          selectInput(
                            ns('filtro_compara'),
                            "Fazer comparação?",
                            c(
                              "Não" = "br",
                              "Sim, com estado" = "est",
                              "Sim, com município" = "muni"
                            ),
                            selected = c("br")
                          ),
                          tippy::tippy_this(
                            elementId = ns("filtro_compara"),
                            tooltip = "Disponível apenas para visualização gráfica.",
                            placement = "right"
                          ),
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'est'",ns("filtro_compara")),
                            selectInput(
                              ns("compara_est"),
                              "Estado de comparação",
                              choices = estados,
                              selected = estados[1]
                            )),
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'muni'",ns("filtro_compara")),
                            selectInput(
                              ns("compara_muni"),
                              "Município de comparação",
                              c(unique(municipios))))
                  ),
              #VISUALIZACAO
              box(title = 'Visualização',
                  status = 'primary',
                  width = 10,
                  div(tabsetPanel(
                    tabPanel("Gráficos",
                             plotly::plotlyOutput(ns("Grafico"))),
                    tabPanel("Tabelas",
                             tabBox(
                               width = 24,
                                 tabPanel(htmlOutput(ns(paste0('print',1)))),
                               tabPanel(htmlOutput(ns(paste0('print',2)))),
                               tabPanel(htmlOutput(ns(paste0('print',3)))),
                               tabPanel(htmlOutput(ns(paste0('print',4)))),
                               tabPanel(htmlOutput(ns(paste0('print',5)))),
                               tabPanel(htmlOutput(ns(paste0('print',6)))),
                               tabPanel(htmlOutput(ns(paste0('print',7)))),
                               tabPanel(htmlOutput(ns(paste0('print',8)))),
                               tabPanel(htmlOutput(ns(paste0('print',9)))),
                               tabPanel(htmlOutput(ns(paste0('print',10)))),
                               tabPanel(htmlOutput(ns(paste0('print',11)))),
                               tabPanel(htmlOutput(ns(paste0('print',12)))),
                               tabPanel(htmlOutput(ns(paste0('print',13)))),
                               tabPanel(htmlOutput(ns(paste0('print',14)))),
                               tabPanel(htmlOutput(ns(paste0('print',15)))),
                               tabPanel(htmlOutput(ns(paste0('print',16)))),
                               tabPanel(htmlOutput(ns(paste0('print',17)))),
                               tabPanel(htmlOutput(ns(paste0('print',18)))),
                               tabPanel(htmlOutput(ns(paste0('print',19)))),
                               tabPanel(htmlOutput(ns(paste0('print',20)))),
                               tabPanel(htmlOutput(ns(paste0('print',21)))),
                               tabPanel(htmlOutput(ns(paste0('print',22)))),
                               tabPanel(htmlOutput(ns(paste0('print',23)))),
                               tabPanel(htmlOutput(ns(paste0('print',24)))),
                               tabPanel(htmlOutput(ns(paste0('print',25)))),
                               tabPanel(htmlOutput(ns(paste0('print',26)))),
                               tabPanel(htmlOutput(ns(paste0('print',27)))),
                               tabPanel(htmlOutput(ns(paste0('print',28)))),
                               tabPanel(htmlOutput(ns(paste0('print',29)))),
                               tabPanel(htmlOutput(ns(paste0('print',30)))),
                               tabPanel(htmlOutput(ns(paste0('print',31)))),
                               tabPanel(htmlOutput(ns(paste0('print',32)))),
                               tabPanel(htmlOutput(ns(paste0('print',33)))),
                               tabPanel(htmlOutput(ns(paste0('print',34)))),
                               tabPanel(htmlOutput(ns(paste0('print',35)))),
                               tabPanel(htmlOutput(ns(paste0('print',36)))),
                               tabPanel(htmlOutput(ns(paste0('print',37)))),
                               tabPanel(htmlOutput(ns(paste0('print',38)))),
                               tabPanel(htmlOutput(ns(paste0('print',39)))),
                               tabPanel(htmlOutput(ns(paste0('print',40)))),
                               tabPanel(htmlOutput(ns(paste0('print',41)))),
                               tabPanel(htmlOutput(ns(paste0('print',42)))),
                               tabPanel(htmlOutput(ns(paste0('print',43)))),
                               tabPanel(htmlOutput(ns(paste0('print',44)))),
                               tabPanel(htmlOutput(ns(paste0('print',45)))),
                               tabPanel(htmlOutput(ns(paste0('print',46)))),
                               tabPanel(htmlOutput(ns(paste0('print',47)))),
                               tabPanel(htmlOutput(ns(paste0('print',48)))),
                               tabPanel(htmlOutput(ns(paste0('print',49)))),
                               tabPanel(htmlOutput(ns(paste0('print',50)))),
                               tabPanel(htmlOutput(ns(paste0('print',51))))
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
    if(indicador == 'incom'){

      #GRAFICOS
      data_filtro <- reactive({

        var_value <- input$vars_select
        dados_inicio <- Sinasc_incom %>%
          dplyr::filter(VARIAVEL %in% var_value) %>%
          dplyr::filter(ANO >= input$filtro_tempo[1] & ANO <= input$filtro_tempo[2])
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
        h_plot <- input$vars_select %>% unique %>% length()
        h_plot <- h_plot * 125
        dados$value <- dados$NULOS + dados$IGNORADOS
        dados$value <- round((dados$value/dados$TOTAIS)*100,2)

        #FINALIZACAO COM GGPLOT -------------------------
        g <- ggplot(data = dados,
                    aes(y = value, x = ANO, fill = LOCALIDADE)) +
          geom_bar(position = "dodge", stat = "identity") +
          facet_grid(rows = vars(VARIAVEL))

        g <- g + labs(x = NULL) +
          labs(y = "Incompletude (%)") +
          scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
          scale_fill_viridis_d() +
          theme_bw() +
          theme(axis.text.x = element_text(
            face = "bold",
            color = "#000000",
            size = 9,
            angle = 45
          ))
         plotly::ggplotly(g, height = (h_plot + 125)) %>%
           layout(legend = list(orientation = "h", y = 20))

        })

      #TABELAS
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
        teste$DADOS <- c('Dados em branco','Dados ignorados','Dados válidos','Total')
        teste
      })

      for(i in 1:72){
        local({
          my_i <- i
          output[[paste('print',i,sep='')]] <- renderText({
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
      }
    }



  })
}


## To be copied in the UI
# mod_SINASC_ui("mod_SINASC_1")

## To be copied in the server
# mod_SINASC_server("mod_SINASC_1")
