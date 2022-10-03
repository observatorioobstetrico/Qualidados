# Qualidados em Modulos
Qualidados com Modulos e Golem para melhor otimização do código.
Segue o modelo de armazenamento do conjunto de arquivos.
## Estrutura do repositório:
    
    
    ├─Repositório
      ├─── R 
         └── app_config.R
         └── app_server.R
         └── app_UI.R
         └── mod_SIVEP_incompletude.R
         └── mod_SIVEP_fct_importacao.R
         └── run_app.R
         └── utils_helpers.R
      ├─── dev
      ├─── inst
      ├─── man 
      ├─── data1

## \R
Aqui estão presentes todos os arquivos .R alocados com as funções necessárias para funcionamento do ShinyApp. Subdividindo para melhor eficiência de trabalho em Módulos que fazem chamadas a submódulos. Além dos próprios arquivos principais referentes ao server e UI.
### mod_SIVEP_incompletude.R
Aqui está presente o modulo que representa a aba de incompletude bem como uma replicação para de implausibilidade, usando o argumento **indicador** para trocar de um indicador para outro. Onde no próprio código fará as alterações necessárias de um para outro.
#### Parte de incompletude:
-
-
-
-

#### Parte de implausibilidade:
-
-
-
-
#### Parte de inconsistência:
--
-
-
-
-
### mod_SIVEP_fct_importação.R
Utilizado de forma auxiliar para importação dos bancos de dados necessários, bem como as alterações necessárias em cada um deles e variáveis auxiliáres, como nome dos municípios.
### utils_helpers.R
Funções menores auxiliáres utilizadas nos módulos
### app_server.R
Server principal onde são feitas as linkagens com os módulos
### app_ui.R
Bem como o server.R, esse arquivo tem como função a linkagem da parte do usuário dos módulos bem como padronização do _dashboard_ ao que se refere a _header_ e _sidebar_.
## \dev
## \inst
## \man
## \data1
Armazenamento dos bancos de dados utilizados na produção do dash
