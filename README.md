# Qualidados em Modulos
Qualidados com Modulos e Golem para melhor otimização do código.
Segue o modelo de armazenamento do conjunto de arquivos.
## O Painel
O objetivo do painel é apresentar qualidade dos dados para SIVEP-Gripe, SINASC e SIM, onde apresentamos os indicadores de qualidade: incompletude, inconsistência e implausibilidade, de maneira intuitiva e simples ao usuário. Com botões de ajuda e informações a respeito de todos os conjuntos de dados e variáveis disponíveis. O projeto se encontra atualmente em construção, mas está sendo constantemente atualizado via plataforma atual.
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
         └── fct_informacoes.R
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
-
-
-
-
-
### mod_SIVEP_fct_importação.R
Utilizado de forma auxiliar para importação dos bancos de dados necessários, bem como as alterações necessárias em cada um deles e variáveis auxiliáres.
### utils_helpers.R
Funções menores auxiliáres utilizadas nos módulos
### app_server.R
Server principal onde são feitas as linkagens com os módulos
### app_ui.R
Bem como o server.R, esse arquivo tem como função a linkagem da parte do usuário dos módulos bem como padronização do _dashboard_ ao que se refere a _header_ e _sidebar_.
### fct_informacoes
Contem informacoes importantes para a filtragem e análise, como nomes disponíveis para as variáveis em cada tipo de indicador, bem como nome dos estados disponiveis no banco de dado. Alem da devida descrição dos indicadores
## \dev
## \inst
## \man
## \data1
Armazenamento dos bancos de dados utilizados na produção do _dashboard_ . Usando arquivos .json para descrição das variáveis em estudo para cada um dos indicadores

# Ajustes a serem feitos e discutidos

## mod_SIVEP_incompletude

- Necessário alteração referente a nome de variáveis para generalização já que agora está sendo utilizado apenas 1 mod para todo SIVEP-Gripe
- Os filtros para casos finalizados na implausibilidade e na Inconsistencia (pode tirar)
- Discutir a respeito das legendas na aba de inconsistencia (tirar o filtro dado plausivel, e o filtro completo para aba tabelas de implausibilidade) 
- Arrumar um jeito de ajustar o tamanho dinamico dos graficos(talvez deixar independente ou usar outra funcao ao inves do facet_grid())
- arrumar um reactive table para os graficos de forma a consumir menos memoria
- construção da parte de dicionario de dados com base em um .MD
- Filtro para exibir dados em graficos
- Verificar banco de dados de inconsistencias, muitas variaveis sem inconsistencia
- Ver como vai ficar a parte de microdados
(colocar micro dados com um filtro de variaveis como coluna e inconsistencia como linha)
