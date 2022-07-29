# Qualidados em Modulos
Qualidados com Modulos e Golem para melhor otimização do código.
Segue o modelo de armazenamento do conjunto de arquivos.
## Estrutura do repositório:
    
    
    ├─Repositório
      ├─── R 
           └──(Todos arquivos .R referentes as funções do pacote Qualidados em Módulos)
         ├── app_config.R
         ├── app_server.R
              └──(Arquivo referente a parte de funcionamento interno ou "backend" do app)
         ├── app_UI.R
              └──(Arquivo referente a parte de visão do usuário ou "frontend" do app)
         ├── mod_SIVEP.R
            └──(Módulo principal com o painel do SIVEP que faz a chamada dos submodulos de cada um dos indicadores)
                ├── mod_SIVEP_incompletude.R
                    └──(Sub Módulo da aba do menu de indicador de incompletude)
                ├── mod_SIVEP_iconsistencia.R
                    └──(Sub Módulo da aba do menu de indicador de iconsistencia)
                ├── mod_SIVEP_implausibilidade.R
                    └──(Sub Módulo da aba do menu de indicador de implausibilidade)
         ├── run_app.R
      ├─── dev
      ├─── inst
      ├─── man      
