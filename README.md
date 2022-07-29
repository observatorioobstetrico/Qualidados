# Qualidados em Modulos
Qualidados com Modulos e Golem para melhor otimização do código.
Segue o modelo de armazenamento do conjunto de arquivos.
## Estrutura do repositório:
    
    
    ├─Repositório
      ├─── R 
         └── app_config.R
         └── app_server.R
         └── app_UI.R
         └── mod_SIVEP.R
                ├── mod_SIVEP_incompletude.R
                ├── mod_SIVEP_iconsistencia.R
                ├── mod_SIVEP_implausibilidade.R
         └── run_app.R
      ├─── dev
      ├─── inst
      ├─── man      

## \R
Aqui estão presentes todos os arquivos .R alocados com as funções necessárias para funcionamento do ShinyApp. Subdividindo para melhor eficiência de trabalho em Módulos que fazem chamadas a submódulos. Além dos próprios arquivos principais referentes ao server e UI.
### mod_SIVEP.R
#### mod_SIVEP_incompletude.R
#### mod_SIVEP_iconsistencia.R
#### mod_SIVEP_implausibilidade.R
## \dev
## \inst
## \man
