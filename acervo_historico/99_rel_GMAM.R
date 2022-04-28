library(dplyr)
library(tidyverse)
library(ggplot2)

# Importar dados de acervo
acervo <- readRDS('01_data/analise_min_alexandre/01_import_acervo.rds')


# Tabela de acervo por dia
acervo %>% 
  group_by(data) %>% 
  summarise(n = n())



# Análise Min. Alexandre

# Diferença de processos entre dia 25 e 26
# Em tese são processos baixados
processos_baixados <-
  anti_join(acervo %>% 
              filter(relator_atual == 'MIN. ALEXANDRE DE MORAES',
                     data == '2021-08-25'),
            acervo %>% 
              filter(relator_atual == 'MIN. ALEXANDRE DE MORAES',
                     data == '2021-08-26'),
            by ='link') %>% 
  select(link, localizacao_atual)

# Verificando se os processos no dia 25 estão de fato no dia 26
# Como deu zerado, sinal que não
acervo %>% 
  filter(link %in% processos_baixados$link, data=='2021-08-26')

# Vamos verificar se os dados estão em baixados
library(readxl)
baixados_ago <- read_excel("01_data/analise_min_alexandre/baixados_ago_GMAM_27082021.xlsx", 
                           col_types = c("text", "numeric", "text", 
                                         "text", "date", "text", "text", "text"))
baixados_ago <- janitor::clean_names(baixados_ago)

baixados_ago %>% nrow()

baixados_ago <- 
  baixados_ago %>% 
  mutate(link = paste0(classe,"-",numero),
         data = stringr::str_sub(data_do_andamento,1,10))


# Quantidade de processos que deram match entre baixados
# e processos que saíram do acervo. Resultado: 90
inner_join(processos_baixados, 
           baixados_ago, 
           by="link") %>% 
  select(link, tipo) %>% 
  nrow()

# Processos que foram baixados, mas não foram encontrados na lista
# de baixados da internet
anti_join(processos_baixados, 
          baixados_ago, by="link")


# QUantidade de processos baixados no dia 25
baixados_ago %>% 
  filter(data == '2021-08-25') %>% 
  nrow()


# Quero ver a diferença de 2 processso
anti_join(
  #base do painel
  baixados_ago %>% 
    filter(data == '2021-08-25'),
  #processo fora do acervo no dia 26
  inner_join(processos_baixados, 
             baixados_ago, 
             by="link") %>% 
    select(link, tipo),
  by = 'link'
)






# a <-
# acervo %>% 
#   filter(data=='2021-08-25') %>% 
#   select(link, localizacao_atual) %>% 
#   sample_n(5)
# 
# acervo %>% 
#   filter(link %in% a$link, data=='2021-08-26')

# Para verificação
# anti_join(acervo %>% 
#             filter(relator_atual == 'MIN. ALEXANDRE DE MORAES',
#                    data == '2021-08-25'),
#           acervo %>% 
#             filter(relator_atual == 'MIN. ALEXANDRE DE MORAES',
#                    data == '2021-08-26'),
#           by ='link') %>% 
#   select(link) %>% 
#   sample_n(size = 5)
# 
# acervo %>% 
#   filter(link=='ARE-1312088', data=='2021-08-26') %>% 
#   select(relator_atual, localizacao_atual)
