library(tidyverse)

acervo <- readRDS(file ='01_data/01_import_acervo.rds')

# Acervo total
acervo %>% 
  group_by(data) %>% 
  summarise(n = n())

# Acervo do Ministro Nunes Marques
acervo %>% 
  filter(relator_atual == "MIN. NUNES MARQUES") %>% 
  group_by(data) %>% 
  summarise(n = n())

# Acervo por classe
acervo_classe <-
  acervo %>% 
    filter(relator_atual == "MIN. NUNES MARQUES") %>% 
    group_by(data, classe) %>% 
    summarise(n = n()) %>% 
    tidyr::pivot_wider(
      names_from = data,
      values_from = n
    )

names(acervo_classe) <- c('classe', 'julho', 'agosto', 'setembro', 'outubro', 'novembro')
