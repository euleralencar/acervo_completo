library(dplyr)
library(tidyverse)
library(ggplot2)

# Importar dados de acervo
acervo <- readRDS('01_data/01_import_acervo.rds')


# Tabela de acervo por dia
acervo %>% 
  group_by(data) %>% 
  summarise(n = n())

# acervo %>% 
#   group_by(relator_atual) %>% 
#   summarise(n = n()) %>% View()


# Identificando se há valores seguidos iguais (sinal de problema na base)
acervo %>% 
  group_by(data) %>% 
  summarise(n = n()) %>% 
    # Faço um novo group_by para verificar quais n se repetem
    group_by(n) %>% 
    summarise(qtd = n()) %>%
    # Filtro apenas os n maior ou igual a 2, pois são os que se repetem
      filter(qtd > 1) %>% 
      # Cruzo com a tabela originada no primeiro group_by pra pegar a data
      left_join(
        acervo %>% 
          group_by(data) %>% 
          summarise(n = n()),
        by = "n"
      ) %>% 
      relocate(data, .before=n) %>% 
      select(data, n)


# Gráfico do acervo por tempo
acervo %>% 
group_by(data) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_area(aes(x=data, y=n),stat = 'identity')+
  #geom_text(size = 1.5, aes(x=data, y=n, label=n), vjust = -1)
  ylim(0, 31000) +
  xlab('') + ylab('') +
  geom_point(aes(x=data, y=n))+
  ggtitle("Acervo nos últimos 30 dias") +
  theme_minimal()


# Quantidade de processos na presidência (gráfico)

acervo %>% 
  filter(relator_atual == 'MINISTRO PRESIDENTE') %>% 
  group_by(data) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_area(aes(x=data, y=n),stat = 'identity')+
  ylim(0, 7500) +
  xlab('') + ylab('') +
  geom_point(aes(x=data, y=n))+
  ggtitle("Comportamento dos processos sem relatoria") +
  theme_minimal()

# Quantidade de processos com NA (grafico)
acervo %>% 
  filter(is.na(relator_atual)) %>% 
  group_by(data) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_area(aes(x=data, y=n),stat = 'identity')+
  ylim(0, 3000) +
  xlab('') + ylab('') +
  geom_point(aes(x=data, y=n))+
  ggtitle("Comportamento dos processos sem relatoria") +
  theme_minimal()


# Processos sem relatoria e min. presidente
left_join(
  acervo %>% 
    filter(relator_atual == 'MINISTRO PRESIDENTE') %>% 
    group_by(data) %>% 
    summarise(presidente = n()),
  acervo %>% 
    filter(is.na(relator_atual)) %>% 
    group_by(data) %>% 
    summarise(sem_relator = n()),
  by = 'data'
) %>% 
  pivot_longer(
    cols = !data,
    names_to = 'tipo',
    values_to = 'acervo'
  ) %>% 
  ggplot()+
  geom_line(aes(x=data, y=acervo, color = tipo)) +
  ylim(0, 7500) +
  xlab('') + ylab('') +
  ggtitle("Comportamento dos processos sem relatoria e Min. Presidente") +
  theme_minimal()


# Fazer o Box-Plot


