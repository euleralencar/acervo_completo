# Fonte para fazer previsão ao final de 30 dias
# https://ggplot2.tidyverse.org/reference/lims.html
options(tibble.print_max = 50)
acervo %>% 
  filter(link %in% c('Ext-1626', 'Ext-1664')) %>% 
  group_by(data, relator_atual, link) %>% 
  summarise(n = n()) %>% 
  pivot_wider(
    names_from = 'link',
    values_from = 'n'
  ) %>% 
  knitr::kable()

# Processo Ext-1626 está com relatoria do Min. Barroso desde 21/02/20. 
# No dia 02/07/21 houve decisão de colocá-lo como segredo de justiça.

# Processo Ext-1664 (processo público) está com relatoria do Min. Barroso 
# desde 26/03/21, distribuído por prevenção. No dia 22/06/21 houve vista 
# a PGR e no dia 10/08 houve recebimento dos autos.

# Quantidade de processos com NA (grafico)
acervo %>% 
  filter(is.na(relator_atual)) %>% 
  group_by(data, ano_de_data_autuacao) %>% 
  summarise(n = n()) %>% 
  rename(ano_aut = ano_de_data_autuacao) %>%
  mutate(ano_aut = as.factor(ano_aut)) %>% 
  #   names_from = ano_aut,
  #   values_from = n
  # )
  ggplot(aes(x = data, y = n, fill = ano_aut)) +
  geom_bar(stat='identity')


# Quantidade de processos com Ministro Presidente (grafico)
acervo %>% 
  filter(relator_atual == 'MINISTRO PRESIDENTE') %>%
  group_by(data, ano_de_data_autuacao) %>% 
  summarise(n = n()) %>% 
  rename(ano_aut = ano_de_data_autuacao) %>%
  mutate(ano_aut = as.factor(ano_aut)) %>% 
  #   names_from = ano_aut,
  #   values_from = n
  # )
  ggplot(aes(x = data, y = n, fill = ano_aut)) +
  geom_bar(stat='identity')

  
# Quantidade de processos com Ministro Presidente (grafico)
acervo %>% 
  filter(relator_atual == 'MINISTRO PRESIDENTE') %>%
  mutate(
    classe2 = case_when(
      classe == "ARE" ~ "ARE",
      classe == "RE" ~ "RE",
      classe == "AI" ~ "AI",
      TRUE ~ "Outros",
    )
  ) %>% 
  group_by(data, classe2) %>% 
  summarise(n = n()) %>% 
  #rename(ano_aut = ano_de_data_autuacao) %>%
  #mutate(ano_aut = as.factor(ano_aut)) %>% 
  #   names_from = ano_aut,
  #   values_from = n
  # )
  ggplot(aes(x = data, y = n, fill = classe2)) +
  geom_bar(stat='identity')


# Acervo atual
acervo %>% 
  filter(data == '2021-08-23') %>% 
  group_by(relator_atual) %>% 
  summarise(n = n()) %>% 
  mutate(relator_atual = 
            ifelse(is.na(relator_atual),'SEM RELATORIA', relator_atual),
         relator_atual = ifelse(n < 30, 'OUTROS', relator_atual)) %>% 
  group_by(relator_atual) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n))

  with(sum(n < 30))

