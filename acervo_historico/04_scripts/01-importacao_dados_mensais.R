# Importação dos arquivos excel
library(dplyr)

# Lista dos acervos salvos no caminho do Onedrive ----------
# __________________________________________________________

# vou setar o caminho que quero mapear os arquivos em excel de interesse
caminho <- 'C:/Users/euler/OneDrive - stf.jus.br/01. acervo_diario'
# lista com os caminhos
arquivos <- list.files(caminho, full.names = TRUE, pattern = ".xlsx") 
head(arquivos)

a <- arquivos[1]
b <- arquivos[stringr::str_detect(arquivos, pattern = "lista_acervo_2021-..-01")]

arquivos <- c(a,b)

## Função de leitura

ler_base <- function(file) {
  # Remoção dos textos que são lixos para imputar a data de coleta do acervo
  path <- file %>% 
    stringr::str_remove("C:/Users/euler/OneDrive - stf.jus.br/01. acervo_diario/lista_acervo_") %>% 
    stringr::str_remove("\\.xlsx") %>% 
    stringr::str_remove('.{9}$')
  # Utilizaçao do texto do arquivo para referencia na base
  readxl::read_excel(file, skip=3, sheet='Lista Geral') %>% 
    mutate(
      data = path
    )
}

# Lendo a base

acervo <- purrr::map_dfr(arquivos, ler_base)
acervo <- janitor::clean_names(acervo)

# Retirando NA ----------------------------------------
# _____________________________________________________

n_antigo <- acervo %>% nrow(); n_antigo

acervo <- 
  acervo %>% 
  filter(!is.na(link))

n_novo <- acervo %>% nrow(); n_novo

# Transformando texto data em data
acervo <-
  acervo %>% 
  mutate(
    data = as.Date(data, '%Y-%m-%d')
  )

# Salvando base ----------------------------------------
# _____________________________________________________

saveRDS(acervo, file ='01_data/01_import_acervo.rds')

