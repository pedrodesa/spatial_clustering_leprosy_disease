# RUN COMMAND
# Rscript ./src/convert.r

setwd('~/Documentos/Projetos/db_sinan')


# Instala os pacotes necessários, se ainda não estiverem instalados
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("read.dbc", quietly = TRUE)) install.packages("read.dbc")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# Carrega as bibliotecas
library(readr)
library(read.dbc)
library(dplyr)

# Lista todos os arquivos com a extensão .dbc no diretório atual
files <- list.files(path = './data', pattern = '\\.dbc$', full.names = TRUE)

# Verifica se a pasta de saída existe, caso contrário, cria a pasta
output_dir <- './data/output'
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Lê cada arquivo .dbc e os combina em uma única tabela
tbl <- lapply(files, read.dbc) |>
  bind_rows()

# Verifica se a tabela foi criada com sucesso
if (!is.null(tbl) && nrow(tbl) > 0) {
  # Escreve a tabela combinada em um arquivo CSV com delimitador ';'
  write_delim(tbl, file.path(output_dir, 'hans.csv'), delim = ';')
} else {
  message("Nenhum dado foi lido ou a tabela está vazia.")
}