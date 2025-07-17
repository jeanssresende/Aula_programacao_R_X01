#------------------------------------------------------------------------------#
#
#             Topico: Analise Estatistica: Teste de Hipotese #
#
#------------------------------------------------------------------------------#
# OBJETIVOS DESTA AULA:
#
# 1. Entender os fundamentos dos testes de hipótese (H0, Ha, p-value).
# 2. Aplicar o Teste t de Student para comparar dois grupos.
# 3. Aplicar a Análise de Variância (ANOVA) para analisar múltiplos fatores.
# 4. Interpretar as saídas do R para tirar conclusões biológicas.
#
# Este script é um "caderno de laboratório digital".
# Execute cada linha ou bloco de código e leia os comentários com atenção.
#------------------------------------------------------------------------------#

# ==============================================================================
# PARTE 1: CONFIGURAÇÃO E PREPARAÇÃO DOS DADOS
# ==============================================================================

# Carregando os pacotes necessários
# `tidyverse` para manipulação e organização de dados (inclui 'dplyr' e 'ggplot2').
# `readxl` para importar o seu arquivo .xlsx.
# Se ainda não tiver os pacotes instalados, remova o '#' da frente das linhas abaixo e execute-as.
# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)

# Nomes das colunas para seu conjunto de dados.
# É uma boa prática definir os nomes de forma clara e padronizada.
nomes_colunas <- c("Uninfected_Day3_Rep1", "Uninfected_Day3_Rep2",
                   "Uninfected_Day3_Rep3", "Infected_Day3_Rep1",
                   "Infected_Day3_Rep2", "Infected_Day3_Rep3",
                   "Uninfected_Day6_Rep1", "Uninfected_Day6_Rep2",
                   "Uninfected_Day6_Rep3", "Infected_Day6_Rep1",
                   "Infected_Day6_Rep2", "Infected_Day6_Rep3",
                   "Uninfected_Day12_Rep1", "Uninfected_Day12_Rep2",
                   "Uninfected_Day12_Rep3", "Infected_Day12_Rep1",
                   "Infected_Day12_Rep2", "Infected_Day12_Rep3")

# Importação dos dados (relembrando a aula anterior e corrigindo o erro)
# Usamos `read_excel` porque o arquivo original é um .xlsx.
# `skip = 1` ignora a primeira linha descritiva.
# `col_names = FALSE` impede que o R use a segunda linha como cabeçalho.
dados_brutos_temp <- read_excel("Tabela_Jean.xlsx", col_names = FALSE, skip = 1)

# A última linha do seu arquivo contém texto de metadados. Vamos removê-la.
# `head(..., -1)` pega todas as linhas, exceto a última.
dados_brutos_limpos <- head(dados_brutos_temp, -1)

# Atribuindo os nomes de coluna que criamos
colnames(dados_brutos_limpos) <- nomes_colunas

# Convertendo todas as colunas para o tipo numérico (`<dbl>`)
# Isso é crucial para que o R possa fazer cálculos e análises.
dados_brutos_num <- dados_brutos_limpos %>%
  mutate(across(everything(), as.numeric))

# Dando uma "espiada" nos dados para confirmar que estão limpos e numéricos
glimpse(dados_brutos_num)

# Transformando os dados para o formato "longo" (tidy data)
# Este formato é ideal para análises e gráficos no R.
# Ele cria colunas para 'Status', 'Dia' e 'Valor_Medida'.
dados_longos <- dados_brutos_num %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Status", "Dia_Texto", "Replicata_Num"),
    names_sep = "_",
    values_to = "Valor_Medida"
  ) %>%
  mutate(
    Dia = as.numeric(str_replace(Dia_Texto, "Day", "")),
    Status = as.factor(Status)
  ) %>%
  select(-Dia_Texto, -Replicata_Num)

# Confirmando a estrutura do formato longo
glimpse(dados_longos)
