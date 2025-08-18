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

# ==============================================================================
# PARTE 2: FUNDAMENTOS DOS TESTES DE HIPÓTESE
# ==============================================================================

# A transição da visualização para a estatística
#
# - A visualização (boxplots, gráficos de linhas) nos ajudou a OBSERVAR uma diferença.
# - O teste estatístico nos ajudará a QUANTIFICAR a PROBABILIDADE de essa diferença
#   ter ocorrido por mero acaso.
#
# Hipótese Nula (H0):
#   - É a hipótese de "não diferença" ou "não efeito".
#   - Ex: A média das medidas nas amostras infectadas é igual à média das não infectadas.
#
# Hipótese Alternativa (Ha):
#   - É a hipótese que queremos provar.
#   - Ex: A média das medidas nas amostras infectadas é diferente da média das não infectadas.
#
# Valor P (p-value):
#   - É a probabilidade de obter os seus resultados (ou resultados mais extremos)
#     se a Hipótese Nula (H0) for, na verdade, verdadeira.
#   - Se o p-value é pequeno, é pouco provável que a H0 seja verdadeira. Rejeitamos H0.
#   - Se o p-value é grande, é provável que a H0 seja verdadeira. Não rejeitamos H0.
#
# Nível de Significância (Alpha, α):
#   - O ponto de corte para decidir se um p-value é "pequeno".
#   - O valor mais comum é 0.05.
#   - Se p <= 0.05: O resultado é estatisticamente significativo.
#   - Se p > 0.05: O resultado NÃO é estatisticamente significativo.

# ==============================================================================
# PARTE 3: TESTE t DE STUDENT - COMPARANDO DOIS GRUPOS
# ==============================================================================

# O Teste t é ideal para comparar as médias de EXATAMENTE DOIS grupos.
# Vamos aplicá-lo para comparar o status de infecção em cada dia separadamente.

# Usaremos o 't.test()' do R.
# A sintaxe 'Valor_Medida ~ Status' significa "compare a média de Valor_Medida
# em função do grupo Status".
# Usaremos 'var.equal = FALSE' para fazer o Teste t de Welch, que é mais robusto
# e não assume que as variâncias dos grupos são iguais.

# -- Teste t para o Dia 3 --
cat("\n--- RESULTADOS DO TESTE t PARA O DIA 3 ---\n")
teste_t_dia3 <- dados_longos %>%
  filter(Dia == 3) %>% # Filtramos apenas os dados do Dia 3
  t.test(Valor_Medida ~ Status, data = ., var.equal = FALSE)

print(teste_t_dia3)


# -- Teste t para o Dia 6 --
cat("\n--- RESULTADOS DO TESTE t PARA O DIA 6 ---\n")
teste_t_dia6 <- dados_longos %>%
  filter(Dia == 6) %>%
  t.test(Valor_Medida ~ Status, data = ., var.equal = FALSE)

print(teste_t_dia6)

# -- Teste t para o Dia 12 --
cat("\n--- RESULTADOS DO TESTE t PARA O DIA 12 ---\n")
teste_t_dia12 <- dados_longos %>%
  filter(Dia == 12) %>%
  t.test(Valor_Medida ~ Status, data = ., var.equal = FALSE)

print(teste_t_dia12)

# ==============================================================================
# PARTE 4: ANÁLISE DE VARIÂNCIA (ANOVA) - MÚLTIPLOS FATORES
# ==============================================================================

# O que o Teste t não faz:
# - Ele não compara o efeito de três ou mais grupos de uma vez (ex: Dia 3 vs Dia 6 vs Dia 12).
# - Ele não avalia o efeito de múltiplos fatores (ex: Status E Dia) e como eles interagem.
#
# Para isso, usamos a ANOVA (Análise de Variância).

# Modelo de ANOVA de Dois Fatores:
# Vamos testar o efeito do 'Status', do 'Dia' e, mais importante,
# da 'interação' entre 'Status' e 'Dia'.
# Uma interação significativa nos diria que o efeito da infecção não é
# o mesmo em todos os dias (exatamente o que queríamos investigar!).

# Usamos a sintaxe 'aov(resposta ~ fator1 * fator2)'
cat("\n--- RESULTADOS DA ANOVA DE DOIS FATORES ---\n")
modelo_anova <- aov(Valor_Medida ~ Status * factor(Dia), data = dados_longos)

# A função 'summary()' nos dá a tabela de resultados da ANOVA.
summary(modelo_anova)

# INTERPRETAÇÃO DA TABELA ANOVA:
# - Olhe para a coluna 'Pr(>F)' (p-value).
#
# 1. Linha 'Status':
#    - Testa o efeito principal do status de infecção, ignorando o tempo.
#    - Provavelmente, o p-value será > 0.05. Isso significa que, em média,
#      ao longo de todo o experimento, não houve um efeito geral significativo
#      da infecção.
#
# 2. Linha 'factor(Dia)':
#    - Testa o efeito principal do tempo, ignorando o status.
#    - Provavelmente, o p-value será > 0.05. Isso significa que, em média,
#      ao longo do tempo, o valor da medida não variou significativamente em geral.
#
# 3. Linha 'Status:factor(Dia)':
#    - Esta é a linha da INTERAÇÃO!
#    - Ela testa se o efeito da infecção no valor da medida muda ao longo dos dias.
#    - Provavelmente, o p-value será > 0.05.
#
# Conclusão da ANOVA:
# A ausência de uma interação significativa nos diz que o efeito do 'Status'
# é consistente ao longo dos dias. O que vimos visualmente (pequena diferença no Dia 6)
# não foi estatisticamente forte o suficiente para ser considerado uma interação.