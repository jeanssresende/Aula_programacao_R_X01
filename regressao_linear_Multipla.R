# Módulo: Análise de Dados com R
# Aula: Regressão Linear Múltipla
#-------------------------------------------------------------------------------

# Carregar o pacote tidyverse
library(tidyverse)

# O conjunto de dados mtcars:
# mpg: Milhas por galão (variável de resposta)
# hp: Potência do motor (horsepower)
# wt: Peso do carro
# drat: Relação de engrenagem (gear ratio)

# Vamos começar com um modelo simples (revisão)
modelo_simples <- lm(mpg ~ hp, data = mtcars)
summary(modelo_simples)

# --- Agora, vamos construir um modelo múltiplo ---

# Pergunta: O consumo de um carro (mpg) pode ser explicado pela sua potência (hp)
# e pelo seu peso (wt)?

# Passo 1: Visualizar a relação entre as variáveis
# Mantenha sempre a visualização como primeiro passo!
ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
# As relações parecem ser lineares e negativas.

# Passo 2: Construir o modelo de regressão linear múltipla
modelo_multiplo <- lm(mpg ~ hp + wt, data = mtcars)

# Passo 3: Analisar os resultados do modelo
summary(modelo_multiplo)

# Interpretação do `summary`:
# - Coeficientes:
#   - hp: O consumo (mpg) diminui em 0.032 para cada aumento de 1 hp,
#     MANTENDO o peso (wt) constante.
#   - wt: O consumo (mpg) diminui em 3.87 para cada aumento de 1000 lbs,
#     MANTENDO a potência (hp) constante.
#     (É importante ser preciso na interpretação, a unidade de `wt` é 1000 lbs).
# - R^2 Ajustado: Agora é 0.814, o que significa que 81.4% da variabilidade
#   de `mpg` é explicada pelo modelo com `hp` e `wt`. Um valor muito bom!

# Passo 4: Diagnóstico do Modelo (verificar as premissas)
# Use a função `plot()` para gerar os gráficos de diagnóstico
plot(modelo_multiplo)
# Peça para o aluno analisar cada um dos 4 gráficos:
# 1. Resíduos vs. Valores Ajustados: Buscamos por um "nuvem" aleatória de pontos.
# 2. Q-Q Normal: Os pontos devem seguir a linha diagonal.
# 3. Scale-Location (sqrt(|Resíduos Padronizados|) vs. Valores Ajustados): Outra
#    forma de checar a homogeneidade da variância. Buscamos por uma linha horizontal.
# 4. Resíduos vs. Alavancagem: Identifica pontos influentes ou outliers.

# Extra: Checando multicolinearidade com o VIF
# install.packages("car") # se ainda não estiver instalado
library(car)
vif(modelo_multiplo)
# O VIF de `hp` e `wt` está bem abaixo de 5, então não há problema de multicolinearidade.

# --- Pergunta: E se adicionarmos mais uma variável? `drat` (relação de engrenagem) ---
modelo_com_drat <- lm(mpg ~ hp + wt + drat, data = mtcars)
summary(modelo_com_drat)

# Pergunta para os alunos: O que aconteceu com o R^2 Ajustado? Ele melhorou?
# Isso nos leva à discussão sobre a importância de escolher as variáveis certas.