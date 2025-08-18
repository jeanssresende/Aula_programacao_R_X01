#-------------------------------------------------------------------------------
# Comparando Múltiplos Grupos
# Tópicos: ANOVA, Kruskal-Wallis e Testes Post-Hoc
#-------------------------------------------------------------------------------

# Carregar pacotes que usaremos
library(tidyverse)
# install.packages("car") # Necessário para o Teste de Levene
library(car)
# install.packages("dunn.test") # Necessário para o Teste de Dunn
library(dunn.test)


# ---- 0. O Problema de Comparações Múltiplas ----
#
# Se temos 3 grupos (A, B, C) e fazemos 3 testes t (A-B, B-C, A-C),
# a chance de ter PELO MENOS UM falso positivo (erro tipo I) não é 5%!
# A probabilidade real é: 1 - (1 - 0.05)^3 = 0.1426, ou seja, ~14.3%!
# Isso é a "inflação do erro do tipo I". Por isso, usamos testes omnibus.
#-------------------------------------------------------------------------------


# ---- 1. Abordagem Paramétrica: ANOVA (Analysis of Variance) ----
#
# Quando usar: Para comparar as MÉDIAS de 3 ou mais grupos independentes.
# Premissas: Normalidade dos resíduos e Homogeneidade de Variâncias.
# Exemplo: O comprimento da sépala (`Sepal.Length`) difere entre as 3 espécies de `iris`?

data(iris)

# 1.1: Visualização e Checagem das Premissas
# Boxplot para visualizar as distribuições
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Comprimento da Sépala por Espécie")

# Premissa 1: Homogeneidade de Variâncias (os "tamanhos" das caixas são parecidos?)
# Teste de Levene: H0 = As variâncias são homogêneas (iguais).
leveneTest(Sepal.Length ~ Species, data = iris)
# p-valor = 0.002259  (< 0.05). Rejeitamos H0. As variâncias não são consideradas homogêneas. OK!

# Premissa 2: Normalidade dos Resíduos (faremos isso após criar o modelo)


# 1.2: Rodando a ANOVA
# Usamos a função aov() (analysis of variance)
modelo_aov <- aov(Sepal.Length ~ Species, data = iris)

# Para checar a normalidade dos resíduos do modelo:
residuos <- residuals(modelo_aov)
shapiro.test(residuos)
# p-valor = 0.2189 (> 0.05). NÃO rejeitamos H0. Os resíduos são normalmente distribuídos. OK!

# se as premissas foram atendidas, podemos olhar o resultado da ANOVA com confiança.
summary(modelo_aov)
# O resultado principal está na linha "Species".
# Pr(>F) ou p-valor é < 2e-16. É extremamente baixo (< 0.05).
# Conclusão da ANOVA: REJEITAMOS a hipótese nula de que todas as médias são iguais.
# Ou seja, existe uma diferença significativa na média do comprimento da sépala
# em PELO MENOS UM dos grupos.


# 1.3: Teste Post-Hoc (Pós-Teste) para ANOVA -> Tukey HSD
# A ANOVA nos deu o "sinal verde". Agora, qual grupo é diferente de qual?
TukeyHSD(modelo_aov)

# Como ler a tabela de saída:
# Ela compara todos os pares de espécies. A coluna "p adj" (p-valor ajustado) é a chave.
# versicolor-setosa: p adj = 0.000... -> Diferença significativa.
# virginica-setosa:  p adj = 0.000... -> Diferença significativa.
# virginica-versicolor: p adj = 0.000... -> Diferença significativa.
# Conclusão Final: Todas as três espécies têm médias de comprimento de sépala
# estatisticamente diferentes umas das outras.


# ---- 2. Abordagem Não Paramétrica: Teste de Kruskal-Wallis ----
#
# Quando usar: Para comparar as DISTRIBUIÇÕES/MEDIANAS de 3 ou mais grupos
# quando as premissas da ANOVA (normalidade/homogeneidade) não são atendidas.

# Criar dados simulados não-normais
set.seed(42)
dados_nao_normais <- data.frame(
  valor = c(rexp(20, 0.5), rexp(20, 0.2), rexp(20, 0.8)),
  grupo = rep(c("Tratamento A", "Tratamento B", "Controle"), each = 20)
)

# 2.1: Visualização e Checagem das Premissas
ggplot(dados_nao_normais, aes(x = grupo, y = valor, fill = grupo)) + geom_boxplot()

# Checando as premissas da ANOVA para justificar o uso do Kruskal-Wallis
leveneTest(valor ~ grupo, data = dados_nao_normais)
# p-valor (< 0.05). A premissa de homogeneidade de variâncias foi VIOLADA.
# Só isso já é motivo suficiente para usar o Kruskal-Wallis.

# 2.2: Rodando o Teste Kruskal-Wallis
kruskal.test(valor ~ grupo, data = dados_nao_normais)
# p-valor = (< 0.05).
# Conclusão: REJEITAMOS a hipótese nula. Existe uma diferença significativa na
# distribuição dos valores em pelo menos um dos grupos.


# 2.3: Teste Post-Hoc para Kruskal-Wallis -> Teste de Dunn
dunn.test(dados_nao_normais$valor, g = dados_nao_normais$grupo, method = "bonferroni")
# O método "bonferroni" é uma forma de ajustar o p-valor para múltiplas comparações.

# Como ler a saída:
# A tabela "Comparison" mostra os pares, e a coluna "P.adjusted" é o p-valor ajustado.
# Tratamento A - Controle: p.adj = 0.0216* -> Significativo.
# Tratamento B - Controle: p.adj = 0.0002* -> Significativo.
# Tratamento B - Tratamento A: p.adj = 0.0021* -> Significativo.
# Conclusão Final: Todos os grupos são estatisticamente diferentes entre si.





