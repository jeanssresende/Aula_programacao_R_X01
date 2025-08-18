#-------------------------------------------------------------------------------
# Aplicação de Testes de Hipótese
# Correlação e Introdução à Regressão Linear
#-------------------------------------------------------------------------------

library(tidyverse)
data(mtcars)

# ---- 1. Teste de Correlação: Pearson vs. Spearman ----
#
# Pearson (Paramétrico): Mede a força da relação LINEAR.
#   - Premissas: Dados bivariados normais, relação linear. Sensível a outliers.
#
# Spearman (Não Paramétrico): Mede a força da relação MONOTÔNICA (se um sobe, o outro
#   sobe ou desce consistentemente, não necessariamente em linha reta).
#   - Premissas: Nenhuma sobre a distribuição. Robusto a outliers.

# Pergunta: Existe uma relação entre a potência (hp) e o consumo (mpg)?

# Passo 1: Visualize!
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Relação entre Potência do Motor e Consumo de Combustível")
# A relação parece linear, mas vamos checar a normalidade por segurança.

# Passo 2: Checar normalidade das variáveis
shapiro.test(mtcars$hp)  # p = 0.04 (< 0.05 -> Não é estritamente normal)
shapiro.test(mtcars$mpg) # p = 0.12 (> 0.05 -> Normal)

# Conclusão da Premissa: Como uma das variáveis (`hp`) não segue uma distribuição normal,
# o teste de Spearman é a escolha mais segura e robusta. No entanto, na prática, muitos
# analistas usam Pearson se a relação visualmente parece linear e não há outliers extremos.
# Vamos rodar os dois para comparar.

# Teste de Pearson
cor.test(~ hp + mpg, data = mtcars, method = "pearson")
#cor.test(mtcars$hp, mtcars$mpg)
# cor = -0.776. Relação linear negativa forte.

# Teste de Spearman
cor.test(~ hp + mpg, data = mtcars, method = "spearman")
# rho = -0.89. Relação monotônica negativa muito forte.
# O resultado de Spearman é até mais forte, pois ele não é "prejudicado" pela
# leve não-linearidade que podemos observar no gráfico.

# ---- 2. Introdução à Regressão Linear Simples ----
# A regressão linear é uma técnica PARAMÉTRICA, conectada à correlação de Pearson.
# Ela tenta modelar a relação LINEAR entre as variáveis.

modelo_regressao <- lm(mpg ~ hp, data = mtcars)
summary(modelo_regressao)

# A interpretação do `summary(modelo_regressao)` continua a mesma:
# Coeficiente de hp: Para cada aumento de 1 hp, o consumo (mpg) diminui em média 0.068.
# Adjusted R-squared: 59% da variabilidade de `mpg` é explicada por `hp` neste modelo.

# Pergunta: Qual seria o consumo esperado de um carro com 200 hp?
novo_carro <- data.frame(hp = 200)

previsao_mpg <- predict(modelo_regressao, newdata = novo_carro)

print(previsao_mpg)
# Saída:
#        1
# 16.45285