# Módulo: Análise de Dados com R
# Aula: Regressão Não Linear (Polinomial)

library(tidyverse)
data(mtcars)

# Visualização da relação
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(title = "Relação entre HP e MPG")

# Construindo um modelo polinomial de grau 2
# A função `I()` é crucial para o R interpretar hp^2
modelo_polinomial <- lm(mpg ~ hp + I(hp^2), data = mtcars)

# Resumo do modelo
summary(modelo_polinomial)

# Avaliação do modelo
# 1. Olhar para os coeficientes: O p-valor para I(hp^2) é significativo?
# 2. Olhar para o R-quadrado ajustado: Ele melhorou em relação ao modelo linear simples?
# 3. Análise visual: Qual gráfico de diagnóstico do `plot()` melhorou?

plot(modelo_polinomial) # Avaliamos os 4 gráficos de novo!


# Visualizando o ajuste do modelo
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_line(aes(y = predict(modelo_polinomial)), color = "blue", size = 1) +
  labs(title = "Modelo Polinomial (Grau 2)")





# Módulo: Análise de Dados com R
# Aula: Modelos Aditivos Generalizados (GAMs)

# install.packages("mgcv")
library(mgcv)
library(tidyverse)
data(mtcars)

# Construindo um modelo GAM
# A função `s()` indica que queremos ajustar um spline suave para `hp`
modelo_gam <- gam(mpg ~ s(hp), data = mtcars)

# Resumo do modelo
summary(modelo_gam)

# Visualizando o ajuste do GAM
plot(modelo_gam, pages = 1) # Gera gráficos de diagnóstico e o ajuste da curva
