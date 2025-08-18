#-------------------------------------------------------------------------------
# Aplicação de Testes de Hipótese
# Comparando Grupos (Paramétrico vs. Não Paramétrico)
#-------------------------------------------------------------------------------

library(tidyverse)

# A regra de ouro: Paramétrico ou Não Paramétrico?
# 1. Cheque a premissa de NORMALIDADE dos dados.
#    - Visual: Gráfico Q-Q (Quantil-Quantil). Se os pontos seguem a linha, é um bom sinal.
#    - Formal: Teste de Shapiro-Wilk (`shapiro.test()`).
#      - H0: Os dados vieram de uma distribuição normal.
#      - Se p-valor > 0.05, NÃO rejeitamos H0 (assumimos normalidade).
#      - Se p-valor < 0.05, REJEITAMOS H0 (os dados NÃO são normais).
#
# 2. Escolha o teste:
#    - Dados Normais -> Teste t de Student (Paramétrico)
#    - Dados Não Normais -> Teste de Wilcoxon (Não Paramétrico)
#-------------------------------------------------------------------------------


# ---- 1. Comparando Amostras INDEPENDENTES ----

# ----- Cenário A: Dados Normais -----
# Pergunta: O comprimento da sépala (`Sepal.Length`) difere entre as espécies "setosa" e "versicolor"?

data(iris)
setosa_sl <- iris %>% filter(Species == "setosa") %>% pull(Sepal.Length)
versicolor_sl <- iris %>% filter(Species == "versicolor") %>% pull(Sepal.Length)

# Passo 1: Checar normalidade para CADA grupo
shapiro.test(setosa_sl)     # p-valor = 0.4595 (> 0.05 -> OK!)
shapiro.test(versicolor_sl) # p-valor = 0.4647 (> 0.05 -> OK!)

# Conclusão da Premissa: Ambos os grupos seguem uma distribuição normal.
# Teste Escolhido: Teste t para amostras independentes.

t.test(setosa_sl, versicolor_sl)
# Resultado: p-valor < 2.2e-16. Há uma diferença significativa nas médias.


# ----- Cenário B: Dados NÃO Normais -----
# Pergunta: O tempo de resposta de dois servidores (A e B) é diferente?
set.seed(123)
servidor_A <- rexp(30, rate = 0.5) # `rexp` gera dados com assimetria à direita (não-normal)
servidor_B <- rexp(30, rate = 0.4)
dados_servidores <- data.frame(
  tempo = c(servidor_A, servidor_B),
  servidor = rep(c("A", "B"), each = 30)
)

# Visualização
ggplot(dados_servidores, aes(x = tempo, fill = servidor)) + geom_density(alpha = 0.5)

# Passo 1: Checar normalidade
shapiro.test(servidor_A) 
shapiro.test(servidor_B) 

# Conclusão da Premissa: Os dados violam a premissa de normalidade.
# Teste Escolhido: Teste de Wilcoxon-Mann-Whitney (o `wilcox.test` para amostras independentes).

wilcox.test(tempo ~ servidor, data = dados_servidores)
# Resultado: Há uma diferença significativa na distribuição
# do tempo de resposta entre os servidores.


# ---- 2. Comparando Amostras PAREADAS ----
# Para testes pareados, a premissa de normalidade se aplica às DIFERENÇAS entre os pares.

# ----- Cenário A: Diferenças Normais -----
# Pergunta: Um programa de treinamento de digitação aumentou a velocidade dos funcionários?
set.seed(42)
velocidade_antes <- rnorm(20, mean = 50, sd = 5)
velocidade_depois <- velocidade_antes + rnorm(20, mean = 5, sd = 2) # Aumento com variação normal

# Passo 1: Calcular as diferenças e checar sua normalidade
diferencas_normais <- velocidade_depois - velocidade_antes
shapiro.test(diferencas_normais) # p-valor (> 0.05 -> As diferenças SÃO normais!)

# Conclusão da Premissa: As diferenças são normais.
# Teste Escolhido: Teste t pareado.

t.test(velocidade_antes, velocidade_depois, paired = TRUE)
# Resultado: Há uma diferença significativa (um aumento) na velocidade.


# ----- Cenário B: Diferenças NÃO Normais -----
# Pergunta: Uma campanha de marketing alterou o número de itens vendidos por dia?
set.seed(42)
vendas_antes <- floor(rexp(25, 0.05))
vendas_depois <- vendas_antes + c(floor(rexp(15, 0.1)), -floor(rexp(10, 0.1))) # Variação não-normal

# Passo 1: Calcular as diferenças e checar sua normalidade
diferencas_nao_normais <- vendas_depois - vendas_antes
shapiro.test(diferencas_nao_normais) # p-valor (< 0.05 -> As diferenças NÃO são normais!)

# Conclusão da Premissa: As diferenças violam a premissa de normalidade.
# Teste Escolhido: Teste dos Postos Sinalizados de Wilcoxon.

wilcox.test(vendas_antes, vendas_depois, paired = TRUE)
# Resultado: p-valor = 0.02. Há uma diferença estatisticamente significativa nas vendas.