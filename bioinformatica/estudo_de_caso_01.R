library(tidyverse)
library(ggpubr)

tcgaACC <- read.table("TCGA-ACC.star_tpm.tsv.gz", header = T)
clinical <- read.delim("TCGA-ACC.clinical.tsv.gz", header = TRUE)

head(tcgaACC)

# Remover a versão do Ensembl ID
tcgaACC$Ensembl_ID <- str_remove(tcgaACC$Ensembl_ID, "\\..*")

# Agrupar pelo Ensembl_ID e calcular a média
dataACC <- tcgaACC %>%
  group_by(Ensembl_ID) %>%
  summarise(across(everything(), median))

# Transformar Ensembl_ID em nomes de linha
dataACC <- column_to_rownames(dataACC, "Ensembl_ID")
head(rownames(dataACC))

# alterando nome das colunas
colnames(dataACC) <- gsub("\\.", "-", colnames(dataACC))
head(colnames(dataACC))

# voltando para tpm sem log2
teste <- 2^dataACC - 1
head(teste$`TCGA-OR-A5K2-01A`)

teste2 <- log2(teste + 1)

head(teste2$`TCGA-OR-A5K2-01A`)
head(dataACC$`TCGA-OR-A5K2-01A`)

dataACC_semlog2 <- 2^dataACC - 1

genes_interesse <- c("ENSG00000148773","ENSG00000141510","ENSG00000168036",
                     "ENSG00000167244","ENSG00000120217")

dataACC_semlog2_genesInteresse <- dataACC_semlog2[rownames(dataACC_semlog2) %in%
                                                    genes_interesse,]


colnames(clinical)

dataClinical <- clinical[clinical$sample %in% colnames(dataACC_semlog2_genesInteresse),
                         c("sample","race.demographic","gender.demographic",
                           "vital_status.demographic", "age_at_index.demographic",
                           "ajcc_pathologic_n.diagnoses")]

rownames(dataACC_semlog2_genesInteresse) <- c("CD274","TP53","MKI67","IGF2","CTNNB1")

rownames(dataClinical) <- NULL
dataClinical <- column_to_rownames(dataClinical, "sample")


head(dataACC_semlog2_genesInteresse)
head(dataClinical)








# Carregar os pacotes necessários
library(tidyverse)
# library(dunn.test) # Seria necessário para um post-hoc de Kruskal-Wallis

# Supondo que seus dados já estejam carregados como:
# dataACC_semlog2_genesInteresse
# dataClinical

# -------------------------------------------------------------------------
# 1. PREPARAÇÃO E LIMPEZA DOS DADOS
# -------------------------------------------------------------------------
# O passo mais crucial é garantir que ambos os dataframes tenham as mesmas
# amostras, na mesma ordem.

# Transpor os dados de expressão para que as amostras fiquem nas linhas
# e os genes nas colunas.
dados_expressao_t <- as.data.frame(t(dataACC_semlog2_genesInteresse))

# Converter os nomes das linhas (que são as amostras) em uma coluna
dados_expressao_t <- rownames_to_column(dados_expressao_t, "sample")

# Juntar os dados de expressão com os dados clínicos pela coluna 'sample'
# (Os nomes de linha em dataClinical são os IDs das amostras)
dados_combinados <- inner_join(
  rownames_to_column(dataClinical, "sample"), # Garante que os IDs das amostras estejam numa coluna
  dados_expressao_t,
  by = "sample"
)

# Inspecionar o resultado final
glimpse(dados_combinados)


# -------------------------------------------------------------------------
# 2. ANÁLISE 1: MKI67 vs. Status Vital (Teste de 2 Grupos)
# -------------------------------------------------------------------------

# Pergunta: A expressão do gene MKI67 (marcador de proliferação)
# é diferente entre pacientes vivos e mortos?

# 2.1. Visualização com Boxplot
ggplot(dados_combinados, aes(x = vital_status.demographic, y = MKI67, fill = vital_status.demographic)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "Expressão de MKI67 por Status Vital",
    x = "Status Vital",
    y = "Expressão de MKI67 (TPM)",
    fill = "Status Vital"
  ) +
  theme_minimal()

# 2.2. Teste de Normalidade para escolher o teste estatístico correto
# H0: Os dados seguem uma distribuição normal.
shapiro.test(filter(dados_combinados, vital_status.demographic == "Alive")$MKI67)
# p-valor << 0.05
shapiro.test(filter(dados_combinados, vital_status.demographic == "Dead")$MKI67)
# p-valor << 0.05

# Conclusão da premissa: Ambos os grupos violam a premissa de normalidade.
# Portanto, devemos usar o teste não paramétrico: Teste de Wilcoxon.

# 2.3. Aplicar o Teste de Wilcoxon
resultado_wilcox <- wilcox.test(MKI67 ~ vital_status.demographic, data = dados_combinados)
print(resultado_wilcox)

# 2.4. Interpretação
# Com um p-valor baixo (geralmente < 0.05), podemos concluir que existe uma
# diferença estatisticamente significativa na expressão de MKI67 entre
# pacientes vivos e mortos. Visualmente, parece que pacientes que foram a óbito
# tinham níveis mais altos de MKI67.


# -------------------------------------------------------------------------
# 3. ANÁLISE 2: Correlação entre TP53 e IGF2
# -------------------------------------------------------------------------

# Pergunta: Existe uma relação entre a expressão de TP53 e IGF2?

# 3.1. Visualização com Gráfico de Dispersão (Scatterplot)
ggplot(dados_combinados, aes(x = TP53, y = IGF2)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Adiciona linha de tendência
  labs(
    title = "Correlação entre Expressão de TP53 e IGF2",
    x = "Expressão de TP53 (TPM)",
    y = "Expressão de IGF2 (TPM)"
  ) +
  theme_bw()

# 3.2. Teste de Normalidade para escolher o método de correlação
shapiro.test(dados_combinados$TP53) # p-valor << 0.05
shapiro.test(dados_combinados$IGF2) # p-valor << 0.05

# Conclusão da premissa: Como os dados não são normais, o método de correlação
# de Spearman (que é não paramétrico e baseado em ranks) é mais apropriado.

# 3.3. Aplicar o Teste de Correlação de Spearman
resultado_cor <- cor.test(~ TP53 + IGF2, data = dados_combinados, method = "spearman")
print(resultado_cor)

# 3.4. Interpretação
# O teste nos dará um coeficiente de correlação 'rho' (entre -1 e 1) e um p-valor.
# Se o p-valor for < 0.05, a correlação é estatisticamente significativa.
# O sinal de 'rho' (positivo ou negativo) indica a direção da relação.


# -------------------------------------------------------------------------
# 4. BÔNUS: Automação com um `for` loop
# -------------------------------------------------------------------------

# Objetivo: Criar rapidamente um boxplot para cada gene de interesse,
# comparando sua expressão entre os grupos de status vital.

# 4.1. Definir a lista de genes que queremos analisar
genes_para_plotar <- c("CD274", "TP53", "MKI67", "IGF2", "CTNNB1")

# 4.2. Criar o loop `for`
for (gene_atual in genes_para_plotar) {
  
  plot <- ggplot(dados_combinados, aes_string(x = "vital_status.demographic", y = gene_atual, fill = "vital_status.demographic")) +
    geom_boxplot() +
    scale_y_log10() +
    labs(
      title = paste("Expressão do Gene", gene_atual, "por Status Vital"),
      x = "Status Vital",
      y = paste("Expressão de", gene_atual)
    ) +
    theme_light()+
    stat_compare_means(method = "wilcox.test", label.y = 5) # Adiciona o p-valor do teste de Wilcoxon
  
  # Imprimir o gráfico no painel de Plots
  print(plot)
  
}
