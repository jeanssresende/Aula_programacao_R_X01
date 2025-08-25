#-------------------------------------------------------------------------------
# Estudo de Caso 01: Análise de Expressão Gênica e Bioestatística com R
# Professor de R
# Objetivo:
# - Manipular dados de expressão gênica e dados clínicos.
# - Aplicar testes estatísticos para comparar grupos.
# - Visualizar os resultados de forma clara e profissional.
# - Introduzir conceitos de análise de expressão diferencial.
#-------------------------------------------------------------------------------

# Carregar os pacotes necessários
library(tidyverse)
library(ggpubr)

# -------------------------------------------------------------------------
# 1. CARREGAMENTO E PRÉ-PROCESSAMENTO DOS DADOS (BIOINFORMÁTICA)
# -------------------------------------------------------------------------
# O seu código original para carregamento e pré-processamento é robusto,
# então vamos mantê-lo, mas com mais comentários explicativos.

tcgaACC <- read.table("TCGA-ACC.star_tpm.tsv.gz", header = T)
clinical <- read.delim("TCGA-ACC.clinical.tsv.gz", header = TRUE)

# Remover a versão do Ensembl ID (Ex: ENSG00... -> ENSG00...)
tcgaACC$Ensembl_ID <- str_remove(tcgaACC$Ensembl_ID, "\\..*")

#sub("\\..*", "", tcgaACC$Ensembl_ID)

# Agrupar pelo Ensembl_ID e calcular a mediana
# A mediana é robusta a outliers e é uma boa prática para genes com múltiplos transcritos.
dataACC_log2 <- tcgaACC %>%
  group_by(Ensembl_ID) %>%
  summarise(across(everything(), median))

# Preparar os dados de expressão para a análise:
# - Transpor (genes nas colunas, amostras nas linhas)
# - Juntar com os dados clínicos
dataACC_log2_t <- as.data.frame(t(dataACC_log2 %>% column_to_rownames("Ensembl_ID")))
dataACC_log2_t <- rownames_to_column(dataACC_log2_t, "sample")



# Juntar os dados de expressão e clínicos pela coluna 'sample'
dataACC_log2_t$sample <- gsub("\\.", "-", dataACC_log2_t$sample)
dados_completos <- inner_join(dataACC_log2_t, clinical, by = "sample")

# Reverter para TPM sem log2, pois alguns testes e a interpretação de Fold Change
# são mais intuitivos na escala linear.
dados_completos <- dados_completos %>%
  mutate(across(starts_with("ENSG"), ~ 2^.x - 1)) # Aplica 2^x - 1 para cada gene

#  -- tarefa -- 

# Renomear os genes de interesse para facilitar a leitura
genes_interesse <- c(
  "ENSG00000148773", "ENSG00000141510", "ENSG00000168036",
  "ENSG00000167244", "ENSG00000120217"
)
nomes_genes <- c("CD274", "TP53", "MKI67", "IGF2", "CTNNB1")
dados_completos_genes <- dados_completos %>%
  select(sample, all_of(genes_interesse), vital_status.demographic, race.demographic) %>%
  rename_with(~ nomes_genes, all_of(genes_interesse))

# -------------------------------------------------------------------------
# 2. ANÁLISE GÊNICA DIFERENCIAL: CD274 vs. Status Vital
# -------------------------------------------------------------------------
# Esta é a parte principal. Vamos demonstrar o fluxo de análise para um gene
# e depois transformá-lo em uma função.

# Pergunta: A expressão de CD274 (gene relacionado à imunidade) difere
# entre pacientes vivos e mortos?

# Passo 1: Limpeza e Checagem do Fator (vital_status.demographic)
dados_filtrados <- dados_completos_genes %>%
  filter(vital_status.demographic %in% c("Alive", "Dead")) %>%
  mutate(vital_status = as.factor(vital_status.demographic))

# Passo 2: Calcular o Log2 Fold Change (LFC)
# Fold Change = Mediana do grupo 1 / Mediana do grupo 2
# LFC = log2(Fold Change)
mediana_alive <- dados_filtrados %>% filter(vital_status == "Alive") %>% summarise(mediana = median(CD274)) %>% pull(mediana)
mediana_dead <- dados_filtrados %>% filter(vital_status == "Dead") %>% summarise(mediana = median(CD274)) %>% pull(mediana)

lfc_cd274 <- log2(mediana_dead / mediana_alive)
print(paste("Log2 Fold Change para CD274 (Dead vs Alive):", round(lfc_cd274, 2)))

# Interpretação: Um LFC positivo indica maior expressão no grupo 'Dead'.
# Um LFC de 1 significa que a expressão no grupo 'Dead' é 2x maior que no 'Alive'.
# Um LFC de 2 significa 4x maior (2^2).

# Passo 3: Avaliação Estatística
# Checagem de Normalidade para o grupo 'Dead' e 'Alive'
# (Os resultados p-valor << 0.05 indicam não-normalidade)
shapiro.test(dados_filtrados %>% filter(vital_status == "Dead") %>% pull(CD274))
shapiro.test(dados_filtrados %>% filter(vital_status == "Alive") %>% pull(CD274))

# Conclusão: Usar o teste não paramétrico de Wilcoxon.
teste_wilcox_cd274 <- wilcox.test(CD274 ~ vital_status, data = dados_filtrados)
print(teste_wilcox_cd274)

# Passo 4: Visualização com p-valor
ggplot(dados_filtrados, aes(x = vital_status, y = CD274, fill = vital_status)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.4, color = "darkred") +
  labs(
    title = "Expressão de CD274 por Status Vital",
    x = "Status Vital",
    y = "Expressão de CD274 (TPM)",
    fill = "Status Vital"
  ) +
  theme_minimal() +
  stat_compare_means(method = "wilcox.test", label.y = max(dados_filtrados$CD274) * 0.9)

# -------------------------------------------------------------------------
# 3. AUTOMAÇÃO DA ANÁLISE COM FUNÇÕES
# -------------------------------------------------------------------------
# Este é um dos pontos mais importantes para a produtividade e replicabilidade.
# Vamos criar uma função para automatizar todo o processo acima.

analise_expressao_grupo <- function(dados, gene, grupo_interesse) {
  
  # Preparar os dados
  dados_analise <- dados %>%
    filter(!is.na(.data[[grupo_interesse]])) %>%
    mutate(grupo = as.factor(.data[[grupo_interesse]]))
  
  # 1. Calcular o Log2 Fold Change (LFC)
  medianas <- dados_analise %>%
    group_by(grupo) %>%
    summarise(mediana = median(.data[[gene]])) %>%
    pull(mediana)
  
  lfc <- log2(medianas[2] / medianas[1])
  
  # 2. Teste Estatístico (Wilcoxon)
  formula_wilcox <- as.formula(paste(gene, "~", "grupo"))
  p_valor <- wilcox.test(formula_wilcox, data = dados_analise)$p.value
  
  # 3. Visualização
  plot <- ggplot(dados_analise, aes_string(x = "grupo", y = gene, fill = "grupo")) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.1, alpha = 0.4) +
    labs(
      title = paste("Expressão do Gene", gene, "por", grupo_interesse),
      subtitle = paste("LFC =", round(lfc, 2), "| p-valor =", format.pval(p_valor, digits = 3)),
      x = grupo_interesse,
      y = paste("Expressão de", gene, "(TPM)"),
      fill = grupo_interesse
    ) +
    theme_minimal() +
    stat_compare_means(method = "wilcox.test", label.y = max(dados_analise[[gene]]) * 0.9)
  
  return(list(plot = plot, lfc = lfc, p_valor = p_valor))
}

# -------------------------------------------------------------------------
# 4. APLICAÇÃO E RESULTADOS
# -------------------------------------------------------------------------
# Usando a nossa nova função para os outros genes de interesse

# Análise de MKI67 vs. vital_status
analise_mki67 <- analise_expressao_grupo(dados_completos_genes, "MKI67", "vital_status.demographic")
print(analise_mki67$plot)
print(analise_mki67$lfc)

# Análise de TP53 vs. vital_status
analise_tp53 <- analise_expressao_grupo(dados_completos_genes, "TP53", "vital_status.demographic")
print(analise_tp53$plot)
print(analise_tp53$lfc)

# E assim por diante para todos os genes!