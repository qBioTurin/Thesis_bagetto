main_el= "tempo"
confunders = c("sex", "bmi", "age")
output_folder <- "C:/AURI/Thesis_bagetto/Maaslin/maaslin_completo"
list.files("C:/AURI/Thesis_bagetto/Maaslin/maaslin_completo", recursive = TRUE, pattern = ".rds$")

  


#QUI CAMBIA IL PERCORSO DELL'RDS DI INTERESSE
rds <- readRDS("C:/AURI/Thesis_bagetto/app/shared_results/SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")
otu <- rds@otu_table
taxa <- rds@tax_table
metadata_df <- as(rds@sam_data, "data.frame")
unique(metadata_df$tempo)


taxa <- as.data.frame(taxa)
taxa$Genus_Species <- paste(taxa$Genus, taxa$Species, sep = "_")
Tnew <- taxa %>% select(Genus_Species)
Tnew$taxid <- rownames(Tnew)
if (confunders %>% length() == 0) {
  formulaInUse <- main_el
} else {
  formulaInUse <- paste(main_el, paste(confunders, collapse = "+"), sep = "+")
}
otu <- as.data.frame(otu)
otu$taxid <- rownames(otu)
# Merge the two data frames by taxid
merged_df <- merge(otu, Tnew, by = "taxid")
rownames(merged_df) <- merged_df$Genus_Species
merged_df <- merged_df %>% select(-c(taxid, Genus_Species))
merged_df <- t(merged_df)

library(phyloseq)
metadata_df <- data.frame(sample_data(rds))



metadata_df$tempo <- factor(
  as.character(metadata_df$tempo),
  levels = c("T0", "T1", "T12"),
  ordered = FALSE
)
unique(metadata_df$tempo)
metadata_df$sex = as.factor(metadata_df$sex)
levels(metadata_df$sex) = c("M", "F")
metadata_df$lesioni =as.factor(metadata_df$lesioni)
levels(metadata_df$lesioni) = c(1, 0)
metadata_df$relapse =as.factor(metadata_df$relapse)
levels(metadata_df$relapse) = c(1, 0)

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

coef_plot_vars <- c("timepoint T1", "timepoint T12")

discr_els_formatted <- unlist(lapply(confunders, function(d) {
  if (is.factor(metadata_df[[d]])) {
    paste(d, levels(metadata_df[[d]])) # tutti i livelli
  } else {
    d # continuous
  }
}))

View(metadata_df)
str(metadata_df$bmi)
metadata_df$bmi <- gsub(",", ".", metadata_df$bmi)
metadata_df$bmi <- as.numeric(metadata_df$bmi)


##for stability of results across runs
formulaInUse_fin = paste0("~ ",formulaInUse, "+(1|id_original)")
set.seed(2204)
median_comparison_abundance_in = FALSE
fx_effects = NULL
random_effects =  NULL
group_effects = NULL
ordered_effects = NULL
strata_effects = NULL
Sys.setenv(OMP_NUM_THREADS=1, MKL_NUM_THREADS=1, OPENBLAS_NUM_THREADS=1)
fit_out <- maaslin3::maaslin3(input_data = merged_df,
  input_metadata = metadata_df,
  output = output_folder,
  formula = formulaInUse_fin,
  fixed_effects = fx_effects,
  plot_summary_plot = TRUE,
  reference = c('tempo,T0'),
  standardize= TRUE,#o continuous guardare se standardizzare o meno le variabili continue
  #coef_plot_vars = coef_plot_vars,
  random_effects = random_effects,
  group_effects = group_effects,
  ordered_effects = ordered_effects,
  strata_effects = strata_effects,
  normalization = "TSS",
  #heatmap_vars = c("timepoint T1", "timepoint T12", "sex F", "bmi", "age"),
  transform = "LOG",
  augment = TRUE,
  max_significance = 0.1,
  median_comparison_abundance = median_comparison_abundance_in,
  median_comparison_prevalence = FALSE,
  max_pngs = 30,
  summary_plot_first_n = 200,
  # pval_filter = 0.05,
  cores = 1,
  small_random_effects = TRUE
)

print(fit_out)


View(metadata_df)
View(merged_df)

# carico il file all.results.tsv
res <- read.table(
  file.path(output_folder, "all_results.tsv"),
  sep = "\t", header = TRUE, stringsAsFactors = FALSE
)

# filtro per metadata=tempo
res_tempo <- subset(res, metadata == "tempo")

#separo abundance e prevalence
res_abund <- subset(res_tempo, model == "abundance")
res_prev  <- subset(res_tempo, model == "prevalence")

# Filtrare per p‑value < 0.05 e q‑value < 0.1
abund_pval <- subset(res_abund, pval_individual < 0.05)
abund_qval <- subset(res_abund, qval_individual < 0.1)

prev_pval  <- subset(res_prev, pval_individual < 0.05)
prev_qval  <- subset(res_prev, qval_individual < 0.1)

### per salvare
# Percorso della cartella di destinazione
save_path <- "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin"

# Salvataggio dei file .tsv
write.table(abund_pval, file.path(save_path, "abundance_pval.tsv"),
            sep = "\t", row.names = FALSE)

write.table(abund_qval, file.path(save_path, "abundance_qval.tsv"),
            sep = "\t", row.names = FALSE)

write.table(prev_pval, file.path(save_path, "prevalence_pval.tsv"),
            sep = "\t", row.names = FALSE)

write.table(prev_qval, file.path(save_path, "prevalence_qval.tsv"),
            sep = "\t", row.names = FALSE)

# salvataggio dei file .csv
# Salva i quattro dataset filtrati in formato CSV
write.csv(abund_pval, file.path(save_path, "abundance_pval.csv"),
          row.names = FALSE)

write.csv(abund_qval, file.path(save_path, "abundance_qval.csv"),
          row.names = FALSE)

write.csv(prev_pval, file.path(save_path, "prevalence_pval.csv"),
          row.names = FALSE)

write.csv(prev_qval, file.path(save_path, "prevalence_qval.csv"),
          row.names = FALSE)




# quanti taxa sono significativi per ciascun filtro
library(ggplot2)

# Calcola il numero di taxa significativi
counts <- data.frame(
  Model = c("Abundance", "Abundance", "Prevalence", "Prevalence"),
  Filter = c("pval < 0.05", "qval < 0.1", "pval < 0.05", "qval < 0.1"),
  Count = c(nrow(abund_pval), nrow(abund_qval), nrow(prev_pval), nrow(prev_qval))
)

# Grafico a barre
ggplot(counts, aes(x = Model, y = Count, fill = Filter)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Maaslin3 significant taxa",
       x = "Model",
       y = "Number of significant taxa") +
  theme_minimal() +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

## salvo grafico
p <- ggplot(counts, aes(x = Model, y = Count, fill = Filter)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Maaslin3 significant taxa",
       x = "Model",
       y = "Number of significant taxa") +
  theme_minimal() +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

ggsave(
  filename = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/confronto_taxa_significativi_euk.png",
  plot = p,
  width = 8, height = 5, dpi = 300
)


# volcano plot comparativo (effetto vs significatività)
library(ggplot2)

# Unisci i due dataset
res_plot <- rbind(
  data.frame(res_abund, Model = "Abundance"),
  data.frame(res_prev, Model = "Prevalence")
)

# Volcano plot
ggplot(res_plot, aes(x = coef, y = -log10(pval_individual), color = Model)) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Volcano plot Maaslin3: Abundance vs Prevalence",
       x = "Effect (coef)",
       y = "-log10(p-value)") +
  theme_minimal() +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))

## niente di significativo per prevalence
## abundance significativo solo per pvalue (no qvalue)

# salvo volcano
ggsave(
  filename = "volcano_abundance_prevalence_euk.png",
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin",
  width = 8,
  height = 6,
  dpi = 300
)

# Dumbbell plot (coef T1 vs T12 per ogni specie significativa)
library(dplyr)
library(tidyr)
library(ggplot2)

# Carica il file già filtrato (solo specie significative)
abund_sig <- read.csv("C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/abundance_pval.csv")

# Trasforma in formato wide per dumbbell plot
abund_wide <- abund_sig %>%
  select(feature, value, coef) %>%
  pivot_wider(names_from = value, values_from = coef, 
              names_prefix = "coef_")

# Dumbbell plot
ggplot(abund_wide, aes(x = coef_T1, xend = coef_T12, y = feature)) +
  geom_segment(aes(y = feature, yend = feature, 
                   x = coef_T1, xend = coef_T12),
               color = "grey60", size = 2) +
  geom_point(aes(x = coef_T1), color = "#0073C2FF", size = 4) +
  geom_point(aes(x = coef_T12), color = "#EFC000FF", size = 4) +
  labs(title = "Dumbbell plot – Significant Species (Abundance)",
       x = "Coefficient",
       y = "Species") +
  theme_minimal()

# salvo dumbbell
ggsave(
  filename = "dumbbell_abundance_T1_T12_euk.png",
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin",
  width = 10,
  height = 8,
  dpi = 300
)


## EUK
# Saccharomyces_cerevisiae

# Funzione per convertire q-value in asterischi
get_stars <- function(q){
  if(q < 0.1) return("*")
  return("")
}

# Parametri della specie
sp <- "Saccharomyces_cerevisiae"
qvalue_T1 <- 0.091
qvalue_T12 <- 0.091

stars_T1  <- get_stars(qvalue_T1)
stars_T12 <- get_stars(qvalue_T12)

# Controllo che la specie esista
if(sp %in% colnames(maaslin_transformed)){
  
  # Costruzione dataframe per il plot
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  # Posizione degli asterischi sotto le boxplot significative
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  # Costruzione del grafico
  p <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # ⭐ Asterisco sotto T1
    geom_text(
      data = subset(plot_df, tempo == "T1"),
      aes(x = tempo, y = y_star),
      label = stars_T1,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    # ⭐ Asterisco sotto T12
    geom_text(
      data = subset(plot_df, tempo == "T12"),
      aes(x = tempo, y = y_star),
      label = stars_T12,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Saccharomyces cerevisiae",
      subtitle = paste0(
        "Beta coefficients: T1 = 0.217 (q = ", qvalue_T1,
        "), T12 = 0.238 (q = ", qvalue_T12, ")"
      ),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    # ⭐ Legenda significatività spostata in basso
    annotate(
      "text",
      x = Inf, y = min(plot_df$abundance, na.rm = TRUE) - 0.5,
      label = "**Statistics:**\n*  q < 0.1",
      hjust = 1.1, vjust = 0,
      size = 3
    )
  
  # Salvataggio del grafico
  ggsave(
    filename = "boxplot_Saccharomyces_cerevisiae.png",
    plot = p,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig_euk",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}




















## BATTERI
### ora voglio ottenere un boxplot per vedere l'abbondanza delle specie significative
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(phyloseq)

rds <- readRDS("C:/AURI/Thesis_bagetto/app/shared_results/SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")
print(rds)

otu_table <- otu_table(rds)
print(otu_table)

tax_table <- tax_table(rds)
print(tax_table)
########################################## prova
rds = readRDS("C:/AURI/Thesis_bagetto/app/shared_results/SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")
metadata_df <- as(rds@sam_data, "data.frame")
unique(metadata_df$tp)

metadata_df$tempo <- factor(
  as.character(metadata_df$tempo),
  levels = c("T0", "T1", "T12"))
unique(metadata_df$tempo)


sig_species <- abund_pval %>%
  filter(pval_individual < 0.05) %>%
  pull(feature) %>%
  unique()


maaslin_transformed <- read.delim(
  "C:/AURI/Thesis_bagetto/Maaslin/maaslin_completo/features/data_transformed.tsv",
  row.names = 1,
  check.names = FALSE
)

# guardo quali specie sono significative
abund_pval <- read.table("C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/abundance_pval.tsv",
                         sep = "\t",
                         header = TRUE)

View(abund_pval)






### grafici singoli per batterio significativamente abbondante
library(ggplot2)
library(ggtext)
library(grid)

# Parabacteroides_sp. CT06
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Parabacteroides_sp. CT06"
pvalue <- 0.0008
stars <- get_stars(pvalue)
value <- "T1"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p1 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Parabacteroides_sp. CT06",
      subtitle = paste0("Beta coefficient: 2.024 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),  # 🔹 tag centrato
      plot.tag.position = c(0.55, 0.995),                   # 🔹 scritta più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.just = "top"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Parabacteroides_sp.CT06.png",
    plot = p1,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}






# Parabacteroides_sp. CT06
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Parabacteroides_sp. CT06"
pvalue <- 0.0008
stars <- get_stars(pvalue)
value <- "T1"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  # Posizione della barra e degli asterischi sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p1 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività tra T0 e T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar, yend = y_bar), color = "black", size = 0.8) +
    geom_text(aes(x = 1.5, y = y_star), label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Parabacteroides_sp. CT06",
      subtitle = paste0("Beta coefficient: 2.024 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.just = "top"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Parabacteroides_sp.CT06.png",
    plot = p1,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}





















# Bacteroides intestinalis
# Funzione per convertire p-value in asterischi
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

# Parametri della specie
sp <- "Bacteroides_intestinalis"
pvalue <- 0.0009
stars <- get_stars(pvalue)
value <- "T12"   # colonna significativa

# Controllo che la specie esista
if(sp %in% colnames(maaslin_transformed)){
  
  # Costruzione dataframe per il plot
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  # Posizione dell’asterisco sotto la boxplot significativa
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  # Costruzione del grafico
  p2 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # ⭐ Asterisco solo sotto la colonna significativa (T12)
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Bacteroides intestinalis",
      subtitle = paste0("Beta coefficient: 1.691 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),   # 🔹 tag centrato
      plot.tag.position = c(0.55, 0.995),                    # 🔹 tag più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.just = "top"
    ) +
    
    # ⭐ Legenda della significatività (titolo “Statistics” e testo più piccolo)
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  # Salvataggio del grafico
  ggsave(
    filename = "boxplot_Bacteroides_intestinalis.png",
    plot = p2,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}




# Thomasclavelia_spiroformis
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Thomasclavelia_spiroformis"
pvalue <- 0.0029
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p3 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Thomasclavelia_spiroformis",
      subtitle = paste0("Beta coefficient: -2.064 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),   # 🔹 tag centrato
      plot.tag.position = c(0.55, 0.995),                    # 🔹 tag più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.just = "top"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Thomasclavelia_spiroformis.png",
    plot = p3,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}


# Lacticaseibacillus_paracasei
# Funzione per convertire p-value in asterischi
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

# Parametri della specie
sp <- "Lacticaseibacillus_paracasei"
pvalue_T1 <- 0.026
pvalue_T12 <- 0.006
stars_T1 <- get_stars(pvalue_T1)
stars_T12 <- get_stars(pvalue_T12)

# Controllo che la specie esista
if(sp %in% colnames(maaslin_transformed)){
  
  # Costruzione dataframe per il plot
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  # Posizione degli asterischi sotto le boxplot significative
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  # Costruzione del grafico
  p4 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # ⭐ Asterisco sotto T1
    geom_text(
      data = subset(plot_df, tempo == "T1"),
      aes(x = tempo, y = y_star),
      label = stars_T1,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    # ⭐ Asterisco sotto T12
    geom_text(
      data = subset(plot_df, tempo == "T12"),
      aes(x = tempo, y = y_star),
      label = stars_T12,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Lacticaseibacillus paracasei",
      subtitle = paste0("Beta coefficients: T1 = -3.014 (p = ", pvalue_T1,
                        "), T12 = -3.750 (p = ", pvalue_T12, ")"),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),   # 🔹 tag centrato
      plot.tag.position = c(0.55, 0.995),                    # 🔹 tag più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.just = "top"
    ) +
    
    # ⭐ Legenda della significatività
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  # Salvataggio del grafico
  ggsave(
    filename = "boxplot_Lacticaseibacillus_paracasei.png",
    plot = p4,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}






# Alistipes_megaguti
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Alistipes_megaguti"
pvalue <- 0.0097
stars <- get_stars(pvalue)
value <- "T1"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p5 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Alistipes_megaguti",
      subtitle = paste0("Beta coefficient: -1.364 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),   # 🔹 tag centrato
      plot.tag.position = c(0.55, 0.995),                    # 🔹 tag più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.just = "top"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Alistipes_megaguti.png",
    plot = p5,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}


# Coprobacter_secundus
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Coprobacter_secundus"
pvalue <- 0.012
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p6 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Coprobacter_secundus",
      subtitle = paste0("Beta coefficient: 2.24 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Coprobacter_secundus.png",
    plot = p6,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}

# Faecalibacterium_prausnitzii
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Faecalibacterium_prausnitzii"
pvalue <- 0.016
stars <- get_stars(pvalue)
value <- "T1"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Faecalibacterium_prausnitzii",
      subtitle = paste0("Beta coefficient: 0.896 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Faecalibacterium_prausnitzii.png",
    plot = p,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}

# Faecalibacterium_sp. I2-3-92
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Faecalibacterium_sp. I2-3-92"
pvalue_T1 <- 0.04309
pvalue_T12 <- 0.04886
stars_T1 <- get_stars(pvalue_T1)
stars_T12 <- get_stars(pvalue_T12)

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == "T1"),
      aes(x = tempo, y = y_star),
      label = stars_T1,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    geom_text(
      data = subset(plot_df, tempo == "T12"),
      aes(x = tempo, y = y_star),
      label = stars_T12,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Faecalibacterium_sp. I2-3-92",
      subtitle = paste0("Beta coefficients: T1 = 0.9779 (p = ", pvalue_T1,
                        "), T12 = 0.9772 (p = ", pvalue_T12, ")"),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Faecalibacterium_sp. I2-3-92.png",
    plot = p,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}


# Oxalobacter_aliiformigenes
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Oxalobacter_aliiformigenes"
pvalue_T1 <- 0.039
pvalue_T12 <- 0.042
stars_T1 <- get_stars(pvalue_T1)
stars_T12 <- get_stars(pvalue_T12)

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p7 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == "T1"),
      aes(x = tempo, y = y_star),
      label = stars_T1,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    geom_text(
      data = subset(plot_df, tempo == "T12"),
      aes(x = tempo, y = y_star),
      label = stars_T12,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Oxalobacter_aliiformigenes",
      subtitle = paste0("Beta coefficients: T1 = 2.786 (p = ", pvalue_T1,
                        "), T12 = 2.547 (p = ", pvalue_T12, ")"),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Oxalobacter_aliiformigenes.png",
    plot = p7,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}


# Segatella_hominis
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Segatella_hominis"
pvalue <- 0.034
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p8 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Segatella_hominis",
      subtitle = paste0("Beta coefficient: -1.919 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Segatella_hominis.png",
    plot = p8,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}

# Eshraghiella_crossota
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Eshraghiella_crossota"
pvalue <- 0.0378
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Eshraghiella_crossota",
      subtitle = paste0("Beta coefficient: 0.814 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Eshraghiella_crossota.png",
    plot = p,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}



# Parabacteroides_absconsus
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Parabacteroides_absconsus"
pvalue <- 0.034
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p9 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Parabacteroides_absconsus",
      subtitle = paste0("Beta coefficient: 1.287 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Parabacteroides_absconsus.png",
    plot = p9,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}


# Streptococcus_ilei
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Streptococcus_ilei"
pvalue <- 0.0364
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p10 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Streptococcus_ilei",
      subtitle = paste0("Beta coefficient: -2.148 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Streptococcus_ilei.png",
    plot = p10,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}

# Streptococcus_salivarius
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Streptococcus_salivarius"
pvalue <- 0.0296
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p11 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Streptococcus_salivarius",
      subtitle = paste0("Beta coefficient: -1.609 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Streptococcus_salivarius.png",
    plot = p11,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}


# Thomasclavelia_ramosa
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Thomasclavelia_ramosa"
pvalue <- 0.0356
stars <- get_stars(pvalue)
value <- "T12"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p12 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Thomasclavelia_ramosa",
      subtitle = paste0("Beta coefficient: -1.020 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Thomasclavelia_ramosa.png",
    plot = p12,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}


# Faecalibacterium_sp. I3-3-89
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Faecalibacterium_sp. I3-3-89"
pvalue <- 0.012
stars <- get_stars(pvalue)
value <- "T1"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p13 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Faecalibacterium_sp. I3-3-89",
      subtitle = paste0("Beta coefficient: 1.009 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),   # centrato
      plot.tag.position = c(0.55, 0.995),                    # più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Faecalibacterium_sp.I3-3-89.png",
    plot = p13,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}

# Streptococcus_agalactiae
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Streptococcus_agalactiae"
pvalue <- 0.004
stars <- get_stars(pvalue)
value <- "T1"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p14 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Streptococcus_agalactiae",
      subtitle = paste0("Beta coefficient: 2.628 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),   # centrato
      plot.tag.position = c(0.55, 0.995),                    # più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Streptococcus_agalactiae.png",
    plot = p14,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}

# Sutterella_faecalis
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Sutterella_faecalis"
pvalue <- 0.036
stars <- get_stars(pvalue)
value <- "T1"

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  y_star <- min(plot_df$abundance, na.rm = TRUE) - 1.5
  
  p15 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    geom_text(
      data = subset(plot_df, tempo == value),
      aes(x = tempo, y = y_star),
      label = stars,
      color = "red",
      size = 8,
      fontface = "bold"
    ) +
    
    theme_bw() +
    labs(
      title = "Sutterella_faecalis",
      subtitle = paste0("Beta coefficient: -3.563 | pvalue: ", pvalue, " | Value: ", value),
      tag = "Significant Abundance",
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),  # 🔹 scritta più in alto
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.just = "top"
    ) +
    
    annotate(
      "text",
      x = Inf, y = Inf,
      label = "**Statistics:**\n*  p < 0.05\n** p < 0.01\n*** p < 0.001",
      hjust = 1.1, vjust = 1.5,
      size = 2.8
    )
  
  ggsave(
    filename = "boxplot_Sutterella_faecalis.png",
    plot = p15,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  message("Specie non trovata: ", sp)
}

library(patchwork)
library(patchwork)
library(ggplot2)

# 🔹 Funzione per rimuovere SOLO la legenda degli asterischi
remove_star_legend <- function(plot) {
  plot$layers <- plot$layers[!sapply(plot$layers, function(l) {
    inherits(l$geom, "GeomText") &&
      !is.null(l$geom_params) &&
      any(grepl("Statistics", unlist(l$geom_params)))
  })]
  return(plot + theme(legend.position = "none"))
}

# 🔹 Applica la funzione a tutti i grafici
plots_clean_1 <- lapply(list(p1, p2, p3, p4, p5, p6, p7, p8), remove_star_legend)

# 🔹 Combina i grafici puliti
combined_plot_1 <- (
  (plots_clean_1[[1]] | plots_clean_1[[2]]) /
    (plots_clean_1[[3]] | plots_clean_1[[4]]) /
    (plots_clean_1[[5]] | plots_clean_1[[6]]) /
    (plots_clean_1[[7]] | plots_clean_1[[8]])
)

# 🔹 Legenda unica esterna
combined_plot_1 <- combined_plot_1 +
  plot_annotation(
    title = "Significant Bacterial Species — Abundance Across Timepoints (Part 1)",
    caption = "Legend: Colors represent timepoints (T0 = red, T1 = green, T12 = blue)",
    theme = theme(
      plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

# 🔹 Salvataggio con grafici più grandi
ggsave(
  filename = "ALL_boxplots_significant_species_part1_clean.png",
  plot = combined_plot_1,
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
  width = 22,
  height = 32,
  dpi = 300
)


library(patchwork)
library(ggplot2)

# 🔹 Rimuove SOLO la legenda degli asterischi
remove_star_legend <- function(plot) {
  plot$layers <- plot$layers[!sapply(plot$layers, function(l) {
    inherits(l$geom, "GeomText") &&
      !is.null(l$geom_params) &&
      any(grepl("Statistics", unlist(l$geom_params)))
  })]
  return(plot + theme(legend.position = "none"))
}

# 🔹 Applica la funzione ai grafici p9–p15
plots_clean_2 <- lapply(
  list(p9, p10, p11, p12, p13, p14, p15),
  remove_star_legend
)

# 🔹 Patchwork corretto per 7 grafici
combined_plot_2 <- (
  (plots_clean_2[[1]] | plots_clean_2[[2]]) /
    (plots_clean_2[[3]] | plots_clean_2[[4]]) /
    (plots_clean_2[[5]] | plots_clean_2[[6]]) /
    (plots_clean_2[[7]])
)

# 🔹 Legenda unica esterna
combined_plot_2 <- combined_plot_2 +
  plot_annotation(
    title = "Significant Bacterial Species — Abundance Across Timepoints (Part 2)",
    caption = "Legend: Colors represent timepoints (T0 = red, T1 = green, T12 = blue).\nAsterisks indicate statistical significance (* p < 0.05, ** p < 0.01, *** p < 0.001).",
    theme = theme(
      plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

# 🔹 Salvataggio
ggsave(
  filename = "ALL_boxplots_significant_species_part2_clean.png",
  plot = combined_plot_2,
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
  width = 22,
  height = 32,
  dpi = 300
)

































### abund_pval rispetto a maaslin_trasnformed
# 1️⃣ Estrai i nomi delle specie da maaslin_transformed (colonne)
all_species <- colnames(maaslin_transformed)

# 2️⃣ Estrai i nomi delle specie da abund_pval (colonna 'feature')
sig_species <- abund_pval$feature

# 3️⃣ Uniforma i formati (minuscole e underscore)
all_species_clean <- tolower(gsub(" ", "_", all_species))
sig_species_clean <- tolower(gsub(" ", "_", sig_species))

# 4️⃣ Confronto
species_in_both <- intersect(all_species_clean, sig_species_clean)
species_only_maaslin <- setdiff(all_species_clean, sig_species_clean)
species_only_abund <- setdiff(sig_species_clean, all_species_clean)

# 5️⃣ Report finale
data.frame(
  total_maaslin = length(all_species_clean),
  total_significant = length(sig_species_clean),
  in_both = length(species_in_both),
  only_maaslin = length(species_only_maaslin),
  only_abund = length(species_only_abund)
)

library(tidyverse)

# 1️⃣ Estrai nomi delle specie
all_species <- colnames(maaslin_transformed)
sig_species <- abund_pval$feature

# 2️⃣ Uniforma formato (minuscole + underscore)
all_species_clean <- tolower(gsub(" ", "_", all_species))
sig_species_clean <- tolower(gsub(" ", "_", sig_species))

# 3️⃣ Confronto
species_in_both <- intersect(all_species_clean, sig_species_clean)
species_only_maaslin <- setdiff(all_species_clean, sig_species_clean)
species_only_abund <- setdiff(sig_species_clean, all_species_clean)

# 4️⃣ Crea tabella per il grafico
df_plot <- tibble(
  categoria = c("Total maaslin_transformed", 
                "Significant species", 
                "In both", 
                "Only maaslin transformed", 
                "Only abund_pval"),
  conteggio = c(length(all_species_clean),
                length(sig_species_clean),
                length(species_in_both),
                length(species_only_maaslin),
                length(species_only_abund))
)

# 5️⃣ Grafico
library(tidyverse)

# 1️⃣ Estrai nomi delle specie
all_species <- colnames(maaslin_transformed)      # specie = colonne
sig_species <- abund_pval$feature                 # specie = righe sotto 'feature'

# 2️⃣ Confronto diretto (nomi già identici)
species_in_both <- intersect(all_species, sig_species)
species_only_maaslin <- setdiff(all_species, sig_species)
species_only_abund <- setdiff(sig_species, all_species)

# 3️⃣ Crea tabella per il grafico
df_plot <- tibble(
  categoria = c("Total Maaslin species",
                "Significant species",
                "Species in both datasets",
                "Only in Maaslin",
                "Only in abund_pval"),
  conteggio = c(length(all_species),
                length(sig_species),
                length(species_in_both),
                length(species_only_maaslin),
                length(species_only_abund))
)

# 4️⃣ Costruisci il grafico
p <- ggplot(df_plot, aes(x = categoria, y = conteggio, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = conteggio), vjust = -0.5, size = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Species comparison plot: maaslin_transformed vs abund_pval",
       y = "Number of species",
       x = "")

# 5️⃣ Salva il grafico in JPEG nella cartella indicata
ggsave(
  filename = "species_comparison_plot.jpeg",
  plot = p,
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
  dpi = 300,
  width = 10,
  height = 7
)



























# codice per association plot significativo da Maaslin
##############################################
### 1. CARICA RISULTATI
##############################################

library(tidyverse)
library(ggplot2)

results <- read_tsv("C:/AURI/Thesis_bagetto/Maaslin/all_results.tsv")

data_norm <- read_tsv(
  "C:/AURI/Thesis_bagetto/Maaslin/shared_results_kraken_20260723_223048/features/data_norm.tsv"
)

###############################################
### 2. CONVERTI DATA_NORM DA WIDE A LONG
###############################################

data_long <- data_norm %>%
  pivot_longer(
    cols = -feature,          # tutte le colonne tranne "feature"
    names_to = "ID_long",     # nome della colonna con gli ID dei campioni
    values_to = "value"       # valori normalizzati (TSS + log)
  )

###############################################
### 3. CREA LA COLONNA TEMPO
###############################################

data_long <- data_long %>%
  mutate(
    tempo = case_when(
      str_detect(ID_long, "T0") ~ "T0",
      str_detect(ID_long, "T1") ~ "T1",
      str_detect(ID_long, "T12") ~ "T12",
      TRUE ~ NA_character_
    )
  )

###############################################
### 4. SELEZIONA LA FEATURE DI INTERESSE
###############################################

feature <- "Parabacteroides_sp._CT06"

df <- data_long %>%
  filter(feature == !!feature)

res <- results %>%
  filter(feature == !!feature)

###############################################
### 5. COSTRUISCI IL BOXPLOT IDENTICO A MAASLIN3
###############################################

ggplot(df, aes(x = tempo, y = value, fill = tempo)) +
  geom_boxplot(color = "black", size = 1.2, alpha = 0.6) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.7) +
  scale_fill_manual(values = c("#F4A582", "#FFF2AE", "#A6D8A8")) +
  labs(
    x = "tempo",
    y = paste0(feature,
               "\n(Normalization: Total sum scaling, Transformation: Log base 2)")
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  annotate(
    "text",
    x = 3, y = max(df$value, na.rm = TRUE) + 0.5,
    hjust = 1, vjust = 1,
    label = paste0(
      "FDR: ", signif(res$qval, 3), "\n",
      "Coefficient (full model): ", signif(res$coef, 3)
    ),
    size = 4
  )

















## summary plot
library(dplyr)
library(tidyr)
library(ggplot2)

# Carica il file con i risultati significativi
abund <- read.csv("C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/abundance_pval.csv")


# Trasforma in formato wide: una riga per specie, colonne coef_T1 e coef_T12
abund_wide <- abund %>%
  select(feature, value, coef) %>%
  pivot_wider(
    names_from = value,
    values_from = coef,
    names_prefix = "coef_"
  )

# Ordina le specie per effetto (opzionale, migliora la leggibilità)
abund_wide <- abund_wide %>%
  arrange(coef_T12)

# Dumbbell plot
ggplot(abund_wide, aes(y = feature)) +
  geom_segment(aes(x = coef_T1, xend = coef_T12, yend = feature),
               color = "grey60", size = 2) +
  geom_point(aes(x = coef_T1), color = "#0073C2FF", size = 4) +
  geom_point(aes(x = coef_T12), color = "#EFC000FF", size = 4) +
  labs(
    title = "Specie significative per abundance (p < 0.05)",
    subtitle = "Confronto dei coefficienti tra T1 e T12",
    x = "Coefficient",
    y = "Specie"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    plot.title = element_text(size = 14, face = "bold")
  )
















# salvare
# Percorso di salvataggio
save_path <- "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin"

# Salva i quattro dataset filtrati in formato CSV
write.csv(abund_pval, file.path(save_path, "abundance_pval.csv"),
          row.names = FALSE)

write.csv(abund_qval, file.path(save_path, "abundance_qval.csv"),
          row.names = FALSE)

write.csv(prev_pval, file.path(save_path, "prevalence_pval.csv"),
          row.names = FALSE)

write.csv(prev_qval, file.path(save_path, "prevalence_qval.csv"),
          row.names = FALSE)












