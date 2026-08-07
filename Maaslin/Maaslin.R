main_el= "tempo"
confunders = c("sex", "bmi", "age")
output_folder <- "C:/AURI/Thesis_bagetto/Maaslin/shared_results_kraken_20260723_223048"
list.files("C:/AURI/Thesis_bagetto/Maaslin/shared_results_kraken_20260723_223048", recursive = TRUE, pattern = ".rds$")

  


#QUI CAMBIA IL PERCORSO DELL'RDS DI INTERESSE
rds <- readRDS("C:/AURI/Thesis_bagetto/Maaslin/shared_results_kraken_20260723_223048/shared_results/SUPERVISED_DEC/Bacteria_Supervised_decontam.rds")
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
  labs(title = "Confronto taxa significativi Maaslin3",
       x = "Modello",
       y = "Numero di taxa significativi") +
  theme_minimal() +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

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
       x = "Effetto (coef)",
       y = "-log10(p-value)") +
  theme_minimal() +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))






# salvare
# Imposta il percorso di destinazione
save_path <- "C:/AURI/Thesis_bagetto/Maaslin"

# Salva i quattro dataset filtrati
write.table(abund_pval, file.path(save_path, "abundance_pval.tsv"),
            sep = "\t", row.names = FALSE)

write.table(abund_qval, file.path(save_path, "abundance_qval.tsv"),
            sep = "\t", row.names = FALSE)

write.table(prev_pval, file.path(save_path, "prevalence_pval.tsv"),
            sep = "\t", row.names = FALSE)

write.table(prev_qval, file.path(save_path, "prevalence_qval.tsv"),
            sep = "\t", row.names = FALSE)







