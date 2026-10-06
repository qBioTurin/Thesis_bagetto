# per stesura
bact <- readRDS("C:/AURI/Thesis_bagetto/app/shared_results/SUPERVISED_DEC/Bacteria_Supervised_decontam.rds")
euka <- readRDS("C:/AURI/Thesis_bagetto/app/shared_results/SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")

tax_bact <- as.data.frame(bact@tax_table)
tax_euka <- as.data.frame(euka@tax_table)

length(unique(tax_bact$Species))   # numero totale di specie batteriche
length(unique(tax_euka$Species))   # numero totale di specie eucariotiche


#tabella pz MAV e TEC
library(dplyr)
library(gtsummary)

# =====================================================
# Import metadata (solo pazienti)
# =====================================================
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_ceci.csv")

# Correzioni manuali
merged[merged$ID == "MAV32", c("bmi", "smoking_habit", "physical_activity")] <- list(33.20, "yes", "sedentary")
merged[merged$ID == "TEC25", c("bmi", "smoking_habit", "physical_activity")] <- list(21.88, "no", "sedentary")
merged[merged$ID == "MAV31", c("bmi", "smoking_habit", "physical_activity")] <- list(27.43, "no", "sedentary")
merged[merged$ID == "TEC26", c("bmi", "smoking_habit", "physical_activity")] <- list(22.46, "yes", "active")
merged[merged$ID == "TEC27", c("bmi", "smoking_habit", "physical_activity")] <- list(25.17, "no", "partially_active")
merged[merged$ID == "TEC32", c("physical_activity")] <- list(NA)
merged[merged$ID == "MAV15", c("bmi")] <- list(32)

# =====================================================
# Funzioni per categorie
# =====================================================
bmi_fun <- function(x) {
  case_when(
    x < 18.5 ~ "Underweight (<18.5)",
    x >= 18.5 & x < 25 ~ "Normal weight (18.5-24.9)",
    x >= 25 ~ "Overweight (≥25)",
    TRUE ~ NA_character_
  )
}

medas_fun <- function(x) {
  case_when(
    x >= 50 ~ "High adherence (≥50)",
    x < 50  ~ "Low adherence (<50)",
    TRUE ~ NA_character_
  )
}

# =====================================================
# Mutate variabili
# =====================================================
merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes", "1") ~ 1,
      gadolinium_contrast %in% c("No", "0")  ~ 0,
      TRUE ~ NA_real_
    ),
    bmi = as.numeric(bmi),
    bmi_cat = bmi_fun(bmi),
    MEDAS = as.numeric(MEDAS),
    medas_cat = medas_fun(MEDAS)
  )

# =====================================================
# TABELLA SOLO PAZIENTI
# =====================================================
tab2 <- merged %>%
  select(
    GROUP,
    age_at_T0,
    sex,
    bmi_cat,
    naive,
    gc_treatment,
    smoking_habit,
    physical_activity,
    medas_cat,
    lesion_burden,
    gadolinium_contrast,
    bone_marrow_lesions,
    subtentorial_lesions
  ) %>%
  tbl_summary(
    by = GROUP,
    missing = "no",
    type = list(
      gadolinium_contrast ~ "dichotomous",
      gc_treatment ~ "dichotomous"
    ),
    value = list(
      gadolinium_contrast ~ 1,
      gc_treatment ~ "positive"
    ),
    statistic = list(
      age_at_T0 ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_at_T0 ~ "Age at baseline (years)",
      sex ~ "Sex",
      bmi_cat ~ "BMI category",
      naive ~ "Treatment-naïve",
      gc_treatment ~ "Glucocorticoid at T0",
      smoking_habit ~ "Smoking habit",
      physical_activity ~ "Physical activity",
      medas_cat ~ "MEDAS adherence",
      lesion_burden ~ "Lesion burden",
      gadolinium_contrast ~ "Gadolinium contrast",
      bone_marrow_lesions ~ "Spinal cord lesions",
      subtentorial_lesions ~ "Subtentorial lesions"
    )
  ) %>%
  add_overall() %>%
  add_n(statistic = "{N_miss}") %>%
  add_p(
    test = list(
      age_at_T0 ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels()

# =====================================================
# Esporta tabella
# =====================================================
tab2_gt <- as_gt(tab2)

Sys.setenv(
  CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
)

gt::gtsave(
  data = tab2_gt,
  filename = "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/tabella_pazienti.png",
  zoom = 2
)


# immagini statistica naive
install.packages("magick")
library(magick)

# Percorso delle immagini
path <- "C:/AURI/Thesis_bagetto/app/stacked_bar_plots/nuove/naive"

# Carica le immagini
img_A <- image_read(file.path(path, "shannon_naive_T0.jpg"))
img_B <- image_read(file.path(path, "simpson_naive_T0.jpg"))
img_C <- image_read(file.path(path, "observed_naive_T.jpg"))
img_D <- image_read(file.path(path, "beta_naive_T0.jpg"))

# Ridimensiona tutte alla stessa altezza
target_height <- 900
img_A <- image_resize(img_A, paste0("x", target_height))
img_B <- image_resize(img_B, paste0("x", target_height))
img_C <- image_resize(img_C, paste0("x", target_height))

# Ridimensiona il grafico D leggermente più piccolo per allinearlo visivamente
img_D <- image_resize(img_D, paste0("x", target_height * 0.9))

# Aggiungi etichette A–D sopra e a destra di ciascun grafico
img_A <- image_annotate(img_A, "A", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_B <- image_annotate(img_B, "B", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_C <- image_annotate(img_C, "C", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_D <- image_annotate(img_D, "D", size = 80, color = "black", gravity = "northeast", location = "+40+40")

# Unisci le immagini in una riga orizzontale perfettamente allineata
collage <- image_append(c(img_A, img_B, img_C, img_D), stack = FALSE)

# Aumenta la risoluzione per maggiore nitidezza
collage <- image_resize(collage, "6500x")

# Salva il collage finale
image_write(collage, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/collage_T0_naive_highres.png", format = "png")




# bmi_T0
library(magick)

# Percorso delle immagini
path <- "C:/AURI/Thesis_bagetto/app/stacked_bar_plots/nuove/bmi_T0"

# Carica le immagini
img_A <- image_read(file.path(path, "shannon_bmi_T0_page-0001.jpg"))
img_B <- image_read(file.path(path, "simpson_bmi_T0_page-0001.jpg"))
img_C <- image_read(file.path(path, "observed_bmi_T0_page-0001.jpg"))
img_D <- image_read(file.path(path, "beta_bmi_T0_page-0001.jpg"))

# Ridimensiona tutte alla stessa altezza
target_height <- 900
img_A <- image_resize(img_A, paste0("x", target_height))
img_B <- image_resize(img_B, paste0("x", target_height))
img_C <- image_resize(img_C, paste0("x", target_height))

# Ridimensiona il grafico D leggermente più piccolo per allinearlo visivamente
img_D <- image_resize(img_D, paste0("x", target_height * 0.95))

# Aggiungi etichette A–D sopra e a destra di ciascun grafico
img_A <- image_annotate(img_A, "A", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_B <- image_annotate(img_B, "B", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_C <- image_annotate(img_C, "C", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_D <- image_annotate(img_D, "D", size = 80, color = "black", gravity = "northeast", location = "+40+40")

# Unisci le immagini in una riga orizzontale perfettamente allineata
collage <- image_append(c(img_A, img_B, img_C, img_D), stack = FALSE)

# Aumenta la risoluzione per maggiore nitidezza
collage <- image_resize(collage, "6500x")

# Salva il collage finale
image_write(collage, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/collage_bmi_T0_highres.png", format = "png")



# bmi T1
library(magick)

# Percorso delle immagini
path <- "C:/AURI/Thesis_bagetto/app/stacked_bar_plots/nuove/bmi_T1"

# Carica le immagini
img_E <- image_read(file.path(path, "shannon_bmi_T1_page-0001.jpg"))
img_F <- image_read(file.path(path, "simpson_bmi_T1_page-0001.jpg"))
img_G <- image_read(file.path(path, "observed_bmi_T1_page-0001.jpg"))
img_H <- image_read(file.path(path, "beta_bmi_T1_page-0001.jpg"))

# Ridimensiona tutte alla stessa altezza
target_height <- 900
img_E <- image_resize(img_E, paste0("x", target_height))
img_F <- image_resize(img_F, paste0("x", target_height))
img_G <- image_resize(img_G, paste0("x", target_height))
img_H <- image_resize(img_H, paste0("x", target_height * 0.95))  # leggermente più piccolo per bilanciare

# Aggiungi etichette E–H sopra e a destra di ciascun grafico
img_E <- image_annotate(img_E, "E", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_F <- image_annotate(img_F, "F", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_G <- image_annotate(img_G, "G", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_H <- image_annotate(img_H, "H", size = 80, color = "black", gravity = "northeast", location = "+40+40")

# Unisci le immagini in una riga orizzontale perfettamente allineata
collage <- image_append(c(img_E, img_F, img_G, img_H), stack = FALSE)

# Aumenta la risoluzione per maggiore nitidezza
collage <- image_resize(collage, "6500x")

# Salva il collage finale
image_write(collage, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/collage_bmi_T1_highres.png", format = "png")




#bmi T12
library(magick)

# Percorso delle immagini
path <- "C:/AURI/Thesis_bagetto/app/stacked_bar_plots/nuove/bmi_T12"

# Carica le immagini
img_I <- image_read(file.path(path, "shannon_bmi_T12_page-0001.jpg"))
img_J <- image_read(file.path(path, "simpson_bmi_T12_page-0001.jpg"))
img_K <- image_read(file.path(path, "observed_bmi_T12_page-0001.jpg"))
img_L <- image_read(file.path(path, "beta_bmi_T12_page-0001.jpg"))

# Ridimensiona tutte alla stessa altezza
target_height <- 900
img_I <- image_resize(img_I, paste0("x", target_height))
img_J <- image_resize(img_J, paste0("x", target_height))
img_K <- image_resize(img_K, paste0("x", target_height))
img_L <- image_resize(img_L, paste0("x", target_height * 0.95))  # leggermente più piccolo per bilanciare

# Aggiungi etichette I–L sopra e a destra di ciascun grafico
img_I <- image_annotate(img_I, "I", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_J <- image_annotate(img_J, "J", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_K <- image_annotate(img_K, "K", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_L <- image_annotate(img_L, "L", size = 80, color = "black", gravity = "northeast", location = "+40+40")

# Unisci le immagini in una riga orizzontale perfettamente allineata
collage <- image_append(c(img_I, img_J, img_K, img_L), stack = FALSE)

# Aumenta la risoluzione per maggiore nitidezza
collage <- image_resize(collage, "6500x")

# Salva il collage finale
image_write(collage, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/collage_bmi_T12_highres.png", format = "png")



# bmi tot
library(magick)

# Percorso della cartella
path <- "C:/AURI/Thesis_Bagetto/grafici_coorte_totale"

# Carica le tre immagini
img_T0 <- image_read(file.path(path, "collage_bmi_T0_highres.png"))
img_T1 <- image_read(file.path(path, "collage_bmi_T1_highres.png"))
img_T12 <- image_read(file.path(path, "collage_bmi_T12_highres.png"))

# Ridimensiona tutte alla stessa larghezza per allineamento perfetto
target_width <- 6500
img_T0 <- image_resize(img_T0, paste0(target_width, "x"))
img_T1 <- image_resize(img_T1, paste0(target_width, "x"))
img_T12 <- image_resize(img_T12, paste0(target_width, "x"))

# Unisci le immagini in verticale (una sotto l’altra)
collage_finale <- image_append(c(img_T0, img_T1, img_T12), stack = TRUE)

# Aumenta la risoluzione per maggiore nitidezza
collage_finale <- image_resize(collage_finale, "6500x")

# Salva il collage finale
image_write(collage_finale, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/collage_bmi_T0_T1_T12_highres.png", format = "png")



#test appaiati per PurrBiom
metadata <- read.csv("C:/AURI/Thesis_bagetto/input/merged_final.csv", sep = ";", header = TRUE)
library(tidyverse)

# Imposta la cartella dove hai i file Bracken
path_bracken <- "C:/AURI/Thesis_bagetto/RstudioXPurrBiom/bracken"

# Importa tutti i file .report senza header e assegna nomi alle colonne
bracken_list <- list.files(path_bracken, pattern = "*.report", full.names = TRUE) %>%
  set_names(basename(.) %>% str_remove("_bracken.report")) %>%
  map(~ read.table(.x, header = FALSE, sep = "\t",
                   col.names = c("fraction_total_reads", "taxonomy_id", "unclassified_reads",
                                 "taxonomy_lvl", "taxon_reads", "name")))

# 🔹 Somma i duplicati per ogni taxon e campione
feature_table <- bracken_list %>%
  bind_rows(.id = "sample") %>%
  group_by(sample, name) %>%
  summarise(fraction_total_reads = sum(fraction_total_reads), .groups = "drop") %>%
  pivot_wider(names_from = name,
              values_from = fraction_total_reads,
              values_fill = 0)   # ✅ correzione definitiva

# 🔍 Verifica la struttura della tabella
head(feature_table[, 1:6])



library(vegan)

# Rimuovi la colonna 'sample' e calcola le metriche
shannon <- diversity(feature_table[,-1], index = "shannon")
simpson <- diversity(feature_table[,-1], index = "simpson")
observed <- rowSums(feature_table[,-1] > 0)

# Crea un dataframe con i risultati
alpha_df <- data.frame(
  sample = feature_table$sample,
  shannon = shannon,
  simpson = simpson,
  observed = observed
)

head(alpha_df)


metadata <- read.csv("C:/AURI/Thesis_bagetto/input/merged_final.csv", sep = ";", header = TRUE)

alpha_merged <- left_join(alpha_df, metadata, by = c("sample" = "ID_long"))



# procedo con tabella
library(tidyverse)

# Filtra i dati per ciascun tempo
tempi <- c("T0", "T1", "T12")

# Variabili da testare
vars <- c("MEDAS_group", "sex", "age_at_T0", "naive", "BMI_group",
          "relapse", "lesioni", "cortisone", "EDSS")

# Funzione per calcolare i p-value per ogni tempo e variabile
get_pvalues <- function(df, tempo, vars) {
  df_tempo <- df %>% filter(Tempo == tempo)
  
  results <- lapply(vars, function(v) {
    # Se la variabile è numerica → test di correlazione
    if (is.numeric(df_tempo[[v]])) {
      p <- cor.test(df_tempo[[v]], df_tempo$shannon, method = "spearman")$p.value
    } else {
      # Se è categoriale → test di Wilcoxon o Kruskal-Wallis
      n_groups <- length(unique(df_tempo[[v]]))
      if (n_groups == 2) {
        p <- wilcox.test(df_tempo$shannon ~ df_tempo[[v]])$p.value
      } else {
        p <- kruskal.test(df_tempo$shannon ~ df_tempo[[v]])$p.value
      }
    }
    data.frame(variable = v, p_value = p)
  })
  
  bind_rows(results) %>% mutate(Tempo = tempo)
}

# Applica la funzione a tutti i tempi
p_table <- map_df(tempi, ~ get_pvalues(alpha_merged, .x, vars))

# Riorganizza la tabella in formato largo
p_table_wide <- p_table %>%
  pivot_wider(names_from = Tempo, values_from = p_value)

# Visualizza la tabella finale
p_table_wide


# tabella finale
library(tidyverse)
library(vegan)

# Definisci i tempi e le variabili da testare
tempi <- c("T0", "T1", "T12")
vars <- c("MEDAS_group", "sex", "age_at_T0", "naive", "BMI_group",
          "relapse", "lesioni", "cortisone", "EDSS")

# Funzione per calcolare p-value per ogni indice di diversità
get_pvalues_all <- function(df, tempo, vars) {
  df_tempo <- df %>% filter(Tempo == tempo)
  
  results <- lapply(vars, function(v) {
    test_metric <- function(metric) {
      if (is.numeric(df_tempo[[v]])) {
        cor.test(df_tempo[[v]], df_tempo[[metric]], method = "spearman")$p.value
      } else {
        n_groups <- length(unique(df_tempo[[v]]))
        if (n_groups == 2) {
          wilcox.test(df_tempo[[metric]] ~ df_tempo[[v]])$p.value
        } else {
          kruskal.test(df_tempo[[metric]] ~ df_tempo[[v]])$p.value
        }
      }
    }
    
    data.frame(
      variable = v,
      Tempo = tempo,
      p_shannon = test_metric("shannon"),
      p_simpson = test_metric("simpson"),
      p_observed = test_metric("observed")
    )
  })
  
  bind_rows(results)
}

# Calcola p-value per Shannon, Simpson e Observed
p_table_alpha <- map_df(tempi, ~ get_pvalues_all(alpha_merged, .x, vars))

# 🔹 Calcola la beta diversity (PERMANOVA su Bray-Curtis)
beta_pvalues <- lapply(vars, function(v) {
  # Rimuove le righe con NA nella variabile corrente
  df_clean <- alpha_merged %>% filter(!is.na(.data[[v]]))
  
  # Allinea la matrice delle abbondanze ai campioni presenti in df_clean
  feature_clean <- feature_table %>% filter(sample %in% df_clean$sample)
  
  # Crea la formula dinamica corretta
  f <- as.formula(paste("feature_clean[,-1] ~", v))
  
  # Esegui PERMANOVA solo se ci sono almeno 2 gruppi
  if (length(unique(df_clean[[v]])) > 1) {
    p <- adonis2(f, data = df_clean, method = "bray")$`Pr(>F)`[1]
  } else {
    p <- NA
  }
  
  data.frame(variable = v, p_beta = p)
}) %>% bind_rows()

# 🔹 Unisci alpha e beta
p_table_final <- left_join(p_table_alpha, beta_pvalues, by = "variable") %>%
  pivot_wider(names_from = Tempo,
              values_from = c(p_shannon, p_simpson, p_observed),
              names_glue = "{Tempo}_{.value}")

# Visualizza la tabella finale
p_table_final




























# tabella alpha
library(tidyverse)

# Tempi da analizzare
tempi <- c("T0", "T1", "T12")

# Variabili da testare
vars <- c("MEDAS_group", "sex", "age_at_T0", "naive", "BMI_group",
          "relapse", "lesioni", "cortisone", "EDSS")

# Funzione per calcolare p-value per ogni tempo e ogni indice
get_alpha_pvalues <- function(df, tempo, vars) {
  
  df_tempo <- df %>% filter(Tempo == tempo)
  
  results <- lapply(vars, function(v) {
    
    test_metric <- function(metric) {
      if (is.numeric(df_tempo[[v]])) {
        cor.test(df_tempo[[v]], df_tempo[[metric]], method = "spearman")$p.value
      } else {
        n_groups <- length(unique(df_tempo[[v]]))
        if (n_groups == 2) {
          wilcox.test(df_tempo[[metric]] ~ df_tempo[[v]])$p.value
        } else {
          kruskal.test(df_tempo[[metric]] ~ df_tempo[[v]])$p.value
        }
      }
    }
    
    data.frame(
      variable = v,
      Tempo = tempo,
      p_shannon = test_metric("shannon"),
      p_simpson = test_metric("simpson"),
      p_observed = test_metric("observed")
    )
  })
  
  bind_rows(results)
}

# Applica la funzione ai tre tempi
p_table_alpha_long <- map_df(tempi, ~ get_alpha_pvalues(alpha_merged, .x, vars))

library(tidyverse)
library(gt)

# 🔹 Crea la riga finale con descrizione dei test
test_row <- data.frame(
  variable = "Statistic Test",
  Tempo = "",
  p_shannon = "Spearman (numerical variables) / Wilcoxon or Kruskal-Wallis (categorical variables)",
  p_simpson = "",
  p_observed = "",
  stringsAsFactors = FALSE
)

# 🔹 Converte i p-value in carattere per poterli unire
p_table_alpha_long_chr <- p_table_alpha_long %>%
  mutate(across(c(p_shannon, p_simpson, p_observed), as.character))

# 🔹 Crea la riga finale con descrizione dei test
test_row <- data.frame(
  variable = "Statistic Test",
  Tempo = "",
  p_shannon = "Spearman (numerical variables) / Wilcoxon or Kruskal-Wallis (categorical variables)",
  p_simpson = "",
  p_observed = "",
  stringsAsFactors = FALSE
)

# 🔹 Rimuove eventuali righe duplicate con la stessa dicitura
p_table_alpha_long_clean <- p_table_alpha_long_chr %>%
  filter(variable != "Spearman (numerical variables) / Wilcoxon or Kruskal-Wallis (categorical variables)")

# 🔹 Aggiunge la riga finale
p_table_alpha_long_final <- bind_rows(p_table_alpha_long_clean, test_row)

# 🔹 Crea la tabella finale con formattazione uniforme
tbl <- gt(p_table_alpha_long_final) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(100)) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(10), align = "center")
    ),
    locations = cells_body(rows = variable == "Statistic Test")
  )

# 🔹 Salva come PNG
gtsave(tbl, filename = "C:/AURI/alpha_table.png")







library(tidyverse)
library(gt)

# Arrotonda i p-value a 3 cifre decimali
p_table_alpha_long_round <- p_table_alpha_long %>%
  mutate(
    p_shannon = round(p_shannon, 3),
    p_simpson = round(p_simpson, 3),
    p_observed = round(p_observed, 3)
  )

# Converte in carattere per poter aggiungere la riga finale
p_table_alpha_long_chr <- p_table_alpha_long_round %>%
  mutate(across(c(p_shannon, p_simpson, p_observed), as.character))

# Crea la riga finale con descrizione dei test (una sola riga)
test_row <- data.frame(
  variable = "Statistic Test",
  Tempo = "",
  p_shannon = "Spearman (numerical variables) / Wilcoxon or Kruskal-Wallis (categorical variables)",
  p_simpson = "",
  p_observed = "",
  stringsAsFactors = FALSE
)

# Rimuove eventuali righe duplicate
p_table_alpha_long_clean <- p_table_alpha_long_chr %>%
  filter(variable != "Spearman (numerical variables) / Wilcoxon or Kruskal-Wallis (categorical variables)")

# Aggiunge la riga finale
p_table_alpha_long_final <- bind_rows(p_table_alpha_long_clean, test_row)

# 🔹 Crea la tabella con spaziatura uniforme e riga finale su una sola linea
tbl <- gt(p_table_alpha_long_final) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(120),
    Tempo ~ px(80),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(10), align = "center")
    ),
    locations = cells_body(rows = variable == "Statistic Test")
  ) %>%
  text_transform(
    locations = cells_body(rows = variable == "Statistic Test", columns = everything()),
    fn = function(x) {
      ifelse(x == "", "Spearman (numerical variables) / Wilcoxon or Kruskal-Wallis (categorical variables)", x)
    }
  )

# Salva come PNG
gtsave(tbl, filename = "C:/AURI/alpha_table_arrotondata.png")





# definitiva
library(tidyverse)
library(gt)

# Arrotonda i p-value a 3 cifre decimali
p_table_alpha_long_round <- p_table_alpha_long %>%
  mutate(
    p_shannon = round(p_shannon, 3),
    p_simpson = round(p_simpson, 3),
    p_observed = round(p_observed, 3)
  )

# 🔹 Aggiunge gli asterischi in base al test utilizzato
p_table_alpha_long_annotated <- p_table_alpha_long_round %>%
  rowwise() %>%
  mutate(
    test_type = case_when(
      is.numeric(alpha_merged[[variable]]) ~ "*",  # Spearman
      length(unique(alpha_merged[[variable]])) == 2 ~ "**",  # Wilcoxon
      length(unique(alpha_merged[[variable]])) > 2 ~ "***",  # Kruskal-Wallis
      TRUE ~ ""
    ),
    p_shannon = paste0(p_shannon, test_type),
    p_simpson = paste0(p_simpson, test_type),
    p_observed = paste0(p_observed, test_type)
  ) %>%
  ungroup() %>%
  select(-test_type)

# 🔹 Converte in carattere per poter aggiungere la riga finale
p_table_alpha_long_chr <- p_table_alpha_long_annotated %>%
  mutate(across(c(p_shannon, p_simpson, p_observed), as.character))

# 🔹 Crea la tabella finale (senza la riga “Statistic Test”)
tbl <- gt(p_table_alpha_long_chr) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(120),
    Tempo ~ px(80),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  ) %>%
  # 🔹 Aggiunge la legenda finale
  tab_source_note(
    source_note = md("**Legenda:**  
    * → Spearman test (numerical variables)  
    ** → Wilcoxon test (2 categorical groups)  
    *** → Kruskal-Wallis test (>2 categorical groups)")
  )

# 🔹 Salva come PNG
gtsave(tbl, filename = "C:/AURI/alpha_table_legenda_finale.png")





library(tidyverse)
library(gt)

# Arrotonda i p-value a 3 cifre decimali
p_table_alpha_long_round <- p_table_alpha_long %>%
  mutate(
    p_shannon = round(p_shannon, 3),
    p_simpson = round(p_simpson, 3),
    p_observed = round(p_observed, 3)
  )

# 🔹 Aggiunge gli asterischi in base al test utilizzato
p_table_alpha_long_annotated <- p_table_alpha_long_round %>%
  rowwise() %>%
  mutate(
    test_type = case_when(
      is.numeric(alpha_merged[[variable]]) ~ "*",  # Spearman
      length(unique(alpha_merged[[variable]])) == 2 ~ "**",  # Wilcoxon
      length(unique(alpha_merged[[variable]])) > 2 ~ "***",  # Kruskal-Wallis
      TRUE ~ ""
    ),
    p_shannon = paste0(p_shannon, test_type),
    p_simpson = paste0(p_simpson, test_type),
    p_observed = paste0(p_observed, test_type)
  ) %>%
  ungroup() %>%
  select(-test_type)

# 🔹 Traduzione dei nomi delle variabili
p_table_alpha_long_annotated <- p_table_alpha_long_annotated %>%
  mutate(
    variable = recode(variable,
                      "MEDAS_group" = "MEDAS group",
                      "sex" = "Sex",
                      "age_at_T0" = "Baseline age",
                      "naive" = "Naïve status",
                      "BMI_group" = "BMI group",
                      "relapse" = "Relapse",
                      "cortisone" = "Corticosteroid",
                      "EDSS" = "EDSS"
    )
  )

# 🔹 Converte in carattere per la tabella
p_table_alpha_long_chr <- p_table_alpha_long_annotated %>%
  mutate(across(c(p_shannon, p_simpson, p_observed), as.character))

# 🔹 Crea la tabella finale con legenda
tbl <- gt(p_table_alpha_long_chr) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(140),
    Tempo ~ px(80),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  ) %>%
  tab_source_note(
    source_note = md("**Legend:**  
    * → Spearman test (numerical variables)  
    ** → Wilcoxon test (2 categorical groups)  
    *** → Kruskal-Wallis test (>2 categorical groups)")
  )

# 🔹 Salva come PNG
gtsave(tbl, filename = "C:/AURI//alpha_table_final_english.png")




library(tidyverse)
library(gt)

# Arrotonda i p-value a 3 cifre decimali
p_table_alpha_long_round <- p_table_alpha_long %>%
  mutate(
    p_shannon = round(p_shannon, 3),
    p_simpson = round(p_simpson, 3),
    p_observed = round(p_observed, 3)
  )

# 🔹 Aggiunge gli asterischi in base al test utilizzato
p_table_alpha_long_annotated <- p_table_alpha_long_round %>%
  rowwise() %>%
  mutate(
    test_type = case_when(
      is.numeric(alpha_merged[[variable]]) ~ "*",  # Spearman
      length(unique(alpha_merged[[variable]])) == 2 ~ "**",  # Wilcoxon
      length(unique(alpha_merged[[variable]])) > 2 ~ "***",  # Kruskal-Wallis
      TRUE ~ ""
    ),
    p_shannon = paste0(p_shannon, test_type),
    p_simpson = paste0(p_simpson, test_type),
    p_observed = paste0(p_observed, test_type)
  ) %>%
  ungroup() %>%
  select(-test_type)

# 🔹 Traduzione dei nomi delle variabili
p_table_alpha_long_annotated <- p_table_alpha_long_annotated %>%
  mutate(
    variable = recode(variable,
                      "MEDAS_group" = "MEDAS group",
                      "sex" = "Sex",
                      "age_at_T0" = "Baseline age",
                      "naive" = "Naïve status",
                      "BMI_group" = "BMI group",
                      "relapse" = "Relapse",
                      "lesioni" = "Lesions",
                      "cortisone" = "Corticosteroid",
                      "EDSS" = "EDSS"
    )
  )

# 🔹 Converte in carattere per la tabella
p_table_alpha_long_chr <- p_table_alpha_long_annotated %>%
  mutate(across(c(p_shannon, p_simpson, p_observed), as.character))

# 🔹 Crea la tabella finale con intestazioni aggiornate
tbl <- gt(p_table_alpha_long_chr) %>%
  cols_label(
    variable = "Variable",
    Tempo = "Timepoint",
    p_shannon = "p_shannon",
    p_simpson = "p_simpson",
    p_observed = "p_observed"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(140),
    Tempo ~ px(100),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  ) %>%
  tab_source_note(
    source_note = md("**Legend:**  
    * → Spearman test (numerical variables)  
    ** → Wilcoxon test (2 categorical groups)  
    *** → Kruskal-Wallis test (>2 categorical groups)")
  )

# 🔹 Salva come PNG
gtsave(tbl, filename = "C:/AURI/alpha_table_final_inglese.png")







library(tidyverse)
library(gt)

# Arrotonda i p-value a 3 cifre decimali
p_table_alpha_long_round <- p_table_alpha_long %>%
  mutate(
    p_shannon = round(p_shannon, 3),
    p_simpson = round(p_simpson, 3),
    p_observed = round(p_observed, 3)
  )

# 🔹 Traduzione dei nomi delle variabili
p_table_alpha_long_annotated <- p_table_alpha_long_round %>%
  mutate(
    variable = recode(variable,
                      "MEDAS_group" = "MEDAS group",
                      "sex" = "Sex",
                      "age_at_T0" = "Baseline age",
                      "naive" = "Naïve status",
                      "BMI_group" = "BMI group",
                      "relapse" = "Relapse",
                      "lesioni" = "Lesions",
                      "cortisone" = "Corticosteroid",
                      "EDSS" = "EDSS"
    )
  )

# 🔹 Converte in carattere per la tabella
p_table_alpha_long_chr <- p_table_alpha_long_annotated %>%
  mutate(across(c(p_shannon, p_simpson, p_observed), as.character))

# 🔹 Crea la tabella finale con intestazioni aggiornate
tbl <- gt(p_table_alpha_long_chr) %>%
  cols_label(
    variable = "Variable",
    Tempo = "Timepoint",
    p_shannon = "p_shannon",
    p_simpson = "p_simpson",
    p_observed = "p_observed"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(140),
    Tempo ~ px(100),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  )

# 🔹 Salva come PNG
gtsave(tbl, filename = "C:/AURI/alpha_table_final_inglese_def.png")

#####################   questa è quella UFFICIALE ###############



































# nomi euk in PurrBiom
library(tidyverse)

# Percorso della cartella dei report
path <- "C:/AURI/Thesis_bagetto/bracken/bracken"

# Lista dei file .report
files <- list.files(path, pattern = "*.report", full.names = TRUE)

for (f in files) {
  df <- read.delim(f, sep = "\t", header = TRUE)
  
  # Sostituisci taxonomy_id con il nome della specie
  df$taxonomy_id <- df$name
  
  # Sovrascrivi il file
  write.table(df, f, sep = "\t", quote = FALSE, row.names = FALSE)
}





















# test appaiati
metadata <- read.csv("C:/AURI/Thesis_bagetto/merged_final.csv", 
                     sep = ";", header = TRUE)
library(tidyverse)

path_bracken <- "C:/AURI/Thesis_bagetto/RstudioXPurrBiom/bracken"

bracken_list <- list.files(path_bracken, pattern = "*.report", full.names = TRUE) %>%
  set_names(basename(.) %>% str_remove("_bracken.report")) %>%
  map(~ read.table(.x, header = FALSE, sep = "\t",
                   col.names = c("fraction_total_reads", "taxonomy_id", "unclassified_reads",
                                 "taxonomy_lvl", "taxon_reads", "name")))

feature_table <- bracken_list %>%
  bind_rows(.id = "sample") %>%
  group_by(sample, name) %>%
  summarise(fraction_total_reads = sum(fraction_total_reads), .groups = "drop") %>%
  pivot_wider(names_from = name,
              values_from = fraction_total_reads,
              values_fill = 0)

library(dplyr)
library(vegan)
library(purrr)

library(dplyr)
library(vegan)
library(purrr)

# 1️⃣ Calcolo indici di diversità
shannon <- diversity(feature_table[,-1], index = "shannon")
simpson <- diversity(feature_table[,-1], index = "simpson")
observed <- rowSums(feature_table[,-1] > 0)

alpha_df <- data.frame(
  sample = feature_table$sample,
  shannon = shannon,
  simpson = simpson,
  observed = observed
)

# 2️⃣ Merge con metadata
alpha_merged <- left_join(alpha_df, metadata, by = c("sample" = "ID_long"))

# 3️⃣ Variabili da analizzare
vars <- c("MEDAS_group", "sex", "naive", "BMI_group",
          "relapse", "lesioni", "cortisone", "EDSS")

# 4️⃣ Funzione per test appaiato T0 vs T12 per ogni variabile
paired_test_var <- function(var) {
  
  lvls <- unique(alpha_merged[[var]])
  
  map_dfr(lvls, function(lvl) {
    
    df_sub <- alpha_merged %>% filter(.data[[var]] == lvl)
    
    # 🔹 Usa ID (paziente) per appaiare
    df_T0 <- df_sub %>% filter(Tempo == "T0") %>% arrange(ID)
    df_T12 <- df_sub %>% filter(Tempo == "T12") %>% arrange(ID)
    
    # 🔹 Trova i pazienti presenti in entrambi i tempi
    common <- intersect(df_T0$ID, df_T12$ID)
    df_T0 <- df_T0 %>% filter(ID %in% common)
    df_T12 <- df_T12 %>% filter(ID %in% common)
    
    # 🔹 Se non ci sono coppie → NA
    if (nrow(df_T0) == 0 | nrow(df_T12) == 0) {
      return(data.frame(
        variable = var,
        level = as.character(lvl),
        p_shannon = NA,
        p_simpson = NA,
        p_observed = NA
      ))
    }
    
    # 🔹 Test appaiato con exact = FALSE (evita errore ties)
    data.frame(
      variable = var,
      level = as.character(lvl),
      p_shannon = wilcox.test(df_T0$shannon, df_T12$shannon,
                              paired = TRUE, exact = FALSE)$p.value,
      p_simpson = wilcox.test(df_T0$simpson, df_T12$simpson,
                              paired = TRUE, exact = FALSE)$p.value,
      p_observed = wilcox.test(df_T0$observed, df_T12$observed,
                               paired = TRUE, exact = FALSE)$p.value
    )
  })
}

# 5️⃣ Applica la funzione a tutte le variabili
paired_results <- map_dfr(vars, paired_test_var)

# 6️⃣ Arrotonda e traduce
paired_results <- paired_results %>%
  mutate(
    p_shannon = round(p_shannon, 3),
    p_simpson = round(p_simpson, 3),
    p_observed = round(p_observed, 3),
    variable = recode(variable,
                      "MEDAS_group" = "MEDAS group",
                      "sex" = "Sex",
                      "naive" = "Naïve status",
                      "BMI_group" = "BMI group",
                      "relapse" = "Relapse",
                      "lesioni" = "Lesions",
                      "cortisone" = "Corticosteroid",
                      "EDSS" = "EDSS")
  )


library(gt)
library(tidyverse)

# Arrotonda i p-value a 3 cifre decimali (se non già fatto)
paired_results_round <- paired_results %>%
  mutate(
    p_shannon = round(p_shannon, 3),
    p_simpson = round(p_simpson, 3),
    p_observed = round(p_observed, 3)
  )

# Converte in carattere per evitare problemi di formattazione
paired_results_chr <- paired_results_round %>%
  mutate(across(c(p_shannon, p_simpson, p_observed), as.character))

# Crea tabella GT
tbl <- gt(paired_results_chr) %>%
  cols_label(
    variable = "Variable",
    level = "Level",
    p_shannon = "Shannon (p)",
    p_simpson = "Simpson (p)",
    p_observed = "Observed (p)"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(160),
    level ~ px(120),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  ) %>%
  tab_source_note(
    source_note = md("**Legend:**  
    * Wilcoxon paired test (T0 vs T12, exact = FALSE)  
    * p-values rounded to 3 decimals")
  )

# Salva come PNG
gtsave(tbl, filename = "C:/AURI/paired_alpha_table.png")



# senza NA
library(dplyr)
library(vegan)
library(purrr)
library(gt)

# 1️⃣ Calcolo indici di diversità
shannon <- diversity(feature_table[,-1], index = "shannon")
simpson <- diversity(feature_table[,-1], index = "simpson")
observed <- rowSums(feature_table[,-1] > 0)

alpha_df <- data.frame(
  sample = feature_table$sample,
  shannon = shannon,
  simpson = simpson,
  observed = observed
)

# 2️⃣ Merge con metadata
alpha_merged <- left_join(alpha_df, metadata, by = c("sample" = "ID_long"))

# 3️⃣ Variabili da analizzare
vars <- c("MEDAS_group", "sex", "naive", "BMI_group",
          "relapse", "lesioni", "cortisone", "EDSS")

# 4️⃣ Funzione per test appaiato T0 vs T12 per ogni variabile
paired_test_var <- function(var) {
  
  lvls <- unique(alpha_merged[[var]])
  
  map_dfr(lvls, function(lvl) {
    
    df_sub <- alpha_merged %>% filter(.data[[var]] == lvl)
    
    df_T0 <- df_sub %>% filter(Tempo == "T0") %>% arrange(ID)
    df_T12 <- df_sub %>% filter(Tempo == "T12") %>% arrange(ID)
    
    common <- intersect(df_T0$ID, df_T12$ID)
    df_T0 <- df_T0 %>% filter(ID %in% common)
    df_T12 <- df_T12 %>% filter(ID %in% common)
    
    if (nrow(df_T0) == 0 | nrow(df_T12) == 0) {
      return(data.frame(
        variable = var,
        level = as.character(lvl),
        p_shannon = NA,
        p_simpson = NA,
        p_observed = NA
      ))
    }
    
    data.frame(
      variable = var,
      level = as.character(lvl),
      p_shannon = wilcox.test(df_T0$shannon, df_T12$shannon,
                              paired = TRUE, exact = FALSE)$p.value,
      p_simpson = wilcox.test(df_T0$simpson, df_T12$simpson,
                              paired = TRUE, exact = FALSE)$p.value,
      p_observed = wilcox.test(df_T0$observed, df_T12$observed,
                               paired = TRUE, exact = FALSE)$p.value
    )
  })
}

# 5️⃣ Applica la funzione a tutte le variabili
paired_results <- map_dfr(vars, paired_test_var)

# 6️⃣ Arrotonda e traduce
paired_results <- paired_results %>%
mutate(
  p_shannon = round(p_shannon, 3),
  p_simpson = round(p_simpson, 3),
  p_observed = round(p_observed, 3),
  variable = recode(variable,
                  
  "MEDAS_group" = "MEDAS group",
 
                    "sex" = "Sex",
                    
                    "naive" = "Naïve status",
                    
                    "BMI_group" = "BMI group",
                    
                    "relapse" = "Relapse",
                    
                    "lesioni" = "Lesions",
                    
                    "cortisone" = "Corticosteroid",
                    
                    "EDSS" = "EDSS")
  
) %>%
  
mutate(
  
  p_shannon = ifelse(p_shannon < 0.05,
                     
                     paste0(sprintf("%.3f", p_shannon), "*"),
                     
                     sprintf("%.3f", p_shannon)),
  
  p_simpson = ifelse(p_simpson < 0.05,
                     paste0(sprintf("%.3f", p_simpson), "*"),
                     
                     sprintf("%.3f", p_simpson)),
  
  p_observed = ifelse(p_observed < 0.05,
                      
                      paste0(sprintf("%.3f", p_observed), "*"),
                      
                      sprintf("%.3f", p_observed))
  
)

# 7️⃣ Rimuovi tutte le righe con NA
paired_results_clean <- paired_results %>%
  filter(!is.na(p_shannon) & !is.na(p_simpson) & !is.na(p_observed))


# 9️⃣ Crea tabella GT
tbl <- gt(paired_results_clean) %>%
  cols_label(
    variable = "Variable",
    level = "Level",
    p_shannon = "Shannon (p)",
    p_simpson = "Simpson (p)",
    p_observed = "Observed (p)"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(160),
    level ~ px(120),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  ) %>%
  tab_source_note(
    source_note = md("**Legend:**  
    Wilcoxon paired test (T0 vs T12, exact = FALSE).  
    Only groups with valid paired samples are shown.")
  )

# 🔟 Salva come PNG
gtsave(tbl, filename = "C:/AURI/paired_alpha_table_clean.png")


# paired_alpha_table_clean_asterischi
library(dplyr)
library(vegan)
library(purrr)
library(gt)

# 1️⃣ Calcolo indici di diversità
shannon <- diversity(feature_table[,-1], index = "shannon")
simpson <- diversity(feature_table[,-1], index = "simpson")
observed <- rowSums(feature_table[,-1] > 0)

alpha_df <- data.frame(
  sample = feature_table$sample,
  shannon = shannon,
  simpson = simpson,
  observed = observed
)

# 2️⃣ Merge con metadata
alpha_merged <- left_join(alpha_df, metadata, by = c("sample" = "ID_long"))

# 3️⃣ Variabili da analizzare
vars <- c("MEDAS_group", "sex", "naive", "BMI_group",
          "relapse", "lesioni", "cortisone", "EDSS")

# 4️⃣ Funzione per test appaiato T0 vs T12 per ogni variabile
paired_test_var <- function(var) {
  
  lvls <- unique(alpha_merged[[var]])
  
  map_dfr(lvls, function(lvl) {
    
    df_sub <- alpha_merged %>% filter(.data[[var]] == lvl)
    
    df_T0 <- df_sub %>%
      filter(Tempo == "T0") %>%
      arrange(ID)
    
    df_T12 <- df_sub %>%
      filter(Tempo == "T12") %>%
      arrange(ID)
    
    common <- intersect(df_T0$ID, df_T12$ID)
    
    df_T0 <- df_T0 %>% filter(ID %in% common)
    df_T12 <- df_T12 %>% filter(ID %in% common)
    
    if (nrow(df_T0) == 0 | nrow(df_T12) == 0) {
      return(data.frame(
        variable = var,
        level = as.character(lvl),
        p_shannon = NA,
        p_simpson = NA,
        p_observed = NA
      ))
    }
    
    data.frame(
      variable = var,
      level = as.character(lvl),
      p_shannon = wilcox.test(
        df_T0$shannon,
        df_T12$shannon,
        paired = TRUE,
        exact = FALSE
      )$p.value,
      
      p_simpson = wilcox.test(
        df_T0$simpson,
        df_T12$simpson,
        paired = TRUE,
        exact = FALSE
      )$p.value,
      
      p_observed = wilcox.test(
        df_T0$observed,
        df_T12$observed,
        paired = TRUE,
        exact = FALSE
      )$p.value
    )
  })
}

# 5️⃣ Applica la funzione a tutte le variabili
paired_results <- map_dfr(vars, paired_test_var)

# 6️⃣ Arrotonda, traduce e aggiunge asterisco ai valori significativi
paired_results <- paired_results %>%
  mutate(
    variable = recode(
      variable,
      "MEDAS_group" = "MEDAS group",
      "sex" = "Sex",
      "naive" = "Naïve status",
      "BMI_group" = "BMI group",
      "relapse" = "Relapse",
      "lesioni" = "Lesions",
      "cortisone" = "Corticosteroid",
      "EDSS" = "EDSS"
    ),
    
    p_shannon_num = round(p_shannon, 3),
    p_simpson_num = round(p_simpson, 3),
    p_observed_num = round(p_observed, 3)
  ) %>%
  mutate(
    p_shannon = ifelse(
      is.na(p_shannon_num),
      NA,
      ifelse(
        p_shannon_num < 0.05,
        paste0(sprintf("%.3f", p_shannon_num), "*"),
        sprintf("%.3f", p_shannon_num)
      )
    ),
    
    p_simpson = ifelse(
      is.na(p_simpson_num),
      NA,
      ifelse(
        p_simpson_num < 0.05,
        paste0(sprintf("%.3f", p_simpson_num), "*"),
        sprintf("%.3f", p_simpson_num)
      )
    ),
    
    p_observed = ifelse(
      is.na(p_observed_num),
      NA,
      ifelse(
        p_observed_num < 0.05,
        paste0(sprintf("%.3f", p_observed_num), "*"),
        sprintf("%.3f", p_observed_num)
      )
    )
  ) %>%
  select(
    variable,
    level,
    p_shannon,
    p_simpson,
    p_observed
  )

# 7️⃣ Rimuovi tutte le righe con NA
paired_results_clean <- paired_results %>%
  filter(
    !is.na(p_shannon) &
      !is.na(p_simpson) &
      !is.na(p_observed)
  )

# 8️⃣ Crea tabella GT
tbl <- gt(paired_results_clean) %>%
  cols_label(
    variable = "Variable",
    level = "Level",
    p_shannon = "Shannon (p)",
    p_simpson = "Simpson (p)",
    p_observed = "Observed (p)"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6)
  ) %>%
  cols_width(
    variable ~ px(160),
    level ~ px(120),
    p_shannon ~ px(120),
    p_simpson ~ px(120),
    p_observed ~ px(120)
  ) %>%
  tab_source_note(
    source_note = md(
      "**Legend:**
Wilcoxon paired test (T0 vs T12, exact = FALSE), * p < 0.05.
Only groups with valid paired samples are shown."
    )
  )

# 9️⃣ Salva come PNG
gtsave(
  tbl,
  filename = "C:/AURI/paired_alpha_table_clean.png"
)























######EUK#######
# bmi_T0
library(magick)

# Percorso delle immagini
path <- "C:/AURI/Thesis_bagetto/app/stacked_bar_plots/nuove/euk/png"

# Carica le immagini
img_A <- image_read(file.path(path, "shannon_EUK_bmi_T0_page-0001.jpg"))
img_B <- image_read(file.path(path, "simpson_EUK_bmi_T0_page-0001.jpg"))
img_C <- image_read(file.path(path, "observed_EUK_bmi_T0_page-0001.jpg"))

# Ridimensiona tutte alla stessa altezza
target_height <- 900
img_A <- image_resize(img_A, paste0("x", target_height))
img_B <- image_resize(img_B, paste0("x", target_height))
img_C <- image_resize(img_C, paste0("x", target_height))


# Aggiungi etichette A-C sopra e a destra di ciascun grafico
img_A <- image_annotate(img_A, "A", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_B <- image_annotate(img_B, "B", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_C <- image_annotate(img_C, "C", size = 80, color = "black", gravity = "northeast", location = "+40+40")

# Unisci le immagini in una riga orizzontale perfettamente allineata
collage <- image_append(c(img_A, img_B, img_C), stack = FALSE)

# Aumenta la risoluzione per maggiore nitidezza
collage <- image_resize(collage, "6500x")

# Salva il collage finale
image_write(collage, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/EUK_collage_bmi_T0_highres.png", format = "png")



# bmi T1
library(magick)

# Percorso delle immagini
path <- "C:/AURI/Thesis_bagetto/app/stacked_bar_plots/nuove/euk/png"

# Carica le immagini
img_D <- image_read(file.path(path, "shannon_EUK_bmi_T1_page-0001.jpg"))
img_E <- image_read(file.path(path, "simpson_EUK_bmi_T1_page-0001.jpg"))
img_F <- image_read(file.path(path, "observed_EUK_bmi_T1_page-0001.jpg"))


# Ridimensiona tutte alla stessa altezza
target_height <- 900
img_D <- image_resize(img_D, paste0("x", target_height))
img_E <- image_resize(img_E, paste0("x", target_height))
img_F <- image_resize(img_F, paste0("x", target_height))

# Aggiungi etichette D-F sopra e a destra di ciascun grafico
img_D <- image_annotate(img_D, "E", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_E <- image_annotate(img_E, "F", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_F <- image_annotate(img_F, "G", size = 80, color = "black", gravity = "northeast", location = "+40+40")

# Unisci le immagini in una riga orizzontale perfettamente allineata
collage <- image_append(c(img_D, img_E, img_F), stack = FALSE)

# Aumenta la risoluzione per maggiore nitidezza
collage <- image_resize(collage, "6500x")

# Salva il collage finale
image_write(collage, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/EUK_collage_bmi_T1_highres.png", format = "png")




#bmi T12
library(magick)

# Percorso delle immagini
path <- "C:/AURI/Thesis_bagetto/app/stacked_bar_plots/nuove/euk/png"

# Carica le immagini
img_G <- image_read(file.path(path, "shannon_EUK_bmi_T12_page-0001.jpg"))
img_H <- image_read(file.path(path, "simpson_EUK_bmi_T12_page-0001.jpg"))
img_I <- image_read(file.path(path, "observed_EUK_bmi_T12_page-0001.jpg"))


# Ridimensiona tutte alla stessa altezza
target_height <- 900
img_G <- image_resize(img_G, paste0("x", target_height))
img_H <- image_resize(img_H, paste0("x", target_height))
img_I <- image_resize(img_I, paste0("x", target_height))

# Aggiungi etichette I–L sopra e a destra di ciascun grafico
img_G <- image_annotate(img_G, "I", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_H <- image_annotate(img_H, "J", size = 80, color = "black", gravity = "northeast", location = "+40+40")
img_I <- image_annotate(img_I, "K", size = 80, color = "black", gravity = "northeast", location = "+40+40")

# Unisci le immagini in una riga orizzontale perfettamente allineata
collage <- image_append(c(img_G, img_H, img_I), stack = FALSE)

# Aumenta la risoluzione per maggiore nitidezza
collage <- image_resize(collage, "6500x")

# Salva il collage finale
image_write(collage, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/EUK_collage_bmi_T12_highres.png", format = "png")



# bmi tot
library(magick)

# Percorso della cartella
path <- "C:/AURI/Thesis_Bagetto/grafici_coorte_totale"

# Carica le tre immagini
img_T0 <- image_read(file.path(path, "EUK_collage_bmi_T0_highres.png"))
img_T1 <- image_read(file.path(path, "EUK_collage_bmi_T1_highres.png"))
img_T12 <- image_read(file.path(path, "EUK_collage_bmi_T12_highres.png"))

# Ridimensiona tutte alla stessa larghezza per allineamento perfetto
target_width <- 6500
img_T0 <- image_resize(img_T0, paste0(target_width, "x"))
img_T1 <- image_resize(img_T1, paste0(target_width, "x"))
img_T12 <- image_resize(img_T12, paste0(target_width, "x"))

# Unisci le immagini in verticale (una sotto l’altra)
collage_finale <- image_append(c(img_T0, img_T1, img_T12), stack = TRUE)

# Aumenta la risoluzione per maggiore nitidezza
collage_finale <- image_resize(collage_finale, "6500x")

# Salva il collage finale
image_write(collage_finale, "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/EUK_collage_bmi_T0_T1_T12_highres.png", format = "png")








# tab intro paper
# Carica i pacchetti necessari
library(tibble)
library(gridExtra)
library(ggplot2)
library(grid)

# Crea la tabella
ms_microbiome <- tibble(
  Authors = c("Miyake et al.", "Chen et al.", "Jangi et al.",
              "Tremlett et al.", "Cekanaviciute et al.", "Berer et al.",
              "Ling et al."),
  Patients = c("20 (RRMS)", "31 (RRMS)", "60 (RRMS)",
               "18 (RRMS)", "71 (RRMS)",
               "22 (RRMS), 7 (SPMS), 3 (CIS), 2 (PPMS)",
               "22 (RRMS)"),
  Altered_OTUs = c(
    "↑ Eggerthella lenta, Streptococcus thermophilus/salivarius; ↓ Clostridium spp., Faecalibacterium prausnitzii, Anaerostipes hadrus",
    "↑ Pseudomonas, Pedobacter, Mycoplana, Blautia",
    "↑ Akkermansia, Methanobrevibacter, Butyricimonas, Parabacteroides, Haemophilus, Slackia",
    "↑ Bilophila, Bifidobacterium, Desulfovibrio, Christensenellaceae",
    "↑ Acinetobacter calcoaceticus, Akkermansia muciniphila, Eggerthella lenta",
    "↑ Akkermansia muciniphila",
    "↓ Faecalibacterium"
  )
)

# Converte la tabella in grafica
table_plot <- tableGrob(ms_microbiome)

# Salva come PNG
png("C:/AURI/Thesis_Bagetto/ms_microbiome_table.png",
    width = 1800, height = 1000, res = 180)
grid.draw(table_plot)
dev.off()


# senza tagli
library(tibble)
library(gridExtra)
library(grid)

# Crea la tabella
ms_microbiome <- tibble(
  Authors = c("Miyake et al.", "Chen et al.", "Jangi et al.",
              "Tremlett et al.", "Cekanaviciute et al.", "Berer et al.",
              "Ling et al."),
  Patients = c("20 (RRMS)", "31 (RRMS)", "60 (RRMS)",
               "18 (RRMS)", "71 (RRMS)",
               "22 (RRMS), 7 (SPMS), 3 (CIS), 2 (PPMS)",
               "22 (RRMS)"),
  Altered_OTUs = c(
    "↑ Eggerthella lenta, Streptococcus thermophilus/salivarius; ↓ Clostridium spp., Faecalibacterium prausnitzii, Anaerostipes hadrus",
    "↑ Pseudomonas, Pedobacter, Mycoplana, Blautia",
    "↑ Akkermansia, Methanobrevibacter, Butyricimonas, Parabacteroides, Haemophilus, Slackia",
    "↑ Bilophila, Bifidobacterium, Desulfovibrio, Christensenellaceae",
    "↑ Acinetobacter calcoaceticus, Akkermansia muciniphila, Eggerthella lenta",
    "↑ Akkermansia muciniphila",
    "↓ Faecalibacterium"
  )
)


# Wrapping del testo per evitare tagli
wrap_text <- function(x, width = 60) {
  sapply(strwrap(x, width = width, simplify = FALSE), paste, collapse = "\n")
}
ms_microbiome$Altered_OTUs <- wrap_text(ms_microbiome$Altered_OTUs, width = 60)

# Crea la tabella grafica con celle adattate
table_plot <- tableGrob(ms_microbiome, rows = NULL)

# Salva come PNG con dimensioni maggiori
png("C:/AURI/Thesis_Bagetto/ms_microbiome_table_wrapped.png",
    width = 2400, height = 1400, res = 200)
grid.draw(table_plot)
dev.off()






#modifico grafici maaslin
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
  
  # Posizione della barra e degli asterischi sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  # Costruzione del grafico
  p2 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività tra T0 e T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar), color = "black", size = 0.8) +
    geom_text(aes(x = 2, y = y_star), label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Bacteroides intestinalis",
      subtitle = paste0("Beta coefficient: 1.691 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar_T1 <- y_max + 0.5
  y_star_T1 <- y_max + 0.7
  
  y_bar_T12 <- y_max + 1.0
  y_star_T12 <- y_max + 1.2
  
  p <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra + asterisco T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar_T1, yend = y_bar_T1),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 1.5, y = y_star_T1),
              label = stars_T1, color = "red", size = 8, fontface = "bold") +
    
    # 🔹 Barra + asterisco T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar_T12, yend = y_bar_T12),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 2, y = y_star_T12),
              label = stars_T12, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Faecalibacterium_sp. I2-3-92",
      subtitle = paste0("Beta coefficients: T1 = 0.9779 (p = ", pvalue_T1,
                        "), T12 = 0.9772 (p = ", pvalue_T12, ")"),
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
    filename = "boxplot_Faecalibacterium_sp.I2-3-92.png",
    plot = p,
    path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
    width = 10, height = 8, dpi = 300
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p3 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 2, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Thomasclavelia_spiroformis",
      subtitle = paste0("Beta coefficient: -2.064 | pvalue: ", pvalue, " | Value: ", value),
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
get_stars <- function(p){
  if(p < 0.001) return("***")
  if(p < 0.01)  return("**")
  if(p < 0.05)  return("*")
  return("")
}

sp <- "Lacticaseibacillus_paracasei"
pvalue_T1 <- 0.026
pvalue_T12 <- 0.006
stars_T1 <- get_stars(pvalue_T1)
stars_T12 <- get_stars(pvalue_T12)

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  
  # Barra T0–T1
  y_bar_T1  <- y_max + 0.5
  y_star_T1 <- y_max + 0.7
  
  # Barra T0–T12
  y_bar_T12  <- y_max + 1.0
  y_star_T12 <- y_max + 1.2
  
  p4 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra + asterisco T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar_T1, yend = y_bar_T1),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 1.5, y = y_star_T1),
              label = stars_T1, color = "red", size = 8, fontface = "bold") +
    
    # 🔹 Barra + asterisco T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar_T12, yend = y_bar_T12),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 2, y = y_star_T12),
              label = stars_T12, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Lacticaseibacillus paracasei",
      subtitle = paste0("Beta coefficients: T1 = -3.014 (p = ", pvalue_T1,
                        "), T12 = -3.750 (p = ", pvalue_T12, ")"),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p5 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 1.5, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Alistipes_megaguti",
      subtitle = paste0("Beta coefficient: -1.364 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p6 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 2, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Coprobacter_secundus",
      subtitle = paste0("Beta coefficient: 2.24 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 1.5, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Faecalibacterium_prausnitzii",
      subtitle = paste0("Beta coefficient: 0.896 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  
  # Barra T0–T1
  y_bar_T1  <- y_max + 0.5
  y_star_T1 <- y_max + 0.7
  
  # Barra T0–T12
  y_bar_T12  <- y_max + 1.0
  y_star_T12 <- y_max + 1.2
  
  p7 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra + asterisco T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar_T1, yend = y_bar_T1),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 1.5, y = y_star_T1),
              label = stars_T1, color = "red", size = 8, fontface = "bold") +
    
    # 🔹 Barra + asterisco T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar_T12, yend = y_bar_T12),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 2, y = y_star_T12),
              label = stars_T12, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Oxalobacter_aliiformigenes",
      subtitle = paste0("Beta coefficients: T1 = 2.786 (p = ", pvalue_T1,
                        "), T12 = 2.547 (p = ", pvalue_T12, ")"),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p8 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 2, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Segatella_hominis",
      subtitle = paste0("Beta coefficient: -1.919 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p9 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 2, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Parabacteroides_absconsus",
      subtitle = paste0("Beta coefficient: 1.287 | pvalue: ", pvalue, " | Value: ", value),
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
    width = 10, height = 8,
    dpi = 300
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p10 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 2, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Streptococcus_ilei",
      subtitle = paste0("Beta coefficient: -2.148 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p11 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 2, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Streptococcus_salivarius",
      subtitle = paste0("Beta coefficient: -1.609 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p12 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 2, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Thomasclavelia_ramosa",
      subtitle = paste0("Beta coefficient: -1.020 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p13 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 1.5, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Faecalibacterium_sp. I3-3-89",
      subtitle = paste0("Beta coefficient: 1.009 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p14 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 1.5, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Streptococcus_agalactiae",
      subtitle = paste0("Beta coefficient: 2.628 | pvalue: ", pvalue, " | Value: ", value),
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
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar <- y_max + 0.5
  y_star <- y_max + 0.7
  
  p15 <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra di significatività T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar, yend = y_bar),
                 color = "black", size = 0.8) +
    
    # 🔹 Asterisco sopra la barra
    geom_text(aes(x = 1.5, y = y_star),
              label = stars, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Sutterella_faecalis",
      subtitle = paste0("Beta coefficient: -3.563 | pvalue: ", pvalue, " | Value: ", value),
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



# collage t1 e t12
library(patchwork)

# 🔹 Riduci la dimensione dei titoli e aumenta lo spazio superiore
p4 <- p4 + theme(
  plot.title = element_text(size = 11, face = "bold", vjust = 1.5),
  plot.subtitle = element_text(size = 9, vjust = 1.5),
  plot.tag = element_markdown(size = 11),
  plot.margin = margin(20, 180, 10, 10)
)

p7 <- p7 + theme(
  plot.title = element_text(size = 11, face = "bold", vjust = 1.5),
  plot.subtitle = element_text(size = 9, vjust = 1.5),
  plot.tag = element_markdown(size = 11),
  plot.margin = margin(20, 180, 10, 10)
)

# 🔹 Collage orizzontale con spazio bilanciato
collage <- p4 | p7 + plot_layout(guides = "collect") & theme(legend.position = "right")

# 🔹 Salvataggio ad alta risoluzione
ggsave(
  filename = "collage_Lacticaseibacillus_Oxalobacter.png",
  plot = collage,
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
  width = 18,
  height = 8,
  dpi = 400
)




# collage t1
library(patchwork)

# 🔹 Funzione per pulire titoli e ingrandire i grafici
adjust_plot <- function(p) {
  p + labs(title = gsub("Significant Abundance", "", p$labels$title)) +
    theme(
      plot.title = element_text(size = 15, face = "bold", vjust = 1.5),
      plot.subtitle = element_text(size = 12, vjust = 1.5),
      plot.tag = element_markdown(size = 12),
      plot.margin = margin(25, 200, 15, 15),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12)
    )
}

# 🔹 Applica la funzione a ciascun grafico
p2 <- adjust_plot(p2)   # Bacteroides intestinalis
p3 <- adjust_plot(p3)   # Thomasclavelia spiroformis
p6 <- adjust_plot(p6)   # Coprobacter secundus
p9 <- adjust_plot(p9)   # Parabacteroides absconsus
p8 <- adjust_plot(p8)   # Segatella hominis
p10 <- adjust_plot(p10) # Streptococcus ilei
p11 <- adjust_plot(p11) # Streptococcus salivarius
p12 <- adjust_plot(p12) # Thomasclavelia ramosa

# 🔹 Collage 4x2 con grafici più grandi e margini ottimizzati
collage_8 <- (p2 | p3 | p6 | p9) /
  (p8 | p10 | p11 | p12) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

# 🔹 Salvataggio ad altissima risoluzione
ggsave(
  filename = "collage_8_species_large.png",
  plot = collage_8,
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
  width = 36,   # 🔸 più largo per grafici grandi
  height = 20,  # 🔸 più alto per maggiore leggibilità
  dpi = 500     # 🔸 risoluzione ultra nitida
)




# collage solo t12
library(patchwork)

# 🔹 Funzione per pulire titoli e ingrandire i grafici
adjust_plot <- function(p) {
  p + labs(title = gsub("Significant Abundance", "", p$labels$title)) +
    theme(
      plot.title = element_text(size = 15, face = "bold", vjust = 1.5),
      plot.subtitle = element_text(size = 12, vjust = 1.5),
      plot.tag = element_markdown(size = 12),
      plot.margin = margin(25, 200, 15, 15),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12)
    )
}

# 🔹 Applica la funzione a ciascun grafico
p1  <- adjust_plot(p1)   # Parabacteroides sp. CT06
p5  <- adjust_plot(p5)   # Alistipes megaguti
p   <- adjust_plot(p)    # Faecalibacterium prausnitzii
p13 <- adjust_plot(p13)  # Faecalibacterium sp. I3-3-89
p14 <- adjust_plot(p14)  # Streptococcus agalactiae
p15 <- adjust_plot(p15)  # Sutterella faecalis

# 🔹 Collage 3x2 con grafici grandi
collage_6 <- (p1 | p5 | p) /
  (p13 | p14 | p15) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

# 🔹 Salvataggio ad alta risoluzione
ggsave(
  filename = "collage_6_species_large.png",
  plot = collage_6,
  path = "C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/boxplot_abund_sig",
  width = 30,   # 🔸 più largo per 3 grafici affiancati
  height = 18,  # 🔸 più alto per maggiore leggibilità
  dpi = 500     # 🔸 risoluzione ultra nitida
)





# maaslin euk
# Saccharomyces_cerevisiae
get_stars <- function(q){
  if(q < 0.1) return("*")
  return("")
}

sp <- "Saccharomyces_cerevisiae"
qvalue_T1 <- 0.091
qvalue_T12 <- 0.091

stars_T1  <- get_stars(qvalue_T1)
stars_T12 <- get_stars(qvalue_T12)

if(sp %in% colnames(maaslin_transformed)){
  
  plot_df <- data.frame(
    tempo = metadata_df$tempo,
    abundance = maaslin_transformed[, sp]
  )
  
  # Posizioni dinamiche sopra il boxplot
  y_max <- max(plot_df$abundance, na.rm = TRUE)
  y_bar_T1  <- y_max + 0.5
  y_star_T1 <- y_max + 0.7
  
  y_bar_T12  <- y_max + 1.0
  y_star_T12 <- y_max + 1.2
  
  p <- ggplot(plot_df, aes(x = tempo, y = abundance, fill = tempo)) +
    geom_boxplot() +
    geom_jitter(width = 0.15, size = 2) +
    
    # 🔹 Barra + asterisco T0–T1
    geom_segment(aes(x = 1, xend = 2, y = y_bar_T1, yend = y_bar_T1),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 1.5, y = y_star_T1),
              label = stars_T1, color = "red", size = 8, fontface = "bold") +
    
    # 🔹 Barra + asterisco T0–T12
    geom_segment(aes(x = 1, xend = 3, y = y_bar_T12, yend = y_bar_T12),
                 color = "black", size = 0.8) +
    geom_text(aes(x = 2, y = y_star_T12),
              label = stars_T12, color = "red", size = 8, fontface = "bold") +
    
    theme_bw() +
    labs(
      title = "Saccharomyces cerevisiae",
      subtitle = paste0(
        "Beta coefficients: T1 = 0.217 (q = ", qvalue_T1,
        "), T12 = 0.238 (q = ", qvalue_T12, ")"
      ),
      x = "Timepoint",
      y = "MaAsLin3 transformed abundance: Log2(relative abundance)"
    ) +
    theme(
      plot.tag = element_markdown(hjust = 0.5, size = 12),
      plot.tag.position = c(0.55, 0.995),
      plot.margin = margin(10, 180, 10, 10),
      legend.position = "right"
    ) +
    
    # 🔹 Legenda spostata in basso
    annotate(
      "text",
      x = Inf, y = min(plot_df$abundance, na.rm = TRUE) - 0.5,
      label = "**Statistics:**\n*  q < 0.1",
      hjust = 1.1, vjust = 0,
      size = 3
    )
  
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
