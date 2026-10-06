# macchina virtuale

# OTU TABLE ABBONDANZE GREZZE FILTRATE
library(phyloseq)
library(dplyr)

# 1. Carica l'oggetto phyloseq
rds <- readRDS("C:/AURI/Thesis_Bagetto/app/shared_results/SUPERVISED_DEC/Bacteria_supervised_decontam.rds")

# 2. Estrai le tabelle
otu <- as.data.frame(otu_table(rds))
tax <- as.data.frame(tax_table(rds))

# 3. Crea una colonna con il nome completo della specie
tax$SpecieCompleta <- paste(tax$Genus, tax$Species, sep = "_")

# 4. Carica le specie significative da Maaslin
sig <- read.csv("C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/abundance_pval.csv")
specie_sig <- unique(sig$feature)  # o sig$taxa, dipende dal nome colonna

# 5. Trova gli ID corrispondenti alle specie significative
id_sig <- rownames(tax)[tax$SpecieCompleta %in% specie_sig]

# 6. Filtra la OTU table mantenendo solo gli ID corrispondenti
otu_sig <- otu[rownames(otu) %in% id_sig, ]

# 7. Sostituisci gli ID numerici con i nomi delle specie
rownames(otu_sig) <- tax$SpecieCompleta[match(rownames(otu_sig), rownames(tax))]

# 8. Salva la OTU filtrata con nomi delle specie
write.csv(otu_sig,
          "C:/AURI/Thesis_Bagetto/CONNECTOR/otu_sig.csv",
          row.names = TRUE)





# OTU TABLE ABBONDANZE RELATIVE NON FILTRATE
library(phyloseq)
library(dplyr)

# 1. Carica l'oggetto phyloseq
rds <- readRDS("C:/AURI/Thesis_Bagetto/app/shared_results/SUPERVISED_DEC/Bacteria_supervised_decontam.rds")

# 2. Converte le abbondanze in relative (proporzioni per campione)
rds_rel <- transform_sample_counts(rds, function(x) x / sum(x))

# 3. Estrai le tabelle dal nuovo oggetto normalizzato
otu <- as.data.frame(otu_table(rds_rel))
tax <- as.data.frame(tax_table(rds_rel))

# 4. Crea una colonna con il nome completo della specie
tax$SpecieCompleta <- paste(tax$Genus, tax$Species, sep = "_")

# 5. Sostituisci gli ID numerici con i nomi delle specie
rownames(otu) <- tax$SpecieCompleta[match(rownames(otu), rownames(tax))]

# 6. Salva la OTU completa con abbondanze relative
write.csv(otu,
          "C:/AURI/Thesis_Bagetto/CONNECTOR/otu_rel_all_species.csv",
          row.names = TRUE)

colSums(otu)[1:5]






# OTU ABBONDANZE RELATIVE FILTRATE
library(phyloseq)
library(dplyr)

# 1. Carica l'oggetto phyloseq
rds <- readRDS("C:/AURI/Thesis_Bagetto/app/shared_results/SUPERVISED_DEC/Bacteria_supervised_decontam.rds")

# 2. Estrai la OTU table e converti in data.frame
otu <- as.data.frame(otu_table(rds))

# 3. Converti le abbondanze grezze in abbondanze relative
#    Ogni colonna = un campione → somma per colonna
otu_rel <- sweep(otu, 2, colSums(otu), FUN = "/")

# 4. Estrai la tax_table
tax <- as.data.frame(tax_table(rds))

# 5. Crea una colonna con il nome completo della specie
tax$SpecieCompleta <- paste(tax$Genus, tax$Species, sep = "_")

# 6. Carica le specie significative da Maaslin
sig <- read.csv("C:/AURI/Thesis_bagetto/Maaslin/Aurelia_maaslin/abundance_pval.csv")
specie_sig <- unique(sig$feature)  

# 7. Trova gli ID corrispondenti alle specie significative
id_sig <- rownames(tax)[tax$SpecieCompleta %in% specie_sig]

# 8. Filtra la OTU table RELATIVA mantenendo solo gli ID corrispondenti
otu_sig <- otu_rel[rownames(otu_rel) %in% id_sig, ]

# 9. Sostituisci gli ID numerici con i nomi delle specie
rownames(otu_sig) <- tax$SpecieCompleta[match(rownames(otu_sig), rownames(tax))]

# 10. Salva la OTU filtrata con nomi delle specie
write.csv(otu_sig,
          "C:/AURI/Thesis_Bagetto/CONNECTOR/otu_sig_relative.csv",
          row.names = TRUE)

# trasferisco in locale
pkgbuild::has_rtools(debug = TRUE)
install.packages(c(
  "devtools",
  "dplyr",
  "ggplot2",
  "readxl",
  "readr",
  "tibble",
  "magrittr",
  "patchwork"
))

devtools::install_github("erocoar/gghalves")
devtools::install_github("qBioTurin/MultiConnector")
library(MultiConnector)

# copio codice da macchina
#ABBONDANZE RELATIVE FILTRATE

# creazione file necessari per connector
library(dplyr)
library(tidyr)
library(readr)

## ricordati di fare upload di otu table con abbondanza relativa oppure otu table filtrata con abbondanza relativa
meta <- read_csv2("C:/AURI/Thesis_bagetto/merged_final.csv")   # separatore ; come nel tuo file
otu <- read_csv("C:/AURI/Thesis_Bagetto/CONNECTOR/input/otu_sig_relative.csv")

## DT: meta e otu sono caricati come special tibble; le variabili non hanno le giuste classi assegnate
# vedere più avanti se questo crea problemi

otu_t <- otu %>%
  rename(feature = 1) %>%   # prima colonna contiene i nomi delle specie
  pivot_longer(
    cols = -feature,
    names_to = "ID_long",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = feature,
    values_from = value
  )

## DT: check del merge
length(unique(meta$ID_long))
#81
length(unique(colnames(otu)))
#64
length(unique(otu_t$ID_long))
#63

# nel merge uso inner_joint e non left_join così tengo i campioni giusti
otu_meta <- meta %>%
  inner_join(otu_t, by = "ID_long")
unique(otu_meta$ID_long)
# 63

## check: 
setdiff(colnames(otu), otu_meta$ID_long)
setdiff(otu_meta$ID_long, colnames(otu))

## check
otu_meta %>% distinct(Tempo)

timeseries <- otu_meta %>%
  pivot_longer(
    cols = starts_with(c(
      "Alistipes", "Parabacteroides", "Segatella", "Thomasclavelia",
      "Oxalobacter", "Sutterella", "Streptococcus", "Faecalibacterium",
      "Coprobacter", "Bacteroides", "Lacticaseibacillus"
    )),
    names_to = "measureID",
    values_to = "value"
  ) %>%
  transmute(
    subjID = ID,
    time   = Tempo,
    measureID,
    value
  ) %>%
  group_by(subjID) %>%
  filter(n_distinct(time) >= 2) %>%
  ungroup()

write_csv(timeseries, "C:/AURI/Thesis_bagetto/CONNECTOR/locale/input/timeseries.csv")

## check che tutti i soggetti abbiano 3 punti
table(timeseries$time)
table(timeseries$subjID)
length(unique(timeseries$subjID))
# restano 21 pazienti

## assegno corrette classi
timeseries <- timeseries %>%
  mutate(
    time =  as.numeric(gsub("T", "", time)), 
    measureID = as.factor(measureID)
  )


annotations <- meta %>%
  group_by(ID) %>%
  summarise(
    sex = first(sex),
    age_at_T0 = first(age_at_T0),
    bmi = first(bmi),
    therapy = first(therapy),
    naive = first(naive),
    disease_activity = first(disease_activity),
    lesion_burden = first(lesion_burden),
    spinal_lesions = first(spinal_lesions),
    subtentorial_lesions = first(subtentorial_lesions),
    gadolinium_contrast = first(gadolinium_contrast),
    EDSS_T0 = EDSS[Tempo == "T0"][1],
    EDSS_T12 = EDSS[Tempo == "T12"][1]
  ) %>%
  rename(subjID = ID) %>%
  mutate(
    
    age_group = case_when(
      age_at_T0 < 30 ~ "<30",
      age_at_T0 < 40 ~ "30-39",
      TRUE ~ ">=40"
    ),
    
    # categoriche
    sex = factor(sex),
    naive = factor(naive),
    therapy = factor(therapy),
    disease_activity = factor(disease_activity),
    age_group = factor(age_group,
                       levels = c("<30","30-39",">=40")),
    
    # binarie radiologiche
    lesion_burden = factor(lesion_burden),
    spinal_lesions = factor(spinal_lesions),
    subtentorial_lesions = factor(subtentorial_lesions),
    gadolinium_contrast = factor(gadolinium_contrast),
    
    # continue
    age_at_T0 = as.numeric(age_at_T0),
    bmi = as.numeric(bmi),
    EDSS_T0 = as.numeric(EDSS_T0),
    EDSS_T12 = as.numeric(EDSS_T12)
  )
str(annotations)

write_csv(annotations, "C:/AURI/Thesis_bagetto/CONNECTOR/locale/input/annotations.csv")


# Create CONNECTORData object
library(MultiConnector)

data <- ConnectorData(timeseries, annotations) 
# 336 observations e 16 measures
saveRDS(data, "C:/AURI/Thesis_bagetto/CONNECTOR/locale/input/data_connector.rds")

# CARICARE DATA
data <- readRDS("C:/AURI/Thesis_bagetto/CONNECTOR/locale/input/data_connector.rds")

# Access slots
head(data@curves)        # Time series data
data@annotations         # Feature annotations
names(data@TimeGrids)    # Available measurements

# Use methods
# guardo tutte le variabili
# EDSS TO
p <- plot(data, feature = "EDSS_T0") ## DT: prova a guardare anche altre variabili; 
p

library(ggplot2)

p2 <- p +
  theme(
    strip.text = element_text(size = 14),  # nomi specie
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

ggsave(
  "C:/AURI/Thesis_bagetto/CONNECTOR/locale/EDSS_T0.png",
  plot = p2,
  width = 16,
  height = 12,
  units = "in",
  dpi = 600
)


show(data) 
#21 soggetti e 16 taxa
getAnnotations(data)     # Lists available features


# dimensione latente p
data@curves$measureID <- as.character(data@curves$measureID)
str(data@curves)
table(data@curves$subjID) 

# quali polinomi meglio descrivono le mie curve? 
# la dimensione del polinomio può essere al massimo quella del numero dei tempi
dimension_results <- estimatepDimension(data, p = 2:3, cores = 2)
dimension_results
library(MultiConnector)

# vettore da considerare dopo per avere il miglior p per ogni specie
vettore_p <- dimension_results$p_suggested

## dopo aver scelto la p, stimo il numero di cluster
cluster_results <- estimateCluster(
  data, 
  G = 2:5,           # Test 2-5 clusters
  p = vettore_p,    
  runs = 50,         # Multiple runs for stability
  cores = 4          # Parallel processing
)

plot(cluster_results) # chose G based on the plot
# salvo il plot
png("C:/AURI/Thesis_bagetto/CONNECTOR/locale/cluster_results.png", width = 1800, height = 1200, res = 150)
plot(cluster_results)
dev.off()


final_clusters <- selectCluster(cluster_results, G = 2, best = "MaxSilhouette")
plot(final_clusters)

table(getClusters(final_clusters))

##############################################################################################
# provo a clusterizzare il cluster A
cl <- getClusters(final_clusters)

table(cl)
A_subjects <- names(cl)[cl == "A"]
B_subjects <- names(cl)[cl == "B"]

head(cl)
str(cl)

head(names(cl))

head(unique(timeseries$subjID))
head(unique(annotations$subjID))


A_subjects <- cl$subjID[cl$cluster == "A"]

length(B_subjects)
# 2
length(A_subjects)
# 19
head(A_subjects)
A_subjects

# estrai soggetti del cluster A
A_subjects <- cl$subjID[cl$cluster == "A"]

length(A_subjects)
# 19

# filtra dati longitudinali
timeseries_A <- timeseries %>%
  filter(subjID %in% A_subjects)

# filtra annotazioni
annotations_A <- annotations %>%
  filter(subjID %in% A_subjects)

# verifica
length(unique(timeseries_A$subjID))
#19
length(unique(annotations_A$subjID))
#19

# ricreo il dataset
data_A <- ConnectorData(
  timeseries_A,
  annotations_A
)

show(data_A)
# 19 soggetti e 16 measures

# stima p
dimension_results_A <- estimatepDimension(
  data_A,
  p = 2:3,
  cores = 1
)

dimension_results_A

vettore_p_A <- dimension_results_A$p_suggested
table(vettore_p_A)

# stima numero cluster
cluster_results_A <- estimateCluster(
  data_A,
  G = 2:4,
  p = 2,
  runs = 50,
  cores = 1
)

plot(cluster_results_A)
png(
  "C:/AURI/Thesis_bagetto/CONNECTOR/locale/cluster_results_A_plot.png",
  width = 2400,
  height = 1600,
  res = 300
)

plot(cluster_results_A)

dev.off()

final_clusters_A <- selectCluster(
  cluster_results_A,
  G = 2,
  best = "MaxSilhouette"
)

plot(final_clusters_A)


########## controlli ##########
# ==========================
# CLUSTER FINALI
# ==========================

cl_A <- getClusters(final_clusters_A)

head(cl_A)
# TEC02, TEC03, TEC04, TEC05, TEC06, TEC07

table(cl_A$cluster)

prop.table(table(cl_B$cluster))

annotations_B_cluster <- annotations_B %>%
  left_join(cl_B, by = "subjID")

head(annotations_B_cluster)

str(annotations_B_cluster)

library(ggplot2)

# AGE
p_age <- ggplot(
  annotations_B_cluster,
  aes(cluster, age_at_T0, fill = cluster)
) +
  geom_boxplot() +
  theme_bw()

ggsave(
  "R/abbondanze_relative_sig/ricluster/age_at_T0_by_cluster.png",
  plot = p_age,
  width = 6,
  height = 5,
  dpi = 300
)



# BMI
ggplot(
  annotations_B_cluster,
  aes(cluster, bmi, fill = cluster)
) +
  geom_boxplot() +
  theme_bw()

# EDSS T0
ggplot(
  annotations_B_cluster,
  aes(cluster, EDSS_T0, fill = cluster)
) +
  geom_boxplot() +
  theme_bw()

# EDSS T12
ggplot(
  annotations_B_cluster,
  aes(cluster, EDSS_T12, fill = cluster)
) +
  geom_boxplot() +
  theme_bw()


kruskal.test(age_at_T0 ~ cluster,
             data = annotations_B_cluster)


kruskal.test(bmi ~ cluster,
             data = annotations_B_cluster)

kruskal.test(EDSS_T0 ~ cluster,
             data = annotations_B_cluster)

kruskal.test(EDSS_T12 ~ cluster,
             data = annotations_B_cluster)

tab_sex <- table(
  annotations_B_cluster$cluster,
  annotations_B_cluster$sex
)

tab_sex

fisher.test(tab_sex)

tab_naive <- table(
  annotations_B_cluster$cluster,
  annotations_B_cluster$naive
)

tab_naive

fisher.test(tab_naive)
##############################################################################
























slotNames(final_clusters_B)
names(final_clusters_B@KData)
names(final_clusters_B@TTandfDBandSil)
table(getClusters(final_clusters_B))


## validare i cluster 
#final clusters
validation <- validateCluster(final_clusters)
print(validation$plot)
print(validation$entropy_silhouette_table)  # Per-curve quality metrics
print(validation$assignmentProbs) 



# salvo il validation plot
# validation
png("C:/AURI/Thesis_bagetto/CONNECTOR/locale/validation_plot.png", width = 1800, height = 1200, res = 150)
print(validation$plot)
dev.off()


DiscriminantPlot(final_clusters)
png("C:/AURI/Thesis_bagetto/CONNECTOR/locale/discriminant_plot.png", width = 1800, height = 1200, res = 150)
DiscriminantPlot(final_clusters)
dev.off()


plot(final_clusters)
p <- plot(final_clusters, feature = "subtentorial_lesions")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
# final clusters
library(ggplot2)

p <- plot(final_clusters, feature = "subtentorial_lesions") +
  scale_x_continuous(breaks = c(0, 1, 12)) +
  theme_bw(base_size = 12) +
  theme(
    # Pannelli più grandi e ben separati
    panel.spacing = unit(1.2, "lines"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    
    # Griglia più chiara
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    
    # Testo assi e titoli
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(size = 10, margin = margin(r = 6)),
    
    # Legenda
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    
    # Nomi delle specie orizzontali
    strip.text.y = element_text(angle = 0, hjust = 0, size = 10, face = "bold"),
    
    # Titoli dei pannelli
    strip.text = element_text(size = 12, face = "bold")
  )

# salvo
ggsave("C:/AURI/Thesis_bagetto/CONNECTOR/locale/final_clusters_subtentorial_lesions_plot.png",
       p,
       width = 14,
       height = 10,
       dpi = 300)






































## esploro meglio i soggetti con SubjectInfo()

SubjectInfo(final_clusters, c("TEC02","TEC03", "TEC04", "TEC05", "TEC06", "TEC07", "TEC08", "TEC10", "TEC11", "TEC14", "TEC15", "TEC16", "TEC17", "TEC19", "TEC20", "TEC21", "TEC22", "TEC24", "TEC30", "TEC31", "TEC32"))

library(ggplot2)

# Estrai il grafico ggplot dalla lista
p <- SubjectInfo(
  final_clusters,
  c("TEC02","TEC03","TEC04","TEC05","TEC06","TEC07","TEC08","TEC10","TEC11",
    "TEC14","TEC15","TEC16","TEC17","TEC19","TEC20","TEC21","TEC22","TEC24",
    "TEC30","TEC31","TEC32")
)$highlighted_plot +
  scale_x_continuous(breaks = c(0, 1, 12)) +
  theme_bw(base_size = 12) +
  theme(
    # 🔹 Pannelli più grandi e ben separati
    panel.spacing = unit(1.2, "lines"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    
    # 🔹 Griglia più chiara
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    
    # 🔹 Testo assi e titoli
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(size = 10, margin = margin(r = 6)),
    
    # 🔹 Legenda
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    
    # 🔹 Nomi delle specie orizzontali e leggibili
    strip.text.y = element_text(angle = 0, hjust = 0, size = 10, face = "bold"),
    
    # 🔹 Titoli dei pannelli (cluster 1 e 2)
    strip.text = element_text(size = 12, face = "bold")
  )

# 🔹 Salva la figura ad alta risoluzione nella cartella R/Aurelia
ggsave("R/abbondanze_relative_sig/G=2/subjectinfo.png",
       p,
       width = 14,
       height = 10,
       dpi = 300)


# analizzo G2 perchè noto una distinzione netta di alcuni pazienti
# TEC15 nel cluster 1
# TEC16 nel cluster 2

SubjectInfo(final_clusters, c("TEC15", "TEC16"))
p <- SubjectInfo(
  final_clusters,
  c("TEC15", "TEC16")
)$highlighted_plot +
  scale_x_continuous(breaks = c(0, 1, 12)) +
  theme_bw(base_size = 12) +
  theme(
    # 🔹 Pannelli più grandi e ben separati
    panel.spacing = unit(1.2, "lines"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    
    # 🔹 Griglia più chiara
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    
    # 🔹 Testo assi e titoli
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(size = 10, margin = margin(r = 6)),
    
    # 🔹 Legenda
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    
    # 🔹 Nomi delle specie orizzontali e leggibili
    strip.text.y = element_text(angle = 0, hjust = 0, size = 10, face = "bold"),
    
    # 🔹 Titoli dei pannelli (cluster 1 e 2)
    strip.text = element_text(size = 12, face = "bold")
  )

# 🔹 Salva la figura ad alta risoluzione nella cartella R/Aurelia
ggsave("R/abbondanze_relative_sig/G=2/subjectinfo_2pz.png",
       p,
       width = 14,
       height = 10,
       dpi = 300)






## oppure i cluster con clusterDistribution()

generateReport(
  data = data,
  clustered_data = list(final_clusters),
  p_analysis = dimension_results,
  G_analysis = cluster_results,
  output_file = "complete_report.html",
  report_title = "My Clinical Analysis",
  features = c("EDSS_T0",
               "EDSS_T12",
               "sex",
               "naive",
               "age_at_T0",
               "therapy",
               "bmi")
)





report <- generateReport(clustered_data = clustered_data) #non funziona

report <- generateReport(clustered_data = list(clustered_data))
browseURL("/home/abagetto/report.html")
file.copy("/home/abagetto/report.html", "R/Aurelia/report.html")



# estraggo tabelle
library(rvest)
library(dplyr)
library(readr)

report_path <- "/home/abagetto/report.html"
html <- read_html(report_path)
tables <- html %>% html_table(fill = TRUE)
length(tables)  # numero di tabelle trovate
tables[[1]]  # riepilogo cluster
tables[[2]]  # metriche fDB, silhouette, TT

# salvo
write_csv(tables[[1]], "R/Aurelia/cluster_summary.csv")
write_csv(tables[[2]], "R/Aurelia/cluster_metrics.csv")



