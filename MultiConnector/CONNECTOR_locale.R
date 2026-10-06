#ABBONDANZE RELATIVE FILTRATE

# creazione file necessari per connector
meta <- read_csv2("C:/AURI/Thesis_bagetto/merged_final.csv")   
otu <- read_csv("C:/AURI/Thesis_Bagetto/CONNECTOR/input/otu_sig_relative.csv")

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

#check del merge
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
  filter(n_distinct(time) >= 2) %>% # almeno due tempi
  ungroup()

write_csv(timeseries, "C:/AURI/Thesis_bagetto/CONNECTOR/locale/input/timeseries.csv")

# check che tutti i soggetti abbiano 3 punti
table(timeseries$time)
table(timeseries$subjID)
length(unique(timeseries$subjID))
# restano 21 pazienti

# assegno corrette classi
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
    MEDAS_group = first(MEDAS_group),
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
    
    BMI_group = case_when(
      bmi >= 25 ~ "overweight",
      TRUE ~ "normalweight"
    ),
    
    # categoriche
    sex = factor(sex),
    naive = factor(naive),
    therapy = factor(therapy),
    disease_activity = factor(disease_activity),
    MEDAS_group = factor(MEDAS_group,
                         levels = c("Low", "High")),
    age_group = factor(age_group,
                       levels = c("<30", "30-39", ">=40")),
    BMI_group = factor(BMI_group,
                       levels = c("normalweight", "overweight")),
    
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
p <- plot(data, feature = "EDSS_T0") 
p
# BMI
p <- plot(data, feature = "bmi")
p
# BMI GROUP
p <- plot(data, feature = "BMI_group")
p
# EDSS_T12
p <- plot(data, feature = "EDSS_T12")
P
# AGE
p <- plot(data, feature = "age_at_T0")
p
#AGE GROUP
p <- plot(data, feature = "age_group")
p
# SEX
p <- plot(data, feature = "sex")
p
# NAIVE
p <- plot(data, feature = "naive")
p
# LESION BURDEN
p <- plot(data, feature = "lesion_burden")
p
#SUBTENTORIAL LESIONS
p <- plot(data, feature = "subtentorial_lesions")
p
# SPINAL LESIONS
p <- plot(data, feature = "spinal_lesions")
p
# GADOLINIUM CONTRAST
p <- plot(data, feature = "gadolinium_contrast")
p


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

# vettore da considerare per avere la miglior p per ogni specie
vettore_p <- dimension_results$p_suggested

# dopo aver scelto la p, stimo il numero di cluster
cluster_results <- estimateCluster(
  data, 
  G = 2:5,           # Test 2-5 clusters
  p = vettore_p,    
  runs = 50,         # Multiple runs for stability
  cores = 4          # Parallel processing
)

plot(cluster_results) # chose G based on the plot

final_clusters <- selectCluster(cluster_results, G = 2, best = "MaxSilhouette")
plot(final_clusters)

table(getClusters(final_clusters))
# 12 4 5 con G=3
# 2 19 con G=2

## validare i cluster 
#final clusters
validation <- validateCluster(final_clusters)
print(validation$plot)
print(validation$entropy_silhouette_table)  # Per-curve quality metrics
print(validation$assignmentProbs) 

DiscriminantPlot(final_clusters)

plot(final_clusters)
p <- plot(final_clusters, feature = "subtentorial_lesions")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
# final clusters
library(ggplot2)

# grafici fatti per ogni feature
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

final_clusters_A <- selectCluster(
  cluster_results_A,
  G = 3,
  best = "MaxSilhouette"
)

plot(final_clusters_A)

## validare i cluster 
#final clusters
validation <- validateCluster(final_clusters_A)
print(validation$plot)
print(validation$entropy_silhouette_table)  # Per-curve quality metrics
print(validation$assignmentProbs) 

DiscriminantPlot(final_clusters_A)

# BMI GROUP
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "BMI_group")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "BMI_group") +
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

# AGE GROUP
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "age_group")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "age_group") +
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

# LESION BURDEN
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "lesion_burden")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "lesion_burden") +
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

# SUBTENTORIAL LESIONS
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "subtentorial_lesions")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "subtentorial_lesions") +
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

# SPINAL LESIONS
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "spinal_lesions")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "spinal_lesions") +
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

# NAIVE
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "naive")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "naive") +
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

# SEX
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "sex")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "sex") +
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

# EDSS T0
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "EDSS_T0")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "EDSS_T0") +
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

# EDSS T12
plot(final_clusters_A)
p <- plot(final_clusters_A, feature = "EDSS_T12")
p + scale_x_continuous(breaks = c(0,1,12)) 

## modifico tema ggplot
p <- plot(final_clusters_A, feature = "EDSS_T12") +
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



########## test statistici
# cluster del riclustering A
cluster_df <- getClusters(final_clusters_A)

cluster_df$cluster <- factor(cluster_df$cluster)

table(cluster_df$cluster)
prop.table(table(cluster_df$cluster))

annot_cluster <- annotations_A %>%
  left_join(cluster_df, by = "subjID")

table(annot_cluster$cluster, useNA = "always")

# var categoriche
# fisher test: i cluster hanno distribuzione diversa di variabile che studio?
vars_cat <- c(
  "disease_activity",
  "gadolinium_contrast",
  "lesion_burden",
  "spinal_lesions",
  "sex",
  "BMI_group",
  "MEDAS_group"
)

for(v in vars_cat){
  
  cat("\n====================\n")
  cat(v, "\n")
  cat("====================\n")
  
  tab <- table(
    annot_cluster$cluster,
    annot_cluster[[v]]
  )
  
  print(tab)
  
  if(all(dim(tab) > 1)){
    print(fisher.test(tab))
  }
}
# BMI group p=0.07, il resto tutto non sig

# var continue
# kruskal test: i cluster contengono pz con variabile x diversi?
kruskal.test(age_at_T0 ~ cluster, data = annot_cluster)

kruskal.test(bmi ~ cluster, data = annot_cluster)

kruskal.test(EDSS_T0 ~ cluster, data = annot_cluster)

kruskal.test(EDSS_T12 ~ cluster, data = annot_cluster)
# nulla sig





# Faecalibacterium prausnitzii
fp_mean <- otu_meta %>%
  group_by(ID) %>%
  summarise(
    Faecalibacterium_prausnitzii =
      mean(Faecalibacterium_prausnitzii, na.rm = TRUE)
  ) %>%
  rename(subjID = ID) %>%
  left_join(cluster_df, by = "subjID")

kruskal.test(
  Faecalibacterium_prausnitzii ~ cluster,
  data = fp_mean
)
# la distribuzione di F. prausnitzii varia in maniera sig nei 3 cluster

library(lme4)
library(lmerTest)

fp_long <- otu_meta %>%
  select(
    ID,
    Tempo,
    bmi,
    BMI_group,
    Faecalibacterium_prausnitzii
  ) %>%
  rename(subjID = ID) %>%
  left_join(cluster_df, by = "subjID")

mod <- lmer(
  Faecalibacterium_prausnitzii ~ Tempo * cluster +
    (1 | subjID),
  data = fp_long
)
anova(mod)
summary(mod)

prova <- lmer(
  Faecalibacterium_prausnitzii ~ Tempo + BMI_group +
    (1 | subjID),
  data = fp_long
)
anova(prova)
summary(prova)

mod2 <- lmer(
  Faecalibacterium_prausnitzii ~ Tempo * cluster + bmi +
    (1 | subjID),
  data = fp_long
)
anova(mod2)
summary(mod2)

# Faecalibacterium prausnitzii è effettivamente associato ai cluster, e non soltanto al BMI.
# gia al baseline A > B e A > C
# la separazione dei cluster è fortemente guidata dal comportamento longitudinale di F. prausnitzii.
# elevata abbondanza iniziale di Faecalibacterium prausnitzii e forte risposta a T1 (Cluster A);
# abbondanza iniziale più bassa e traiettoria più piatta (Cluster B);
# abbondanza iniziale più bassa con andamento diverso nel follow-up (Cluster C).
