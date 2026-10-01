merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_ceci.csv")
merged_final <- read.csv2("C:/AURI/Thesis_bagetto/merged_final.csv")
library(dplyr)

merged_ceci_mav <- merged_ceci %>%
  filter(grepl("^MAV", ID))


# modifiche presentazione
### Lineplot EDSS progression ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(pals)

# Dataset lungo
edss_long <- merged %>%
  select(ID, cambio_terapia, starts_with("EDSS_T")) %>%
  mutate(
    cambio_terapia = factor(
      tidyr::replace_na(cambio_terapia, 0),
      levels = c(0, 1),
      labels = c("No", "Sì")
    )
  ) %>%
  pivot_longer(
    cols = starts_with("EDSS_T"),
    names_to = "timepoint",
    values_to = "EDSS"
  ) %>%
  filter(!is.na(EDSS)) %>%
  mutate(
    timepoint = factor(
      timepoint,
      levels = paste0("EDSS_T", c(0, 6, 12, 18, 24, 30, 36)),
      labels = paste0("T", c(0, 6, 12, 18, 24, 30, 36))
    )
  )

# Ultimo timepoint per ciascun paziente
last_points <- edss_long %>%
  group_by(ID) %>%
  filter(as.numeric(gsub("T", "", timepoint)) == max(as.numeric(gsub("T", "", timepoint)))) %>%
  ungroup()

# Grafico
p <- ggplot(edss_long, aes(x = timepoint, y = EDSS, group = ID)) +
  
  # Linee dei singoli pazienti
  geom_line(aes(color = ID), alpha = 0.6, linewidth = 0.6) +
  
  # Punti con jitter
  geom_point(
    aes(color = ID),
    alpha = 0.6,
    size = 1.2,
    position = position_jitter(width = 0.1, height = 0.08, seed = 42)
  ) +
  
  # Ultimo punto evidenziato
  geom_point(
    data = last_points,
    aes(color = ID, shape = cambio_terapia),
    size = 3
  ) +
  
  # Etichette ID
  geom_text_repel(
    data = last_points,
    aes(label = ID, color = ID),
    size = 3,
    fontface = "bold",
    nudge_x = 0.2,
    segment.alpha = 0,
    show.legend = FALSE
  ) +
  
  # Media della coorte
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.3,
    color = "black"
  ) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "point",
    size = 2.5,
    color = "black",
    shape = 18
  ) +
  
  # Colori
  scale_color_manual(
    values = as.vector(pals::polychrome(length(unique(edss_long$ID)))),
    guide = "none"
  ) +
  
  # Forma ultimo punto
  scale_shape_manual(
    values = c("No" = 16, "Sì" = 17),
    name = "Therapy switch"
  ) +
  
  labs(
    title = "EDSS Progression Over Time",
    subtitle = "Black: cohort average | Coloured: each TEC patient",
    x = "Timepoints",
    y = "EDSS value"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30"),
    plot.margin = margin(10, 40, 10, 10)
  )

# Salvataggio del grafico
ggsave(
  filename = "EDSS_progression_lineplot.png",
  plot = p,
  path = "C:/AURI/Thesis_bagetto/grafici_report_coorte",
  width = 10,
  height = 6,
  dpi = 300
)


### sankey plot ####
# Classi EDSS
sankey_data <- merged %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    )
  )

links <- bind_rows(
  sankey_data %>% count(T0, T6) %>% rename(source=T0,target=T6,value=n),
  sankey_data %>% count(T6, T12) %>% rename(source=T6,target=T12,value=n),
  sankey_data %>% count(T12, T18) %>% rename(source=T12,target=T18,value=n),
  sankey_data %>% count(T18, T24) %>% rename(source=T18,target=T24,value=n),
  sankey_data %>% count(T24, T30) %>% rename(source=T24,target=T30,value=n),
  sankey_data %>% count(T30, T36) %>% rename(source=T30,target=T36,value=n)
)

links <- bind_rows(
  sankey_data %>% transmute(source=paste0("T0_",T0), target=paste0("T6_",T6)),
  sankey_data %>% transmute(source=paste0("T6_",T6), target=paste0("T12_",T12)),
  sankey_data %>% transmute(source=paste0("T12_",T12), target=paste0("T18_",T18)),
  sankey_data %>% transmute(source=paste0("T18_",T18), target=paste0("T24_",T24)),
  sankey_data %>% transmute(source=paste0("T24_",T24), target=paste0("T30_",T30)),
  sankey_data %>% transmute(source=paste0("T30_",T30), target=paste0("T36_",T36))
) %>%
  count(source, target, name="value")

nodes <- data.frame(
  name = unique(c(links$source, links$target))
)

links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 25
)






merged_ceci_TEC <- merged %>%
  filter(grepl("^TEC", ID))


# glucocorticoid al T0

#=========================
# Filtro dataset e creazione gruppi
#=========================

merged_naive <- merged_ceci_TEC %>%
  filter(naive != "No") %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0),
    gc_group = case_when(
      gc == 1 ~ "TEC_GC+",
      gc == 0 ~ "TEC_GC-"
    )
  ) %>%
  filter(gc_group %in% c("TEC_GC+", "TEC_GC-")) %>%
  mutate(
    gc_group = factor(gc_group, levels = c("TEC_GC+", "TEC_GC-"))
  )

#=========================
# Ultimo EDSS disponibile
#=========================

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS
#=========================

merged_naive <- merged_naive %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi NEDA
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_6_C  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = EDSS_worsening | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = EDSS_worsening | relapse_T36 == 1 | lesioni_T36 == 1
  )

#=========================
# Tempo al primo evento
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_time_C = case_when(
      event_6_C  ~ 6,
      event_12_C ~ 12,
      event_18_C ~ 18,
      event_24_C ~ 24,
      event_30_C ~ 30,
      event_36_C ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_C = ifelse(is.na(event_time_C), 0, 1),
    time_C = ifelse(is.na(event_time_C), 36, event_time_C)
  )

#=========================
# Kaplan-Meier (solo TEC)
#=========================

surv_obj <- Surv(time = merged_naive$time_C,
                 event = merged_naive$event_status_C)

fit <- survfit(surv_obj ~ gc_group, data = merged_naive)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-"),
  xlab = "Months",
  ylab = "NEDA-free survival"
)

#=========================
# Tabella riepilogativa
#=========================

table(merged_naive$gc_group, merged_naive$gc_treatment)



#### gc treatment solo EDSS #####
# Gruppi GC (solo TEC)

merged_naive <- merged_naive %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0),
    gc_group = case_when(
      gc == 1 ~ "TEC_GC+",
      gc == 0 ~ "TEC_GC-"
    )
  )

merged_naive$gc_group <- factor(
  merged_naive$gc_group,
  levels = c("TEC_GC+", "TEC_GC-")
)

#=========================
# Ultimo EDSS disponibile
#=========================

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS
#=========================

merged_naive <- merged_naive %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Survival (solo EDSS)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_status_EDSS = ifelse(EDSS_worsening, 1, 0),
    time_EDSS = ifelse(EDSS_worsening, 6, 36)
  )

#=========================
# Kaplan-Meier
#=========================

surv_obj <- Surv(
  time = merged_naive$time_EDSS,
  event = merged_naive$event_status_EDSS
)

fit <- survfit(
  surv_obj ~ gc_group,
  data = merged_naive
)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-"),
  xlab = "Months",
  ylab = "EDSS worsening-free survival"
)


## gc treatment per lesioni
#### gc treatment solo EDSS (ora: solo LESIONI) #####
#=========================
# Gruppi GC (solo TEC)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0),
    gc_group = case_when(
      gc == 1 ~ "TEC_GC+",
      gc == 0 ~ "TEC_GC-"
    )
  )

merged_naive$gc_group <- factor(
  merged_naive$gc_group,
  levels = c("TEC_GC+", "TEC_GC-")
)

#=========================
# Evento LESIONI (0/1)
#=========================
# Qui sostituiamo EDSS_worsening con lesioni_T36, lesioni_T30, ecc.

merged_naive <- merged_naive %>%
  mutate(
    lesion_event = case_when(
      lesioni_T6  == 1 ~ 6,
      lesioni_T12 == 1 ~ 12,
      lesioni_T18 == 1 ~ 18,
      lesioni_T24 == 1 ~ 24,
      lesioni_T30 == 1 ~ 30,
      lesioni_T36 == 1 ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_lesion = ifelse(is.na(lesion_event), 0, 1),
    time_lesion = ifelse(is.na(lesion_event), 36, lesion_event)
  )

#=========================
# Kaplan-Meier (solo TEC, solo lesioni)
#=========================

surv_obj <- Surv(
  time = merged_naive$time_lesion,
  event = merged_naive$event_status_lesion
)

fit <- survfit(
  surv_obj ~ gc_group,
  data = merged_naive
)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-"),
  xlab = "Months",
  ylab = "Lesion-free survival"
)



####NEDA 3 MAV E TEC PER SAMPLE SIZE

##### neda3 che si fermi al t30 #####

merged_ceci$GROUP <- factor(
  merged_ceci$GROUP,
  levels = c("MAV", "TEC")
)

# Ultimo EDSS disponibile per ogni paziente
merged_ceci <- merged_ceci %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T30, EDSS_T24, EDSS_T18,
             EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

# Peggioramento EDSS valutato tra basale e ultimo follow-up disponibile
merged_ceci <- merged_ceci %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

# Eventi NEDA3
merged_ceci <- merged_ceci %>%
  mutate(
    event_6_CT  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = EDSS_worsening | relapse_T30 == 1 | lesioni_T30 == 1
  )

# Tempo al primo evento
merged_ceci <- merged_ceci %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      TRUE ~ NA_real_
    ),
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 30, event_time_CT)
  )

# Kaplan-Meier
surv_obj4 <- Surv(time = merged_ceci$time_CT,
                  event = merged_ceci$event_status_CT)

fit4 <- survfit(surv_obj4 ~ GROUP, data = merged_ceci)

ggsurvplot(
  fit4,
  data = merged_ceci,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  xlab = "Months",
  ylab = "NEDA-free survival"
)

table(merged_ceci$GROUP[merged_ceci$event_status_CT == 0])






##### neda3 che si fermi al t24 #####

merged_ceci$GROUP <- factor(
  merged_ceci$GROUP,
  levels = c("MAV", "TEC")
)

# Ultimo EDSS disponibile per ogni paziente
merged_ceci <- merged_ceci %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T24, EDSS_T18,
             EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

# Peggioramento EDSS valutato tra basale e ultimo follow-up disponibile
merged_ceci <- merged_ceci %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

# Eventi NEDA3
merged_ceci <- merged_ceci %>%
  mutate(
    event_6_CT  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1
  )

# Tempo al primo evento
merged_ceci <- merged_ceci %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      TRUE ~ NA_real_
    ),
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 24, event_time_CT)
  )

# Kaplan-Meier
surv_obj4 <- Surv(time = merged_ceci$time_CT,
                  event = merged_ceci$event_status_CT)

fit4 <- survfit(surv_obj4 ~ GROUP, data = merged_ceci)

ggsurvplot(
  fit4,
  data = merged_ceci,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  xlab = "Months",
  ylab = "NEDA-free survival"
)

table(merged_ceci$GROUP[merged_ceci$event_status_CT == 0])







### neda3 al t18 ###

merged_ceci$GROUP <- factor(
  merged_ceci$GROUP,
  levels = c("MAV", "TEC")
)

# Ultimo EDSS disponibile per ogni paziente
merged_ceci <- merged_ceci %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T18,
             EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

# Peggioramento EDSS valutato tra basale e ultimo follow-up disponibile
merged_ceci <- merged_ceci %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

# Eventi NEDA3
merged_ceci <- merged_ceci %>%
  mutate(
    event_6_CT  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1
  )

# Tempo al primo evento
merged_ceci <- merged_ceci %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      TRUE ~ NA_real_
    ),
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 18, event_time_CT)
  )

# Kaplan-Meier
surv_obj4 <- Surv(time = merged_ceci$time_CT,
                  event = merged_ceci$event_status_CT)

fit4 <- survfit(surv_obj4 ~ GROUP, data = merged_ceci)

ggsurvplot(
  fit4,
  data = merged_ceci,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  xlab = "Months",
  ylab = "NEDA-free survival"
)

table(merged_ceci$GROUP[merged_ceci$event_status_CT == 0])






### neda3 al t12 ###

merged_ceci$GROUP <- factor(
  merged_ceci$GROUP,
  levels = c("MAV", "TEC")
)

# Ultimo EDSS disponibile per ogni paziente
merged_ceci <- merged_ceci %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(
        EDSS_T12, EDSS_T6, EDSS_T0
      )
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

# Peggioramento EDSS valutato tra basale e ultimo follow-up disponibile
merged_ceci <- merged_ceci %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

# Eventi NEDA3
merged_ceci <- merged_ceci %>%
  mutate(
    event_6_CT  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1
  )

# Tempo al primo evento
merged_ceci <- merged_ceci %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      TRUE ~ NA_real_
    ),
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 12, event_time_CT)
  )

# Kaplan-Meier
surv_obj4 <- Surv(time = merged_ceci$time_CT,
                  event = merged_ceci$event_status_CT)

fit4 <- survfit(surv_obj4 ~ GROUP, data = merged_ceci)

ggsurvplot(
  fit4,
  data = merged_ceci,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  xlab = "Months",
  ylab = "NEDA-free survival"
)

table(merged_ceci$GROUP[merged_ceci$event_status_CT == 0])







### neda3 al t6 ###

merged_ceci$GROUP <- factor(
  merged_ceci$GROUP,
  levels = c("MAV", "TEC")
)

# Ultimo EDSS disponibile per ogni paziente
merged_ceci <- merged_ceci %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

# Peggioramento EDSS valutato tra basale e ultimo follow-up disponibile
merged_ceci <- merged_ceci %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

# Eventi NEDA3
merged_ceci <- merged_ceci %>%
  mutate(
    event_6_CT  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1
  )

# Tempo al primo evento
merged_ceci <- merged_ceci %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      TRUE ~ NA_real_
    ),
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 6, event_time_CT)
  )

# Kaplan-Meier
surv_obj4 <- Surv(time = merged_ceci$time_CT,
                  event = merged_ceci$event_status_CT)

fit4 <- survfit(surv_obj4 ~ GROUP, data = merged_ceci)

ggsurvplot(
  fit4,
  data = merged_ceci,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  xlab = "Months",
  ylab = "NEDA-free survival"
)

table(merged_ceci$GROUP[merged_ceci$event_status_CT == 0])












merged_naive <- merged_naive %>%
  mutate(
    cambio_terapia = factor(
      cambio_terapia,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )


###### cambio terapia
#escludendo i non naive composito x cambio terapia
#############################
#=========================
# Creazione gruppi TEC_change+ / TEC_change-
#=========================

merged <- merged %>%
  mutate(
    therapy = ifelse(cambio_terapia == "1", 1, 0),
    therapy_group = case_when(
      therapy == 1 ~ "TEC_change+",
      therapy == 0 ~ "TEC_change-"
    )
  ) %>%
  mutate(
    therapy_group = factor(therapy_group,
                           levels = c("TEC_change+", "TEC_change-"))
  )

#=========================
# Filtro naive
#=========================

merged_naive <- merged %>%
  filter(naive != "no")

#=========================
# Ultimo EDSS disponibile
#=========================

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS
#=========================

merged_naive <- merged_naive %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi NEDA
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_6_C  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = EDSS_worsening | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = EDSS_worsening | relapse_T36 == 1 | lesioni_T36 == 1
  )

#=========================
# Tempo al primo evento
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_time_C = case_when(
      event_6_C  ~ 6,
      event_12_C ~ 12,
      event_18_C ~ 18,
      event_24_C ~ 24,
      event_30_C ~ 30,
      event_36_C ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_C = ifelse(is.na(event_time_C), 0, 1),
    time_C = ifelse(is.na(event_time_C), 36, event_time_C)
  )

#=========================
# Kaplan-Meier
#=========================

surv_obj <- Surv(time = merged_naive$time_C,
                 event = merged_naive$event_status_C)

fit <- survfit(surv_obj ~ therapy_group, data = merged_naive)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
 #legend.labs = c("TEC_change+", "TEC_change-"),
  xlab = "Months",
  ylab = "NEDA-free survival"
)





### relapse
#=========================
# Creazione gruppi TEC_change+ / TEC_change-
#=========================

merged <- merged %>%
  mutate(
    therapy = ifelse(cambio_terapia == "1", 1, 0),
    therapy_group = case_when(
      therapy == 1 ~ "TEC_change+",
      therapy == 0 ~ "TEC_change-"
    )
  ) %>%
  mutate(
    therapy_group = factor(therapy_group,
                           levels = c("TEC_change+", "TEC_change-"))
  )

#=========================
# Filtro naive
#=========================

merged_naive <- merged %>%
  filter(naive != "no")

#=========================
# Ultimo EDSS disponibile (lo lasciamo, non influisce)
#=========================

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS (non usato nella survival relapse)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi RELAPSE (solo relapse)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_6_R  = relapse_T6  == 1,
    event_12_R = relapse_T12 == 1,
    event_18_R = relapse_T18 == 1,
    event_24_R = relapse_T24 == 1,
    event_30_R = relapse_T30 == 1,
    event_36_R = relapse_T36 == 1
  )

#=========================
# Tempo al primo relapse
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_time_R = case_when(
      event_6_R  ~ 6,
      event_12_R ~ 12,
      event_18_R ~ 18,
      event_24_R ~ 24,
      event_30_R ~ 30,
      event_36_R ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_R = ifelse(is.na(event_time_R), 0, 1),
    time_R = ifelse(is.na(event_time_R), 36, event_time_R)
  )

#=========================
# Kaplan-Meier (solo relapse)
#=========================

surv_obj <- Surv(time = merged_naive$time_R,
                 event = merged_naive$event_status_R)

fit <- survfit(surv_obj ~ therapy_group, data = merged_naive)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC_change+", "TEC_change-"),
  xlab = "Months",
  ylab = "Relapse-free survival"
)

# lesioni
#=========================
# Creazione gruppi TEC_change+ / TEC_change-
#=========================

merged <- merged %>%
  mutate(
    therapy = ifelse(cambio_terapia == "1", 1, 0),
    therapy_group = case_when(
      therapy == 1 ~ "TEC_change+",
      therapy == 0 ~ "TEC_change-"
    )
  ) %>%
  mutate(
    therapy_group = factor(therapy_group,
                           levels = c("TEC_change+", "TEC_change-"))
  )

#=========================
# Filtro naive
#=========================

merged_naive <- merged %>%
  filter(naive != "no")

#=========================
# Ultimo EDSS disponibile (lo lasciamo, non influisce)
#=========================

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS (non usato nella survival lesioni)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi LESIONI (solo lesioni)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_6_L  = lesioni_T6  == 1,
    event_12_L = lesioni_T12 == 1,
    event_18_L = lesioni_T18 == 1,
    event_24_L = lesioni_T24 == 1,
    event_30_L = lesioni_T30 == 1,
    event_36_L = lesioni_T36 == 1
  )

#=========================
# Tempo al primo evento LESIONI
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_time_L = case_when(
      event_6_L  ~ 6,
      event_12_L ~ 12,
      event_18_L ~ 18,
      event_24_L ~ 24,
      event_30_L ~ 30,
      event_36_L ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_L = ifelse(is.na(event_time_L), 0, 1),
    time_L = ifelse(is.na(event_time_L), 36, event_time_L)
  )

#=========================
# Kaplan-Meier (solo lesioni)
#=========================

surv_obj <- Surv(time = merged_naive$time_L,
                 event = merged_naive$event_status_L)

fit <- survfit(surv_obj ~ therapy_group, data = merged_naive)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC_change+", "TEC_change-"),
  xlab = "Months",
  ylab = "Lesion-free survival"
)




#### spinal lesion composito
#escludendo i non naive composito x cambio terapia
#############################
#=========================
# Creazione gruppi TEC_spinal+ / TEC_spinal-
#=========================

merged <- merged %>%
  mutate(
    spinal = ifelse(bone_marrow_lesions == 1, 1, 0),
    therapy_group = case_when(
      spinal == 1 ~ "TEC_spinal+",
      spinal == 0 ~ "TEC_spinal-"
    )
  ) %>%
  mutate(
    therapy_group = factor(therapy_group,
                           levels = c("TEC_spinal+", "TEC_spinal-"))
  )

#=========================
# Filtro naive
#=========================

merged_naive <- merged %>%
  filter(naive != "no")

#=========================
# Ultimo EDSS disponibile
#=========================

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS
#=========================

merged_naive <- merged_naive %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi NEDA (evento composito)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_6_C  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = EDSS_worsening | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = EDSS_worsening | relapse_T36 == 1 | lesioni_T36 == 1
  )

#=========================
# Tempo al primo evento
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_time_C = case_when(
      event_6_C  ~ 6,
      event_12_C ~ 12,
      event_18_C ~ 18,
      event_24_C ~ 24,
      event_30_C ~ 30,
      event_36_C ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_C = ifelse(is.na(event_time_C), 0, 1),
    time_C = ifelse(is.na(event_time_C), 36, event_time_C)
  )

#=========================
# Kaplan-Meier
#=========================

surv_obj <- Surv(time = merged_naive$time_C,
                 event = merged_naive$event_status_C)

fit <- survfit(surv_obj ~ therapy_group, data = merged_naive)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC_spinal+", "TEC_spinal-"),
  xlab = "Months",
  ylab = "NEDA-free survival"
)




#### subtentorial composito
#### subtentorial lesion composito
#escludendo i non naive composito x cambio terapia
#############################
#=========================
# Creazione gruppi TEC_subt+ / TEC_subt-
#=========================

merged_naive <- merged_naive %>%
  mutate(
    subt = ifelse(subtentorial_lesions == 1, 1, 0),
    therapy_group = case_when(
      subt == 1 ~ "TEC_subt+",
      subt == 0 ~ "TEC_subt-"
    )
  ) %>%
  mutate(
    therapy_group = factor(therapy_group,
                           levels = c("TEC_subt+", "TEC_subt-"))
  )

#=========================
# Filtro naive
#=========================

merged_naive <- merged %>%
  filter(naive != "no")

#=========================
# Ultimo EDSS disponibile
#=========================

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS
#=========================

merged_naive <- merged_naive %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi NEDA (evento composito)
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_6_C  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = EDSS_worsening | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = EDSS_worsening | relapse_T36 == 1 | lesioni_T36 == 1
  )

#=========================
# Tempo al primo evento
#=========================

merged_naive <- merged_naive %>%
  mutate(
    event_time_C = case_when(
      event_6_C  ~ 6,
      event_12_C ~ 12,
      event_18_C ~ 18,
      event_24_C ~ 24,
      event_30_C ~ 30,
      event_36_C ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_C = ifelse(is.na(event_time_C), 0, 1),
    time_C = ifelse(is.na(event_time_C), 36, event_time_C)
  )

#=========================
# Kaplan-Meier
#=========================

surv_obj <- Surv(time = merged_naive$time_C,
                 event = merged_naive$event_status_C)

fit <- survfit(surv_obj ~ therapy_group, data = merged_naive)

ggsurvplot(
  fit,
  data = merged_naive,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC_subt+", "TEC_subt-"),
  xlab = "Months",
  ylab = "NEDA-free survival"
)

merged <- merged %>%
  mutate(
    cambio_terapia = factor(
      cambio_terapia,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

####### bmi composito senza filtrare i naive
#### BMI composito
#escludendo i non naive composito x cambio terapia
#############################
#=========================
# Creazione gruppi TEC_highBMI / TEC_lowBMI
#=========================

merged <- merged %>%
  mutate(
    bmi_group_var = ifelse(bmi >= 24, 1, 0),   # esempio: high BMI = sovrappeso/obeso
    therapy_group = case_when(
      bmi_group_var == 1 ~ "TEC_highBMI",
      bmi_group_var == 0 ~ "TEC_lowBMI"
    )
  ) %>%
  mutate(
    therapy_group = factor(therapy_group,
                           levels = c("TEC_highBMI", "TEC_lowBMI"))
  )

#=========================
# NESSUN filtro naive
#=========================

#=========================
# Ultimo EDSS disponibile
#=========================

merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS
#=========================


merged <- merged %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi NEDA (evento composito)
#=========================

merged <- merged %>%
  mutate(
    event_6_C  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = EDSS_worsening | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = EDSS_worsening | relapse_T36 == 1 | lesioni_T36 == 1
  )

#=========================
# Tempo al primo evento
#=========================

merged <- merged %>%
  mutate(
    event_time_C = case_when(
      event_6_C  ~ 6,
      event_12_C ~ 12,
      event_18_C ~ 18,
      event_24_C ~ 24,
      event_30_C ~ 30,
      event_36_C ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_C = ifelse(is.na(event_time_C), 0, 1),
    time_C = ifelse(is.na(event_time_C), 36, event_time_C)
  )

#=========================
# Kaplan-Meier
#=========================

surv_obj <- Surv(time = merged$time_C,
                 event = merged$event_status_C)

fit <- survfit(surv_obj ~ therapy_group, data = merged)

ggsurvplot(
  fit,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC_highBMI", "TEC_lowBMI"),
  xlab = "Months",
  ylab = "NEDA-free survival"
)




####### bmi composito filtrando i pz che non hanno cambiato terapia
#### BMI composito
#############################
#=========================
# Creazione gruppi TEC_highBMI / TEC_lowBMI
#=========================

merged <- merged %>%
  mutate(
    bmi_group_var = ifelse(bmi >= 24, 1, 0),   # esempio: high BMI = sovrappeso/obeso
    therapy_group = case_when(
      bmi_group_var == 1 ~ "TEC_highBMI",
      bmi_group_var == 0 ~ "TEC_lowBMI"
    )
  ) %>%
  mutate(
    therapy_group = factor(therapy_group,
                           levels = c("TEC_highBMI", "TEC_lowBMI"))
  )

#=========================
# filtro cambio_terapia=no
#=========================

merged <- merged %>%
  filter(cambio_terapia != "No")


class(merged$cambio_terapia)

#=========================
# Ultimo EDSS disponibile
#=========================

merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_last = {
      x <- c(EDSS_T36, EDSS_T30, EDSS_T24,
             EDSS_T18, EDSS_T12, EDSS_T6, EDSS_T0)
      x[which(!is.na(x))[1]]
    }
  ) %>%
  ungroup()

#=========================
# Peggioramento EDSS
#=========================


merged <- merged %>%
  mutate(
    EDSS_worsening = case_when(
      EDSS_T0 == 0 ~ (EDSS_last - EDSS_T0) >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_last - EDSS_T0) >= 1,
      EDSS_T0 > 5 ~ (EDSS_last - EDSS_T0) >= 0.5,
      TRUE ~ FALSE
    )
  )

#=========================
# Eventi NEDA (evento composito)
#=========================

merged <- merged %>%
  mutate(
    event_6_C  = EDSS_worsening | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = EDSS_worsening | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = EDSS_worsening | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = EDSS_worsening | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = EDSS_worsening | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = EDSS_worsening | relapse_T36 == 1 | lesioni_T36 == 1
  )

#=========================
# Tempo al primo evento
#=========================

merged <- merged %>%
  mutate(
    event_time_C = case_when(
      event_6_C  ~ 6,
      event_12_C ~ 12,
      event_18_C ~ 18,
      event_24_C ~ 24,
      event_30_C ~ 30,
      event_36_C ~ 36,
      TRUE ~ NA_real_
    ),
    event_status_C = ifelse(is.na(event_time_C), 0, 1),
    time_C = ifelse(is.na(event_time_C), 36, event_time_C)
  )

#=========================
# Kaplan-Meier
#=========================

surv_obj <- Surv(time = merged$time_C,
                 event = merged$event_status_C)

fit <- survfit(surv_obj ~ therapy_group, data = merged)

ggsurvplot(
  fit,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC_highBMI", "TEC_lowBMI"),
  xlab = "Months",
  ylab = "NEDA-free survival"
)










######### COX PLOT #############
library(dplyr)
library(survival)
library(survminer)

# ️Dataset metadata
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_ceci.csv")
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")

## GC TREATMENT
merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes","1") ~ 1,
      gadolinium_contrast %in% c("No","0")  ~ 0,
      TRUE ~ NA_real_
    ),
    gadolinium_contrast = factor(gadolinium_contrast, levels = c(0,1)),
    MEDAS = as.numeric(str_replace(MEDAS, ",", ".")),
    bmi   = as.numeric(str_replace(bmi, ",", ".")),
    naive        = factor(naive),
    sex          = factor(sex),
    gc_treatment = factor(gc_treatment),
    GROUP        = factor(GROUP)
  )


merged <- merged %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# NEDA-3

merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# dataframe per cox
df_cox <- merged %>%
  select(
    time_followup,
    status_NEDA3,
    MEDAS,
    age_at_T0,
    naive,
    sex,
    bmi,
    gc_treatment,
    GROUP
  ) %>%
  na.omit()


# modello cox ridotto

cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    MEDAS + age_at_T0 + naive + sex + bmi + gc_treatment + GROUP,
  data = df_cox
)

summary(cox_neda3)


# forest 

# Estrai i coefficienti dal modello Cox
df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))  # ordina dall'alto verso il basso
  )

# Forest plot
ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )





##### GC AT LEAST

merged <- merged %>%
  mutate(
    gc_at_least = factor(
      if_else(
        if_any(starts_with("cortisone_T"), ~ . > 0),
        "positive",
        "negative"
      ),
      levels = c("negative", "positive")
    )
  ) 


merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes","1") ~ 1,
      gadolinium_contrast %in% c("No","0")  ~ 0,
      TRUE ~ NA_real_
    ),
    gadolinium_contrast = factor(gadolinium_contrast, levels = c(0,1)),
    MEDAS = as.numeric(str_replace(MEDAS, ",", ".")),
    bmi   = as.numeric(str_replace(bmi, ",", ".")),
    naive        = factor(naive),
    sex          = factor(sex),
    gc_at_least = factor(gc_at_least),
    GROUP        = factor(GROUP)
  )


merged <- merged %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# NEDA-3

merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# dataframe per cox
df_cox <- merged %>%
  select(
    time_followup,
    status_NEDA3,
    MEDAS,
    age_at_T0,
    naive,
    sex,
    bmi,
    gc_at_least,
    GROUP
  ) %>%
  na.omit()


# modello cox ridotto

cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    MEDAS + age_at_T0 + naive + sex + bmi + gc_at_least + GROUP,
  data = df_cox
)

summary(cox_neda3)


# forest 

# Estrai i coefficienti dal modello Cox
df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))  # ordina dall'alto verso il basso
  )

# Forest plot
ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )


########################################
# aggiungo bmi in merged
merged <- merged %>%
  mutate(
    bmi = ifelse(ID == "TEC25", 21.88, bmi)
  )

merged <- merged %>%
  mutate(
    bmi = ifelse(ID == "TEC26", 22.46, bmi)
  )

merged <- merged %>%
  mutate(
    bmi = ifelse(ID == "TEC27", 25.17, bmi)
  )


## aggiungo gli stessi bmi anche in first report

first_report <- first_report %>%
  mutate(
    bmi = ifelse(ID == "TEC25", 21.88, bmi)
  )

first_report <- first_report %>%
  mutate(
    bmi = ifelse(ID == "TEC26", 22.46, bmi)
  )

first_report <- first_report %>%
  mutate(
    bmi = ifelse(ID == "TEC27", 25.17, bmi)
  )

# e in merged_final
merged_final <- merged_final %>%
  mutate(
    bmi = ifelse(ID == "TEC25", 21.88, bmi)
  )

merged_final <- merged_final %>%
  mutate(
    bmi = ifelse(ID == "TEC26", 22.46, bmi)
  )

merged_final <- merged_final %>%
  mutate(
    bmi = ifelse(ID == "TEC27", 25.17, bmi)
  )


## COX GC TREATMENT SOLO TEC 
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")
merged_naive <- merged %>% 
  filter(naive == "yes")


merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes","1") ~ 1,
      gadolinium_contrast %in% c("No","0")  ~ 0,
      TRUE ~ NA_real_
    ),
    gadolinium_contrast = factor(gadolinium_contrast, levels = c(0,1)),
    
    bmi = as.numeric(str_replace(bmi, ",", ".")),
    
    lesion_burden       = factor(lesion_burden, levels = c(0,1)),
    bone_marrow_lesions = factor(bone_marrow_lesions, levels = c(0,1)),
    subtentorial_lesions = factor(subtentorial_lesions, levels = c(0,1)),
    
    gc_treatment = factor(gc_treatment)
  )


merged <- merged %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# NEDA-3

merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# dataframe per cox — SOLO VARIABILI NON CLINICHE
df_cox <- merged %>%
  select(
    time_followup,
    status_NEDA3,
    bmi,
    lesion_burden,
    gadolinium_contrast,
    bone_marrow_lesions,
    subtentorial_lesions,
    gc_treatment
  ) %>%
  na.omit()


# modello cox ridotto — SOLO VARIABILI NON CLINICHE
cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    bmi + lesion_burden + gadolinium_contrast +
    bone_marrow_lesions + subtentorial_lesions + gc_treatment,
  data = df_cox
)

summary(cox_neda3)


# forest 

df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))
  )

ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )



### GC TREATMENT SOLO TEC NAIVE

merged_naive <- merged_naive %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes","1") ~ 1,
      gadolinium_contrast %in% c("No","0")  ~ 0,
      TRUE ~ NA_real_
    ),
    gadolinium_contrast = factor(gadolinium_contrast, levels = c(0,1)),
    
    bmi = as.numeric(str_replace(bmi, ",", ".")),
    
    lesion_burden       = factor(lesion_burden, levels = c(0,1)),
    bone_marrow_lesions = factor(bone_marrow_lesions, levels = c(0,1)),
    subtentorial_lesions = factor(subtentorial_lesions, levels = c(0,1)),
    
    gc_treatment = factor(gc_treatment)
  )


merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# NEDA-3

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# dataframe per cox — SOLO VARIABILI NON CLINICHE
df_cox <- merged_naive %>%
  select(
    time_followup,
    status_NEDA3,
    bmi,
    lesion_burden,
    gadolinium_contrast,
    bone_marrow_lesions,
    subtentorial_lesions,
    gc_treatment
  ) %>%
  na.omit()


# modello cox ridotto — SOLO VARIABILI NON CLINICHE
cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    bmi + lesion_burden + gadolinium_contrast +
    bone_marrow_lesions + subtentorial_lesions + gc_treatment,
  data = df_cox
)

summary(cox_neda3)


# forest 

df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))
  )

ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )








######################################################################à
# GC AT LEAST SOLO TEC
# creo colonna gc at least con livelli corretti 

merged <- merged %>%
  mutate(
    cortisone_any = rowSums(select(., starts_with("cortisone_T")) != 0, na.rm = TRUE) > 0,
    
    gc_at_least = case_when(
      gc_treatment == "negative" & !cortisone_any ~ 0,
      gc_treatment == "positive" & !cortisone_any ~ 1,
      gc_treatment == "negative" & cortisone_any ~ 2,
      gc_treatment == "positive" & cortisone_any ~ 3,
      TRUE ~ NA_real_
    ),
    
    gc_at_least = factor(gc_at_least, levels = c(0,1,2,3))
  ) %>%
  select(-cortisone_any)




class(merged$gc_at_least) #deve essere factor


merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes","1") ~ 1,
      gadolinium_contrast %in% c("No","0")  ~ 0,
      TRUE ~ NA_real_
    ),
    gadolinium_contrast = factor(gadolinium_contrast, levels = c(0,1)),
    
    bmi = as.numeric(str_replace(bmi, ",", ".")),
    
    lesion_burden       = factor(lesion_burden, levels = c(0,1)),
    bone_marrow_lesions = factor(bone_marrow_lesions, levels = c(0,1)),
    subtentorial_lesions = factor(subtentorial_lesions, levels = c(0,1)),
    
    gc_at_least = factor(gc_at_least)
  )


merged <- merged %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# NEDA-3

merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# dataframe per cox — SOLO VARIABILI NON CLINICHE
df_cox <- merged %>%
  select(
    time_followup,
    status_NEDA3,
    bmi,
    lesion_burden,
    gadolinium_contrast,
    bone_marrow_lesions,
    subtentorial_lesions,
    gc_at_least
  ) %>%
  na.omit()


# modello cox ridotto — SOLO VARIABILI NON CLINICHE
cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    bmi + lesion_burden + gadolinium_contrast +
    bone_marrow_lesions + subtentorial_lesions + gc_at_least,
  data = df_cox
)

summary(cox_neda3)


# forest 

df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))
  )

ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )




#### GC AT LEAST PER TEC NAIVE

merged_naive <- merged_naive %>%
  mutate(
    cortisone_any = rowSums(select(., starts_with("cortisone_T")) != 0, na.rm = TRUE) > 0,
    
    gc_at_least = case_when(
      gc_treatment == "negative" & !cortisone_any ~ 0,
      gc_treatment == "positive" & !cortisone_any ~ 1,
      gc_treatment == "negative" & cortisone_any ~ 2,
      gc_treatment == "positive" & cortisone_any ~ 3,
      TRUE ~ NA_real_
    ),
    
    gc_at_least = factor(gc_at_least, levels = c(0,1,2,3))
  ) %>%
  select(-cortisone_any)




class(merged_naive$gc_at_least) #deve essere factor


merged_naive <- merged_naive %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes","1") ~ 1,
      gadolinium_contrast %in% c("No","0")  ~ 0,
      TRUE ~ NA_real_
    ),
    gadolinium_contrast = factor(gadolinium_contrast, levels = c(0,1)),
    
    bmi = as.numeric(str_replace(bmi, ",", ".")),
    
    lesion_burden       = factor(lesion_burden, levels = c(0,1)),
    bone_marrow_lesions = factor(bone_marrow_lesions, levels = c(0,1)),
    subtentorial_lesions = factor(subtentorial_lesions, levels = c(0,1)),
    
    gc_at_least = factor(gc_at_least)
  )


merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# NEDA-3

merged_naive <- merged_naive %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# dataframe per cox — SOLO VARIABILI NON CLINICHE
df_cox <- merged_naive %>%
  select(
    time_followup,
    status_NEDA3,
    bmi,
    lesion_burden,
    gadolinium_contrast,
    bone_marrow_lesions,
    subtentorial_lesions,
    gc_at_least
  ) %>%
  na.omit()


# modello cox ridotto — SOLO VARIABILI NON CLINICHE
cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    bmi + lesion_burden + gadolinium_contrast +
    bone_marrow_lesions + subtentorial_lesions + gc_at_least,
  data = df_cox
)

summary(cox_neda3)


# forest 

df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))
  )

ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )










# Modello 1 - demographics and lifestyle: bmi + sex + age + medas (+ smoking + physical activity)
# --- Pulizia variabili richieste ---
merged <- merged %>%
  mutate(
    MEDAS = as.numeric(MEDAS),
    bmi   = as.numeric(str_replace(bmi, ",", ".")),
  )


# --- TIME FOLLOW-UP ---
merged <- merged %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# --- NEDA-3 ---
merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# --- DATAFRAME PER COX: SOLO VARIABILI RICHIESTE ---
df_cox <- merged %>%
  select(
    time_followup,
    status_NEDA3,
    MEDAS,
    bmi,
  ) %>%
  na.omit()


# --- MODELLO COX ---
cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    MEDAS + bmi,
  data = df_cox
)

summary(cox_neda3)


# --- FOREST PLOT ---
df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))
  )

ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )




# Modello 2 - clinical status at T0 and treatment history: naive + gc_at_least + gadolinium + lesion burden + spinal + subtentorial.
# --- Pulizia variabili richieste ---
merged <- merged %>%
  mutate(
    lesion_burden = factor(lesion_burden),
    bone_marrow_lesions = factor(bone_marrow_lesions),
  )


# --- TIME FOLLOW-UP ---
merged <- merged %>%
  rowwise() %>%
  mutate(
    time_followup = case_when(
      !is.na(EDSS_T36) ~ 36,
      !is.na(EDSS_T30) ~ 30,
      !is.na(EDSS_T24) ~ 24,
      !is.na(EDSS_T18) ~ 18,
      !is.na(EDSS_T12) ~ 12,
      !is.na(EDSS_T6)  ~ 6,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


# --- NEDA-3 ---
merged <- merged %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T18,
      EDSS_T24, EDSS_T30, EDSS_T36
    )))
  ) %>%
  ungroup() %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0,
    
    EDSS_progression = case_when(
      EDSS_T0 == 0                ~ delta_EDSS >= 1.5,
      EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ delta_EDSS >= 1,
      EDSS_T0 > 5                 ~ delta_EDSS >= 0.5,
      TRUE                        ~ NA
    ),
    
    clinical_activity = (relapse_T6  > 0 |
                           relapse_T12 > 0 |
                           relapse_T18 > 0 |
                           relapse_T24 > 0 |
                           relapse_T30 > 0 |
                           relapse_T36 > 0),
    
    radiological_activity = (lesioni_T6  > 0 |
                               lesioni_T12 > 0 |
                               lesioni_T18 > 0 |
                               lesioni_T24 > 0 |
                               lesioni_T30 > 0 |
                               lesioni_T36 > 0),
    
    status_NEDA3 = ifelse(
      clinical_activity | radiological_activity | EDSS_progression,
      1, 0
    )
  )


# --- DATAFRAME PER COX: SOLO VARIABILI RICHIESTE ---
df_cox <- merged %>%
  select(
    time_followup,
    status_NEDA3,
    lesion_burden,
    bone_marrow_lesions,
  ) %>%
  na.omit()


# --- MODELLO COX ---
cox_neda3 <- coxph(
  Surv(time_followup, status_NEDA3) ~ 
    lesion_burden + bone_marrow_lesions,
  data = df_cox
)

summary(cox_neda3)


# --- FOREST PLOT ---
df_forest <- tidy(cox_neda3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    HR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high),
    term = factor(term, levels = rev(term))
  )

ggplot(df_forest, aes(x = term, y = HR)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Forest plot – Cox NEDA-3",
    x = "",
    y = "Hazard Ratio (HR)"
  )












































































####### SANKEY PLOT ###########
sankey_data <- merged %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    )
  )

links <- bind_rows(
  sankey_data %>% count(T0, T6) %>% rename(source=T0,target=T6,value=n),
  sankey_data %>% count(T6, T12) %>% rename(source=T6,target=T12,value=n),
  sankey_data %>% count(T12, T18) %>% rename(source=T12,target=T18,value=n),
  sankey_data %>% count(T18, T24) %>% rename(source=T18,target=T24,value=n),
  sankey_data %>% count(T24, T30) %>% rename(source=T24,target=T30,value=n),
  sankey_data %>% count(T30, T36) %>% rename(source=T30,target=T36,value=n)
)

links <- bind_rows(
  sankey_data %>% transmute(source=paste0("T0_",T0), target=paste0("T6_",T6)),
  sankey_data %>% transmute(source=paste0("T6_",T6), target=paste0("T12_",T12)),
  sankey_data %>% transmute(source=paste0("T12_",T12), target=paste0("T18_",T18)),
  sankey_data %>% transmute(source=paste0("T18_",T18), target=paste0("T24_",T24)),
  sankey_data %>% transmute(source=paste0("T24_",T24), target=paste0("T30_",T30)),
  sankey_data %>% transmute(source=paste0("T30_",T30), target=paste0("T36_",T36))
) %>%
  count(source, target, name="value")

nodes <- data.frame(
  name = unique(c(links$source, links$target))
)

links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 25
)

######### SANKEY PLOT MODIFICATO
library(dplyr)
library(networkD3)

# 1️⃣ Creazione dataset con fasce EDSS
sankey_data <- merged %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    )
  )

# 2️⃣ Creazione dei link tra timepoints
links <- bind_rows(
  sankey_data %>% transmute(source=paste0("T0_",T0), target=paste0("T6_",T6)),
  sankey_data %>% transmute(source=paste0("T6_",T6), target=paste0("T12_",T12)),
  sankey_data %>% transmute(source=paste0("T12_",T12), target=paste0("T18_",T18)),
  sankey_data %>% transmute(source=paste0("T18_",T18), target=paste0("T24_",T24)),
  sankey_data %>% transmute(source=paste0("T24_",T24), target=paste0("T30_",T30)),
  sankey_data %>% transmute(source=paste0("T30_",T30), target=paste0("T36_",T36))
) %>%
  count(source, target, name="value")

# 3️⃣ Creazione nodi con colori e posizioni verticali forzate
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  mutate(
    color = case_when(
      grepl("≥6", name) ~ "#E74C3C",    # rosso per EDSS ≥6 (in alto)
      grepl("3-5.5", name) ~ "#F1C40F", # giallo per EDSS 3–5.5 (centro)
      grepl("<3", name) ~ "#2ECC71",    # verde per EDSS <3 (in basso)
      TRUE ~ "#BDC3C7"
    ),
    y = case_when(
      grepl("≥6", name) ~ 0.1,   # alto
      grepl("3-5.5", name) ~ 0.5, # centro
      grepl("<3", name) ~ 0.9     # basso
    )
  )

# 4️⃣ Rimappatura indici
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# 5️⃣ Sankey plot con layout bloccato e colori corretti
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)






######### SANKEY PLOT CORRETTO DA DORA
library(dplyr)
library(networkD3)

# Creazione dataset con fasce EDSS
# Creazione dei link tra timepoints
links <- bind_rows(
  sankey_data %>% transmute(source = paste0("T0_",  T0),  target = paste0("T6_",  T6)),
  sankey_data %>% transmute(source = paste0("T6_",  T6),  target = paste0("T12_", T12)),
  sankey_data %>% transmute(source = paste0("T12_", T12), target = paste0("T18_", T18)),
  sankey_data %>% transmute(source = paste0("T18_", T18), target = paste0("T24_", T24)),
  sankey_data %>% transmute(source = paste0("T24_", T24), target = paste0("T30_", T30)),
  sankey_data %>% transmute(source = paste0("T30_", T30), target = paste0("T36_", T36))
) %>%
  count(source, target, name = "value")

# Creazione nodi con gruppo "pulito" (senza prefisso Txx_) per il colore
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  mutate(
    group = case_when(
      grepl("≥6", name)    ~ "≥6",
      grepl("3-5.5", name) ~ "3-5.5",
      grepl("<3", name)    ~ "<3",
      TRUE ~ "altro"
    ),
    y = case_when(
      grepl("≥6", name)    ~ 0.1,  # alto
      grepl("3-5.5", name) ~ 0.5,  # centro
      grepl("<3", name)    ~ 0.9   # basso
    )
  )

# Controllo rapido: verifica che i gruppi siano assegnati correttamente
print(table(nodes$group))

# Rimappatura indici (da 0 per JS)
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Aggiungi il gruppo anche ai link (basato sul nodo di destinazione)
links <- links %>%
  mutate(group = nodes$group[target + 1])

# Sankey plot con colori corretti
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)



### ulteriori modifiche sankey
library(dplyr)
library(tidyr)
library(networkD3)

######### SANKEY PLOT CORRETTO

# Librerie necessarie
library(dplyr)
library(tidyr)
library(networkD3)
library(htmlwidgets)
library(webshot2)

# Creazione dei link tra timepoints
links <- bind_rows(
  sankey_data %>% transmute(source = paste0("T0_",  T0),  target = paste0("T6_",  T6)),
  sankey_data %>% transmute(source = paste0("T6_",  T6),  target = paste0("T12_", T12)),
  sankey_data %>% transmute(source = paste0("T12_", T12), target = paste0("T18_", T18)),
  sankey_data %>% transmute(source = paste0("T18_", T18), target = paste0("T24_", T24)),
  sankey_data %>% transmute(source = paste0("T24_", T24), target = paste0("T30_", T30)),
  sankey_data %>% transmute(source = paste0("T30_", T30), target = paste0("T36_", T36))
) %>%
  count(source, target, name = "value")

# Creazione nodi
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  separate(name, into = c("time", "edss"), sep = "_", remove = FALSE) %>%
  mutate(
    group = case_when(
      grepl("≥6", edss)    ~ "≥6",
      grepl("3-5.5", edss) ~ "3-5.5",
      grepl("<3", edss)    ~ "<3",
      TRUE ~ "altro"
    ),
    time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36")),
    group_order = factor(group, levels = c("≥6","3-5.5","<3"))
  ) %>%
  arrange(time, group_order) %>%
  mutate(name = paste(time, edss, sep = "_")) %>%
  select(name, group)

# Rimappatura indici JS
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Assegna gruppo ai link in base al nodo di destinazione
links <- links %>%
  mutate(group = nodes$group[target + 1])

# Creazione Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)

sankey_plot
# Salvataggio HTML interattivo
htmlwidgets::saveWidget(
  sankey_plot,
  file = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS.html",
  selfcontained = TRUE
)

# Salvataggio PNG ad alta risoluzione
webshot2::webshot(
  url = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS.html",
  file = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS.png",
  vwidth = 1600,
  vheight = 900,
  zoom = 2
)



## sankey TEC
# Librerie necessarie
library(dplyr)
library(tidyr)
library(networkD3)
library(htmlwidgets)
library(webshot2)


# 1️⃣ Creazione dataset con fasce EDSS
sankey_data <- merged %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    )
  )

# 2️⃣ Creazione dei link tra timepoints
links <- bind_rows(
  sankey_data %>% transmute(source = paste0("T0_",  T0),  target = paste0("T6_",  T6)),
  sankey_data %>% transmute(source = paste0("T6_",  T6),  target = paste0("T12_", T12)),
  sankey_data %>% transmute(source = paste0("T12_", T12), target = paste0("T18_", T18)),
  sankey_data %>% transmute(source = paste0("T18_", T18), target = paste0("T24_", T24)),
  sankey_data %>% transmute(source = paste0("T24_", T24), target = paste0("T30_", T30)),
  sankey_data %>% transmute(source = paste0("T30_", T30), target = paste0("T36_", T36))
) %>%
  count(source, target, name = "value")

# 3️⃣ Creazione nodi
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  separate(name, into = c("time", "edss"), sep = "_", remove = FALSE) %>%
  mutate(
    group = case_when(
      grepl("≥6", edss)    ~ "≥6",
      grepl("3-5.5", edss) ~ "3-5.5",
      grepl("<3", edss)    ~ "<3",
      TRUE ~ "altro"
    ),
    time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36")),
    group_order = factor(group, levels = c("≥6","3-5.5","<3"))
  ) %>%
  arrange(time, group_order) %>%
  mutate(name = paste(time, edss, sep = "_")) %>%
  select(name, group)

# Controllo rapido
print(table(nodes$group))

# 4️⃣ Rimappatura indici JS
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Assegna gruppo ai link in base al nodo di destinazione
links <- links %>%
  mutate(group = nodes$group[target + 1])

# 5️⃣ Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)

# 6️⃣ Salvataggio HTML
htmlwidgets::saveWidget(
  sankey_plot,
  file = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS_TEC.html",
  selfcontained = TRUE
)

# 7️⃣ Salvataggio PNG
webshot2::webshot(
  url = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS_TEC.html",
  file = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS_TEC.png",
  vwidth = 1600,
  vheight = 900,
  zoom = 2
)


# SANKEY TOTALE
# Librerie necessarie
library(dplyr)
library(tidyr)
library(networkD3)
library(htmlwidgets)
library(webshot2)


# 1️⃣ Creazione dataset con fasce EDSS
sankey_data <- merged%>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      TRUE ~ "≥6"
    )
  )

# 2️⃣ Creazione dei link tra timepoints
links <- bind_rows(
  sankey_data %>% transmute(source = paste0("T0_",  T0),  target = paste0("T6_",  T6)),
  sankey_data %>% transmute(source = paste0("T6_",  T6),  target = paste0("T12_", T12)),
  sankey_data %>% transmute(source = paste0("T12_", T12), target = paste0("T18_", T18)),
  sankey_data %>% transmute(source = paste0("T18_", T18), target = paste0("T24_", T24)),
  sankey_data %>% transmute(source = paste0("T24_", T24), target = paste0("T30_", T30)),
  sankey_data %>% transmute(source = paste0("T30_", T30), target = paste0("T36_", T36))
) %>%
  count(source, target, name = "value")

# 3️⃣ Creazione nodi
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  separate(name, into = c("time", "edss"), sep = "_", remove = FALSE) %>%
  mutate(
    group = case_when(
      grepl("≥6", edss)    ~ "≥6",
      grepl("3-5.5", edss) ~ "3-5.5",
      grepl("<3", edss)    ~ "<3",
      TRUE ~ "altro"
    ),
    time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36")),
    group_order = factor(group, levels = c("≥6","3-5.5","<3"))
  ) %>%
  arrange(time, group_order) %>%
  mutate(name = paste(time, edss, sep = "_")) %>%
  select(name, group)

# Controllo rapido
print(table(nodes$group))

# 4️⃣ Rimappatura indici JS
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Assegna gruppo ai link in base al nodo di destinazione
links <- links %>%
  mutate(group = nodes$group[target + 1])

# 5️⃣ Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)

# 6️⃣ Salvataggio HTML
htmlwidgets::saveWidget(
  sankey_plot,
  file = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS.html",
  selfcontained = TRUE
)

# 7️⃣ Salvataggio PNG
webshot2::webshot(
  url = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS.html",
  file = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/sankey_EDSS.png",
  vwidth = 1600,
  vheight = 900,
  zoom = 2
)







merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")
merged_subset <- read_excel("input/merged_subset.xlsx")
merged_finale <- merged %>%
  inner_join(merged_subset, by = "ID")

merged_finale_naive <- merged %>%
  inner_join(merged_subset, by = "ID")



merged_final <- merged_final %>%
  filter(ID != "TEC12")

merged_final <- merged_final %>%
  filter(ID != "TEC13")

merged_final <- merged_final %>%
  filter(ID != "TEC18")

merged_final <- merged_final %>%
  filter(ID != "TEC23")

#### SALVARE
write.csv2(merged, "C:/AURI/Thesis_bagetto/input/merged.csv", row.names = FALSE)
write.csv2(first_report, "C:/AURI/Thesis_bagetto/input/first_report.csv", row.names = FALSE)
write.csv2(merged_naive, "C:/AURI/Thesis_bagetto/input/merged_naive.csv", row.names = FALSE)
write.csv2(merged_final, "C:/AURI/Thesis_bagetto/input/merged_final.csv", row.names = FALSE)
write.csv2(merged_ceci, "C:/AURI/Thesis_bagetto/input/merged_ceci.csv", row.names = FALSE)


#DEFINITIVO CON I PAZIENTI CHE SI FERMANO ALL'NA
###################################################
#DEFINITIVO CON I PAZIENTI CHE SI FERMANO ALL'NA
###################################################
# 1️⃣ Fasce EDSS in formato long, troncate al primo NA
sankey_long <- merged %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      EDSS_T0 >= 6 ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      EDSS_T6 >= 6 ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      EDSS_T12 >= 6 ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      EDSS_T18 >= 6 ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      EDSS_T24 >= 6 ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      EDSS_T30 >= 6 ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      EDSS_T36 >= 6 ~ "≥6"
    )
  ) %>%
  pivot_longer(-ID, names_to = "time", values_to = "band") %>%
  mutate(time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36"))) %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  filter(cumsum(is.na(band)) == 0) %>%   # tiene le osservazioni fino al primo NA (escluso)
  ungroup()

# 2️⃣ Link tra timepoint consecutivi
links <- sankey_long %>%
  group_by(ID) %>%
  mutate(
    source = paste0(time, "_", band),
    target = lead(paste0(time, "_", band))
  ) %>%
  ungroup() %>%
  filter(!is.na(target)) %>%        # l'ultima osservazione di ogni paziente non genera link
  count(source, target, name = "value")

# Creazione nodi
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  separate(name, into = c("time", "edss"), sep = "_", remove = FALSE) %>%
  mutate(
    group = case_when(
      grepl("≥6", edss)    ~ "≥6",
      grepl("3-5.5", edss) ~ "3-5.5",
      grepl("<3", edss)    ~ "<3"
    ),
    time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36")),
    group_order = factor(group, levels = c("≥6","3-5.5","<3"))
  ) %>%
  arrange(time, group_order) %>%
  mutate(name = paste(time, edss, sep = "_")) %>%
  select(name, group)

# Controllo rapido
print(table(nodes$group))

# Rimappatura indici JS
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Assegna gruppo ai link in base al nodo di destinazione
links <- links %>%
  mutate(group = nodes$group[target + 1])

# Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)

sankey_plot

sempre_verdi <- sankey_long %>%
  group_by(ID) %>%
  summarise(sempre_verde = all(band == "<3")) %>%
  filter(sempre_verde)

nrow(sempre_verdi)


pazienti_stabili <- sankey_long %>%
  group_by(ID) %>%
  summarise(
    n_categorie = n_distinct(band),
    stabile = n_categorie == 1
  )

n_stabili <- sum(pazienti_stabili$stabile)

n_stabili



#tabella descrittiva pz tot
merged <- merged %>%
  mutate(
    bmi_cat = case_when(
      bmi < 18.5 ~ "Underweight (bmi< 18.49)",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight (18.5>bmi>24.9)",
      bmi >= 25 ~ "Overweight (bmi > 24.5)",
      TRUE ~ NA_character_
    ))


tab2 <- merged %>%
  select(
    GROUP,
    age_at_T0,
    sex,
    bmi_cat,
    naive,
    lesion_burden,
    smoking_habit,
    physical_activity,
    therapy
  ) %>%
  tbl_summary(
    by = GROUP,
    missing = "no",
    statistic = list(
      age_at_T0 ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_at_T0 ~ "Age at baseline (years)",
      sex ~ "Sex",
      bmi_cat ~ "BMI category",
      naive ~ "Treatment-naïve",
      lesion_burden ~ "Lesion burden",
      smoking_habit ~ "Smoking habit",
      physical_activity ~ "Physical activity",
      therapy ~ "Treatment"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      age_at_T0 ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    ),
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels() %>%
  modify_header(label = "*Variables*")

tab2

# modifica
tab2 <- merged %>%
  select(
    GROUP,
    age_at_T0,
    sex,
    bmi_cat,
    naive,
    lesion_burden,
    bone_marrow_lesions,      # dopo lesion_burden
    subtentorial_lesions,     # dopo lesion_burden
    smoking_habit,
    physical_activity,
    antibiotic_use,           # dopo physical_activity
    therapy
  ) %>%
  tbl_summary(
    by = GROUP,
    missing = "no",
    statistic = list(
      age_at_T0 ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_at_T0 ~ "Age at baseline (years)",
      sex ~ "Sex",
      bmi_cat ~ "BMI category",
      naive ~ "Treatment-naïve",
      lesion_burden ~ "Lesion burden",
      bone_marrow_lesions ~ "Spinal lesions",
      subtentorial_lesions ~ "Subtentorial lesions",
      smoking_habit ~ "Smoking habit",
      physical_activity ~ "Physical activity",
      antibiotic_use ~ "Antibiotic use",
      therapy ~ "Treatment"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      age_at_T0 ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    ),
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels() %>%
  modify_header(label = "*Variables*")

tab2

library(gtsummary)
library(gt)

# Conversione della tabella in formato gt
gt_tab2 <- as_gt(tab2)

# Salvataggio come immagine PNG
gtsave(
  data = gt_tab2,
  filename = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/tabella_baseline.png",
  vwidth = 2200,   # larghezza in pixel
  vheight = 3000,  # altezza in pixel
  zoom = 1.5       # migliora la nitidezza
)

# check mav
library(dplyr)

merged %>%
  filter(grepl("^MAV", ID) & subtentorial_lesions == 0) %>%
  summarise(n_pazienti = n())


meta <- read_csv2("C:/AURI/Thesis_bagetto/merged_final.csv") 

