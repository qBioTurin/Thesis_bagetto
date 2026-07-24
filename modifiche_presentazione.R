merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")


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
  filter(as.numeric(timepoint) == max(as.numeric(timepoint))) %>%
  ungroup()

# Grafico
ggplot(edss_long, aes(x = timepoint, y = EDSS, group = ID)) +
  
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

