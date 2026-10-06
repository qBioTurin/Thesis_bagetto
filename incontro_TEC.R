
#apertura di merged
merged_ceci <- read.csv2("input/merged_ceci.csv")
merged_ceci_TEC <- merged_ceci %>% 
  filter(GROUP == "TEC")

#PRIMO QUESITO 
# COLONNA EDSS FINALE
merged_ceci_TEC <- merged_ceci_TEC %>%
  rowwise() %>%
  mutate(
    EDSS_finale = last(na.omit(c(
      EDSS_T0,
      EDSS_T6,
      EDSS_T12,
      EDSS_T18,
      EDSS_T24,
      EDSS_T30,
      EDSS_T36
    )))
  ) %>%
  ungroup()

# EDSS DELTA
merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    delta_EDSS = EDSS_finale - EDSS_T0
  )

# COLONNA EDSS ANDAMENTO
merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    andamento_EDSS = case_when(
      delta_EDSS >= 0.5 ~ "EDSS_Worsening",
      delta_EDSS <= -0.5 ~ "EDSS_Improvement",
      abs(delta_EDSS) < 0.5 & EDSS_T0 < 4 ~ "Low stable",
      abs(delta_EDSS) < 0.5 & EDSS_T0 >= 4 ~ "High stable"
    )
  )

# PATTERN
merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    EDSS_class = ifelse(
      EDSS_T0 >= 4,
      "EDSS ≥ 4",
      "EDSS < 4"
    )
  )

# ANDAMENTO TABELLA1
table(merged_ceci_TEC$andamento_EDSS)

table(merged_ceci_TEC$GROUP,
      merged_ceci_TEC$andamento_EDSS)

tabella1 <- table(
  merged_ceci_TEC$GROUP,
  merged_ceci_TEC$andamento_EDSS
)

fisher.test(tabella1)

# EVENTO COMPOSITO
merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    evento_EDSS = delta_EDSS >= 0.5,
    evento_lesioni =
      lesioni_T6 == 1 |
      lesioni_T12 == 1 |
      lesioni_T18 == 1 |
      lesioni_T24 == 1 |
      lesioni_T30 == 1 |
      lesioni_T36 == 1,
    evento_relapse =
      relapse_T6 == 1 |
      relapse_T12 == 1 |
      relapse_T18 == 1 |
      relapse_T24 == 1 |
      relapse_T30 == 1 |
      relapse_T36 == 1
  )

merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    worsening =
      evento_EDSS |
      evento_lesioni |
      evento_relapse
  )

merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(worsening = replace_na(worsening, FALSE))

table(merged_ceci_TEC$worsening)

# evento di worsening
tabella2 <- table(merged_ceci_TEC$GROUP,
                  merged_ceci_TEC$worsening)

table(merged_ceci_TEC$GROUP,
      merged_ceci_TEC$worsening)

fisher.test(tabella2)


# BARPLOT CON TEXTURE

plot_df <- merged_ceci_TEC %>%
  filter(!is.na(andamento_EDSS)) %>%
  count(andamento_EDSS, GROUP, EDSS_class)

ggplot(
  plot_df,
  aes(
    x = interaction(andamento_EDSS, GROUP),
    y = n,
    fill = GROUP,
    pattern = EDSS_class
  )
) +
  
  geom_col_pattern(
    position = "stack",
    colour = "black",
    pattern_fill = "black",
    pattern_colour = "black"
  ) +
  
  scale_pattern_manual(
    values = c(
      "EDSS < 4" = "stripe",
      "EDSS ≥ 4" = "circle"
    )
  )

# PERCENTUALE DI WORSENING
ggplot(merged_ceci_TEC,
       aes(x = GROUP,
           fill = worsening)) +
  
  geom_bar(position = "fill") +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(
    x = "Group",
    y = "Percentage",
    fill = "At least 1 worsening"
  ) +
  
  theme_minimal()

# GC TREATMENT (solo TEC)
merged_ceci_TEC <- merged_ceci_TEC %>%
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

table(merged_ceci_TEC$gc_at_least, merged_ceci_TEC$GROUP)

# frequenza
table(merged_ceci_TEC$GROUP, merged_ceci_TEC$gc_treatment)

# durata del trattamento
merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0)
  )

merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    gc_days_total = rowSums(
      across(starts_with("cortisone_T")),
      na.rm = TRUE
    )
  )

merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    gc_at_least = if_else(
      if_any(starts_with("cortisone_T"), ~ . > 0),
      "TEC_GC+",
      "TEC_GC-"
    )
  )

#SURVIVAL PLOT
merged_ceci_TEC <- merged_ceci_TEC %>%
  mutate(
    event_6_C  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1 | lesioni_T36 == 1
  )

merged_ceci_TEC <- merged_ceci_TEC %>%
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

surv_obj <- Surv(time = merged_ceci_TEC$time_C,
                 event = merged_ceci_TEC$event_status_C)

fit <- survfit(surv_obj ~ gc_at_least, data = merged_ceci_TEC)

ggsurvplot(
  fit,
  data = merged_ceci_TEC,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-"),
  xlab = "Months",
  ylab = "Event-free survival"
)










ggsurvplot(
  fit,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-", "MAV GC+", "MAV GC-"),
  xlab = "Months",
  ylab = "Event-free survival"
)

table(merged$GROUP, merged$gc_treatment)

#TEST TRA MAV
mav <- merged %>%
  filter(GROUP == "MAV")

# Log-rank
lr <- survdiff(
  Surv(time_C, event_status_C) ~ gc_group,
  data = mav
)

pval <- 1 - pchisq(
  lr$chisq,
  df = length(lr$n) - 1
)

cat("Log-rank p =", round(pval, 4), "\n")

#TEST TRA TEC
tec <- merged %>%
  filter(GROUP == "TEC")

# Log-rank
lr <- survdiff(
  Surv(time_C, event_status_C) ~ gc_group,
  data = tec
)

pval <- 1 - pchisq(
  lr$chisq,
  df = length(lr$n) - 1
)

cat("Log-rank p =", round(pval, 4), "\n")


#SURVIVAL SOLO EDSS-GC TREAT

merged <- merged %>%
  mutate(
    event_6_1   = (EDSS_T6  - EDSS_T0 >= 0.5),
    event_12_1  = (EDSS_T12 - EDSS_T6 >= 0.5),
    event_18_1  = (EDSS_T18 - EDSS_T12 >= 0.5),
    event_24_1  = (EDSS_T24 - EDSS_T18 >= 0.5),
    event_30_1  = (EDSS_T30 - EDSS_T24 >= 0.5),
    event_36_1  = (EDSS_T36 - EDSS_T30 >= 0.5)
  )

merged <- merged %>%
  mutate(
    event_time_1 = case_when(
      event_6_1   ~ 6,
      event_12_1  ~ 12,
      event_18_1  ~ 18,
      event_24_1  ~ 24,
      event_30_1  ~ 30,
      event_36_1  ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_1 = ifelse(is.na(event_time_1), 0, 1),
    time_1 = ifelse(is.na(event_time_1), 36, event_time_1)
  )

surv_obj1 <- Surv(time = merged$time_1, event = merged$event_status_1)
fit1 <- survfit(surv_obj1 ~ gc_group, data = merged)

ggsurvplot(
  fit1,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-", "MAV GC+", "MAV GC-"),
  xlab = "Months",
  ylab = "EDSS progression-free survival"
)

#SURVIVAL RELAPSE- GC TREAT
merged <- merged %>%
  mutate(
    event_6_2   = relapse_T6  == 1,
    event_12_2  = relapse_T12  == 1,
    event_18_2  = relapse_T18  == 1,
    event_24_2  = relapse_T24  == 1,
    event_30_2  = relapse_T30  == 1,
    event_36_2  = relapse_T36  == 1
  )

merged <- merged %>%
  mutate(
    event_time_2 = case_when(
      event_6_2   ~ 6,
      event_12_2  ~ 12,
      event_18_2  ~ 18,
      event_24_2  ~ 24,
      event_30_2  ~ 30,
      event_36_2  ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_2 = ifelse(is.na(event_time_2), 0, 1),
    time_2 = ifelse(is.na(event_time_2), 36, event_time_2)
  )

surv_obj2 <- Surv(time = merged$time_2, event = merged$event_status_2)
fit2 <- survfit(surv_obj2 ~ gc_group, data = merged)

ggsurvplot(
  fit2,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-", "MAV GC+", "MAV GC-"),
  xlab = "Months",
  ylab = "Relapse-free survival"
)




#SURVIVAL LESIONI-GC TREAT 
merged <- merged %>%
  mutate(
    event_6_3   = lesioni_T6  == 1,
    event_12_3  = lesioni_T12 == 1,
    event_18_3  = lesioni_T18  == 1,
    event_24_3  = lesioni_T24  == 1,
    event_30_3  = lesioni_T30  == 1,
    event_36_3  = lesioni_T36  == 1
  )

merged <- merged %>%
  mutate(
    event_time_3 = case_when(
      event_6_3   ~ 6,
      event_12_3  ~ 12,
      event_18_3  ~ 18,
      event_24_3  ~ 24,
      event_30_3  ~ 30,
      event_36_3  ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_3 = ifelse(is.na(event_time_3), 0, 1),
    time_3 = ifelse(is.na(event_time_3), 36, event_time_3)
  )

surv_obj3 <- Surv(time = merged$time_3, event = merged$event_status_3)
fit3 <- survfit(surv_obj3 ~ gc_group, data = merged)

ggsurvplot(
  fit3,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-", "MAV GC+", "MAV GC-"),
  xlab = "Months",
  ylab = "Lesion-free survival"
)


#CATEGORIE DI DURATA CORTISONE
merged <- merged %>%
  mutate(
    gc_category = case_when(
      rowSums(across(starts_with("cortisone_T"), ~ . > 0), na.rm = TRUE) == 0 ~ "None",
      rowSums(across(starts_with("cortisone_T"), ~ . > 0), na.rm = TRUE) <= 1 ~ "1",
      TRUE ~ "More 1"
    ),
    gc_category = factor(
      gc_category,
      levels = c("None", "1", "More 1")
    )
  )

ggplot(merged, aes(x = gc_category)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~GROUP) +
  labs(
    x = "GC category",
    y = "Number of patients"
  ) +
  theme_classic()


#plot durata cortisone 
gc_long <- merged %>%
  select(
    ID,
    GROUP,
    cortisone_T6,
    cortisone_T12,
    cortisone_T18,
    cortisone_T24,
    cortisone_T30,
    cortisone_T36
  ) %>%
  pivot_longer(
    cols = starts_with("cortisone_T"),
    names_to = "Timepoint",
    values_to = "Days"
  ) %>%
  mutate(
    Timepoint = gsub("cortisone_", "", Timepoint),
    Days = replace_na(Days, 0)
  )


ggplot(gc_long,
       aes(x = factor(ID),
           y = Days,
           fill = Timepoint)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = ifelse(Days > 0, Days, "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    x = "Patient ID",
    y = "Days of corticosteroid treatment",
    fill = "Timepoint"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

#violin plot durata cortisone 
gc_long2 <- merged %>%
  select(
    ID,
    GROUP,
    cortisone_T6,
    cortisone_T12,
    cortisone_T18,
    cortisone_T24,
    cortisone_T30,
    cortisone_T36
  ) %>%
  pivot_longer(
    cols = starts_with("cortisone_T"),
    names_to = "Timepoint",
    values_to = "Days"
  ) %>%
  mutate(
    Timepoint = gsub("cortisone_", "", Timepoint),
    Timepoint = factor(
      Timepoint,
      levels = c("T6","T12","T18","T24","T30","T36")
    )
  )
ggplot(gc_long2,
       aes(x = Timepoint,
           y = Days,
           fill =GROUP)) +
  
  geom_violin(position = position_dodge(width = 0.8),
              trim = FALSE,
              alpha = 0.7) +
  
  geom_boxplot(position = position_dodge(width = 0.8),
               width = 0.15,
               outlier.shape = NA,
               alpha = 0.8) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    position = position_dodge(width = 0.8),
    shape = 23,
    size = 2.5,
    fill = "white"
  ) +
  
  labs(
    x = "Timepoint",
    y = "Days of corticosteroid treatment",
    fill = "Group"
  ) +
  
  theme_classic()

#SURVIVAL COMPOSITO PER MAV
dfmav <- merged %>%
  filter(GROUP == "MAV")

dftec <- merged %>%
  filter(GROUP == "TEC")

dfmav <- dfmav %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0),
    group = GROUP,  # TEC / MAV
    gc_group = case_when(
      group == "MAV" & gc == 1 ~ "MAV_GC+",
      group == "MAV" & gc == 0 ~ "MAV_GC-"
    )
  )

dftec <- dftec %>%
  mutate(
    gc = ifelse(gc_treatment == "positive", 1, 0),
    group = GROUP,  # TEC / MAV
    gc_group = case_when(
      group == "TEC" & gc == 1 ~ "TEC_GC+",
      group == "TEC" & gc == 0 ~ "TEC_GC-"
    )
  )

dfmav <- dfmav %>%
  mutate(
    event_6_C  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )

dfmav <- dfmav %>%
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



surv_obj <- Surv(time = dfmav$time_C, event = dfmav$event_status_C)
fit <- survfit(surv_obj ~ gc_group, data = dfmav)

ggsurvplot(
  fit,
  data = dfmav,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV GC+", "MAV GC-"),
  xlab = "Months",
  ylab = "Event-free survival"
)


#TEC
dftec <- dftec %>%
  mutate(
    event_6_C  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_C = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_C = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_C = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_C = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_C = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )

dftec <- dftec %>%
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



surv_obj <- Surv(time = dftec$time_C, event = dftec$event_status_C)
fit <- survfit(surv_obj ~ gc_group, data = dftec)

ggsurvplot(
  fit,
  data = dftec,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("TEC GC+", "TEC GC-"),
  xlab = "Months",
  ylab = "Event-free survival"
)


#colonna cambio terapia
#quanti cambiano
merged <- merged %>%
  mutate(
    cambio_terapia = case_when(
      cambio_terapia == "1" ~ "Yes",
      cambio_terapia == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )
table(merged$cambio_terapia, merged$GROUP)

ggplot(subset(merged, !is.na(cambio_terapia) & !is.na(GROUP)),
       aes(x = cambio_terapia, fill = GROUP)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Therapy change",
    y = "Patients",
    fill = "GROUP"
  ) +
  theme_minimal()

    
#mesi al cambio
merged <- merged %>%
  mutate(
    data_prelievo_T0 = as.Date(data_prelievo_T0),
    data_cambio_terapia = as.Date(data_cambio_terapia),
    
    mesi_cambio = interval(data_prelievo_T0,
                           data_cambio_terapia) / months(1)
  )


table(merged$GROUP, merged$cambio_terapia)


#SURVIVAL PLOT
#creo cambio_group 
merged <- merged %>%
  mutate(
    cambio_group = case_when(
      GROUP == "MAV" &
        cambio_terapia == "Yes" ~ "MAV cambio +",
      
      GROUP == "MAV" &
        cambio_terapia == "No" ~ "MAV cambio -",
      
      GROUP == "TEC" &
        cambio_terapia == "Yes" ~ "TEC cambio +",
      
      GROUP == "TEC" &
        cambio_terapia == "No" ~ "TEC cambio -"
    )
  )

#survival
merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )

surv_obj4 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit4 <- survfit(surv_obj4 ~ cambio_group, data = merged)

ggsurvplot(
  fit4,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV cambio+", "MAV cambio-", "TEC cambio+", "TEC cambio-"),
  xlab = "Months",
  ylab = "Event-free survival"
)

#TEST TRA MAV
mav <- merged %>%
  filter(GROUP == "MAV")

# Log-rank
lr <- survdiff(
  Surv(time_CT, event_status_CT) ~ cambio_group,
  data = mav
)

pval <- 1 - pchisq(
  lr$chisq,
  df = length(lr$n) - 1
)

cat("Log-rank p =", round(pval, 4), "\n")

#TEST TRA TEC
tec <- merged %>%
  filter(GROUP == "TEC")

# Log-rank
lr <- survdiff(
  Surv(time_CT, event_status_CT) ~ cambio_group,
  data = tec
)

pval <- 1 - pchisq(
  lr$chisq,
  df = length(lr$n) - 1
)

cat("Log-rank p =", round(pval, 4), "\n")

#SURVIVAL EDSS- CAMBIO TERAPIA
surv_obj5 <- Surv(time = merged$time_1, event = merged$event_status_1)
fit5 <- survfit(surv_obj5 ~ cambio_group, data = merged)

ggsurvplot(
  fit5,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV cambio+", "MAV cambio-", "TEC cambio+", "TEC cambio-"),
  xlab = "Months",
  ylab = "EDSS progression-free survival"
)

#SURVIVALRELAPSE- CAMBIO TERAPIA
surv_obj6 <- Surv(time = merged$time_2, event = merged$event_status_2)
fit6 <- survfit(surv_obj6 ~ cambio_group, data = merged)

ggsurvplot(
  fit6,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV cambio+", "MAV cambio-", "TEC cambio+", "TEC cambio-"),
  xlab = "Months",
  ylab = "Relapse-free survival"
)

#SURVIVAL LESIONI- CAMBIO GRUPPO
surv_obj7 <- Surv(time = merged$time_3, event = merged$event_status_3)
fit7 <- survfit(surv_obj7 ~ cambio_group, data = merged)

ggsurvplot(
  fit7,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV cambio+", "MAV cambio-", "TEC cambio+", "TEC cambio-"),
  xlab = "Months",
  ylab = "Lesion-free survival"
)



#GADOLINIUM -> analisi aggiuntive
table(merged$gadolinium_contrast, merged$GROUP)

merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast == "1" ~ "Yes",
      gadolinium_contrast == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

merged <- merged %>%
  mutate(
    gado_group = case_when(
      GROUP == "MAV" &
        gadolinium_contrast == "Yes" ~ "MAV gado +",
      
      GROUP == "MAV" &
        gadolinium_contrast == "No" ~ "MAV gado -",
      
      GROUP == "TEC" &
        gadolinium_contrast == "Yes" ~ "TEC gado +",
      
      GROUP == "TEC" &
        gadolinium_contrast == "No" ~ "TEC gado -"
    )
  )

#survival composito x gado
merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )
surv_obj8 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit8 <- survfit(surv_obj8 ~ gado_group, data = merged)

ggsurvplot(
  fit8,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV gado+", "MAV gado-", "TEC gado+", "TEC gado-"),
  xlab = "Months",
  ylab = "Event-free survival"
)

#survival composito x bone_marrow
table(merged$bone_marrow_lesions, merged$GROUP)
merged <- merged %>%
  mutate(
    bone_marrow_lesions = case_when(
      bone_marrow_lesions == "1" ~ "Yes",
      bone_marrow_lesions == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

merged <- merged %>%
  mutate(
    bone_group = case_when(
      GROUP == "MAV" &
        bone_marrow_lesions == "Yes" ~ "MAV spinal +",
      
      GROUP == "MAV" &
        bone_marrow_lesions == "No" ~ "MAV spinal -",
      
      GROUP == "TEC" &
        bone_marrow_lesions == "Yes" ~ "TEC spinal +",
      
      GROUP == "TEC" &
        bone_marrow_lesions == "No" ~ "TEC spinal -"
    )
  )


surv_obj9 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit9 <- survfit(surv_obj9 ~ bone_group, data = merged)

ggsurvplot(
  fit9,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV spinal +", "MAV spinal -", "TEC spinal+", "TEC spinal-"),
  xlab = "Months",
  ylab = "Event-free survival"
)

#survival composito x lesion_burden
table(merged$lesion_burden, merged$GROUP)
merged <- merged %>%
  mutate(
    lesion_burden = case_when(
      lesion_burden == "1" ~ "Yes",
      lesion_burden == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

merged <- merged %>%
  mutate(
    burden_group = case_when(
      GROUP == "MAV" &
        lesion_burden == "Yes" ~ "MAV burden +",
      
      GROUP == "MAV" &
        lesion_burden == "No" ~ "MAV burden -",
      
      GROUP == "TEC" &
        lesion_burden == "Yes" ~ "TEC burden +",
      
      GROUP == "TEC" &
        lesion_burden == "No" ~ "TEC burden -"
    )
  )


surv_obj10 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit10 <- survfit(surv_obj10 ~ burden_group, data = merged)

ggsurvplot(
  fit10,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV burden+", "MAV burden-", "TEC burden+", "TEC burden-"),
  xlab = "Months",
  ylab = "Event-free survival"
)

merged <- merged %>%
  mutate(
    lesion_burden = case_when(
      lesion_burden == "1" ~ "Yes",
      lesion_burden == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )

#survival composito x subtentorial
table(merged$subtentorial_lesions, merged$GROUP)
merged <- merged %>%
  mutate(
    subtentorial_lesions = case_when(
      subtentorial_lesions == "1" ~ "Yes",
      subtentorial_lesions == "0" ~ "No",
      TRUE ~ NA_character_
    )
  )
merged <- merged %>%
  mutate(
    sub_group = case_when(
      GROUP == "MAV" &
        subtentorial_lesions == "Yes" ~ "MAV subt +",
      
      GROUP == "MAV" &
        subtentorial_lesions == "No" ~ "MAV subt -",
      
      GROUP == "TEC" &
        subtentorial_lesions == "Yes" ~ "TEC subt +",
      
      GROUP == "TEC" &
        subtentorial_lesions == "No" ~ "TEC subt -"
    )
  )


surv_obj11 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit11 <- survfit(surv_obj11 ~ sub_group, data = merged)

ggsurvplot(
  fit11,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV subt+", "MAV subt-", "TEC subt+", "TEC subt-"),
  xlab = "Months",
  ylab = "Event-free survival"
)


#NEDA 3 SURVIVAL
merged <- merged %>%
  mutate(
    event_6_CT =
      (
        case_when(
          EDSS_T0 == 0 ~ (EDSS_T6 - EDSS_T0) >= 1.5,
          EDSS_T0 >= 1 & EDSS_T0 <= 5 ~ (EDSS_T6 - EDSS_T0) >= 1,
          EDSS_T0 > 5 ~ (EDSS_T6 - EDSS_T0) >= 0.5,
          TRUE ~ FALSE
        )
      ) | relapse_T6 == 1 | lesioni_T6 == 1,
    
    event_12_CT =
      (
        case_when(
          EDSS_T6 == 0 ~ (EDSS_T12 - EDSS_T6) >= 1.5,
          EDSS_T6 >= 1 & EDSS_T6 <= 5 ~ (EDSS_T12 - EDSS_T6) >= 1,
          EDSS_T6 > 5 ~ (EDSS_T12 - EDSS_T6) >= 0.5,
          TRUE ~ FALSE
        )
      ) | relapse_T12 == 1 | lesioni_T12 == 1,
    
    event_18_CT =
      (
        case_when(
          EDSS_T12 == 0 ~ (EDSS_T18 - EDSS_T12) >= 1.5,
          EDSS_T12 >= 1 & EDSS_T12 <= 5 ~ (EDSS_T18 - EDSS_T12) >= 1,
          EDSS_T12 > 5 ~ (EDSS_T18 - EDSS_T12) >= 0.5,
          TRUE ~ FALSE
        )
      ) | relapse_T18 == 1 | lesioni_T18 == 1,
    
    event_24_CT =
      (
        case_when(
          EDSS_T18 == 0 ~ (EDSS_T24 - EDSS_T18) >= 1.5,
          EDSS_T18 >= 1 & EDSS_T18 <= 5 ~ (EDSS_T24 - EDSS_T18) >= 1,
          EDSS_T18 > 5 ~ (EDSS_T24 - EDSS_T18) >= 0.5,
          TRUE ~ FALSE
        )
      ) | relapse_T24 == 1 | lesioni_T24 == 1,
    
    event_30_CT =
      (
        case_when(
          EDSS_T24 == 0 ~ (EDSS_T30 - EDSS_T24) >= 1.5,
          EDSS_T24 >= 1 & EDSS_T24 <= 5 ~ (EDSS_T30 - EDSS_T24) >= 1,
          EDSS_T24 > 5 ~ (EDSS_T30 - EDSS_T24) >= 0.5,
          TRUE ~ FALSE
        )
      ) | relapse_T30 == 1 | lesioni_T30 == 1,
    
    event_36_CT =
      (
        case_when(
          EDSS_T30 == 0 ~ (EDSS_T36 - EDSS_T30) >= 1.5,
          EDSS_T30 >= 1 & EDSS_T30 <= 5 ~ (EDSS_T36 - EDSS_T30) >= 1,
          EDSS_T30 > 5 ~ (EDSS_T36 - EDSS_T30) >= 0.5,
          TRUE ~ FALSE
        )
      ) | relapse_T36 == 1 | lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )

surv_obj4 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit4 <- survfit(surv_obj4 ~ GROUP, data = merged)

ggsurvplot(
  fit4,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV", "TEC"),
  xlab = "Months",
  ylab = "NEDA-free survival"
)


#sex

merged <- merged %>%
  mutate(
    sex_group = case_when(
      GROUP == "MAV" &
        sex == "F" ~ "MAV F",
      
      GROUP == "MAV" &
        sex == "M" ~ "MAV M",
      
      GROUP == "TEC" &
        sex == "F" ~ "TEC F",
      
      GROUP == "TEC" &
        sex == "M" ~ "TEC M"
    )
  )

merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )
surv_obj8 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit8 <- survfit(surv_obj8 ~ sex_group, data = merged)

ggsurvplot(
  fit8,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV F", "MAV M", "TEC F", "TEC M"),
  xlab = "Months",
  ylab = "Event-free survival"
)


#age at t0
merged <- merged %>%
  mutate(
    median_age = factor(
      ifelse(age_at_T0 >= 40, "older", "younger"),
      levels = c("younger", "older")
    )
  )

merged <- merged %>%
  mutate(
    age_group = case_when(
      GROUP == "MAV" &
        median_age == "older" ~ "MAV older",
      
      GROUP == "MAV" &
        median_age == "younger" ~ "MAV younger",
      
      GROUP == "TEC" &
        median_age == "older" ~ "TEC older",
      
      GROUP == "TEC" &
        median_age == "younger" ~ "TEC younger"
    )
  )
merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )
surv_obj8 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit8 <- survfit(surv_obj8 ~ age_group, data = merged)

ggsurvplot(
  fit8,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV older", "MAV younger", "TEC older", "TEC younger"),
  xlab = "Months",
  ylab = "Event-free survival"
)

#medas
merged <- merged %>%
  mutate(
    median_medas = factor(
      ifelse(MEDAS >= 46, "high adherence", "low adherence"),
      levels = c("high adherence", "low adherence")
    )
  )

merged <- merged %>%
  mutate(
    medas_group = case_when(
      GROUP == "MAV" &
        median_medas == "high adherence" ~ "MAV high adherence",
      
      GROUP == "MAV" &
        median_medas == "low adherence" ~ "MAV low adherence",
      
      GROUP == "TEC" &
        median_medas == "high adherence" ~ "TEC high adherence",
      
      GROUP == "TEC" &
        median_medas == "low adherence" ~ "TEC low adherence"
    )
  )
merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )
surv_obj8 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit8 <- survfit(surv_obj8 ~ medas_group, data = merged)

ggsurvplot(
  fit8,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV high adherence", "MAV low adherence", "TEC high adherence", "TEC low adherence"),
  xlab = "Months",
  ylab = "Event-free survival"
)


#bmi
merged <- merged %>%
  mutate(
    median_bmi = factor(
      ifelse(bmi >= 24, "high", "low"),
      levels = c("high", "low")
    )
  )

merged <- merged %>%
  mutate(
    bmi_group = case_when(
      GROUP == "MAV" &
        median_bmi == "high" ~ "MAV high",
      
      GROUP == "MAV" &
        median_bmi == "low" ~ "MAV low",
      
      GROUP == "TEC" &
        median_bmi == "high" ~ "TEC high",
      
      GROUP == "TEC" &
        median_bmi == "low" ~ "TEC low"
    )
  )
merged <- merged %>%
  mutate(
    event_6_CT  = (EDSS_T6  - EDSS_T0 >= 0.5) | relapse_T6  == 1 | lesioni_T6  == 1,
    event_12_CT = (EDSS_T12 - EDSS_T6 >= 0.5) | relapse_T12 == 1 | lesioni_T12 == 1,
    event_18_CT = (EDSS_T18 - EDSS_T12 >= 0.5) | relapse_T18 == 1 | lesioni_T18 == 1,
    event_24_CT = (EDSS_T24 - EDSS_T18 >= 0.5) | relapse_T24 == 1 | lesioni_T24 == 1,
    event_30_CT = (EDSS_T30 - EDSS_T24 >= 0.5) | relapse_T30 == 1 | lesioni_T30 == 1,
    event_36_CT = (EDSS_T36 - EDSS_T30 >= 0.5) | relapse_T36 == 1| lesioni_T36 == 1
  )
merged <- merged %>%
  mutate(
    event_time_CT = case_when(
      event_6_CT  ~ 6,
      event_12_CT ~ 12,
      event_18_CT ~ 18,
      event_24_CT ~ 24,
      event_30_CT ~ 30,
      event_36_CT ~ 36,
      TRUE ~ NA_real_
    ),
    
    event_status_CT = ifelse(is.na(event_time_CT), 0, 1),
    time_CT = ifelse(is.na(event_time_CT), 36, event_time_CT)
  )
surv_obj8 <- Surv(time = merged$time_CT, event = merged$event_status_CT)
fit8 <- survfit(surv_obj8 ~ bmi_group, data = merged)

ggsurvplot(
  fit8,
  data = merged,
  risk.table = FALSE,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "Groups",
  legend.labs = c("MAV high", "MAV low", "TEC high", "TEC low"),
  xlab = "Months",
  ylab = "Event-free survival"
)


