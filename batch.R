#importo dataset merged
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")

# importo i batch 
batch_sequenziamento <- read_excel("batch_sequenziamento.xlsx")

# tengo solo i TEC
batch_seq_TEC <- batch_sequenziamento %>%
  filter(grepl("^TEC", ID))

# crea le combinazioni ID-tempo
merged_long <- expand.grid(
  ID = merged$ID,
  Tempo = c("T0", "T1", "T6", "T12")
) %>%
  mutate(ID_long = paste0(ID, Tempo)) %>%
  left_join(merged, by = "ID") %>%
  # Ordino correttamente per paziente e tempo
  separate(ID, into = c("prefix", "num"), sep = 3, remove = FALSE) %>%
  mutate(
    num = as.numeric(num),
    Tempo_num = case_when(
      Tempo == "T0"  ~ 0,
      Tempo == "T1"  ~ 1,
      Tempo == "T6"  ~ 6,
      Tempo == "T12" ~ 12
    )
  ) %>%
  arrange(num, Tempo_num) %>%
  mutate(ID_long = paste0(ID, Tempo))

# merge merged-batch_seq_TEC
merged_final <- merged_long %>%
  left_join(batch_seq_TEC, by = c("ID_long" = "ID"))

# elimino la vecchia colonna spike_in 
merged_final <- merged_final %>%
  select(-spike_in)

# elimino i T6 perchè non mi servono per analisi microbiota
merged_final <- merged_final %>%
  filter(Tempo != "T6")

# colonne separate e valori corretti al giusto timepoint
merged_final_clean <- merged_final %>%
  mutate(
    EDSS = case_when(
      Tempo == "T0"  ~ EDSS_T0,
      Tempo == "T1"  ~ EDSS_T0,   # T1 usa i valori di T0
      Tempo == "T12" ~ EDSS_T12,
      TRUE ~ NA_real_
    ),
    lesioni = case_when(
      Tempo == "T0"  ~ NA_real_,  # nessun valore al T0
      Tempo == "T1"  ~ NA_real_,  # nessun valore al T1
      Tempo == "T12" ~ lesioni_T12,
      TRUE ~ NA_real_
    ),
    relapse = case_when(
      Tempo == "T0"  ~ NA_real_,
      Tempo == "T1"  ~ NA_real_,
      Tempo == "T12" ~ relapse_T12,
      TRUE ~ NA_real_
    ),
    cortisone = case_when(
      Tempo == "T0"  ~ NA_real_,
      Tempo == "T1"  ~ NA_real_,
      Tempo == "T12" ~ cortisone_T12,
      TRUE ~ NA_real_
    )
  )

# elimino vecchie colonne
merged_final_clean <- merged_final_clean %>%
  select(-matches("_T0$"),
         -matches("_T6$"),
         -matches("_T12$"),
         -matches("_T18$"),
         -matches("_T24$"),
         -matches("_T30$"),
         -matches("_T36$"))

merged_final_clean <- merged_final_clean %>%
  select(-pyr_mds)

merged_final_clean <- merged_final_clean %>%
  select(-glucocorticoid_treatment)


#ordino le colonne
merged_final_clean <- merged_final_clean %>%
  select(
    ID, prefix, num, Tempo, ID_long,
    data_di_nascita, MEDAS, CENTRO, DATA_ESORDIO, data_diagnosi,
    age_at_diagnosis,
    EDSS, lesioni, relapse, cortisone,
    everything()
  )


# salvo
write.csv2(merged_final, "C:/AURI/Thesis_bagetto/input/merged_final.csv", row.names = FALSE)
write.csv2(merged_final_clean, "C:/AURI/Thesis_bagetto/input/merged_final_clean.csv", row.names = FALSE)
