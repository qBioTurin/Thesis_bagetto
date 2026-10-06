merged_final <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_final.csv")
View(merged_final)

# elimino colonne inutili per l'app e che presentano solo NA
merged_final <- read.csv2("merged_final.csv")
merged_final <- merged_final %>%
  select(-NOTE, -nuova_TERAPIA)

merged_final <- merged_final %>%
  select(-treg_il10_pos, -treg_cd39_pos, -percent_th17)

merged_final <- merged_final %>%
  select(-starts_with("data_referto"))

merged_final <- merged_final %>%
  select(-Tempo_num)

merged_final <- merged_final[-46, ]



# colonna EDSS longitudinale
library(dplyr)
library(stringr)

merged_final <- merged_final %>%
  mutate(
    Tempo = str_extract(ID_long, "T[0-9]+"),
    EDSS = case_when(
      Tempo == "T0"  ~ EDSS_T0,
      Tempo == "T1"  ~ EDSS_T6,     # <-- qui usiamo T6 per T1
      Tempo == "T12" ~ EDSS_T12
    )
  )

merged_final <- merged_final %>%
  select(-starts_with("EDSS_T"))


# colonna relapse longitudinale

merged_final <- merged_final %>%
  mutate(
    Tempo = str_extract(ID_long, "T[0-9]+"),
    relapse = case_when(
      Tempo == "T0"  ~ NA_real_,      # <-- non esiste relapse_T0
      Tempo == "T1"  ~ relapse_T6,    # <-- assegniamo T6 ai pazienti T1
      Tempo == "T12" ~ relapse_T12
    )
  )


merged_final <- merged_final %>%
  select(-starts_with("relapse_T"))

merged_final <- merged_final %>%
  mutate(relapse = if_else(is.na(relapse), 0, relapse))


View(merged_final)
merged_final <- merged_final[-55, ]


# colonna lesioni longitudinale
merged_final <- merged_final %>%
  mutate(
    Tempo = str_extract(ID_long, "T[0-9]+"),
    lesioni = case_when(
      Tempo == "T0"  ~ lesion_burden,   # <-- valore unico usato solo per T0
      Tempo == "T1"  ~ lesioni_T6,      # <-- assegniamo T6 ai pazienti T1
      Tempo == "T12" ~ lesioni_T12
    )
  )

merged_final <- merged_final %>%
  select(-starts_with("lesioni_T"))

merged_final <- merged_final %>%
  mutate(lesioni = if_else(is.na(lesioni), 0, lesioni))


# colonna cortisone longitudinale

merged_final <- merged_final %>%
  mutate(
    Tempo = str_extract(ID_long, "T[0-9]+"),
    cortisone = case_when(
      Tempo == "T0"  ~ NA_real_,       # <-- non esiste cortisone_T0
      Tempo == "T1"  ~ cortisone_T6,   # <-- assegniamo T6 ai pazienti T1
      Tempo == "T12" ~ cortisone_T12
    )
  )

merged_final <- merged_final %>%
  select(-starts_with("cortisone_T"))

merged_final <- merged_final %>%
  mutate(cortisone = if_else(Tempo == "T0",
                             if_else(gc_treatment == "positive", 1, 0),
                             cortisone))

merged_final <- merged_final %>%
  select(-gc_treatment)

merged_final <- merged_final %>%
  mutate(cortisone = if_else(is.na(cortisone), 0, cortisone))

merged_final <- merged_final %>%
  relocate(sequencing_batch, .after = Sequencing.Batch)

merged_final <- merged_final %>% select(-sequencing_batch)
merged_final <- merged_final %>%
  rename(sequencing_batch = Sequencing.Batch)


merged_final <- merged_final %>%
  rename(spike_in = Spike.in)

merged_final <- merged_final %>%
  rename(spinal_lesions = bone_marrow_lesions)

merged_final <- merged_final[-46, ]




merged_ceci <- read_csv2("input/merged_ceci.csv")

write.csv2(merged_final, "merged_final.csv", row.names = FALSE)

write.table(merged_final, "C:/AURI/Thesis_bagetto/input/merged_final.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")




R.version.string
