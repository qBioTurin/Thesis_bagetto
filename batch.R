#importo dataset 
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")
merged_final_clean <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_final_clean.csv")
View(merged_final_clean)

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

class(merged_final_clean$gc_treatment)
merged_final_clean <- merged_final_clean %>%
  mutate(
    gc_treatment = case_when(
      gc_treatment == "positive" ~ "1",
      gc_treatment == "negative" ~ "0",
      TRUE ~ NA_character_
    ),
    gc_treatment = factor(gc_treatment, levels = c("0", "1"))
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesion_burden = factor(lesion_burden)
  )

class(merged_final_clean$lesion_burden)

merged_final_clean <- merged_final_clean %>%
  distinct()

merged_final_clean <- merged_final_clean %>%
  filter(!(ID_long == "TEC19T12" & duplicated(ID_long)))


# inserisco gc treatment solo al T0
merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC02T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC04T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC05T0",  
      0,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC06T0",  
      0,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC07T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC08T0",  
      0,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC11T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC15T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC16T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC17T0",  
      0,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC22T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC24T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC26T0",  
      0,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC27T0",  
      0,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC28T0",  
      0,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC29T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC30T0",  
      1,                     
      cortisone              
    )
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    cortisone = if_else(
      ID_long == "TEC31T0",  
      0,                     
      cortisone              
    )
  )

# inserisco lesion burden al T0
merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC02T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC04T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC05T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC06T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC07T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC08T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC11T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC15T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC16T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC17T0", 0, lesioni)
  )



merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC22T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC24T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC25T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC26T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC27T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC28T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC29T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC30T0", 0, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC31T0", 1, lesioni)
  )

merged_final_clean <- merged_final_clean %>%
  mutate(
    lesioni = if_else(ID_long == "TEC32T0", 1, lesioni)
  )

# inserisco gli NA restanti, per le analisi longitudinali utilizzo T6 come T1
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC01T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC01T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC01T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC01T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC01T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC01T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC02T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC02T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC02T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC02T1"] <- 0

merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC03T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC03T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC03T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC03T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC03T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC03T1"] <- 0

merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC04T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC04T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC04T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC04T1"] <- 0

merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC05T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC05T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC05T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC05T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC06T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC06T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC06T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC06T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC07T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC07T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC07T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC07T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC08T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC08T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC08T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC08T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC10T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC10T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC10T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC10T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC10T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC10T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC11T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC11T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC11T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC11T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC14T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC14T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC14T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC14T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC14T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC14T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC15T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC15T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC15T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC15T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC16T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC16T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC16T1"] <- 1
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC16T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC17T0"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC17T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC17T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC17T1"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC19T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC19T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC19T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC19T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC19T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC19T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC20T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC20T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC20T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC20T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC20T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC20T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC21T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC21T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC21T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC21T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC21T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC21T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC22T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC22T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC22T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC22T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC23T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC23T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC23T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC23T0"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC23T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC23T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC24T1"] <- 1
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC24T1"] <- 1
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC24T1"] <- 4
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC24T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC25T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC25T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC25T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC25T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC26T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC26T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC26T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC26T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC27T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC27T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC27T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC27T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC28T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC28T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC28T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC28T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC29T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC29T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC29T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC29T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC30T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC30T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC30T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC30T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC31T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC31T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC31T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC31T0"] <- 0

merged_final_clean$relapse[merged_final_clean$ID_long == "TEC32T1"] <- 0
merged_final_clean$lesioni[merged_final_clean$ID_long == "TEC32T1"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC32T1"] <- 0
merged_final_clean$relapse[merged_final_clean$ID_long == "TEC32T0"] <- 0
merged_final_clean$cortisone[merged_final_clean$ID_long == "TEC32T0"] <- 0



# app scaricata
# creo un dataset in cui tengo i pazienti di cui ho i primi report
first_report <- merged_final_clean
first_report <- first_report %>%
  filter(!ID %in% c("TEC01", "TEC18", "TEC32"))

first_report <- first_report %>%
  filter(!ID_long %in% c(
    "TEC03T0", "TEC03T1", "TEC05T0", "TEC06T0", "TEC06T12",
    "TEC10T0", "TEC14T0", "TEC19T0", "TEC20T0", "TEC20T12",
    "TEC21T0", "TEC21T12", "TEC22T12", "TEC23T0", "TEC23T12",
    "TEC24T0", "TEC25T0", "TEC25T12", "TEC26T1", "TEC26T12",
    "TEC27T1", "TEC27T12", "TEC28T1", "TEC28T12", "TEC31T0"
  ))


first_report <- first_report %>%
  select(-gc_treatment,
         -lesion_burden,
         -percent_th17,
         -treg_il10_pos,
         -treg_cd39_pos,
         -delta_EDSS,
         -Tempo_num)

# rinomino bone marrow in spinal
first_report <- first_report %>%
  rename(spinal_lesions = bone_marrow_lesions)


# per far funzionare MEDAS_group nell'app
first_report <- first_report %>% filter(!is.na(MEDAS_group))
first_report <- first_report %>% filter(!is.na(BMI_group))
first_report <- first_report %>%
  rename(id = ID_long)
first_report <- first_report %>%
  rename(id_original = ID)

names(first_report)
sample_names(phyloseq_object)

first_report <- merged_final_clean %>%
  left_join(first_report %>% select(ID_long, MEDAS_group), by = "ID_long")

first_report <- first_report %>%
  rename(medas_group = MEDAS_group.x)
first_report <- first_report %>%
  select(-MEDAS_group.y)

first_report <- first_report %>%
  rename(bmi_group = BMI_group)


# salvo
write.csv2(merged_final, "C:/AURI/Thesis_bagetto/input/merged_final.csv", row.names = FALSE)
write.csv2(merged_final_clean, "C:/AURI/Thesis_bagetto/input/merged_final_clean.csv", row.names = FALSE)
write.csv2(first_report, "C:/AURI/Thesis_bagetto/input/first_report.csv", row.names = FALSE)
write.csv(first_report, "C:/AURI/Thesis_bagetto/input/first_report.csv", row.names = FALSE)
write.table(first_report, "C:/AURI/Thesis_bagetto/input/first_report.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

read.csv("C:/AURI/Thesis_bagetto/input/first_report.csv", nrows = 5)


#