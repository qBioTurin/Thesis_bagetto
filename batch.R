#importo dataset merged
merged <- read.csv("C:/AURI/Thesis_bagetto/input/merged.csv")

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
