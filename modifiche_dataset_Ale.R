metadata_cleaned_completo <- read.csv("metadata_cleaned_completo.csv", sep = ";")
View(metadata_cleaned_completo)

metadata_cleaned_completo <- metadata_cleaned_completo %>%
  filter(grepl("^(TEC|MAV)", ID))


metadata_cleaned_completo <- metadata_cleaned_completo %>%
  mutate(ID = sub("T[0-9]+$", "", ID))


metadata_cleaned_completo <- metadata_cleaned_completo %>%
  separate(ID, into = c("prefix", "num"), sep = 3, remove = FALSE) %>%
  mutate(
    num = as.numeric(num)   # converte 01 → 1, 02 → 2, ecc.
  ) %>%
  arrange(prefix, num) %>%  # ordina prima per TEC/MAV, poi per numero
  mutate(
    ID = paste0(prefix, sprintf("%02d", num))  # ricostruisce TEC01, MAV02, ecc.
  )

#elimino riga di TEC09
metadata_cleaned_completo <- metadata_cleaned_completo %>%
  filter(ID != "TEC09")


# modifico il mio dataset merged
merged$EDSS_T30[merged$ID == "TEC02"] <- 1
merged$relapse_T30[merged$ID == "TEC02"] <- 0
merged$cortisone_T30[merged$ID == "TEC02"] <- 0
merged$EDSS_T30[merged$ID == "TEC02"] <- 1
merged$EDSS_T18[merged$ID == "TEC04"] <- 1
merged$lesioni_T18[merged$ID == "TEC04"] <- 0
merged$relapse_T18[merged$ID == "TEC04"] <- 0
merged$cortisone_T18[merged$ID == "TEC04"] <- 0
merged$EDSS_T30[merged$ID == "TEC04"] <- 1
merged$EDSS_T6[merged$ID == "TEC05"] <- 1
merged$lesioni_T6[merged$ID == "TEC05"] <- 0
merged$relapse_T6[merged$ID == "TEC05"] <- 0
merged$cortisone_T6[merged$ID == "TEC05"] <- 0
merged$EDSS_T6[merged$ID == "TEC06"] <- 1
merged$lesioni_T12[merged$ID == "TEC06"] <- 0
merged$relapse_T12[merged$ID == "TEC06"] <- 0
merged$EDSS_T18[merged$ID == "TEC06"] <- 1
merged$lesioni_T18[merged$ID == "TEC06"] <- 0
merged$relapse_T18[merged$ID == "TEC06"] <- 0
merged$lesioni_T24[merged$ID == "TEC06"] <- 0
merged$relapse_T24[merged$ID == "TEC06"] <- 0
merged$EDSS_T30[merged$ID == "TEC06"] <- 0
merged$lesioni_T30[merged$ID == "TEC06"] <- 0
merged$relapse_T30[merged$ID == "TEC06"] <- 0
merged$lesioni_T12[merged$ID == "TEC07"] <- 0
merged$relapse_T12[merged$ID == "TEC07"] <- 0
merged$cortisone_T12[merged$ID == "TEC07"] <- 0
merged$EDSS_T18[merged$ID == "TEC07"] <- 1
merged$lesioni_T18[merged$ID == "TEC07"] <- 0
merged$relapse_T18[merged$ID == "TEC07"] <- 0
merged$cortisone_T18[merged$ID == "TEC07"] <- 0
merged$lesioni_T24[merged$ID == "TEC07"] <- 0
merged$relapse_T24[merged$ID == "TEC07"] <- 0
merged$cortisone_T24[merged$ID == "TEC07"] <- 0

merged$lesioni_T18[merged$ID == "TEC08"] <- 0
merged$relapse_T18[merged$ID == "TEC08"] <- 1

merged$EDSS_T6[merged$ID == "TEC10"] <- 1.5
merged$EDSS_T18[merged$ID == "TEC10"] <- 2

merged$EDSS_T30[merged$ID == "TEC10"] <- 1.5
merged$lesioni_T30[merged$ID == "TEC10"] <- 0
merged$relapse_T30[merged$ID == "TEC10"] <- 0
merged$cortisone_T30[merged$ID == "TEC10"] <- 0

merged$EDSS_T6[merged$ID == "TEC15"] <- 0
merged$lesioni_T6[merged$ID == "TEC15"] <- 0
merged$relapse_T6[merged$ID == "TEC15"] <- 0
merged$cortisone_T6[merged$ID == "TEC15"] <- 0

merged$EDSS_T6[merged$ID == "TEC16"] <- 1
merged$lesioni_T6[merged$ID == "TEC16"] <- 1
merged$relapse_T6[merged$ID == "TEC16"] <- 0
merged$cortisone_T6[merged$ID == "TEC16"] <- 0
merged$EDSS_T30[merged$ID == "TEC16"] <- 1
merged$lesioni_T30[merged$ID == "TEC16"] <- 0
merged$relapse_T30[merged$ID == "TEC16"] <- 0
merged$cortisone_T30[merged$ID == "TEC16"] <- 0

merged$EDSS_T6[merged$ID == "TEC17"] <- 0
merged$lesioni_T6[merged$ID == "TEC17"] <- 0
merged$relapse_T6[merged$ID == "TEC17"] <- 0
merged$cortisone_T6[merged$ID == "TEC17"] <- 0
merged$lesioni_T18[merged$ID == "TEC17"] <- 0
merged$relapse_T18[merged$ID == "TEC17"] <- 0
merged$cortisone_T18[merged$ID == "TEC17"] <- 0
merged$lesioni_T24[merged$ID == "TEC17"] <- 0
merged$relapse_T24[merged$ID == "TEC17"] <- 0
merged$cortisone_T24[merged$ID == "TEC17"] <- 0
merged$lesioni_T30[merged$ID == "TEC17"] <- 0
merged$relapse_T30[merged$ID == "TEC17"] <- 0
merged$cortisone_T30[merged$ID == "TEC17"] <- 0

merged$EDSS_T6[merged$ID == "TEC18"] <- 1.5
merged$relapse_T6[merged$ID == "TEC18"] <- 0
merged$cortisone_T6[merged$ID == "TEC18"] <- 0
merged$EDSS_T18[merged$ID == "TEC18"] <- 1.5
merged$relapse_T18[merged$ID == "TEC18"] <- 0
merged$cortisone_T18[merged$ID == "TEC18"] <- 0

merged <- merged %>%
  mutate(MEDAS = ifelse(ID == "TEC29", 50, MEDAS))

merged <- merged %>%
  mutate(MEDAS = ifelse(ID == "TEC26", 57, MEDAS))

merged <- merged %>%
  mutate(MEDAS = ifelse(ID == "TEC30", 57, MEDAS))

merged$EDSS_T6[merged$ID== "TEC19"] <- 1
merged$lesioni_T12[merged$ID== "TEC20"] <- 0
merged$relapse_T12[merged$ID== "TEC20"] <- 0
merged$cortisone_T12[merged$ID== "TEC20"] <- 0

merged$EDSS_T6[merged$ID== "TEC21"] <- 1.5
merged$lesioni_T6[merged$ID== "TEC21"] <- 0
merged$relapse_T6[merged$ID== "TEC21"] <- 0
merged$cortisone_T6[merged$ID== "TEC21"] <- 0

merged$EDSS_T18[merged$ID== "TEC22"] <- 1.5
merged$lesioni_T18[merged$ID== "TEC22"] <- 0
merged$relapse_T18[merged$ID== "TEC22"] <- 0
merged$cortisone_T18[merged$ID== "TEC22"] <- 0

merged$EDSS_T36[merged$ID== "TEC22"] <- 1
merged$lesioni_T36[merged$ID== "TEC22"] <- 0
merged$relapse_T36[merged$ID== "TEC22"] <- 0
merged$cortisone_T36[merged$ID== "TEC22"] <- 0


merged$EDSS_T6[merged$ID== "TEC23"] <- 0
merged$lesioni_T6[merged$ID== "TEC23"] <- 0
merged$relapse_T6[merged$ID== "TEC23"] <- 0
merged$cortisone_T6[merged$ID== "TEC23"] <- 0

merged$EDSS_T18[merged$ID== "TEC23"] <- 0
merged$lesioni_T18[merged$ID== "TEC23"] <- 0
merged$relapse_T18[merged$ID== "TEC23"] <- 0
merged$cortisone_T18[merged$ID== "TEC23"] <- 0

merged$EDSS_T24[merged$ID== "TEC23"] <- 0
merged$lesioni_T24[merged$ID== "TEC23"] <- 0
merged$relapse_T24[merged$ID== "TEC23"] <- 0
merged$cortisone_T24[merged$ID== "TEC23"] <- 0

merged$EDSS_T6[merged$ID== "TEC24"] <- 1
merged$relapse_T12[merged$ID== "TEC24"] <- 1

merged$EDSS_T6[merged$ID== "TEC25"] <- 1.5
merged$EDSS_T18[merged$ID== "TEC25"] <- 1.5

merged$EDSS_T6[merged$ID== "TEC26"] <- 2
merged$lesioni_T6[merged$ID== "TEC26"] <- 0
merged$relapse_T6[merged$ID== "TEC26"] <- 0
merged$EDSS_T18[merged$ID== "TEC26"] <- 2
merged$lesioni_T18[merged$ID== "TEC26"] <- 0
merged$relapse_T18[merged$ID== "TEC26"] <- 0

merged$EDSS_T6[merged$ID== "TEC27"] <- 2
merged$lesioni_T6[merged$ID== "TEC27"] <- 0
merged$relapse_T6[merged$ID== "TEC27"] <- 0
merged$cortisone_T6[merged$ID== "TEC27"] <- 0
merged$cortisone_T12[merged$ID== "TEC27"] <- 0
merged$EDSS_T18[merged$ID== "TEC27"] <- 2.5
merged$lesioni_T18[merged$ID== "TEC27"] <- 0
merged$relapse_T18[merged$ID== "TEC27"] <- 0
merged$cortisone_T18[merged$ID== "TEC27"] <- 0
merged$cortisone_T24[merged$ID== "TEC27"] <- 0

merged$EDSS_T6[merged$ID== "TEC28"] <- 2
merged$lesioni_T6[merged$ID== "TEC28"] <- 0
merged$relapse_T6[merged$ID== "TEC28"] <- 0
merged$cortisone_T6[merged$ID== "TEC28"] <- 0
merged$EDSS_T12[merged$ID== "TEC28"] <- 2
merged$relapse_T12[merged$ID== "TEC28"] <- 0
merged$lesioni_T12[merged$ID== "TEC28"] <- 0
merged$EDSS_T18[merged$ID== "TEC28"] <- 3
merged$lesioni_T18[merged$ID== "TEC28"] <- 0
merged$relapse_T18[merged$ID== "TEC28"] <- 0
merged$cortisone_T18[merged$ID== "TEC28"] <- 0
merged$sex[merged$ID== "TEC32"] <- "F"

merged$EDSS_T6[merged$ID== "TEC29"] <- 3
merged$EDSS_T18[merged$ID== "TEC29"] <- 3
merged$lesioni_T18[merged$ID== "TEC29"] <- 0
merged$relapse_T18[merged$ID== "TEC29"] <- 0
merged$cortisone_T18[merged$ID== "TEC29"] <- 0

merged$EDSS_T6[merged$ID== "TEC30"] <- 0
merged$lesioni_T6[merged$ID== "TEC30"] <- 0
merged$relapse_T6[merged$ID== "TEC30"] <- 0
merged$cortisone_T6[merged$ID== "TEC30"] <- 0
merged$EDSS_T18[merged$ID== "TEC30"] <- 0
merged$lesioni_T18[merged$ID== "TEC30"] <- 0
merged$relapse_T18[merged$ID== "TEC30"] <- 0
merged$cortisone_T18[merged$ID== "TEC30"] <- 0


merged$EDSS_T6[merged$ID== "TEC31"] <- 1
merged$lesioni_T6[merged$ID== "TEC31"] <- 0
merged$relapse_T6[merged$ID== "TEC31"] <- 0
merged$cortisone_T6[merged$ID== "TEC31"] <- 0
merged$EDSS_T18[merged$ID== "TEC31"] <- 1
merged$lesioni_T18[merged$ID== "TEC31"] <- 0
merged$relapse_T18[merged$ID== "TEC31"] <- 0
merged$cortisone_T18[merged$ID== "TEC31"] <- 0


merged$EDSS_T6[merged$ID== "TEC32"] <- 2
merged$lesioni_T6[merged$ID== "TEC32"] <- 0
merged$relapse_T6[merged$ID== "TEC32"] <- 0
merged$cortisone_T6[merged$ID== "TEC32"] <- 0
merged$EDSS_T12[merged$ID== "TEC32"] <- 2
merged$lesioni_T12[merged$ID== "TEC32"] <- 0
merged$relapse_T12[merged$ID== "TEC32"] <- 0
merged$cortisone_T12[merged$ID== "TEC32"] <- 0
merged$EDSS_T18[merged$ID== "TEC32"] <- 2
merged$lesioni_T18[merged$ID== "TEC32"] <- 0
merged$relapse_T18[merged$ID== "TEC32"] <- 0
merged$cortisone_T18[merged$ID== "TEC32"] <- 0
merged$naive[merged$ID== "TEC32"] <- "no"
merged$naive[merged$ID== "TEC29"] <- "no"
merged$naive[merged$ID== "TEC25"] <- "no"
merged$gc_treatment[merged$ID== "TEC11"] <- "positive"

merged %>% 
  filter(is.na(gadolinium_contrast)) %>% 
  select(ID, gadolinium_contrast)

# elimino pazienti senza dataset
merged <- merged %>%
  filter(ID != "TEC12")
merged <- merged %>%
  filter(ID != "TEC13")
merged <- merged %>%
  filter(ID != "TEC18")

merged_final_clean <- merged_final_clean %>%
  filter(ID != "TEC12")
merged_final_clean <- merged_final_clean %>%
  filter(ID != "TEC13")
merged_final_clean <- merged_final_clean %>%
  filter(ID != "TEC18")

merged$EDSS_T30[merged$ID== "TEC31"] <- 1.5
merged$lesioni_T30[merged$ID== "TEC31"] <- 0
merged$relapse_T30[merged$ID== "TEC31"] <- 0
merged$cortisone_T30[merged$ID== "TEC31"] <- 0
merged$EDSS_T36[merged$ID== "TEC31"] <- 1.5
merged$lesioni_T36[merged$ID== "TEC31"] <- 0
merged$relapse_T36[merged$ID== "TEC31"] <- 0
merged$cortisone_T36[merged$ID== "TEC31"] <- 0


merged$EDSS_T30[merged$ID== "TEC30"] <- 0
merged$lesioni_T30[merged$ID== "TEC30"] <- 0
merged$relapse_T30[merged$ID== "TEC30"] <- 0
merged$cortisone_T30[merged$ID== "TEC30"] <- 0
merged$EDSS_T36[merged$ID== "TEC30"] <- 0
merged$lesioni_T36[merged$ID== "TEC30"] <- 0
merged$relapse_T36[merged$ID== "TEC30"] <- 0
merged$cortisone_T36[merged$ID== "TEC30"] <- 0

merged$lesioni_T6[merged$ID== "TEC29"] <- 0
merged$relapse_T6[merged$ID== "TEC29"] <- 0
merged$cortisone_T6[merged$ID== "TEC29"] <- 0
merged$EDSS_T30[merged$ID== "TEC29"] <- 3.5
merged$lesioni_T30[merged$ID== "TEC29"] <- 0
merged$relapse_T30[merged$ID== "TEC29"] <- 0
merged$cortisone_T30[merged$ID== "TEC29"] <- 0
merged$EDSS_T36[merged$ID== "TEC29"] <- 3.5
merged$lesioni_T36[merged$ID== "TEC29"] <- 0
merged$relapse_T36[merged$ID== "TEC29"] <- 0
merged$cortisone_T36[merged$ID== "TEC29"] <- 0

merged$EDSS_T12[merged$ID== "TEC28"] <- 3
merged$cortisone_T12[merged$ID== "TEC28"] <- 0
merged$cortisone_T18[merged$ID== "TEC28"] <- 0
merged$EDSS_T24[merged$ID== "TEC28"] <- 3.5
merged$lesioni_T24[merged$ID== "TEC28"] <- 0
merged$relapse_T24[merged$ID== "TEC28"] <- 0
merged$cortisone_T24[merged$ID== "TEC28"] <- 0
merged$EDSS_T30[merged$ID== "TEC28"] <- 3.5
merged$lesioni_T30[merged$ID== "TEC28"] <- 0
merged$relapse_T30[merged$ID== "TEC28"] <- 0
merged$cortisone_T30[merged$ID== "TEC28"] <- 0
merged$EDSS_T36[merged$ID== "TEC28"] <- 3.5
merged$lesioni_T36[merged$ID== "TEC28"] <- 0
merged$relapse_T36[merged$ID== "TEC28"] <- 0
merged$cortisone_T36[merged$ID== "TEC28"] <- 0

merged$EDSS_T30[merged$ID== "TEC27"] <- 2.5
merged$lesioni_T30[merged$ID== "TEC27"] <- 0
merged$relapse_T30[merged$ID== "TEC27"] <- 0
merged$cortisone_T30[merged$ID== "TEC27"] <- 0
merged$EDSS_T36[merged$ID== "TEC27"] <- 2.5
merged$lesioni_T36[merged$ID== "TEC27"] <- 0
merged$relapse_T36[merged$ID== "TEC27"] <- 0
merged$cortisone_T36[merged$ID== "TEC27"] <- 0

merged$cortisone_T6[merged$ID== "TEC26"] <- 0
merged$cortisone_T12[merged$ID== "TEC26"] <- 0
merged$cortisone_T18[merged$ID== "TEC26"] <- 0
merged$cortisone_T24[merged$ID== "TEC26"] <- 0
merged$EDSS_T30[merged$ID== "TEC26"] <- 2
merged$lesioni_T30[merged$ID== "TEC26"] <- 0
merged$relapse_T30[merged$ID== "TEC26"] <- 0
merged$cortisone_T30[merged$ID== "TEC26"] <- 0
merged$EDSS_T36[merged$ID== "TEC26"] <- 2
merged$lesioni_T36[merged$ID== "TEC26"] <- 0
merged$relapse_T36[merged$ID== "TEC26"] <- 0
merged$cortisone_T36[merged$ID== "TEC26"] <- 0

merged$lesioni_T6[merged$ID== "TEC25"] <- 0
merged$relapse_T6[merged$ID== "TEC25"] <- 0
merged$cortisone_T6[merged$ID== "TEC25"] <- 0
merged$lesioni_T18[merged$ID== "TEC25"] <- 0
merged$relapse_T18[merged$ID== "TEC25"] <- 0
merged$cortisone_T18[merged$ID== "TEC25"] <- 0
merged$cortisone_T12[merged$ID== "TEC25"] <- 0
merged$cortisone_T24[merged$ID== "TEC25"] <- 0

merged$lesioni_T18[merged$ID== "TEC24"] <- 0
merged$relapse_T18[merged$ID== "TEC24"] <- 0
merged$EDSS_T24[merged$ID== "TEC24"] <- 1.5
merged$lesioni_T24[merged$ID== "TEC24"] <- 0
merged$relapse_T24[merged$ID== "TEC24"] <- 0
merged$cortisone_T24[merged$ID== "TEC24"] <- 0
merged$EDSS_T30[merged$ID== "TEC24"] <- 1.5
merged$lesioni_T30[merged$ID== "TEC24"] <- 0
merged$relapse_T30[merged$ID== "TEC24"] <- 0
merged$cortisone_T30[merged$ID== "TEC24"] <- 0
merged$EDSS_T36[merged$ID== "TEC24"] <- 1.5
merged$lesioni_T36[merged$ID== "TEC24"] <- 0
merged$relapse_T36[merged$ID== "TEC24"] <- 0
merged$cortisone_T36[merged$ID== "TEC24"] <- 0

merged$lesioni_T6[merged$ID== "TEC19"] <- 0
merged$cortisone_T6[merged$ID== "TEC19"] <- 0
merged$lesioni_T12[merged$ID== "TEC19"] <- 0
merged$relapse_T12[merged$ID== "TEC19"] <- 0
merged$cortisone_T12[merged$ID== "TEC19"] <- 0
merged$EDSS_T18[merged$ID== "TEC19"] <- 1
merged$lesioni_T18[merged$ID== "TEC19"] <- 0
merged$relapse_T18[merged$ID== "TEC19"] <- 0
merged$cortisone_T18[merged$ID== "TEC19"] <- 0

merged$EDSS_T18[merged$ID== "TEC17"] <- 0
merged$EDSS_T24[merged$ID== "TEC17"] <- 0
merged$EDSS_T30[merged$ID== "TEC17"] <- 0

merged$EDSS_T18[merged$ID== "TEC15"] <- 0
merged$lesioni_T18[merged$ID== "TEC15"] <- 0
merged$relapse_T18[merged$ID== "TEC15"] <- 0
merged$cortisone_T18[merged$ID== "TEC15"] <- 0
merged$EDSS_T24[merged$ID== "TEC15"] <- 0
merged$lesioni_T24[merged$ID== "TEC15"] <- 0
merged$relapse_T24[merged$ID== "TEC15"] <- 0
merged$cortisone_T24[merged$ID== "TEC15"] <- 0
merged$EDSS_T30[merged$ID== "TEC15"] <- 0
merged$lesioni_T30[merged$ID== "TEC15"] <- 0
merged$relapse_T30[merged$ID== "TEC15"] <- 0
merged$cortisone_T30[merged$ID== "TEC15"] <- 0
merged$EDSS_T36[merged$ID== "TEC15"] <- 0
merged$lesioni_T36[merged$ID== "TEC15"] <- 0
merged$relapse_T36[merged$ID== "TEC15"] <- 0
merged$cortisone_T36[merged$ID== "TEC15"] <- 0

merged$lesioni_T6[merged$ID== "TEC10"] <- 0
merged$relapse_T6[merged$ID== "TEC10"] <- 0
merged$cortisone_T6[merged$ID== "TEC10"] <- 0
merged$lesioni_T12[merged$ID== "TEC10"] <- 0
merged$relapse_T12[merged$ID== "TEC10"] <- 0
merged$cortisone_T12[merged$ID== "TEC10"] <- 0
merged$lesioni_T18[merged$ID== "TEC10"] <- 0
merged$relapse_T18[merged$ID== "TEC10"] <- 0
merged$cortisone_T18[merged$ID== "TEC10"] <- 0

merged$EDSS_T18[merged$ID== "TEC08"] <- 1

merged$lesioni_T6[merged$ID== "TEC06"] <- 0
merged$relapse_T6[merged$ID== "TEC06"] <- 0
merged$cortisone_T6[merged$ID== "TEC06"] <- 0
merged$cortisone_T12[merged$ID== "TEC06"] <- 0
merged$cortisone_T18[merged$ID== "TEC06"] <- 0
merged$cortisone_T24[merged$ID== "TEC06"] <- 0
merged$cortisone_T30[merged$ID== "TEC06"] <- 0

merged$EDSS_T6[merged$ID== "TEC04"] <- 1
merged$lesioni_T6[merged$ID== "TEC04"] <- 0
merged$relapse_T6[merged$ID== "TEC04"] <- 0
merged$cortisone_T6[merged$ID== "TEC04"] <- 0
merged$lesioni_T30[merged$ID== "TEC04"] <- 0
merged$relapse_T30[merged$ID== "TEC04"] <- 0
merged$cortisone_T30[merged$ID== "TEC04"] <- 0

merged$lesioni_T12[merged$ID== "TEC03"] <- 0
merged$relapse_T12[merged$ID== "TEC03"] <- 0
merged$cortisone_T12[merged$ID== "TEC03"] <- 0
merged$EDSS_T18[merged$ID== "TEC03"] <- 1.5
merged$lesioni_T18[merged$ID== "TEC03"] <- 0
merged$relapse_T18[merged$ID== "TEC03"] <- 0
merged$cortisone_T18[merged$ID== "TEC03"] <- 0
merged$EDSS_T24[merged$ID== "TEC03"] <- 1.5
merged$lesioni_T24[merged$ID== "TEC03"] <- 0
merged$relapse_T24[merged$ID== "TEC03"] <- 0
merged$cortisone_T24[merged$ID== "TEC03"] <- 0
merged$EDSS_T30[merged$ID== "TEC03"] <- 1.5
merged$lesioni_T30[merged$ID== "TEC03"] <- 0
merged$relapse_T30[merged$ID== "TEC03"] <- 0
merged$cortisone_T30[merged$ID== "TEC03"] <- 0





#SALVO MODIFICHE
write.csv2(merged,
           "C:/AURI/Thesis_bagetto/input/merged.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")


write.csv2(merged_final_clean,
           "C:/AURI/Thesis_bagetto/input/merged_final_clean.csv",
           row.names = FALSE, 
           fileEncoding = "UTF-8")

# APRO MERGED
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")

# MERGED DI CECI MAV+TEC
merged_ceci <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_ceci (2).csv")


