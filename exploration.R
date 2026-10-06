str(merged)
write.csv(merged, "merged.csv", row.names = FALSE)
getwd()
write.csv(
  merged,
  "C:/AURI/Thesis_Bagetto/input/merged.csv",
  row.names = FALSE
)
file.exists("C:/AURI/Thesis_Bagetto/input/merged.csv")
write.csv(merged, "input/merged.csv", row.names = FALSE)
getwd()
setwd("C:/AURI/Thesis_Bagetto")
####### sistemo le variabili #######

## DT: versione codice più compatta, da finire di completare:
merged <- merged %>%
  mutate(
    # --- DATE ---
    across(
      c(
        data_di_nascita,
        DATA_ESORDIO,
        data_diagnosi,
        data_prelievo_T0,
        data_cambio_terapia,
        data_referto_T6,
        data_referto_T12,
        data_referto_T18,
        data_referto_T24,
        data_referto_T30,
        data_referto_T36,
        sample_collection_date,
        glc_date_before_t0
      ),
      ~ as.Date(.x, origin = "1899-12-30")
    ),
    
    # --- FACTOR: lesioni / relapse ---
    across(starts_with("lesioni_T"), as.factor),
    across(starts_with("relapse_T"), as.factor),
    
    # --- FACTOR: variabili binarie ---
    across(
      c(cambio_terapia, bone_marrow_lesions, subtentorial_lesions,
        gadolinium_contrast, full_recovery),
      as.factor
    ),
    
    # --- FACTOR: yes/no ---
    across(
      c(naive, spike_in),
      ~ factor(.x, levels = c("no", "yes"))
    ),
    
    across(
      c(smoking_habit, antibiotic_use),
      ~ factor(.x, levels = c("yes", "no"))
    ),
    
    # --- FACTOR: positive/negative ---
    across(
      c(glucocorticoid_treatment, gc_treatment, prognosis_at_onset),
      ~ factor(.x, levels = c("positive", "negative"))
    ),
    
    # --- FACTOR: sex, batch ---
    sex = factor(sex, levels = c("M", "F")),
    simplified_batch = factor(simplified_batch, levels = c("1-3", "4-6"))
  )

####### sistemo la tabella e inserisco i dati mancanti #####
#recupero colonna smoking_habit
merged <- merge(merged,merged_data[, c("ID", "smoking_habit")], by = "ID", all.x = TRUE)
#elimino vecchia colonna sbagliata
merged$smoking_habit.x <- NULL
merged <- merged %>% relocate(smoking_habit.y, .after = pyr_mds)
library(dplyr)

names(merged)[names(merged) == "smoking_habit.y"] <- "smoking_habit"

class(merged$smoking_habit.y)
merged$smoking_habit <- factor(merged$smoking_habit,levels = c("yes", "no"))

merged$data_prelievo_T0[merged$ID == "TEC26"] <- "2021-09-08"
merged$data_prelievo_T0 <- as.Date(as.numeric(merged$data_prelievo_T0), origin = "1899-12-30")

merged$data_prelievo_T0[merged$ID == "TEC27"] <- as.Date("2021-03-31")
merged$data_prelievo_T0[merged$ID == "TEC28"] <- as.Date("2021-08-02")
merged$data_prelievo_T0[merged$ID == "TEC25"] <- as.Date("2021-06-03")
merged$data_prelievo_T0[merged$ID == "TEC29"] <- as.Date("2021-08-02")
merged$data_prelievo_T0[merged$ID == "TEC30"] <- as.Date("2022-01-27")
merged$data_prelievo_T0[merged$ID == "TEC31"] <- as.Date("2022-03-29")
merged$data_prelievo_T0[merged$ID == "TEC32"] <- as.Date("2022-07-13")
class(merged$data_prelievo_T0)

merged$data_prelievo_T0 <- merged_data$data_prelievo_T0[match(merged$ID, merged_data$ID)]

merged$data_cambio_terapia[merged$ID == "TEC32"] <- as.Date("2023-09-29")
merged$data_cambio_terapia[merged$ID == "TEC25"] <- as.Date("2021-01-09")
merged$nuova_TERAPIA[merged$ID == "TEC32"] <- "OCRELIZUMAB"
merged$nuova_TERAPIA[merged$ID == "TEC25"] <- "MAVENCLAD"

merged$data_di_nascita[merged$ID == "TEC32"] <- as.Date("1966-05-25", format="%Y%m%d")
class(merged$data_di_nascita)

merged$data_di_nascita <- as.Date(merged$data_di_nascita)
merged$data_di_nascita[merged$ID == "TEC32"] <- as.Date("1966-05-25")


class(merged$DATA_ESORDIO)
merged$DATA_ESORDIO <- as.Date(merged$DATA_ESORDIO)
merged$DATA_ESORDIO[merged$ID == "TEC25"] <- as.Date("2021-02-15")
merged$DATA_ESORDIO[merged$ID == "TEC26"] <- as.Date("2021-08-15")
merged$DATA_ESORDIO[merged$ID == "TEC27"] <- as.Date("2021-10-15")
merged$DATA_ESORDIO[merged$ID == "TEC28"] <- as.Date("2020-03-01")
merged$DATA_ESORDIO[merged$ID == "TEC29"] <- as.Date("2020-06-15")
merged$DATA_ESORDIO[merged$ID == "TEC30"] <- as.Date("2020-12-30")
merged$DATA_ESORDIO[merged$ID == "TEC31"] <- as.Date("1987-06-01")
merged$DATA_ESORDIO[merged$ID == "TEC32"] <- as.Date("2019-01-01")

class(merged$data_diagnosi)
merged$data_diagnosi <- as.Date(merged$data_diagnosi)
merged$data_diagnosi[merged$ID == "TEC25"] <- as.Date("2021-03-15")
merged$data_diagnosi[merged$ID == "TEC26"] <- as.Date("2021-09-08")
merged$data_diagnosi[merged$ID == "TEC27"] <- as.Date("2021-03-31")
merged$data_diagnosi[merged$ID == "TEC28"] <- as.Date("2021-07-15")
merged$data_diagnosi[merged$ID == "TEC29"] <- as.Date("2021-07-15")
merged$data_diagnosi[merged$ID == "TEC30"] <- as.Date("2022-01-15")
merged$data_diagnosi[merged$ID == "TEC31"] <- as.Date("1996-06-01")
merged$data_diagnosi[merged$ID == "TEC32"] <- as.Date("2022-07-01")

class(merged$glc_date_before_t0)
merged$glc_date_before_t0 <- as.Date(merged$glc_date_before_t0)
merged$glc_date_before_t0[merged$ID == "TEC30"] <- as.Date("2021-01-01")

class(merged$lesion_burden)
merged$lesion_burden[merged$ID == "TEC25"] <- 1
merged$lesion_burden[merged$ID == "TEC29"] <- 1
merged$lesion_burden[merged$ID == "TEC32"] <- 1

class(merged$EDSS_T12)
merged$EDSS_T12[merged$ID == "TEC25"] <- 1.5
merged$EDSS_T12[merged$ID == "TEC26"] <- 2
merged$EDSS_T12[merged$ID == "TEC27"] <- 2.5
merged$EDSS_T12[merged$ID == "TEC29"] <- 3
merged$EDSS_T12[merged$ID == "TEC30"] <- 0
merged$EDSS_T12[merged$ID == "TEC31"] <- 1
merged$EDSS_T12[merged$ID == "TEC32"] <- 2
merged$EDSS_T12[merged$ID == "TEC10"] <- 2

class(merged$EDSS_T0)
merged$EDSS_T0[merged$ID == "TEC28"] <- 2
merged$EDSS_T0[merged$ID == "TEC29"] <- 3
merged$EDSS_T0[merged$ID == "TEC30"] <- 0
merged$EDSS_T0[merged$ID == "TEC31"] <- 1
merged$EDSS_T0[merged$ID == "TEC32"] <- 2

class(merged$EDSS_T24)
merged$EDSS_T24[merged$ID == "TEC25"] <- 1.5
merged$EDSS_T24[merged$ID == "TEC26"] <- 2
merged$EDSS_T24[merged$ID == "TEC27"] <- 2.5
merged$EDSS_T24[merged$ID == "TEC29"] <- 3.5
merged$EDSS_T24[merged$ID == "TEC30"] <- 0
merged$EDSS_T24[merged$ID == "TEC31"] <- 1.5
merged$EDSS_T24[merged$ID == "TEC32"] <- 2

class(merged$bone_marrow_lesions)
merged$bone_marrow_lesions <- as.factor(merged$bone_marrow_lesions)
merged$bone_marrow_lesions[merged$ID == "TEC25"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC26"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC27"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC28"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC29"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC30"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC31"] <- 1
merged$bone_marrow_lesions[merged$ID == "TEC32"] <- 1

class(merged$lesioni_T12)
merged$lesioni_T12 <- as.factor(merged$lesioni_T12)
merged$lesioni_T12[merged$ID == "TEC25"] <- 1
merged$lesioni_T12[merged$ID == "TEC26"] <- 0
merged$lesioni_T12[merged$ID == "TEC27"] <- 0
merged$lesioni_T12[merged$ID == "TEC29"] <- 0
merged$lesioni_T12[merged$ID == "TEC30"] <- 0
merged$lesioni_T12[merged$ID == "TEC31"] <- 0
merged$lesioni_T12[merged$ID == "TEC32"] <- 0

class(merged$relapse_T12)
merged$relapse_T12[merged$ID == "TEC24"] <- 0
merged$relapse_T12[merged$ID == "TEC30"] <- 0
merged$relapse_T12[merged$ID == "TEC31"] <- 0
merged$relapse_T12[merged$ID == "TEC25"] <- 0
merged$relapse_T12[merged$ID == "TEC32"] <- 0
merged$relapse_T12[merged$ID == "TEC26"] <- 0
merged$relapse_T12[merged$ID == "TEC27"] <- 0
merged$relapse_T12[merged$ID == "TEC29"] <- 0

class(merged$lesioni_T24)
merged$lesioni_T24 <- as.factor(merged$lesioni_T24)
merged$lesioni_T24[merged$ID == "TEC25"] <- 0
merged$lesioni_T24[merged$ID == "TEC26"] <- 0
merged$lesioni_T24[merged$ID == "TEC27"] <- 0
merged$lesioni_T24[merged$ID == "TEC29"] <- 0
merged$lesioni_T24[merged$ID == "TEC30"] <- 0
merged$lesioni_T24[merged$ID == "TEC31"] <- 0
merged$lesioni_T24[merged$ID == "TEC32"] <- 0

class(merged$subtentorial_lesions)
merged$subtentorial_lesions <- as.factor(merged$subtentorial_lesions)
merged$subtentorial_lesions[merged$ID == "TEC25"] <- 1
merged$subtentorial_lesions[merged$ID == "TEC26"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC27"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC28"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC29"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC30"] <- 1
merged$subtentorial_lesions[merged$ID == "TEC31"] <- 0

class(merged$relapse_T24)
merged$relapse_T24 <- as.factor(merged$relapse_T24)
merged$relapse_T24[merged$ID == "TEC25"] <- 0
merged$relapse_T24[merged$ID == "TEC26"] <- 0
merged$relapse_T24[merged$ID == "TEC27"] <- 0
merged$relapse_T24[merged$ID == "TEC29"] <- 0
merged$relapse_T24[merged$ID == "TEC30"] <- 0
merged$relapse_T24[merged$ID == "TEC31"] <- 0
merged$relapse_T24[merged$ID == "TEC32"] <- 0

class(merged$gadolinium_contrast)
merged$gadolinium_contrast <- as.factor(merged$gadolinium_contrast)
merged$gadolinium_contrast[merged$ID == "TEC25"] <- 0
merged$gadolinium_contrast[merged$ID == "TEC26"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC27"] <- 0
merged$gadolinium_contrast[merged$ID == "TEC28"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC29"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC30"] <- 0
merged$gadolinium_contrast[merged$ID == "TEC31"] <- 0
merged$gadolinium_contrast[merged$ID == "TEC32"] <- 0

class(merged$full_recovery)
merged$full_recovery <- as.factor(merged$full_recovery)
merged$full_recovery[merged$ID == "TEC25"] <- 1
merged$full_recovery[merged$ID == "TEC26"] <- 0
merged$full_recovery[merged$ID == "TEC27"] <- 1
merged$full_recovery[merged$ID == "TEC28"] <- 1
merged$full_recovery[merged$ID == "TEC29"] <- 0
merged$full_recovery[merged$ID == "TEC30"] <- 1
merged$full_recovery[merged$ID == "TEC31"] <- 1
merged$full_recovery[merged$ID == "TEC32"] <- 1
merged$full_recovery[merged$ID == "TEC18"] <- 0

class(merged$clinical_presentation)
merged$clinical_presentation[merged$ID == "TEC25"] <- "sensory_symptoms"
merged$clinical_presentation[merged$ID == "TEC26"] <- "optic_neuritis"
merged$clinical_presentation[merged$ID == "TEC27"] <- "sensory_symptoms"
merged$clinical_presentation[merged$ID == "TEC28"] <- "sensory_symptoms"
merged$clinical_presentation[merged$ID == "TEC29"] <- "brain_stem"
merged$clinical_presentation[merged$ID == "TEC30"] <- "brain_stem"
merged$clinical_presentation[merged$ID == "TEC31"] <- "optic_neuritis"
merged$clinical_presentation[merged$ID == "TEC32"] <- "optic_neuritis"

class(merged$naive)
merged$naive <- as.factor(merged$naive)
merged$naive[merged$ID == "TEC13"] <- "no"
merged$naive[merged$ID == "TEC23"] <- "yes"

class(merged$previous_therapy)
merged$previous_therapy[merged$ID == "TEC13"] <- "interferon_beta"

str(merged)

merged$data_cambio_terapia <- as.Date(merged$data_cambio_terapia, origin = "1899-12-30")
class(merged$data_cambio_terapia)
merged$data_referto_T6  <- as.Date(merged$data_referto_T6,  origin = "1899-12-30")
merged$data_referto_T12 <- as.Date(merged$data_referto_T12, origin = "1899-12-30")
merged$data_referto_T18 <- as.Date(merged$data_referto_T18, origin = "1899-12-30")
merged$data_referto_T24 <- as.Date(merged$data_referto_T24, origin = "1899-12-30")
merged$data_referto_T30 <- as.Date(merged$data_referto_T30, origin = "1899-12-30")
class(merged$data_referto_T30)
merged$data_referto_T36 <- as.Date(merged$data_referto_T36, origin = "1899-12-30")
class(merged$data_referto_T36)
merged$sample_collection_date <- as.Date(merged$sample_collection_date, origin = "1899-12-30")
class(merged$sample_collection_date)


#da int a categoriche (relapse, lesion, cambio terapia)
merged$lesioni_T6 <- factor(merged$lesioni_T6)
merged$lesioni_T18 <- factor(merged$lesioni_T18)
merged$lesioni_T30 <- factor(merged$lesioni_T30)
merged$lesioni_T36 <- factor(merged$lesioni_T36)

merged$relapse_T6 <- factor(merged$relapse_T6)
merged$relapse_T12 <- factor(merged$relapse_T12)
merged$relapse_T18<- factor(merged$relapse_T18)
merged$relapse_T30 <- factor(merged$relapse_T30)
merged$relapse_T36 <- factor(merged$relapse_T36)
merged$cambio_terapia<- factor(merged$cambio_terapia)

merged$sex <- factor(merged$sex, levels = c("M", "F"))
merged$antibiotic_use <- factor(merged$antibiotic_use, levels = c("yes", "no"))
merged$spike_in <- factor(merged$spike_in, levels = c("no", "yes"))
merged$glucocorticoid_treatment <- factor(merged$glucocorticoid_treatment,levels = c("positive", "negative"))
merged$gc_treatment <- factor(merged$gc_treatment, levels = c("positive", "negative"))
merged$prognosis_at_onset <- factor(merged$prognosis_at_onset, levels = c("positive", "negative"))

merged$simplified_batch <- factor(merged$simplified_batch)
class(merged$simplified_batch)
str(merged)

class(merged$data_di_nascita)
merged$data_di_nascita[merged$ID == "TEC27"] <- as.Date("1975-01-11")
merged$data_di_nascita[merged$ID == "TEC25"] <- as.Date("1997-07-21")
merged$data_di_nascita[merged$ID == "TEC28"] <- as.Date("1989-05-26")
merged$data_di_nascita[merged$ID == "TEC29"] <- as.Date("1964-12-23")
merged$data_di_nascita[merged$ID == "TEC26"] <- as.Date("1996-04-04")

class(merged$EDSS_T0)
merged$EDSS_T0[merged$ID == "TEC32"] <- 2.0
merged$EDSS_T12[merged$ID == "TEC32"] <- 2.0
merged$EDSS_T24[merged$ID == "TEC32"] <- 2.0

class(merged$DATA_ESORDIO)
merged$DATA_ESORDIO[merged$ID == "TEC32"] <- as.Date("2019-01-01")

class(merged$data_diagnosi)
merged$data_diagnosi[merged$ID == "TEC32"] <- as.Date("2022-07-01")

class(merged$lesion_burden)
merged$lesion_burden <- as.factor(merged$lesion_burden)
merged$lesion_burden[merged$ID == "TEC32"] <- "1"

class(merged$bone_marrow_lesions)
merged$bone_marrow_lesions[merged$ID == "TEC32"] <- "1"

class(merged$gadolinium_contrast)
merged$gadolinium_constrast[merged$ID == "TEC32"] <- "no"

library(dplyr)
merged <- merged %>% filter(ID != "TEC09")

class(merged$age_at_diagnosis)
merged$age_at_diagnosis[merged$ID == "TEC25"] <- 24
merged$age_at_diagnosis[merged$ID == "TEC26"] <- 25
merged$age_at_diagnosis[merged$ID == "TEC27"] <- 46
merged$age_at_diagnosis[merged$ID == "TEC28"] <- 32
merged$age_at_diagnosis[merged$ID == "TEC29"] <- 57
merged$age_at_diagnosis[merged$ID == "TEC32"] <- 56

class(merged$clinical_presentation)
merged$clinical_presentation[merged$ID == "TEC32"] <- "sensory_symptoms"

class(merged$lesioni_T12)
merged$lesioni_T12[merged$ID == "TEC32"] <- 0
merged$relapse_T12[merged$ID == "TEC32"] <- 0
merged$lesioni_T24[merged$ID == "TEC32"] <- 0
merged$relapse_T24[merged$ID == "TEC32"] <- 0

merged$cambio_terapia[merged$ID == "TEC30"] <- 0
merged$age_at_T0[merged$ID == "TEC30"] <- 22

class(merged$glc_date_before_t0)
merged$data_diagnosi[merged$ID == "TEC30"] <- as.Date("2021-01-01")

merged$cambio_terapia[merged$ID == "TEC31"] <- 0
merged$age_at_T0[merged$ID == "TEC31"] <- 57
merged$bmi[merged$ID == "TEC31"] <- 25

merged$antibiotic_use[merged$ID == "TEC25"] <- "no"
merged$antibiotic_use[merged$ID == "TEC29"] <- "no"
merged$data_referto_T6[merged$ID == "TEC26"] <- as.Date("2022-02-22")

merged$gadolinium_constrast[merged$ID == "TEC02"] <- "no"
merged <- merged[, -76]

merged$gadolinium_contrast[merged$ID == "TEC05"] <- 0

merged$bmi[merged$ID == "TEC13"] <- 21.83
merged$smoking_habit[merged$ID == "TEC13"] <- "yes"
merged$physical_activity[merged$ID == "TEC13"] <- "active"


merged$lesion_burden[merged$ID == "TEC18"] <- 0
merged$bone_marrow_lesions[merged$ID == "TEC18"] <- 0
merged$subtentorial_lesions[merged$ID == "TEC18"] <- 1
merged$gadolinium_contrast[merged$ID == "TEC18"] <- 0
merged$clinical_presentation[merged$ID == "TEC18"] <- "brain_stem"

merged$cambio_terapia[merged$ID == "TEC25"] <- 1
merged$data_cambio_terapia[merged$ID == "TEC25"] <- as.Date("2021-01-09")
class(merged$data_di_nascita)

library(lubridate)
merged$age <- as.integer(floor(lubridate::time_length(lubridate::interval(merged$data_di_nascita, Sys.Date()), "years")))
merged$age_at_T0 <- as.integer(floor(lubridate::time_length(lubridate::interval(merged$data_di_nascita, merged$data_prelievo_T0), "years")))
merged$age_at_diagnosis <- as.integer(floor(lubridate::time_length(lubridate::interval(merged$data_di_nascita, merged$data_diagnosi), "years")))

merged$cambio_terapia[merged$ID == "TEC26"] <- 0
merged$cambio_terapia[merged$ID == "TEC27"] <- 0
merged$cambio_terapia[merged$ID == "TEC28"] <- 0
merged$cambio_terapia[merged$ID == "TEC29"] <- 0

merged$glc_date_before_t0[merged$ID == "TEC29"] <- as.Date("2021-07-15")
class(merged$gc_treatment)
merged$gc_treatment[merged$ID == "TEC29"] <- "positive"
merged$cortisone_T24[merged$ID == "TEC29"] <- 0
merged$cortisone_T12[merged$ID == "TEC29"] <- 0

merged$cortisone_T24[merged$ID == "TEC30"] <- 0
merged$cortisone_T12[merged$ID == "TEC30"] <- 0

merged$cortisone_T24[merged$ID == "TEC31"] <- 0
merged$cortisone_T12[merged$ID == "TEC31"] <- 0

merged$cambio_terapia[merged$ID == "TEC32"] <- 1
merged$data_cambio_terapia[merged$ID == "TEC32"] <- as.Date("2023-09-29")
merged$cortisone_T24[merged$ID == "TEC32"] <- 0
merged$cortisone_T12[merged$ID == "TEC32"] <- 0

merged$sample_type[merged$ID == "TEC13"] <- "sample"
merged$sample_type[merged$ID == "TEC18"] <- "sample"
merged$sample_type[merged$ID == "TEC23"] <- "sample"
merged$sample_type[merged$ID == "TEC25"] <- "sample"
merged$sample_type[merged$ID == "TEC29"] <- "sample"
merged$sample_type[merged$ID == "TEC32"] <- "sample"

merged$disease_activity[merged$ID == "TEC11"] <- "lower_activity"
merged$disease_activity[merged$ID == "TEC13"] <- "lower_activity"
merged$disease_activity[merged$ID == "TEC18"] <- "lower_activity"
merged$disease_activity[merged$ID == "TEC23"] <- "lower_activity"
merged$disease_activity[merged$ID == "TEC25"] <- "lower_activity"
merged$disease_activity[merged$ID == "TEC29"] <- "lower_activity"
merged$disease_activity[merged$ID == "TEC32"] <- "lower_activity"

class(merged$simplified_batch)
merged$simplified_batch[merged$ID == "TEC13"] <- "1-3"
merged$simplified_batch[merged$ID == "TEC18"] <- "1-3"
merged$simplified_batch[merged$ID == "TEC23"] <- "1-3"
merged$simplified_batch[merged$ID == "TEC25"] <- "1-3"
merged$simplified_batch[merged$ID == "TEC29"] <- "1-3"
merged$simplified_batch[merged$ID == "TEC32"] <- "1-3"

merged$MEDAS[merged$ID == "TEC31"] <- 71
merged$bmi[merged$ID == "TEC31"] <- 25.45
merged$smoking_habit[merged$ID == "TEC31"] <- "no"
merged$physical_activity[merged$ID == "TEC31"] <- "active"

class(merged$relapse_T6)
merged$relapse_T6 <- factor(merged$relapse_T6)

merged$relapse_T6[merged$ID == "TEC04"] <- 0
merged$relapse_T6[merged$ID == "TEC05"] <- 1

levels(merged$relapse_T6)
merged$relapse_T6 <- as.numeric(as.character(merged$relapse_T6))
str(merged)
head(merged)


library(dplyr)

merged <- merged %>%
  mutate(ID2 = ID) %>%                     # duplica ID
  relocate(ID2, .after = cortisone_T6)     # posiziona ID2 nel punto giusto

merged$relapse_T6[merged$ID == "TEC12"] <- 0
merged$relapse_T6[merged$ID == "TEC13"] <- 0
merged$relapse_T6[merged$ID == "TEC15"] <- 1
merged$relapse_T6[merged$ID == "TEC19"] <- 0
merged$relapse_T6[merged$ID == "TEC23"] <- 0
merged$relapse_T6[merged$ID == "TEC26"] <- 0

library(dplyr)

merged <- merged %>%
  relocate(ID2, .after = cortisone_T12)

merged$relapse_T12[merged$ID == "TEC06"] <- 0
merged$relapse_T12[merged$ID == "TEC07"] <- 0
merged$relapse_T12[merged$ID == "TEC19"] <- 0

merged <- merged %>%
  relocate(ID2, .after = full_recovery)

merged <- merged %>% select(-ID2)



#### inizio analisi esplorativa ########

str(merged)
dim(merged)
names(merged)

#### #missing values ####
colSums(is.na(merged))
round(colMeans(is.na(merged)) * 100, 1)

#### DESCRIZIONE VARIABILI NUMERICHE #### 
#MEDAS
summary(merged$MEDAS)
mean(merged$MEDAS, na.rm = TRUE)
sd(merged$MEDAS, na.rm = TRUE)
median(merged$MEDAS, na.rm = TRUE)
IQR(merged$MEDAS, na.rm = TRUE)


#età
summary(merged$age_at_T0)
mean(merged$age_at_T0, na.rm = TRUE)
sd(merged$age_at_T0, na.rm = TRUE)
median(merged$age_at_T0, na.rm = TRUE)
IQR(merged$age_at_T0, na.rm = TRUE)
range(merged$age_at_T0, na.rm = TRUE)

summary(merged$age_at_diagnosis)
mean(merged$age_at_diagnosis, na.rm = TRUE)
sd(merged$age_at_diagnosis, na.rm = TRUE)
median(merged$age_at_diagnosis, na.rm = TRUE)
IQR(merged$age_at_diagnosis, na.rm = TRUE)
range(merged$age_at_diagnosis, na.rm = TRUE)

#bmi
summary(merged$bmi)
mean(merged$bmi, na.rm = TRUE)
sd(merged$bmi, na.rm = TRUE)
median(merged$bmi, na.rm = TRUE)
IQR(merged$bmi, na.rm = TRUE)
range(merged$bmi, na.rm = TRUE)


#EDSS
summary(merged$EDSS_T0)
mean(merged$EDSS_T0, na.rm = TRUE)
sd(merged$EDSS_T0, na.rm = TRUE)
median(merged$EDSS_T0, na.rm = TRUE)
IQR(merged$EDSS_T0, na.rm = TRUE)
range(merged$EDSS_T0, na.rm = TRUE)

summary(merged$EDSS_T6)
mean(merged$EDSS_T6, na.rm = TRUE)
sd(merged$EDSS_T6, na.rm = TRUE)
median(merged$EDSS_T6, na.rm = TRUE)
IQR(merged$EDSS_T6, na.rm = TRUE)
range(merged$EDSS_T6, na.rm = TRUE)

summary(merged$EDSS_T12)
mean(merged$EDSS_T12, na.rm = TRUE)
sd(merged$EDSS_T12, na.rm = TRUE)
median(merged$EDSS_T12, na.rm = TRUE)
IQR(merged$EDSS_T12, na.rm = TRUE)
range(merged$EDSS_T12, na.rm = TRUE)

summary(merged$EDSS_T18)
mean(merged$EDSS_T18, na.rm = TRUE)
sd(merged$EDSS_T18, na.rm = TRUE)
median(merged$EDSS_T18, na.rm = TRUE)
IQR(merged$EDSS_T18, na.rm = TRUE)
range(merged$EDSS_T18, na.rm = TRUE)

summary(merged$EDSS_T24)
mean(merged$EDSS_T24, na.rm = TRUE)
sd(merged$EDSS_T24, na.rm = TRUE)
median(merged$EDSS_T24, na.rm = TRUE)
IQR(merged$EDSS_T24, na.rm = TRUE)
range(merged$EDSS_T24, na.rm = TRUE)

summary(merged$EDSS_T30)
mean(merged$EDSS_T30, na.rm = TRUE)
sd(merged$EDSS_T30, na.rm = TRUE)
median(merged$EDSS_T30, na.rm = TRUE)
IQR(merged$EDSS_T30, na.rm = TRUE)
range(merged$EDSS_T30, na.rm = TRUE)

summary(merged$EDSS_T36)
mean(merged$EDSS_T36, na.rm = TRUE)
sd(merged$EDSS_T36, na.rm = TRUE)
median(merged$EDSS_T36, na.rm = TRUE)
IQR(merged$EDSS_T36, na.rm = TRUE)
range(merged$EDSS_T36, na.rm = TRUE)

#### CONTROLLO DISTRIBUZIONE VARIABILI NUMERICHE per capire come scrivere i risultati ####
#MEDAS
par(mfrow=c(1,3))
hist(merged$MEDAS)
qqnorm(merged$MEDAS)
qqline(merged$MEDAS)
boxplot(merged$MEDAS)
shapiro.test(merged$MEDAS)

#age_at_diagnosis
par(mfrow=c(1,3))
hist(merged$age_at_diagnosis)
qqnorm(merged$age_at_diagnosis)
qqline(merged$age_at_diagnosis)
boxplot(merged$age_at_diagnosis)
shapiro.test(merged$age_at_diagnosis)

#age_at_T0
par(mfrow=c(1,3))
hist(merged$age_at_T0)
qqnorm(merged$age_at_T0)
qqline(merged$age_at_T0)
boxplot(merged$age_at_T0)
shapiro.test(merged$age_at_T0)

#bmi
par(mfrow=c(1,3))
hist(merged$bmi)
qqnorm(merged$bmi)
qqline(merged$bmi)
boxplot(merged$bmi)
shapiro.test(merged$bmi)

#EDSS_T0
par(mfrow=c(1,3))
hist(merged$EDSS_T0)
qqnorm(merged$EDSS_T0)
qqline(merged$EDSS_T0)
boxplot(merged$EDSS_T0)
shapiro.test(merged$EDSS_T0)

#EDSS_T6
par(mfrow=c(1,3))
hist(merged$EDSS_T6)
qqnorm(merged$EDSS_T6)
qqline(merged$EDSS_T6)
boxplot(merged$EDSS_T6)
shapiro.test(merged$EDSS_T6)

#EDSS_T12
par(mfrow=c(1,3))
hist(merged$EDSS_T12)
qqnorm(merged$EDSS_T12)
qqline(merged$EDSS_T12)
boxplot(merged$EDSS_T12)
shapiro.test(merged$EDSS_T12)

#EDSS_T18
par(mfrow=c(1,3))
hist(merged$EDSS_T18)
qqnorm(merged$EDSS_T18)
qqline(merged$EDSS_T18)
boxplot(merged$EDSS_T18)
shapiro.test(merged$EDSS_T18)

#EDSS_T24
par(mfrow=c(1,3))
hist(merged$EDSS_T24)
qqnorm(merged$EDSS_T24)
qqline(merged$EDSS_T24)
boxplot(merged$EDSS_T24)
shapiro.test(merged$EDSS_T24)

#EDSS_T30
par(mfrow=c(1,3))
hist(merged$EDSS_T30)
qqnorm(merged$EDSS_T30)
qqline(merged$EDSS_T30)
boxplot(merged$EDSS_T30)
shapiro.test(merged$EDSS_T30)

#EDSS_T36
par(mfrow=c(1,3))
hist(merged$EDSS_T36)
qqnorm(merged$EDSS_T36)
qqline(merged$EDSS_T36)
boxplot(merged$EDSS_T36)
shapiro.test(merged$EDSS_T36)

#### VARIABILI CATEGORICHE #####
#smoking_habit
table(merged$smoking_habit)
prop.table(table(merged$smoking_habit)) * 100

#physical activity
merged$physical_activity <- factor(merged$physical_activity, levels = c("sedentary", "partially_active", "active"), ordered = TRUE)
table(merged$physical_activity)
prop.table(table(merged$physical_activity)) * 100

#lesion_burden
table(merged$lesion_burden)
prop.table(table(merged$lesion_burden)) * 100

#clinical_presentation
table(merged$clinical_presentation)
prop.table(table(merged$clinical_presentation)) * 100

#naive
table(merged$naive)
prop.table(table(merged$naive)) * 100

#sex
table(merged$sex)
prop.table(table(merged$sex)) * 100

#antibiotic_use
table(merged$antibiotic_use)
prop.table(table(merged$antibiotic_use)) * 100

#glucocorticoid_treatment
table(merged$glucocorticoid_treatment)
prop.table(table(merged$glucocorticoid_treatment)) * 100

#bone_marrow_lesions
table(merged$bone_marrow_lesions)
prop.table(table(merged$bone_marrow_lesions)) * 100

#subtentorial_lesions
table(merged$subtentorial_lesions)
prop.table(table(merged$subtentorial_lesions)) * 100

#gadolinium_contrast
table(merged$gadolinium_contrast)
prop.table(table(merged$gadolinium_contrast)) * 100

#full_recovery
table(merged$full_recovery)
prop.table(table(merged$full_recovery)) * 100

#cambio_terapia
table(merged$cambio_terapia)
prop.table(table(merged$cambio_terapia)) * 100

##### tableone ####
install.packages("tableone")
library(tableone)
vars <- c("MEDAS",  "age_at_diagnosis",  "age_at_T0",  "smoking_habit", "physical_activity", "EDSS_T0", "EDSS_T6", "EDSS_T12", "EDSS_T18", "EDSS_T24", "EDSS_T30", "EDSS_T36", "lesion_burden", "clinical_presentation", "naive", "sex", "bmi", "antibiotic_use", "glucocorticoid_treatment", "bone_marrow_lesions", "subtentorial_lesions", "gadolinium_contrast", "full_recovery")
factorVars <- c("smoking_habit",  "physical_activity",  "lesion_burden",  "clinical_presentation",  "naive",  "sex",  "antibiotic_use",  "glucocorticoid_treatment",  "bone_marrow_lesions",  "subtentorial_lesions",  "gadolinium_contrast",  "full_recovery")
table1 <- CreateTableOne(vars = vars, data = merged, factorVars = factorVars)
print(table1)

install.packages("gtsummary")
library(gtsummary)

library(gtsummary)

merged %>%
  select(all_of(vars)) %>%
  tbl_summary()
install.packages("xfun")
library(dplyr)
library(gtsummary)

# ANALISI LONGITUDINALE
### ANALISI DESCRITTIVA LONGITUDINALE EDSS #####
#Mediana/IQR a ogni tempo
median(merged$EDSS_T0, na.rm=TRUE)
IQR(merged$EDSS_T0, na.rm=TRUE)

median(merged$EDSS_T6, na.rm=TRUE)
IQR(merged$EDSS_T6, na.rm=TRUE)

median(merged$EDSS_T12, na.rm=TRUE)
IQR(merged$EDSS_T12, na.rm=TRUE)

median(merged$EDSS_T18, na.rm=TRUE)
IQR(merged$EDSS_T18, na.rm=TRUE)

median(merged$EDSS_T24, na.rm=TRUE)
IQR(merged$EDSS_T24, na.rm=TRUE)

median(merged$EDSS_T30, na.rm=TRUE)
IQR(merged$EDSS_T30, na.rm=TRUE)

median(merged$EDSS_T36, na.rm=TRUE)
IQR(merged$EDSS_T36, na.rm=TRUE)

#Numero di pazienti disponibili per ogni visita
sum(!is.na(merged$EDSS_T0))
sum(!is.na(merged$EDSS_T6))
sum(!is.na(merged$EDSS_T12))
sum(!is.na(merged$EDSS_T18))
sum(!is.na(merged$EDSS_T24))
sum(!is.na(merged$EDSS_T30))
sum(!is.na(merged$EDSS_T36))

#Andamento disabilità nel tempo -> mediana della coorte nel tempo (ricorda che il numero di pazienti varia ad ogni tempo)
edss_medians <- c(
  median(merged$EDSS_T0, na.rm=TRUE),
  median(merged$EDSS_T6, na.rm=TRUE),
  median(merged$EDSS_T12, na.rm=TRUE),
  median(merged$EDSS_T18, na.rm=TRUE),
  median(merged$EDSS_T24, na.rm=TRUE),
  median(merged$EDSS_T30, na.rm=TRUE),
  median(merged$EDSS_T36, na.rm=TRUE)
)

time <- c(0,6,12,18,24,30,36)

plot(time, edss_medians, type="b")


time <- c(0,6,12,18,24,30,36)

#Line plot della mediana/media nel tempo -> trend globale per vedere miglioramento, peggioramento o stabilità
edss_medians <- c(
  median(merged$EDSS_T0, na.rm=TRUE),
  median(merged$EDSS_T6, na.rm=TRUE),
  median(merged$EDSS_T12, na.rm=TRUE),
  median(merged$EDSS_T18, na.rm=TRUE),
  median(merged$EDSS_T24, na.rm=TRUE),
  median(merged$EDSS_T30, na.rm=TRUE),
  median(merged$EDSS_T36, na.rm=TRUE)
)

plot(
  time,
  edss_medians,
  type="b",
  pch=16,
  col="blue",
  lwd=2,
  xlab="Months",
  ylab="Median EDSS"
)

par(mfrow=c(1,1))

#Boxplot longitudinali EDSS -> mediana, variabilità, outlier, dispersione, eventuale peggioramento
boxplot(
  merged$EDSS_T0,
  merged$EDSS_T6,
  merged$EDSS_T12,
  merged$EDSS_T18,
  merged$EDSS_T24,
  merged$EDSS_T30,
  merged$EDSS_T36,
  
  names=c("T0","T6","T12","T18","T24","T30","T36"),
  
  col="lightblue",
  
  ylab="EDSS",
  xlab="Follow-up"
)

#### RELAPSE #####
table(merged$relapse_T6)
prop.table(table(merged$relapse_T6))*100

table(merged$relapse_T12)
prop.table(table(merged$relapse_T12))*100

table(merged$relapse_T18)
prop.table(table(merged$relapse_T18))*100

table(merged$relapse_T24)
prop.table(table(merged$relapse_T24))*100

table(merged$relapse_T30)
prop.table(table(merged$relapse_T30))*100

table(merged$relapse_T36)
prop.table(table(merged$relapse_T36))*100

# Relapses occurred in: 17% at T6 (esempio)


#Barplot percentuali: Percentage of patients experiencing relapse at each follow-up.
relapse_perc <- c(prop.table(table(merged$relapse_T6))[2]*100, prop.table(table(merged$relapse_T12))[2]*100, prop.table(table(merged$relapse_T18))[2]*100, prop.table(table(merged$relapse_T24))[2]*100,prop.table(table(merged$relapse_T30))[2]*100, prop.table(table(merged$relapse_T36))[2]*100)

time <- c("T6","T12","T18","T24", "T30", "T36")

barplot(
  relapse_perc,
  names.arg=time,
  col="tomato",
  ylab="% relapse"
)

###### ANALISI LESIONI #####
table(merged$lesioni_T6)
prop.table(table(merged$lesioni_T6))*100

table(merged$lesioni_T12)
prop.table(table(merged$lesioni_T12))*100

table(merged$lesioni_T18)
prop.table(table(merged$lesioni_T18))*100

table(merged$lesioni_T24)
prop.table(table(merged$lesioni_T24))*100

table(merged$lesioni_T30)
prop.table(table(merged$lesioni_T30))*100

table(merged$lesioni_T36)
prop.table(table(merged$lesioni_T36))*100

lesioni_perc <- c(
  
  mean(merged$lesioni_T6 == "1", na.rm=TRUE) * 100,
  
  mean(merged$lesioni_T12 == "1", na.rm=TRUE) * 100,
  
  mean(merged$lesioni_T18 == "1", na.rm=TRUE) * 100,
  
  mean(merged$lesioni_T24 == "1", na.rm=TRUE) * 100,
  
  mean(merged$lesioni_T30 == "1", na.rm=TRUE) * 100,
  
  mean(merged$lesioni_T36 == "1", na.rm=TRUE) * 100
)

plot(
  c(6,12,18,24,30,36),
  lesioni_perc,
  
  type="b",
  pch=16,
  lwd=2,
  col="darkgreen",
  
  xlab="Months",
  ylab="% patients with new lesions",
  
  ylim=c(0,100)
)


#### CORTISONE #####
table(merged$cortisone_T6)
prop.table(table(merged$cortisone_T6))*100

table(merged$cortisone_T12)
prop.table(table(merged$cortisone_T12))*100

table(merged$cortisone_T18)
prop.table(table(merged$cortisone_T18))*100

table(merged$cortisone_T24)
prop.table(table(merged$cortisone_T24))*100

table(merged$cortisone_T30)
prop.table(table(merged$cortisone_T30))*100

table(merged$cortisone_T36)
prop.table(table(merged$cortisone_T36))*100


data.frame(Time = time_points, Median = medians, IQR = iqr, N = n)


#### formato long ####
library(tidyverse)
library(tidyr)
long_data <- merged %>%
  
  pivot_longer(
    
    cols = matches(
      "^(EDSS|relapse|lesion|cortisone)_T"
    ),
    
    names_to = c(".value", "time"),
    
    names_pattern = "(.*)_T(.*)"
  )

head(long_data)
str(long_data)

#trasformo time in variabile num
long_data$time <- as.numeric(long_data$time)
str(long_data)
#per controllare le variabili binarie
table(long_data$relapse)

# grafico longitudinale con ggplot: mediana EDSS nel tempo
ggplot(long_data,
       aes(x=time, y=EDSS)) +
  
  geom_boxplot(fill="lightblue") +
  
  xlab("Months") +
  ylab("EDSS")

# relapse longitudinali
ggplot(long_data,
       aes(x=factor(time),
           fill=relapse)) +
  
  geom_bar(position="fill") +
  
  ylab("Proportion")

# percentuale relapse nel tempo
long_data %>%
  
  group_by(time) %>%
  
  summarise(
    relapse_rate =
      mean(relapse == "1",
           na.rm=TRUE)*100
  )

### FRIEDMAN TEST EDSS #####
#disabilità cambia nel tempo?
# seleziono solo EDSS
long_data %>%
  
  group_by(time) %>%
  
  summarise(
    relapse_rate =
      mean(relapse == "1",
           na.rm=TRUE)*100
  )

edss_data_short <- merged %>%
  
  select(
    ID,
    EDSS_T0,
    EDSS_T6,
    EDSS_T12,
    EDSS_T24
  )

edss_complete_short <- na.omit(edss_data_short)
dim(edss_complete_short)
friedman.test(
  as.matrix(
    edss_complete_short[, -1]
  )
)

###### relapse longitudinali ####
# frequenza di ricadute cambia nel tempo?
install.packages("DescTools")
library(DescTools)
relapse_data <- merged %>%
  
  select(
    relapse_T6,
    relapse_T12,
    relapse_T18,
    relapse_T24,
    relapse_T30,
    relapse_T36
  )
# trasformo in variabile num
relapse_data <- relapse_data %>%
  
  mutate(
    across(
      everything(),
      ~ as.numeric(as.character(.))
    )
  )

# cochranQtest
CochranQTest(
  as.matrix(relapse_data)
)


### lesioni longitudinali ####
# l'attività radiologica cambia nel tempo?
# stesso procedimento di relapse
library(dplyr)
library(tidyverse)
#uso solo timepoint con variabilità
lesion_data <- merged %>%
  
  select(
    lesioni_T6,
    lesioni_T12,
    lesioni_T18
  )

#trasformo in numeric
lesion_data <- lesion_data %>%
  
  mutate(
    across(
      everything(),
      ~ as.numeric(as.character(.))
    )
  )
#elimino NA
lesion_complete <- na.omit(lesion_data)
#controllo variabilità
summary(lesion_complete)
apply(lesion_complete, 2, table)

#cochrantest
library(DescTools)

CochranQTest(
  as.matrix(lesion_complete)
)

#####figure nel tempo####
#EDSS: boxplot longitudinale

ggplot(
  long_data,
  aes(
    x = factor(time),
    y = EDSS
  )
) +
  
  geom_boxplot(
    fill = "lightblue",
    color = "darkblue"
  ) +
  
  theme_minimal() +
  
  xlab("Follow-up (months)") +
  
  ylab("EDSS") +
  
  ggtitle("Longitudinal EDSS distribution")

#relapse nel tempo: barplot percentuali
relapse_summary <- long_data %>%
  
  group_by(time) %>%
  
  summarise(
    relapse_rate =
      mean(relapse == "1",
           na.rm = TRUE) * 100
  )

ggplot(
  relapse_summary,
  aes(
    x = factor(time),
    y = relapse_rate
  )
) +
  
  geom_col(
    fill = "tomato"
  ) +
  
  theme_minimal() +
  
  ylim(0,100) +
  
  xlab("Follow-up (months)") +
  
  ylab("% patients with relapse") +
  
  ggtitle("Relapse frequency over time")
#lesioni nel tempo: barplot o lineplot
lesioni_summary <- long_data %>%
  
  group_by(time) %>%
  
  summarise(
    lesioni_rate =
      mean(lesion_burden == "1",
           na.rm = TRUE) * 100
  )

ggplot(
  lesioni_summary,
  aes(
    x = time,
    y = lesioni_rate
  )
) +
  
  geom_line(
    color = "darkgreen",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "darkgreen",
    size = 3
  ) +
  
  theme_minimal() +
  
  ylim(0,100) +
  
  xlab("Follow-up (months)") +
  
  ylab("% patients with new lesions") +
  
  ggtitle("New lesions over time")

#### analisi comparative: creo gruppi####
#MEDAS alto vs basso
merged$MEDAS_group <- ifelse(
  merged$MEDAS >= median(merged$MEDAS, na.rm=TRUE),
  "High",
  "Low"
)

ifelse(
  merged$MEDAS >= 8,
  "High",
  "Low"
)
table(merged$MEDAS_group)

#MEDAS-relapseT24
table(
  merged$MEDAS_group,
  merged$relapse_T24
)

fisher.test(
  table(
    merged$MEDAS_group,
    merged$relapse_T24
  )
)

#physical activity
table(merged$physical_activity)
#physical activity-EDSST24
kruskal.test(
  EDSS_T24 ~ physical_activity,
  data = merged
)
#physical acitvity-relapseT24
table(
  merged$physical_activity,
  merged$relapse_T24
)

#smoking
table(merged$smoking_habit)

#smoking-EDSST24
wilcox.test(
  EDSS_T24 ~ smoking_habit,
  data = merged, exact= FALSE
)
#smoking-relapseT24
fisher.test(
  table(
    merged$smoking_habit,
    merged$relapse_T24
  )
)

#sex
table(merged$sex)
#sez-EDSST24
wilcox.test(
  EDSS_T24 ~ sex,
  data = merged, exact= FALSE
)


#naive vs non naive
table(merged$naive)
#naive-relapse T24
fisher.test(
  table(
    merged$naive,
    merged$relapse_T24
  )
)

#naive-EDSST24
wilcox.test(
  EDSS_T24 ~ naive,
  data = merged, exact= FALSE
)

#BMI
merged$BMI_group <- case_when(
  
  merged$bmi < 25 ~ "Normal",
  
  merged$bmi >= 25 ~ "Overweight"
)

table(merged$BMI_group)

#BMI-EDSST24
wilcox.test(
  EDSS_T24 ~ BMI_group,
  data = merged,
  exact = FALSE
)

aggregate(
  EDSS_T24 ~ BMI_group,
  data = merged,
  median,
  na.rm = TRUE
)

ggplot(
  merged,
  aes(
    x = BMI_group,
    y = EDSS_T24
  )
) +
  
  geom_boxplot(
    fill = "orange"
  ) +
  
  theme_minimal() +
  
  xlab("BMI group") +
  
  ylab("EDSS at T24")

#relapse tra gruppi BMI
table(
  merged$BMI_group,
  merged$relapse_T24
)

fisher.test(
  table(
    merged$BMI_group,
    merged$relapse_T24
  )
)

#lesioni tra gruppi BMI
fisher.test(
  table(
    merged$BMI_group,
    merged$lesioni_T24
  )
)


#full recovery tra gruppi BMI
fisher.test(
  table(
    merged$BMI_group,
    merged$full_recovery
  )
)

#MEDAS-full recovery
fisher.test(
  table(
    merged$MEDAS_group,
    merged$full_recovery
  )
)

#physical activity-full recovery
fisher.test(
  table(
    merged$physical_activity,
    merged$full_recovery
  )
)

#senza creare gruppi potrei:
cor.test(
  merged$MEDAS,
  merged$EDSS_T24,
  method = "spearman", exact= FALSE
)

####### fase INTERPRETATIVA ########
#considero la progressione clinica
merged$delta_EDSS <- merged$EDSS_T24 - merged$EDSS_T0 #peggioramento, stabile, miglioramento
#confronto con MEDAS
wilcox.test(
  delta_EDSS ~ MEDAS_group,
  data = merged,
  exact = FALSE
)

ggplot(
  merged,
  aes(
    x = MEDAS_group,
    y = delta_EDSS
  )
) +
  
  geom_boxplot(fill="skyblue") +
  
  theme_minimal()

#disease activity: stabile o attiva
merged$disease_activity <- ifelse(
  
  merged$relapse_T24 == "1" |
    merged$lesion_T24 == "1" |
    merged$delta_EDSS > 0,
  
  "Yes",
  "No"
)


fisher.test(
  table(
    merged$MEDAS_group,
    merged$disease_activity
  )
)


#####correlazioni continue#########
#MEDAS vs EDSS
cor.test(
  merged$MEDAS,
  merged$EDSS_T24,
  method = "spearman", exact= FALSE
)

#BMI vs EDSS
cor.test(
  merged$bmi,
  merged$EDSS_T24,
  method = "spearman", exact= FALSE
)

#età vs EDSS
cor.test(
  merged$age_at_T0,
  merged$EDSS_T24,
  method = "spearman", exact= FALSE
)

####delta_EDSS#####
merged$delta_EDSS <- merged$EDSS_T24 - merged$EDSS_T0
summary(merged$delta_EDSS)
#miglioramento, stabile, peggioramento
hist(
  merged$delta_EDSS,
  col = "lightblue",
  main = "Distribution of delta EDSS",
  xlab = "EDSS_T24 - EDSS_T0"
)

####outcome clinico composito#####
merged$disease_activity <- ifelse(
  
  merged$relapse_T24 == "1" |
    merged$lesioni_T24 == "1" |
    merged$delta_EDSS > 0,
  
  "Yes",
  "No"
)
table(merged$disease_activity) #attività di malattia o stabile


#####analisi mirate######
#MEDAS VS deltaEDSS
wilcox.test(
  delta_EDSS ~ MEDAS_group,
  data = merged,
  exact = FALSE
)

ggplot(
  merged,
  aes(
    x = MEDAS_group,
    y = delta_EDSS
  )
) +
  
  geom_boxplot(fill = "skyblue") +
  
  theme_minimal() +
  
  xlab("MEDAS group") +
  
  ylab("Delta EDSS")


#MEDAS VS disease_activity
fisher.test(
  table(
    merged$MEDAS_group,
    merged$disease_activity
  )
)


#BMI VS deltaEDSS
wilcox.test(
  delta_EDSS ~ BMI_group,
  data = merged,
  exact = FALSE
)

#BMI VS disease_activity
fisher.test(
  table(
    merged$BMI_group,
    merged$disease_activity
  )
)

#physical_activity VS deltaEDSS
kruskal.test(
  delta_EDSS ~ physical_activity,
  data = merged
)

ggplot(
  merged,
  aes(
    x = physical_activity,
    y = delta_EDSS
  )
) +
  
  geom_boxplot(fill = "lightgreen") +
  
  theme_minimal()

#physical_activity VS disease_activity
fisher.test(
  table(
    merged$physical_activity,
    merged$disease_activity
  )
)
#smoking VS deltaEDSS
wilcox.test(
  delta_EDSS ~ smoking_habit,
  data = merged,
  exact = FALSE
)

#smoking VS disease_activity
fisher.test(
  table(
    merged$smoking_habit,
    merged$disease_activity
  )
)

#MEDAS continuo VS deltaEDSS
cor.test(
  merged$MEDAS,
  merged$delta_EDSS,
  method = "spearman", exact= FALSE
)

#BMI continuo VS deltaEDSS
cor.test(
  merged$bmi,
  merged$delta_EDSS,
  method = "spearman", exact= FALSE
)

merged <- read.csv("C:/AURI/Thesis_bagetto/input/merged.csv")
view(merged)


merged <- read.csv("C:/AURI/Thesis_bagetto/input/merged.csv")
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")
View(merged)
