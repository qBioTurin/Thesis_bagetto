merged <- read.csv("C:/AURI/Thesis_bagetto/input/merged_data.csv")
str(merged)

merged$data_di_nascita <- as.Date(merged$data_di_nascita, origin= "1899-12-30")
merged$DATA_ESORDIO        <- as.Date(merged$DATA_ESORDIO, origin = "1899-12-30")
merged$data_diagnosi       <- as.Date(merged$data_diagnosi, origin = "1899-12-30")
merged$data_prelievo_T0    <- as.Date(merged$data_prelievo_T0, origin = "1899-12-30")
merged$data_cambio_terapia <- as.Date(merged$data_cambio_terapia, origin = "1899-12-30")
merged$data_referto_T6  <- as.Date(merged$data_referto_T6,  origin = "1899-12-30")
merged$data_referto_T12 <- as.Date(merged$data_referto_T12, origin = "1899-12-30")
merged$data_referto_T18 <- as.Date(merged$data_referto_T18, origin = "1899-12-30")
merged$data_referto_T24 <- as.Date(merged$data_referto_T24, origin = "1899-12-30")
merged$data_referto_T30 <- as.Date(merged$data_referto_T30, origin = "1899-12-30")
merged$data_referto_T36 <- as.Date(merged$data_referto_T36, origin = "1899-12-30")
merged$sample_collection_date <- as.Date(merged$sample_collection_date, origin = "1899-12-30")
merged$glc_date_before_t0 <- as.Date(merged$glc_date_before_t0, origin="1899-12-30")

#da int a categoriche (relapse, lesion, cambio terapia)
merged$lesioni_T6 <- factor(merged$lesioni_T6)
merged$lesioni_T12 <- factor(merged$lesioni_T12)
merged$lesioni_T18 <- factor(merged$lesioni_T18)
merged$lesioni_T24 <- factor(merged$lesioni_T24)
merged$lesioni_T30 <- factor(merged$lesioni_T30)
merged$lesioni_T36 <- factor(merged$lesioni_T36)

merged$relapse_T6 <- factor(merged$relapse_T6)
merged$relapse_T12 <- factor(merged$relapse_T12)
merged$relapse_T18<- factor(merged$relapse_T18)
merged$relapse_T24 <- factor(merged$relapse_T24)
merged$relapse_T30 <- factor(merged$relapse_T30)
merged$relapse_T36 <- factor(merged$relapse_T36)

merged$cambio_terapia<- factor(merged$cambio_terapia)


merged$bone_marrow_lesions <- as.factor(merged$bone_marrow_lesions)
merged$subtentorial_lesions <- as.factor(merged$subtentorial_lesions)
merged$gadolinium_contrast <- as.factor(merged$gadolinium_contrast)
merged$full_recovery <- as.factor(merged$full_recovery)

#da chr a categoriche
merged$naive <- factor(merged$naive,levels = c("no", "yes"))
merged$sex <- factor(merged$sex,levels = c("M", "F"))
merged$smoking_habit <- factor(merged$smoking_habit,levels = c("yes", "no"))
merged$antibiotic_use <- factor(merged$antibiotic_use,levels = c("yes", "no"))
merged$spike_in <- factor(merged$spike_in,levels = c("no", "yes"))
merged$glucocorticoid_treatment <- factor(merged$glucocorticoid_treatment,levels = c("positive", "negative"))
merged$gc_treatment <- factor(merged$gc_treatment,levels = c("positive", "negative"))
merged$prognosis_at_onset <- factor(merged$prognosis_at_onset,levels = c("positive", "negative"))
merged$simplified_batch<- factor (merged$simplified_batch, levels= c("1-3", "4-6"))

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

