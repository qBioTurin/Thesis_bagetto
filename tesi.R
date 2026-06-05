#importo la tabella del paper e tengo solo i pazienti TEC ]
metadata_paper <- metadata_paper[grepl("MAV|TEC", metadata_paper$id), ]
View(metadata_paper)
metadata_paper<- metadata_paper[grepl("TEC", metadata_paper$id),]
View(metadata_paper)

#importo la tabella metadata_pazienti
install.packages("readxl") #scarico solo la prima volta
#devo ricordarmi di caricare il pacchetto ogni volta che apro Rstudio
library(readxl)
metadata_pazienti <- read_excel("input/metadata_pazienti.xlsx")
metadata_paper <- read_excel("input/metadata_paper.xlsx")

warnings()
View(metadata_pazienti)

metadata_pazienti <- metadata_pazienti[grepl("TEC", metadata_pazienti$ID), ] #per tenere solo i pazienti TEC
View(metadata_pazienti)
metadata_pazienti <- metadata_pazienti[, -2] #elimino la colonna 2 perchè inutile

#imposto le colonne DATA_ come variabile date
metadata_pazienti$DATA_ESORDIO <- as.Date(as.numeric(metadata_pazienti$DATA_ESORDIO), origin = "1899-12-30")
metadata_pazienti$data_prelievo_T0 <- as.Date(as.numeric(metadata_pazienti$data_prelievo_T0), origin = "1899-12-30")
metadata_pazienti$data_cambio_terapia <- as.Date(as.numeric(metadata_pazienti$data_cambio_terapia), origin = "1899-12-30")
metadata_pazienti$data_referto_T24 <- as.Date(as.numeric(metadata_pazienti$data_referto_T24), origin = "1899-12-30")
metadata_pazienti$data_referto_T12 <- as.Date(as.numeric(metadata_pazienti$data_referto_T12), origin = "1899-12-30")

str(metadata_pazienti)

metadata_pazienti$data_diagnosi <- as.Date(metadata_pazienti$data_diagnosi, origin = "1899-12-30")

#imposto gli EDSS e il cortisone come variabile numerica
metadata_pazienti$EDSS_T12 <- as.numeric(metadata_pazienti$EDSS_T12)
metadata_pazienti$EDSS_T24 <- as.numeric(metadata_pazienti$EDSS_T24)
metadata_pazienti$cortisone_T6 <- as.numeric(metadata_pazienti$cortisone_T6)


#imposto lesioni e relapse come variabili categoriche in cui 0=no e 1=si
metadata_pazienti$lesioni_T6 <- factor(metadata_pazienti$lesioni_T6)
metadata_pazienti$lesioni_T12 <- factor(metadata_pazienti$lesioni_T12)
metadata_pazienti$lesioni_T18 <- factor(metadata_pazienti$lesioni_T18)
metadata_pazienti$lesioni_T24 <- factor(metadata_pazienti$lesioni_T24)
metadata_pazienti$lesioni_T30 <- factor(metadata_pazienti$lesioni_T30)
metadata_pazienti$lesioni_T36 <- factor(metadata_pazienti$lesioni_T36)

metadata_pazienti$relapse_T6 <- factor(metadata_pazienti$relapse_T6)
metadata_pazienti$relapse_T12 <- factor(metadata_pazienti$relapse_T12)
metadata_pazienti$relapse_T18 <- factor(metadata_pazienti$relapse_T18)
metadata_pazienti$relapse_T24 <- factor(metadata_pazienti$relapse_T24)
metadata_pazienti$relapse_T30 <- factor(metadata_pazienti$relapse_T30)

metadata_pazienti$relapse_T36 <- factor(metadata_pazienti$relapse_T36)


str(metadata_pazienti)


# Rinomino la colonna "id" in "ID" in metadata_paper 
colnames(metadata_paper)[colnames(metadata_paper) == "id"] <- "ID"
View(metadata_paper)

#elimino le colonne in metadata_paper che non mi interessano
metadata_paper <- metadata_paper[, -c(1,3,13,23,24,25,26,27,37,38,39,40,41,42,47)]


#inserisco una data che non è stata importata
metadata_pazienti$DATA_ESORDIO[metadata_pazienti$ID == "TEC17"] <- as.Date("2017-01-01")

#rinomino gli ID in metadata_paper per fare in modo che siano uguali a quelli in metadata_pazienti
metadata_paper$ID <- sub("T0$", "", metadata_paper$ID)

#faccio il merge per ID
merged_data <- merge(metadata_pazienti, metadata_paper, by = "ID", all.x = TRUE)
View(merged_data)

#completo dati mancanti in therapy
merged_data$therapy <- "dimethyl_fumarate"

#installo dplyr perchè mi sarebbe molto utile
install.packages("dplyr")
library(dplyr) #non lo apre

install.packages("rlang", type = "binary")
install.packages("dplyr", type = "binary")
library(dplyr)

#assegno il sesso ai soggetti che risultano NA
merged_data$sex[merged_data$ID == "TEC13"] <- "F"
merged_data$sex[merged_data$ID == "TEC18"] <- "M"
merged_data$sex[merged_data$ID == "TEC23"] <- "M"
merged_data$sex[merged_data$ID == "TEC25"] <- "F"
merged_data$sex[merged_data$ID == "TEC29"] <- "M"

#inserisco le date tenendo conto della data di nascita e le inserisco in modo che possano aggiornarsi nel tempo
install.packages("lubridate")
library(lubridate)
merged_data$age <- floor(interval(merged_data$data_di_nascita, Sys.Date()) / years(1))

#inserisco alcune date mancanti
merged_data$data_prelievo_T0[merged_data$ID == "TEC19"] <- as.Date("2022-10-18")
merged_data$data_prelievo_T0[merged_data$ID == "TEC23"] <- as.Date("2023-06-26")

#modifico la colonna simplified_batch con valori corretti
merged_data$simplified_batch[merged_data$simplified_batch == "3-Jan"] <- "1-3"
merged_data$simplified_batch <- factor(merged_data$simplified_batch)

install.packages("dplyr")
library(dplyr)

#aggiungo colonna age_at_T0 
merged_data$age_at_T0 <- as.integer(format(as.Date(merged_data$data_prelievo_T0), "%Y")) -
  as.integer(format(as.Date(merged_data$data_di_nascita), "%Y")) -
  (format(as.Date(merged_data$data_prelievo_T0), "%m-%d") <
     format(as.Date(merged_data$data_di_nascita), "%m-%d"))

#sposto la colonna creata vicino alla colonna age
library(dplyr)

merged_data <- merged_data %>%
  relocate(age_at_T0, .after = age)

#aggiungo colonna age_at_diagnosis
merged_data$age_at_diagnosis <- as.integer(format(as.Date(merged_data$data_diagnosi), "%Y")) -
  as.integer(format(as.Date(merged_data$data_di_nascita), "%Y")) -
  (format(as.Date(merged_data$data_diagnosi), "%m-%d") <
     format(as.Date(merged_data$data_di_nascita), "%m-%d"))

#sposto la colonna vicino alla colonna della data di diagnosi
library(dplyr)

#aggiungo merged_data a github
merged_data <- merged_data %>%
  relocate(age_at_diagnosis, .after = data_diagnosi)

write.csv(merged_data, "C:/AURI/Thesis_bagetto/input/merged_data.csv", row.names = FALSE)

#controllo variabili
str(merged_data)

merged_data$age_at_diagnosis <- as.numeric(merged_data$age_at_diagnosis)
class(merged_data$age_at_diagnosis)

merged_data$age_at_T0 <- as.numeric(merged_data$age_at_T0)
class(merged_data$age_at_T0)

merged_data$cambio_terapia <- factor(merged_data$cambio_terapia)
class(merged_data$cambio_terapia)

merged_data$naive <- factor(merged_data$naive)
class(merged_data$naive)

merged_data$sex <- factor(merged_data$sex, levels = c("F", "M"))
class(merged_data$sex)

merged_data$smoking_habit <- factor(merged_data$sex)
class(merged_data$smoking_habit)

merged_data$antibiotic_use <- factor(merged_data$antibiotic_use)
class(merged_data$antibiotic_use)

merged_data$spike_in <- factor(merged_data$spike_in)
class(merged_data$spike_in)

merged_data$glucocorticoid_treatment <- factor(merged_data$glucocorticoid_treatment)
class(merged_data$glucocorticoid_treatment)

merged_data$gc_treatment <- factor(merged_data$gc_treatment)
class(merged_data$gc_treatment)

merged_data$prognosis_at_onset <- factor(merged_data$prognosis_at_onset)
class(merged_data$prognosis_at_onset)

merged_data$bone_marrow_lesions <- factor(merged_data$bone_marrow_lesions)
class(merged_data$bone_marrow_lesions)

merged_data$subtentorial_lesions <- factor(merged_data$subtentorial_lesions)
class(merged_data$subtentorial_lesions)

merged_data$gadolinium_contrast <- factor(merged_data$gadolinium_contrast)
class(merged_data$gadolinium_contrast)

merged_data$full_recovery <- factor(merged_data$full_recovery)
class(merged_data$full_recovery)

merged_data$sample_collection_date <- as.Date(merged_data$sample_collection_date, format = "%m/%d/%y")
class(merged_data$sample_collection_date)

merged_data$glc_date_before_t0 <- as.Date(merged_data$glc_date_before_t0, format = "%m/%d/%y")
class(merged_data$glc_date_before_t0)

str(merged_data)

names(merged_data)[names(merged_data) == "disease_actvity"] <- "disease_activity"
