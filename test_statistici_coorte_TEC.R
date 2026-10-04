# test statistici coorte TEC
# importo dataset
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")

# FRIEDMAN test EDSS
# selezione delle variabili EDSS
edss_data_short <- merged %>%
  select(ID, EDSS_T0, EDSS_T6, EDSS_T12, EDSS_T24)

# mantengo solo soggetti completi
edss_complete_short <- na.omit(edss_data_short)

# controllo dimensione
dim(edss_complete_short)

# esecuzione test
friedman.test(
  as.matrix(edss_complete_short[, -1])
)
#0.005

# Cochrans Q Test ricadute e lesioni
#relapse
relapse_data <- merged %>%
  select(
    relapse_T6,
    relapse_T12,
    relapse_T18,
    relapse_T24,
    relapse_T30,
    relapse_T36
  ) %>%
  mutate(
    across(everything(), ~ as.numeric(as.character(.)))
  )

CochranQTest(as.matrix(relapse_data))
#0.8

#lesion
lesion_data <- merged %>%
  select(
    lesioni_T6,
    lesioni_T12,
    lesioni_T18
  ) %>%
  mutate(
    across(everything(), ~ as.numeric(as.character(.)))
  )

# rimozione dei missing
lesion_complete <- na.omit(lesion_data)

# esecuzione test
CochranQTest(as.matrix(lesion_complete))
#0.78

