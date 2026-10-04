#ANALISI COMUNI
# tabella pazienti
library(dplyr)
library(gtsummary)
library(gt)
# =====================================================
# Import metadata (solo pazienti)
# =====================================================

merged <- read.csv2(
  "C:/AURI/Thesis_bagetto/input/merged_ceci.csv",
  stringsAsFactors = FALSE
)

# =====================================================
# Correzioni manuali
# =====================================================

merged[merged$ID == "MAV32",
       c("bmi", "smoking_habit", "physical_activity")] <-
  list(33.20, "yes", "sedentary")

merged[merged$ID == "TEC25",
       c("bmi", "smoking_habit", "physical_activity")] <-
  list(21.88, "no", "sedentary")

merged[merged$ID == "MAV31",
       c("bmi", "smoking_habit", "physical_activity")] <-
  list(27.43, "no", "sedentary")

merged[merged$ID == "TEC26",
       c("bmi", "smoking_habit", "physical_activity")] <-
  list(22.46, "yes", "active")

merged[merged$ID == "TEC27",
       c("bmi", "smoking_habit", "physical_activity")] <-
  list(25.17, "no", "partially_active")

merged[merged$ID == "TEC32",
       c("physical_activity")] <-
  list(NA)

merged[merged$ID == "MAV15",
       c("bmi")] <-
  list(32)

# =====================================================
# Funzioni per categorie
# =====================================================

bmi_fun <- function(x) {
  case_when(
    x < 25 ~ "Normal weight (<25)",
    x >= 25 ~ "Overweight (≥25)",
    TRUE ~ NA_character_
  )
}

medas_fun <- function(x) {
  case_when(
    x >= 50 ~ "High adherence (≥50)",
    x < 50 ~ "Low adherence (<50)",
    TRUE ~ NA_character_
  )
}

# =====================================================
# Conversione numerica
# =====================================================

merged <- merged %>%
  mutate(
    bmi = as.numeric(gsub(",", ".", bmi)),
    MEDAS = as.numeric(gsub(",", ".", MEDAS)),
    age_at_T0 = as.numeric(gsub(",", ".", age_at_T0))
  )

# =====================================================
# Creazione variabili
# =====================================================

merged <- merged %>%
  mutate(
    gadolinium_contrast = case_when(
      gadolinium_contrast %in% c("Yes", "1") ~ 1,
      gadolinium_contrast %in% c("No", "0") ~ 0,
      TRUE ~ NA_real_
    ),
    bmi_cat = bmi_fun(bmi),
    medas_cat = medas_fun(MEDAS)
  )

# Controllo rapido
table(merged$bmi_cat, useNA = "ifany")
table(merged$GROUP, merged$bmi_cat)

# =====================================================
# TABELLA PAZIENTI
# =====================================================

tab2 <- merged %>%
  select(
    GROUP,
    age_at_T0,
    sex,
    bmi_cat,
    naive,
    gc_treatment,
    smoking_habit,
    physical_activity,
    medas_cat,
    lesion_burden,
    gadolinium_contrast,
    bone_marrow_lesions,
    subtentorial_lesions
  ) %>%
  tbl_summary(
    by = GROUP,
    missing = "no",
    
    type = list(
      gadolinium_contrast ~ "dichotomous",
      gc_treatment ~ "dichotomous"
    ),
    
    value = list(
      gadolinium_contrast ~ 1,
      gc_treatment ~ "positive"
    ),
    
    statistic = list(
      age_at_T0 ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    digits = list(
      age_at_T0 ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    ),
    
    label = list(
      age_at_T0 ~ "Age at baseline (years)",
      sex ~ "Sex",
      bmi_cat ~ "BMI category",
      naive ~ "Treatment-naïve",
      gc_treatment ~ "Glucocorticoid at baseline",
      smoking_habit ~ "Smoking habit",
      physical_activity ~ "Physical activity",
      medas_cat ~ "MEDAS adherence",
      lesion_burden ~ "Lesion burden",
      gadolinium_contrast ~ "Gadolinium contrast",
      bone_marrow_lesions ~ "Spinal cord lesions",
      subtentorial_lesions ~ "Subtentorial lesions"
    )
  ) %>%
  add_overall() %>%
  add_n() %>%
  add_p(
    test = list(
      age_at_T0 ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    ),
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels()

# =====================================================
# Esporta tabella
# =====================================================

tab2_gt <- as_gt(tab2)

gtsave(
  data = tab2_gt,
  filename = "C:/AURI/Thesis_Bagetto/grafici_coorte_totale/tabella_pazienti_FINALE.png",
  zoom = 2
)

# =====================================================
# Controlli finali
# =====================================================

str(merged$MEDAS)
summary(merged$MEDAS)

table(merged$medas_cat, useNA = "ifany")
table(merged$GROUP, merged$medas_cat)