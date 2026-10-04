# test statistici analisi comuni
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")
merged_ceci <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_ceci.csv")

# age at T0
shapiro.test(merged$age_at_T0)
#0.03 non-normal distribution

wilcox.test(age_at_T0 ~ GROUP,
            data = merged_ceci,
            exact = FALSE)
##0.39

# MEDAS continuo
shapiro.test(merged$MEDAS)
#0.26
wilcox.test(MEDAS ~ GROUP,
            data = merged_ceci,
            exact = FALSE)
#0.71

#MEDAS categorico
fisher.test(table(merged_ceci$GROUP, merged_ceci$medas_cat))
#0.564

#BMI continuo
shapiro.test(merged$bmi)
#0.21
wilcox.test(bmi ~ GROUP,
            data = merged_ceci,
          exact = FALSE)
#0.53

#BMI categorica
fisher.test(table(merged_ceci$GROUP, merged_ceci$bmi_cat))
#1 

#naive
fisher.test(table(merged_ceci$GROUP, merged_ceci$naive))
#1

# physical activity
fisher.test(table(merged_ceci$GROUP, merged_ceci$physical_activity))
#0.43

# smoking
fisher.test(table(merged_ceci$GROUP, merged_ceci$smoking_habit))
#0.09

#sex
fisher.test(table(merged_ceci$GROUP, merged_ceci$sex))
#1

#lesion burden
fisher.test(table(merged_ceci$GROUP, merged_ceci$lesion_burden))
#0.02

#gadolinium contrast
fisher.test(table(merged_ceci$GROUP, merged_ceci$gadolinium_contrast))
#0.22

#spinal lesion
fisher.test(table(merged_ceci$GROUP, merged_ceci$bone_marrow_lesions))
#0.39

#subtentorial lesion
fisher.test(table(merged_ceci$GROUP, merged_ceci$subtentorial_lesions))
#0.77

#glucocorticoid at T0
fisher.test(table(merged_ceci$GROUP, merged_ceci$gc_treatment))
#0.77