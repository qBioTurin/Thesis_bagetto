#TABELLA SUMMARY 
merged <- read.csv2("C:/AURI/Thesis_bagetto/merged.csv")

N <- nrow(merged)

tab <- merged %>%
  select(
    GROUP,
    age,
    sex,
    bmi,
    starts_with("EDSS_"),
    starts_with("relapse_"),
    naive,
    disease_activity,
    lesion_burden,
    smoking_habit,
    physical_activity,
    therapy
  ) %>%
  tbl_summary( by = GROUP, 
               statistic = list(
                 all_continuous() ~ "{mean} ± {sd}",
                 starts_with("EDSS_") ~ "{median} ({p25}, {p75})",
                 all_categorical() ~ "{n} ({p}%)"
               ),
               label = list(
                 # EDSS
                 EDSS_T0  ~ "T0",
                 EDSS_T6  ~ "T6",
                 EDSS_T12 ~ "T12",
                 EDSS_T18 ~ "T18",
                 EDSS_T24 ~ "T24",
                 EDSS_T30 ~ "T30",
                 EDSS_T36 ~ "T36",
                 
                 # RELAPSE
                 relapse_T6  ~ "T6",
                 relapse_T12 ~ "T12",
                 relapse_T18 ~ "T18",
                 relapse_T24 ~ "T24",
                 relapse_T30 ~ "T30",
                 relapse_T36 ~ "T36", 
                 smoking_habit ~ "Smokers"
               ),
               missing_text = "Missing values"
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  # ✅ intestazione EDSS
  modify_table_body(
    ~ .x %>%
      dplyr::add_row(
        label = "*EDSS*",
        .before = which(.x$variable == "EDSS_T0")
      )
  ) %>%
  
  # ✅ intestazione RELAPSE
  modify_table_body(
    ~ .x %>%
      dplyr::add_row(
        label = "*Relapse*",
        .before = which(.x$variable == "relapse_T6")
      )
  )%>%
  
  modify_header(label = "*Variables*")%>%
  
  
  add_p(
    test = list(
      age ~ "wilcox.test",
      bmi ~ "wilcox.test",
      starts_with("EDSS_") ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    ), 
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  )

tab

gt_tabella <- as_gt(tab)

gtsave(
  data = gt_tabella,
  filename = "C:/AURI/Thesis_bagetto/grafici_coorte_totale/summary_CECI.png"
)
