LEAPS <- function(bact_path, archea_path, euk_path, predictors, row_sel, els_row, file_name, output_folder, pipeline_type) { # output_folder, filename
       B <- readRDS(bact_path) 
       E <- readRDS(euk_path) 
       A <- readRDS(archea_path) 
       A@phy_tree <- NULL
       B@phy_tree <- NULL
       E@phy_tree <- NULL

       baselines_dec <- phyloseq::merge_phyloseq(A, B, E)

       baselines_dec_metadata <- data.frame(sample_data(baselines_dec))
       # library(dplyr)

       # keep samples whose 'row_sel' column value is present in els_row
       if (!row_sel %in% colnames(baselines_dec_metadata)) {
              stop(paste("Column", row_sel, "not found in sample data"))
       }
       keep2 <- rownames(baselines_dec_metadata)[
              as.character(baselines_dec_metadata[[row_sel]]) %in% as.character(els_row)
       ]

       baselines_dec <- prune_samples(keep2, baselines_dec)

       baselines_dec_otu <- baselines_dec@otu_table

       baselines_dec_metadata <- data.frame(sample_data(baselines_dec))

       # Calculate alpha diversity
       ###########################
       if (pipeline_type == "kraken") {
              baselines_dec_richness <- estimate_richness(baselines_dec_otu, split = TRUE, measures = c("Observed", "Shannon", "Simpson", "Chao1"))
              baselines_dec_richness <- data.frame(id = row.names(baselines_dec_richness), baselines_dec_richness)
              # unisco metadati con le diversity calcolate in un unico dataframe
              baselines_dec_metadata <- merge(baselines_dec_richness, baselines_dec_metadata, all.y = TRUE, by = "id")
       } else if (pipeline_type == "metaphlan") {
              baselines_dec_richness <- microbiome::alpha(baselines_dec, index = c("Observed", "diversity_shannon", "diversity_gini_simpson", "Chao1"))
              baselines_dec_richness <- data.frame(id = row.names(baselines_dec_richness), baselines_dec_richness)
              baselines_dec_metadata <- merge(baselines_dec_richness, baselines_dec_metadata, all.y = TRUE, by = "id")
              # rename metaphlan alpha columns to match kraken names (if present) and set outcomes
              rename_map <- c(
                     observed = "Observed",
                     diversity_shannon = "Shannon",
                     diversity_gini_simpson = "Simpson"
              )
              for (orig in names(rename_map)) {
                     new <- rename_map[[orig]]
                     if (exists("baselines_dec_richness") && orig %in% colnames(baselines_dec_richness)) {
                            colnames(baselines_dec_richness)[colnames(baselines_dec_richness) == orig] <- new
                     }
                     if (orig %in% colnames(baselines_dec_metadata)) {
                            colnames(baselines_dec_metadata)[colnames(baselines_dec_metadata) == orig] <- new
                     }
              }
       }
       outcome <- c("Observed", "Shannon", "Simpson")
       # Perform model regression analysis  prendere la tabella iniziale con i metadati
       ####################################
       # baselines_dec_metadata <- baselines_dec_metadata %>%
       #        mutate(
       #               across(.cols = c(id), .fns = as.character),
       #               across(
       #                      .cols = c(
       #                             sex, category, clinical_presentation, gc_treatment,
       #                             subtentorial_lesions, spinal_cord_lesion, gadolinium_contrast,
       #                             sequencing_batch, WORSENING, EDSS_DIAGNOSI, EDSS_PROGRESSIONE, Event,
       #                             naive, previous_therapy, antibiotic_use, sample_type, lesion_burden
       #                      ),
       #                      .fns = as.factor
       #               ),
       #               across(.cols = c(age, bmi, EventTime), .fns = as.numeric),
       #               across(.cols = c(sample_collection_date), .fns = as.Date)
       #        )

       # Define predictors and outcomes
       ################################


       # Run function
       ###############
       source("R/FunctionLRM.R")
       res <- LRM_microbiome(metadata = baselines_dec_metadata %>% as.data.frame(), predictors = predictors, outcomes = outcome)

       # Save results
       ##############
       filename <- paste0(file_name, "_", Sys.Date(), ".txt")
       filename <- paste0(output_folder, filename)

       sink(file = filename, type = c("output", "message"), split = TRUE)
       print(paste("Observed", " ", filename))
       print(res$Observed$Intercept)
       print(paste("Shannon", " ", filename))
       print(res$Shannon$Intercept)
       print(paste("Simpson", " ", filename))
       print(res$Simpson$Intercept)

       sink()
}
# bact_path <- "/home/franky/Desktop/meta_app/META_SUPERVISED_DEC/Bacteria_Supervised_meta_no_strain.rds"
# archea_path <- "/home/franky/Desktop/meta_app/META_SUPERVISED_DEC/Archaea_Supervised_meta_no_strain.rds"
# euk_path <- "/home/franky/Desktop/meta_app/META_SUPERVISED_DEC/Eukaryota_Supervised_meta_no_strain.rds"
# output_folder <- "/home/franky/Desktop/meta_app/LEAPS"
# row_sel <- "category"
# els_row <- c("MS")
# predictors <- c("age", "bmi", "lesion_burden", "gc_treatment", "gadolinium_contrast")
# # #LEAPS(
# # #       bact_path = Bacteria,
# # #       archea_path = Archaea,
# # #       euk_path = Eukaryota,
# # #       predictors = els_row,
# # #       row_sel = row_sel,
# # #       els_row = els_row,
# file_name <- "LEAPS_results"
# # #       ,output_folder = output_folder
# pipeline_type <- "metaphlan"
# # #)
