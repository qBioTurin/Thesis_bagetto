AncomBC2_run <- function(rds, metadata_df, analysis, subcategory, sub_name, output_folder, discriminant, significance_metric="q_value") {
    rds <- readRDS(rds)
    taxa <- as.data.frame(rds@tax_table)
    taxa$Genus_Species <- paste(taxa$Genus, taxa$Species, sep = " ")
    taxa$otu <- rownames(taxa)
    taxa_tab <- taxa %>% select(otu, Genus_Species)
    if (significance_metric == "p_value") {
        significance_metric <- "p"
    } else {
        significance_metric <- "q"
    }

    # Filtraggio campioni
    otu <- as.data.frame(rds@otu_table)
    # metadata_df <- read.csv(metadata_df,
    #     header = TRUE,
    #     sep = ",",
    #     na = c("", " ", "NA"),
    #     check.names = TRUE
    # )
    ids_to_retain <- metadata_df$id[metadata_df[[subcategory]] %in% sub_name & !is.na(metadata_df$id)]
    otu <- otu[, colnames(otu) %in% ids_to_retain]
    metadata_df <- metadata_df[metadata_df$id %in% ids_to_retain, ]

    # Variabile discriminante
    # metadata_df[[analysis]] <- as.factor(metadata_df[[analysis]])
    # numeric_vars <- c("bmi", "age")




    # for (v in numeric_vars) {
    #     metadata_df[[v]] <- as.numeric(as.character(metadata_df[[v]]))
    # }
    metadata_df <- as.data.frame(metadata_df)
    rownames(metadata_df) <- metadata_df$id
    metadata_fin_df <- metadata_df %>%
        drop_na(all_of(c(analysis, discriminant))) %>%
        select(all_of(c(analysis, discriminant)))

    # if ("gc_treatment" %in% discriminant) {
    #     metadata_fin_df$gc_treatment <- as.factor(metadata_fin_df$gc_treatment)
    # }
    # if("category" %in% discriminant) {
    #     metadata_fin_df$category <- as.factor(metadata_fin_df$category)
    # }
    # levels(metadata_fin_df$category)
    # metadata_fin_df$sex <- as.factor(metadata_fin_df$sex)
    formula <- analysis
    print("Discriminant levels:")
    print(paste(
        analysis,
        paste(unique(metadata_fin_df[[analysis]]), collapse = ", ")
    ))
    for (el in discriminant) {
        formula <- paste0(formula, " + ", el)
        print(paste(
            el,
            paste(unique(metadata_fin_df[[el]]), collapse = ", ")
        ))
    }
    print(paste("Formula ANCOMBC2:", formula))
    # Creazione output folder
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }
    if (length(unique(metadata_fin_df[[analysis]])) < 2) {
        stop(paste("ANCOMBC2: variabile", analysis, "ha meno di 2 livelli dopo il subset"))
    }

    # ANCOM-BC2
    res <- tryCatch(
        {
            ANCOMBC::ancombc2(
                data = otu,
                meta_data = metadata_df,
                fix_formula = formula,
                tax_level = NULL,
                p_adj_method = "BH",
                rand_formula = NULL,
                prv_cut = 0.0,
                lib_cut = 0,
                struc_zero = FALSE
            )
        },
        error = function(e) {
            warning("ANCOMBC2 failed: ", e$message)
            return(NULL)
        }
    )

    # Se ANCOMBC fallisce, ritorna CSV vuoto
    out_file <- file.path(output_folder, paste0(analysis, "_ANCOMBC2_significant_results.csv"))
    if (is.null(res)) {
        warning("ANCOMBC2 run skipped, returning empty CSV")
        write.csv(data.frame(), out_file, row.names = FALSE)
        return(out_file)
    }

    # Se tutto ok, continua normalmente
    results <- res$res
    discriminant1 <- paste0(significance_metric, "_", analysis, levels(metadata_fin_df[[analysis]])[2])
    results <- results[, c("taxon", discriminant1)]
    results <- as.data.frame(results)
    results <- results %>% dplyr::rename(otu = taxon)

    # Merge con tassonomia
    out2 <- merge(results, taxa_tab, by = "otu")
    out2 <- out2 %>% select(Genus_Species, all_of(discriminant1))
    print(min(out2[[discriminant1]], na.rm = TRUE))
    write.csv(out2, file.path(output_folder, paste0(analysis, "_ANCOMBC2_results.csv")), row.names = FALSE)

    # Salvataggio risultati significativi (p-value < 0.05)
    sig_results <- out2 %>% filter(!!sym(discriminant1) < 0.05)
    write.csv(sig_results, out_file, row.names = FALSE)

    return(out_file)
}
