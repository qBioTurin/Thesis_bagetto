DASMaaslin3 <- function(rds_path, metadata, main_el, discr_els, categorySel, elements,output_folder,significance_metric = "q_value",
                        model_filter = "both",
                        fx_effects = NULL, random_effects = NULL, group_effects = NULL,
                        ordered_effects = NULL, strata_effects = NULL ) {
    if(significance_metric == "p_value") {
        significance_metric <- "pval_individual"
    } else {
        significance_metric <- "qval_individual"
    }
    # Create data for maaslin3
    rds <- readRDS(rds_path)
    otu <- rds@otu_table
    taxa <- rds@tax_table
    metadata <- as.data.frame(metadata)

    metadata_filt <- metadata %>%
        dplyr::filter(.data[[categorySel]] %in% elements)
    if (nrow(metadata_filt) == 0) {
        stop("Maaslin3: no metadata rows remain after subsetting. Check sub_category and sub_names.")
    }
    if (length(discr_els) == 0) {
        formulaInUse <- main_el
    } else {
        formulaInUse <- paste(main_el, paste(discr_els, collapse = "+"), sep = "+")
    }
    metadata_df <- metadata_filt
    ids_to_retain <- metadata_df$id
    otu <- otu[, colnames(otu) %in% ids_to_retain]
    if (ncol(otu) == 0) {
        stop("Maaslin3: no OTU samples match metadata ids after filtering.")
    }
    otu <- otu[rowSums(otu, na.rm = TRUE) > 0, , drop = FALSE]
    if (nrow(otu) == 0) {
        stop("Maaslin3: no taxa remain after removing zero-sum features.")
    }
    metadata_df <- metadata_df[metadata_df$id %in% colnames(otu), , drop = FALSE]
    if (length(unique(metadata_df[[main_el]])) < 2) {
        stop(paste("Maaslin3:", main_el, "has fewer than 2 levels after filtering."))
    }
    taxa <- as.data.frame(taxa)
    taxa$Genus_Species <- paste(taxa$Genus, taxa$Species, sep = "_")
    Tnew <- taxa %>% dplyr::select(Genus_Species)
    Tnew$taxid <- rownames(Tnew)

    otu <- as.data.frame(otu)
    otu$taxid <- rownames(otu)
    # Merge the two data frames by taxid
    merged_df <- merge(otu, Tnew, by = "taxid")
    rownames(merged_df) <- merged_df$Genus_Species
    merged_df <- merged_df %>% dplyr::select(-c(taxid, Genus_Species))
    merged_df <- t(merged_df)
    ds_names <- if (length(discr_els) == 0) "none" else paste(discr_els, collapse = "_")
    output_folder <- file.path(
        output_folder,
        paste0(main_el, "_plus_", ds_names, "_", Sys.Date())
    )
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }
    if (!is.null(formulaInUse)) {
        formulaInUse <- paste("~", formulaInUse, sep = "")
    }

    metadata_df[[categorySel]] <- factor(metadata_df[[categorySel]],
        levels = elements
    )

    unique_els <- unique(metadata_df[[main_el]])
    metadata_df[[main_el]] <- factor(metadata_df[[main_el]], levels = unique_els)
    for (el in discr_els) {
        unique_els <- unique(metadata_df[[el]])
        metadata_df[[el]] <- factor(metadata_df[[el]], levels = unique_els)
    }
    rownames(metadata_df) <- metadata_df$id
    fit_out <- maaslin3::maaslin3(
        input_data = merged_df,
        input_metadata = metadata_df,
        output = output_folder,
        formula = formulaInUse,
        fixed_effects = fx_effects,
        random_effects = random_effects,
        group_effects = group_effects,
        ordered_effects = ordered_effects,
        strata_effects = strata_effects,
        normalization = "TSS",
        transform = "LOG",
        augment = TRUE,
        standardize = FALSE,
        max_significance = 0.1,
        median_comparison_abundance = TRUE,
        median_comparison_prevalence = FALSE,
        save_plots_rds = TRUE,
        max_pngs = 250,
        cores = 1
    )

    results_path <- file.path(output_folder, "all_results.tsv")
    if (!file.exists(results_path)) {
        stop(paste("Maaslin3: expected results file not found:", results_path))
    }
    all_results <- readr::read_tsv(results_path, show_col_types = FALSE)

    # Always export the 6 DA views requested, independent of UI/input filters.
    first_non_na <- function(x) {
        idx <- which(!is.na(x))
        if (length(idx) == 0) {
            return(NA_real_)
        }
        x[idx[1]]
    }

    build_da_export <- function(df, metric_col, model_value = NULL) {
        df_main <- df %>% dplyr::filter(metadata == main_el)

        out <- df_main %>%
            dplyr::filter(
                !is.na(.data[[metric_col]]),
                .data[[metric_col]] < 0.05
            )
        if (!is.null(model_value)) {
            out <- out %>% dplyr::filter(model == model_value)
        }

        # Backfill missing coef/stderr from rows with available estimates.
        ref_by_model <- df_main %>%
            dplyr::group_by(feature, model) %>%
            dplyr::summarise(
                coef_ref_model = first_non_na(coef),
                stderr_ref_model = first_non_na(stderr),
                .groups = "drop"
            )

        ref_by_feature <- df_main %>%
            dplyr::group_by(feature) %>%
            dplyr::summarise(
                coef_ref_any = first_non_na(coef),
                stderr_ref_any = first_non_na(stderr),
                .groups = "drop"
            )

        out <- out %>%
            dplyr::left_join(ref_by_model, by = c("feature", "model")) %>%
            dplyr::left_join(ref_by_feature, by = "feature") %>%
            dplyr::mutate(
                coef = dplyr::coalesce(coef, coef_ref_model, coef_ref_any),
                stderr = dplyr::coalesce(stderr, stderr_ref_model, stderr_ref_any)
            )

        metric_name <- if (metric_col == "pval_individual") "p_value" else "q_value"

        if (is.null(model_value)) {
            # Combined export: keep abundance and prevalence metrics in separate columns.
            out_metric <- out %>%
                dplyr::filter(model %in% c("abundance", "prevalence")) %>%
                dplyr::transmute(
                    Species = feature,
                    name = .data$name,
                    model = model,
                    value_individual = .data[[metric_col]]
                ) %>%
                dplyr::group_by(Species, name, model) %>%
                dplyr::summarise(value_individual = suppressWarnings(min(value_individual, na.rm = TRUE)), .groups = "drop") %>%
                tidyr::pivot_wider(
                    names_from = model,
                    values_from = value_individual
                )

            out_null <- out %>%
                dplyr::filter(model %in% c("abundance", "prevalence")) %>%
                dplyr::transmute(
                    Species = feature,
                    name = .data$name,
                    model = model,
                    null_hypothesis = .data$null_hypothesis
                ) %>%
                dplyr::group_by(Species, name, model) %>%
                dplyr::summarise(
                    null_hypothesis = {
                        nh <- na.omit(null_hypothesis)
                        if (length(nh) == 0) NA_character_ else as.character(nh[[1]])
                    },
                    .groups = "drop"
                ) %>%
                tidyr::pivot_wider(
                    names_from = model,
                    values_from = null_hypothesis
                )

            out <- out_metric %>%
                dplyr::left_join(out_null, by = c("Species", "name"), suffix = c("", "_null"))

            if (!"abundance" %in% colnames(out)) {
                out$abundance <- NA_real_
            }
            if (!"prevalence" %in% colnames(out)) {
                out$prevalence <- NA_real_
            }
            if (!"abundance_null" %in% colnames(out)) {
                out$abundance_null <- NA_character_
            }
            if (!"prevalence_null" %in% colnames(out)) {
                out$prevalence_null <- NA_character_
            }

            out <- out %>%
                dplyr::select(Species, name, abundance, prevalence, abundance_null, prevalence_null)
            colnames(out) <- c(
                "Species",
                "name",
                paste0(metric_name, "_abundance"),
                paste0(metric_name, "_prevalence"),
                "null_hypothesis_abundance",
                "null_hypothesis_prevalence"
            )
            return(out)
        }

        out <- out %>%
            dplyr::transmute(
                Species = feature,
                name = .data$name,
                value_individual = .data[[metric_col]],
                null_hypothesis = .data$null_hypothesis,
                coef = .data$coef,
                stderr = .data$stderr
            )
        colnames(out)[3] <- paste0(metric_name, "_individual")
        out
    }

    export_specs <- list(
        list(metric = "pval_individual", model = "prevalence", suffix = "p_value_prevalence"),
        list(metric = "qval_individual", model = "prevalence", suffix = "q_value_prevalence"),
        list(metric = "pval_individual", model = "abundance", suffix = "p_value_abundance"),
        list(metric = "qval_individual", model = "abundance", suffix = "q_value_abundance")
    )

    for (spec in export_specs) {
        export_df <- build_da_export(all_results, spec$metric, spec$model)
        out_xlsx <- file.path(
            output_folder,
            paste0(main_el, "_MAASLIN3_DA_", spec$suffix, "_", Sys.Date(), ".xlsx")
        )
        writexl::write_xlsx(export_df, out_xlsx)
    }

    all_results_sig <- all_results %>% dplyr::filter(metadata == main_el & .data[[significance_metric]] < 0.05)
    model_filter <- tolower(model_filter)
    if (model_filter %in% c("abundance", "prevalence")) {
        all_results_sig <- all_results_sig %>% dplyr::filter(model == model_filter)
    }
    all_results_sig <- all_results_sig %>%
        dplyr::select(feature, metadata, value, coef, null_hypothesis, pval_individual, qval_individual, pval_individual, pval_individual, model, stderr)
    if (nrow(all_results_sig) == 0) {
        warning(paste("Maaslin3: no significant results found for", main_el, "after filtering."))
    }
    write.csv(all_results_sig, file.path(output_folder, paste0(main_el, "_MAASLIN3_SR_", Sys.Date(), ".csv")), row.names = FALSE)
    features_unique <- unique(all_results_sig$feature)
    features_unique <- data.frame(Species = features_unique)
    write.csv(features_unique, file.path(output_folder, paste0(main_el, "_MAASLIN3_SR_features_", Sys.Date(), ".csv")), row.names = FALSE, quote=FALSE)
    return(file.path(output_folder, paste0(main_el, "_MAASLIN3_SR_features_", Sys.Date(), ".csv")))

}
