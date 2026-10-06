# # Read CSV file
# data <- read_tsv("/media/franky/dati/University_data/unitor/unitor_8_kraken_humann/pathabundance_data/HUMANN_OUTPUT/merge_tables/merged_pathabundance.tsv")
# metadata_path <- "/media/franky/dati/University_data/unitor/unitor_8_kraken_res/input/metadata_unitor_8_140126.csv"

#  main_disc="mouse_group"
# # discr_els= c("time_span") 
# output_folder="/media/franky/dati/University_data/unitor/unitor_8_kraken_humann/MAASLIN_PATHWAY"
# PathwaySearcher(data, metadata_path, main_disc, discr_els, output_folder)
PathwaySearcher <- function(data, metadata, main_disc, discr_els=NULL, output_folder) {
    data<-read_tsv(data)
    array_data <- data$`# Pathway`
    colnames(data) <- gsub("_final.*|*_output", "", colnames(data))
    colnames(data)<-gsub("-", "_", colnames(data))
    data <- as.data.frame(data)
    rownames(data) <- data[[1]]
    # Extract unique elements from first column, keeping only text before : or |
    # Remove UNMAPPED and UNINTEGRATED entries
    data2 <- data %>% select(-"# Pathway")
    # metadata <- read.csv(metadata_path,
    #     header = TRUE,
    #     sep = ",",
    #     na = c("", " ", "NA"),
    #     check.names = TRUE
    # )
    rownames(metadata) <- metadata$id
    data2 <- data2[, colnames(data2) %in% metadata$id]


    # Remove rows with UNMAPPED or UNINTEGRATED in column names
    data2 <- data2[!grepl("UNMAPPED|UNINTEGRATED", rownames(data2)), ]

    formulaInUse <- paste0("~ ", main_disc)
    if (!missing(discr_els) && length(discr_els) > 0) {
        formulaInUse <- paste(formulaInUse, paste(discr_els, collapse = "+"), sep = "+")
    }
    fx_effects <- NULL
    random_effects <- NULL
    group_effects <- NULL
    ordered_effects <- NULL
    strata_effects <- NULL
    fit_out <- maaslin3::maaslin3(
        input_data = data2,
        input_metadata = metadata,
        output = output_folder,
        formula = formulaInUse,
        fixed_effects = fx_effects,
        random_effects = random_effects,
        group_effects = group_effects,
        ordered_effects = ordered_effects,
        strata_effects = strata_effects,
        reference = "time_span,T0",
        normalization = "TSS",
        transform = "LOG",
        augment = TRUE,
        standardize = FALSE,
        max_significance = 0.1,
        median_comparison_abundance = TRUE,
        median_comparison_prevalence = FALSE,
        max_pngs = 250,
        cores = 1
    )
    results_file <- paste0(output_folder, "/all_results.tsv")
    DAP <- read_tsv(results_file)
    DAP<-as.data.frame(DAP)
    DAP$path_name <- sub(":.*", "", DAP$feature)
    # DAP <- DAP %>%
    #     dplyr::filter(metadata == main_disc) %>%
    #     dplyr::distinct(path_name, .keep_all = TRUE) %>%
    #     dplyr::arrange(qval_joint)
    # DAP_array <- DAP %>%
    #     dplyr::mutate(qval_joint = as.numeric(qval_joint)) %>%
    #     dplyr::filter(!is.na(qval_joint)) %>%
    #     dplyr::arrange(qval_joint) %>%
    #     dplyr::pull(path_name)

    array_path <- unique(DAP$path_name)
    #array_data2 <- array_data[!grepl("UNMAPPED|UNINTEGRATED", array_data)]
    #array_data2 <- sub(":.*", "", array_data2)
    # array_data3 <- unique(array_data2)


    # # Separate common and unique elements
    # common_elements <- array_data3[array_data3 %in% array_path]
    # unique_elements <- array_data3[!array_data3 %in% DAP_array]

    # # Sort both groups alphabetically
    # common_elements <- sort(common_elements)
    # unique_elements <- sort(unique_elements)

    # # Add asterisk to common elements
    # common_elements <- paste0(common_elements, "*")

    # # Combine: common elements first, then unique elements
    # array_data <- c(common_elements, unique_elements)

    return(list(fit_out = DAP, array_path = array_path))
}

