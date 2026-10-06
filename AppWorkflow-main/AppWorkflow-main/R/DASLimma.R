#' @title Run Differential Abundance Analysis using LIMMA
#'
#' @description
#' Executes a differential abundance analysis (DAS) on microbiome data using the LIMMA pipeline.
#' The function extracts normalized genus-level data from a phyloseq object, subsets metadata based on a grouping variable and optional subcategory,
#' performs log-transformation, constructs a design matrix, and runs LIMMA to identify significantly differentially abundant features.
#'
#' @param baselines_dec A string. Path to an `.rds` file containing the decontaminated phyloseq object.
#' @param metadata A string. Path to a `.csv` file containing metadata corresponding to the samples.
#' @param analisys A string. Main variable used for group comparison in the LIMMA model (e.g., "lesion_burden").
#' @param subcategory A string. Secondary variable used to filter samples (e.g., "gc_treatment"). Use `"none"` to skip filtering.
#' @param sub_name A character vector. Values of `subcategory` to retain (e.g., `c("positive")`).
#' @param output_folder A string. Folder path to write the LIMMA result file (a `.csv` file with features for heatmap input).
#'
#' @return A string containing the path to the `.csv` file with selected differentially abundant features (p < 0.05).
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Loads and subsets the phyloseq object using metadata filters.
#'   \item Extracts taxonomic and abundance data at the Genus-Species level.
#'   \item Normalizes the data using compositional transformation and log2.
#'   \item Constructs a LIMMA design matrix and fits a model.
#'   \item Filters the top significant features with FDR-adjusted p-values < 0.05.
#'   \item Saves the filtered table for downstream heatmap visualization.
#' }
#'
#'
#' @import dplyr limma phyloseq tidyr readr tibble
#' @importFrom utils write.csv read.csv
#' @importFrom stats model.matrix
#'
#' @export

#  baselines_dec <- "/home/franky/Desktop/ilario/SUPERVISED_DEC/Bacteria_Supervised_decontam.rds"
#  metadata <- "/home/franky/Desktop/ilario/ALPHA/Bacteria_alpha_metadata.csv"
#  metadata <- read.csv(metadata,
#    header = TRUE,
#    sep = ",",
#    na.strings = c("", " ", "NA"),
#    check.names = TRUE
#  )
# analisys <- "type"
#  subcategory <- "time"
#  sub_name <- c("T14", "T7", "W9")
#  output_folder <- "/home/franky/Desktop/ilario/lefse/"

DASLimma <- function(baselines_dec, metadata, analisys,
                     subcategory, sub_name, output_folder) {
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  cat("TYPE baselines_dec:", class(baselines_dec), "\n")
  cat("TYPE metadata:", class(metadata), "\n")
  domain <- find_domain(baselines_dec)
  baselines_dec <- readRDS(baselines_dec)

  filename <- gsub(" ", "", paste0(
    "limma_", domain, "_", subcategory, "_",
    analisys, ".csv"
  ))
  fullname <- gsub(" ", "", paste0(output_folder, filename))
  # Estrai i dati di sample_data e converti in un tibble
  samples <- as.data.frame(as_tibble(sample_data(baselines_dec)))

  # Filtra solo se subcategory è diverso da "none"
  if (subcategory != "none" | subcategory != "") {
    samples <- samples %>% filter(.data[[subcategory]] %in% sub_name)
  }
  baselines_dec <- prune_samples(samples$id, baselines_dec)
  print(baselines_dec)

  # Prepare taxonomic and OTU tables
  taxatables <- as.data.frame(tax_table(baselines_dec)) %>%
    rownames_to_column(var = "otu_id")

  otutables <- data.frame(otu_table(baselines_dec))
  otutables <- as.data.frame(abundances(otutables, transform = "compositional"))
  otutables <- rownames_to_column(otutables, var = "otu_id")

  combined_df <- full_join(taxatables, otutables, by = "otu_id") %>%
    mutate(Genus_species = paste(Genus, Species, sep = " ")) %>%
    select(Genus_species, everything(), -otu_id, -c(2:8))

  norm_data <- combined_df
  colnames(norm_data) <- gsub("[^[:alnum:]_]+", "_", colnames(norm_data))

  rownames(norm_data) <- norm_data$Genus_species
  norm_data <- norm_data[, -c(1, length(norm_data))]

  # Read metadata
  metadata <- read.csv(metadata,
    header = TRUE,
    sep = ",",
    na = c("", " ", "NA"),
    check.names = TRUE
  )
  samples <- colnames(norm_data)
  metadata$id <- gsub("[^[:alnum:]_]+", "_", metadata$id)
  # replace any non-alphanumeric or underscore characters with underscore
  samples <- gsub("[^[:alnum:]_]+", "_", samples)
  metadata <- metadata[metadata$id %in% samples, ]

  if (length(sub_name) > 0) {
    metadata_hm <- metadata
    rownames(metadata_hm) <- metadata_hm$id
    metadata_hm <- metadata_hm %>% filter(.data[[subcategory]] %in% sub_name)
    metadata_hm <- metadata_hm %>% mutate(across(all_of(analisys), as.factor))
  } else {
    metadata_hm <- metadata %>%
      filter(id %in% colnames(norm_data))

    rownames(metadata_hm) <- metadata_hm$id

    metadata_hm <- metadata_hm %>%
      select(all_of(c(analisys))) %>%
      mutate(across(all_of(analisys), as.factor))
  }

  # Ensure data consistency
  norm_data <- norm_data %>%
    select(rownames(metadata_hm))

  log_data <- log2(norm_data + 1)

  formula <- paste0("~ ", analisys)

  design <- model.matrix(as.formula(formula), data = metadata_hm)


  cat("Dimension log_data:", dim(log_data), "\n")
  cat("Dimension design:", dim(design), "\n")
  ncol(log_data)
  array1 <- colnames(log_data)
  array2 <- rownames(design)
  diff <- setdiff(array1, array2)
  if (length(diff) > 0) {
    cat("Samples in log_data not in design:", diff, "\n")
    log_data <- log_data[, !colnames(log_data) %in% diff]
  } else {
    cat("All samples in log_data are present in design.\n")
  }

  ### LIMMA DAS Analysis
  fit <- lmFit(log_data, design)
  fit <- eBayes(fit)

  # remove NA coefficients
  estimable_coefs <- colnames(fit$coefficients)[!is.na(fit$coefficients[1, ])]

  if (length(estimable_coefs) == 0) {
    stop("Nessun coefficiente stimabile nel modello LIMMA.")
  }

  top_table <- topTable(fit,
    coef = estimable_coefs,
    number = Inf,
    adjust = "fdr"
  ) %>%
    filter(P.Value < 0.05)
  # Save results
  filter_for_heatmap <- norm_data[rownames(top_table), ]
  write.csv(filter_for_heatmap, file = gsub(" ", "", fullname))
  return(fullname)
}
