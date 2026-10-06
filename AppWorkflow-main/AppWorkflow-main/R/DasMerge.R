#' @title Merge Differential Abundance Results from LEfSe and LIMMA
#'
#' @description
#' This function merges results from LEfSe and LIMMA differential abundance analyses
#' to retain all unique significant taxa. It then filters a phyloseq object accordingly,
#' pruning both taxa and samples based on the given `sub_category` and `sub_name`.
#'
#' @param baselines_dec A string. Path to an `.rds` file containing the decontaminated phyloseq object.
#' @param das_lefse A data frame. LEfSe results containing taxonomic columns including `Genus` and `Species`.
#' @param das_limma A data frame. LIMMA results, with a column named `Genus_Species` and compositional abundances.
#' @param final_output A string. Path to save the filtered and merged phyloseq object (`.rds` format).
#' @param sub_category A string. Metadata variable name used to filter samples (e.g., `"gc_treatment"`).
#' @param sub_name A character vector. Subgroup levels of `sub_category` to retain (e.g., `c("positive")`).
#'
#' @return The filtered phyloseq object (invisibly returned), also saved to the given path as an `.rds` file.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Loads and normalizes abundance data from the given phyloseq object.
#'   \item Merges significant taxa from LEfSe and LIMMA based on `Genus_Species`.
#'   \item Prunes taxa and samples accordingly.
#'   \item Saves and returns the updated phyloseq object.
#' }
#'
#' Handles edge cases where both DAS lists are empty or mismatched. Warnings are printed but execution is safeguarded.
#'
#' @note Assumes that both `das_lefse` and `das_limma` have taxonomic labels formatted consistently (including `Genus_Species`).
#'
#' @import dplyr phyloseq tibble
#' @importFrom utils saveRDS
#'
#' @export

SOIMerge <- function(baselines_dec, das_limma, final_output,
                     sub_category, sub_name) {
  baselines_dec <- readRDS(baselines_dec)

  output_folder <- dirname(final_output)
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  otu <- as.data.frame(abundances(otu_table(baselines_dec), transform = "compositional"))
  baselines_dec <- phyloseq(otu_table(otu, taxa_are_rows = TRUE), tax_table(baselines_dec), sample_data(baselines_dec))
  taxa <- as.data.frame(baselines_dec@tax_table)
  taxa <- taxa %>%
    mutate(Genus_Species = paste(Genus, Species, sep = " "))
  colnames(das_limma)[colnames(das_limma) == "X"] <- "Genus_Species"
  limma_list <- das_limma$Genus_Species
  if (length(limma_list) == 0) {
    print(paste("Error: limma list is empty. You will not see this rds."))
    return(NULL)
  }

  DAS <- limma_list
  taxa <- taxa[taxa$Genus_Species %in% DAS, ]
  taxa <- taxa[, -8]
  keep_otu <- rownames(taxa)
  baselines_dec <- prune_taxa(keep_otu, baselines_dec)
  samples <- as(sample_data(baselines_dec), "data.frame")
  samples <- samples %>%
    filter(!!sym(sub_category) %in% sub_name) %>%
    droplevels()

  baselines_dec <- prune_samples(samples$id, baselines_dec)

  print(baselines_dec)
  saveRDS(baselines_dec, file = final_output)
  return(baselines_dec)
}

get_das_species <- function(maaslin_df = NULL, ancombc_df = NULL) {
  if (is.null(maaslin_df) && is.null(ancombc_df)) {
    stop("Both DAS inputs are NULL")
  }

  if (!is.null(maaslin_df)) {
    maaslin_species <- maaslin_df$Species
  } else {
    maaslin_species <- NULL
  }

  if (!is.null(ancombc_df)) {
    ancombc_species <- ancombc_df[[1]]
  } else {
    ancombc_species <- NULL
  }

  if (!is.null(maaslin_species) && !is.null(ancombc_species)) {
    intersect(maaslin_species, ancombc_species)
  } else {
    unique(c(maaslin_species, ancombc_species))
  }
}


filter_phyloseq_by_das <- function(
    rds_path,
    maaslin_df = NULL,
    ancombc_df = NULL,
    output_rds,
    sub_category,
    sub_name) {
  das_species <- get_das_species(maaslin_df, ancombc_df)
  print(paste("Number of DAS species to retain:", length(das_species)))

  rds <- readRDS(rds_path)

  # ---- sample filter ----------------------------------------
  meta <- as.data.frame(sample_data(rds))
  if (!(sub_category %in% colnames(meta))) {
    stop(paste0(
      "DAS filtering error: subset column '",
      sub_category,
      "' not found in sample_data."
    ))
  }

  keep_samples <- rownames(meta)[as.character(meta[[sub_category]]) %in% as.character(sub_name)]
  print(paste("Number of samples to keep:", length(keep_samples)))
  if (length(keep_samples) == 0) {
    stop(paste0(
      "DAS filtering error: no samples match subset column '",
      sub_category,
      "' with selected values [",
      paste(sub_name, collapse = ", "),
      "]."
    ))
  }

  rds <- prune_samples(keep_samples, rds)

  # ---- taxa filter ------------------------------------------
  tax <- as.data.frame(tax_table(rds))
  tax$GenusSpecies <- paste(tax$Genus, tax$Species, sep = "_")
  tax$Genus_Species2 <- paste(tax$Genus, tax$Species, sep = " ") # maybe its possible to simply remove _ and rep to " " in maaslin need to verify
  keep_taxa <- rownames(tax)[(tax$GenusSpecies %in% das_species) | (tax$Genus_Species2 %in% das_species)]
  print(paste("Number of taxa to keep:", length(keep_taxa)))
  if (length(keep_taxa) == 0) {
    warning("No taxa match DAS species. Returning unfiltered phyloseq object.")
  } else {
    rds <- prune_taxa(keep_taxa, rds)
  }
  methods_used <- ""
  if (!is.null(maaslin_df)) {
    methods_used <- paste0(methods_used, "Maaslin3")
  }
  if (!is.null(ancombc_df)) {
    if (methods_used != "") methods_used <- paste0(methods_used, "_")
    methods_used <- paste0(methods_used, "Ancombc2")
  }
  name_rds <- paste0("DAS_", methods_used, "_", sub_category, "_", Sys.Date(), ".rds")
  dir.create(dirname(output_rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(rds, output_rds)

  return(rds)
}
