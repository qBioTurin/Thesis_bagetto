
#' @title Generate Stacked Barplot from Phyloseq Object
#'
#' @description
#' This function creates a relative abundance stacked barplot at a specified taxonomic level (e.g., Genus, Species, Strain) from a `phyloseq` object, optionally binning or excluding ambiguous taxa.
#'
#' @param baseline_dec A string path to a serialized RDS file containing a `phyloseq` object (already decontaminated and normalized or to be normalized internally).
#' @param SP_levels A character string indicating the taxonomic level to use (e.g., `"Genus"`, `"Species"`, `"Strain"`).
#' @param division A column name from sample metadata to use for facetting (e.g., `"Group"`, `"Timepoint"`).
#' @param bin Logical; whether to allow taxa with `"GB"` labels at the chosen level (used for strain binning). Default is `FALSE`.
#'
#' @return A `ggplot` object showing a faceted stacked barplot of the top 10 taxa at the specified level, with "Other" grouping all remaining taxa.
#'
#' @details
#' The function performs the following steps:
#' - Reads and identifies the taxonomic domain (Bacteria, Archaea, Eukaryota).
#' - Converts absolute to relative abundances.
#' - Filters taxa based on resolution and ambiguity at lower levels.
#' - Aggregates data to the specified level (`SP_levels`) using `microbiomeutilities::aggregate_top_taxa2()`.
#' - Adds metadata and taxonomy for plotting.
#' - Plots taxa as a stacked barplot, grouped by `division`, and sorted (if applicable) by `edss`.
#'
#' The colors are derived from a custom palette (`colors_10` from `Colorpalette.R`), and unclassified/other taxa are shown in gray.
#'
#' @import phyloseq
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom microbiomeutilities aggregate_top_taxa2
#'
#' @examples
#' \dontrun{
#' GraphStackedBar(
#'   baseline_dec = "Output/kraken_bacteria_norm.rds",
#'   SP_levels = "Genus",
#'   division = "group"
#' )
#' }
#'
#' @export
# baseline_dec <-"/home/franky/Desktop/PRJNA/SUPERVISED_DEC/Bacteria_Supervised_decontam.rds"
#   SP_levels <-"Species"
#   division <- "diagnosis"
#   bin <- FALSE
#   subset <-"sex"
#   subset_el <-c("Female","Male")
GraphStackedBar <- function(baseline_dec, df, SP_levels, division, bin = FALSE, subset,subset_el, match_id_col = "id") {
  normalize_metadata_names <- function(x) {
    names(x) <- tolower(trimws(names(x)))
    x
  }

  align_metadata_classes <- function(source_df, target_df) {
    common_cols <- intersect(names(source_df), names(target_df))

    for (col_name in common_cols) {
      source_col <- source_df[[col_name]]
      target_col <- target_df[[col_name]]

      if (is.factor(source_col)) {
        target_df[[col_name]] <- factor(
          target_col,
          levels = levels(source_col),
          ordered = is.ordered(source_col)
        )
      } else if (inherits(source_col, "Date")) {
        target_df[[col_name]] <- as.Date(target_col)
      } else if (is.numeric(source_col)) {
        target_df[[col_name]] <- suppressWarnings(as.numeric(target_col))
      } else if (is.logical(source_col)) {
        target_df[[col_name]] <- as.logical(target_col)
      } else if (is.character(source_col)) {
        target_df[[col_name]] <- as.character(target_col)
      }
    }

    target_df
  }

  if (SP_levels == "Strain" && bin == FALSE) {
    stop("Error: Strain level cannot be used with binning. Please set bin=TRUE.")
  }
  source("Colorpalette.R")
  df <- normalize_metadata_names(as.data.frame(df))
  division <- tolower(trimws(division))
  subset <- tolower(trimws(subset))
  match_id_col <- tolower(trimws(match_id_col))

  if (grepl("bact", baseline_dec, ignore.case = TRUE)) {
    domain <- "Bacteria"
  } else if (grepl("arch", baseline_dec, ignore.case = TRUE)) {
    domain <- "Archaea"
  } else if (grepl("euk", baseline_dec, ignore.case = TRUE)) {
    domain <- "Eukaryota"
  } else {
    domain <- "Unknown"
  }
  capitalize <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
  }
  
  baseline_dec <- readRDS(baseline_dec)
  metadata <- normalize_metadata_names(as.data.frame(sample_data(baseline_dec)))
  filtered_samples <- metadata[[subset]] %in% subset_el
  baseline_dec <- prune_samples(filtered_samples, baseline_dec)

  rel_abund <- as.data.frame(abundances(baseline_dec, transform = "compositional"))
  baseline_dec <- phyloseq(otu_table(rel_abund, taxa_are_rows = TRUE), tax_table(baseline_dec), sample_data(baseline_dec))

  baselines_metadata <- normalize_metadata_names(data.frame(sample_data(baseline_dec)))
  baselines_metadata <- align_metadata_classes(df, baselines_metadata)

  if (!(division %in% colnames(baselines_metadata))) {
    stop(paste("Division column not found in metadata:", division))
  }

  division_values <- baselines_metadata[[division]]
  max_unique_values <- max(1, ceiling(nrow(baselines_metadata) / 2))
  n_unique_division <- length(unique(stats::na.omit(division_values)))

  if (!is.factor(division_values) && n_unique_division > max_unique_values) {
    stop(
      paste0(
        "Division column '", division, "' has ", n_unique_division,
        " unique values after subsetting. Use a factor or choose a character/numeric column with at most ",
        max_unique_values, " unique values."
      )
    )
  }

  if (is.character(division_values)) {
    baselines_metadata[[division]] <- factor(division_values, levels = unique(stats::na.omit(division_values)))
  } else if (is.numeric(division_values)) {
    division_levels <- sort(unique(stats::na.omit(division_values)))
    baselines_metadata[[division]] <- factor(
      division_values,
      levels = division_levels,
      ordered = TRUE
    )
  }

  taxdf <- as.data.frame(tax_table(baseline_dec))
  high_sp <- capitalize(SP_levels)

  tax_levels <- colnames(taxdf)


  level_index <- match(high_sp, tax_levels)

  is_defined_to_level <- !is.na(taxdf[[high_sp]]) & taxdf[[high_sp]] != "unknown"


  if (level_index < length(tax_levels)) {
    lower_levels <- tax_levels[(level_index + 1):length(tax_levels)]
  } else {
    lower_levels <- character(0)
  }

  if (length(lower_levels) > 0) {
    is_empty_below <- apply(taxdf[, lower_levels, drop = FALSE], 1, function(row) {
      all(is.na(row) | row == "unknown")
    })
  } else {
    is_empty_below <- rep(TRUE, nrow(taxdf))
  }
  if (!bin) {
    no_bin <- !grepl("GB", taxdf[[high_sp]])
  } else {
    no_bin <- rep(TRUE, nrow(taxdf))
  }
  if ("Strain" %in% colnames(taxdf)) {
    valid_taxa <- is_defined_to_level & is_empty_below & no_bin

    baseline_dec <- prune_taxa(valid_taxa, baseline_dec)
  }

  if (high_sp == "Species"){
    tax_table <- as.data.frame(tax_table(baseline_dec))
    tax_table$Species <- paste0(tax_table$Genus, " ", tax_table$Species)
    tax_table(baseline_dec) <- as.matrix(tax_table)
  }

  tax<- as.data.frame(tax_table(baseline_dec))
  baseline_dc_genus <- baseline_dec %>% microbiomeutilities::aggregate_top_taxa2(top = 10, high_sp)


  # For species: merge genus and species names
  otu_table<- otu_table(baseline_dc_genus)
  genus <- rownames(otu_table(baseline_dc_genus))
  tax_df <- as.data.frame(tax_table(baseline_dec))

  save_array <- genus
  save_array <- c(save_array[save_array != "Other"], "Other")

  # Create dataframe easily accessible by ggplot
  dat <- as.data.frame(otu_table(baseline_dc_genus))
  dat <- dat %>%
    mutate(OTU_id = genus) %>%
    pivot_longer(-OTU_id, names_to = "id", values_to = "count")

  # Add taxonomy
  tax <- as.data.frame(tax_table(baseline_dc_genus))
  tax <- tax %>%
    mutate(OTU_id = genus)

  # Merge counts with taxonomy information
  dat <- dat %>%
    dplyr::left_join(tax, by = "OTU_id")

  # Add metadata information
  dat <- dat %>%
    dplyr::left_join(baselines_metadata, by = setNames(match_id_col, "id"))

  dat$OTU_id <- factor(dat$OTU_id, levels = save_array)
  palet_10 <- colors_10
  names(palet_10) <- levels(dat$OTU_id)
  palet_10["Other"] <- "#9b9a9a"

  faceting_formula <- as.formula(paste("~", division))


  pl <- dat %>%
    ggplot(aes_string(x = match_id_col, y = "count")) +
    facet_grid(faceting_formula, scales = "free_x", space = "free_x") +
    geom_bar(aes(fill = OTU_id), stat = "identity", position = "fill", width = 1) +
    xlab("Sample ID") +
    scale_fill_manual(values = palet_10) +
    scale_y_continuous(name = "Relative Abundance", labels = scales::percent) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(color = "black", size = 12),
      strip.text = element_text(face = "bold", size = 20),
      strip.background = element_blank()
    ) +
    guides(fill = guide_legend(title = SP_levels))
  pl
  return(pl)
}

