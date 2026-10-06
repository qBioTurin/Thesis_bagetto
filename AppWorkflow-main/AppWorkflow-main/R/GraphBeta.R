#' @title Beta Diversity PCoA Plot with Bray-Curtis Distance
#'
#' @description
#' Generates a PCoA plot (Principal Coordinates Analysis) using Bray-Curtis distances on a `phyloseq` object, colored by a selected metadata variable. It also adds ellipses and uses a predefined color palette.
#'
#' @param baselines_dec A string path to an RDS file containing a `phyloseq` object.
#' The file name should contain "bact", "arch", or "euk" to identify the domain (used for error checking).
#' @param element A string. The name of a variable from the `sample_data()` of the `phyloseq` object to color the samples.
#'
#' @return A `ggplot` object displaying a 2D ordination plot of beta diversity based on Bray-Curtis distance.
#'
#' @details
#' The function determines the biological domain based on the filename (`baselines_dec`) and loads the object.
#' Bray-Curtis distances are calculated using `ordinate()` and plotted with `plot_ordination()`.
#' Color is mapped to the provided metadata variable (`element`), with ellipses added to visualize group dispersion.
#'
#' The function relies on a predefined color palette `colors_10` loaded via `source("Colorpalette.R")`.
#'
#' @import phyloseq
#' @import ggplot2
#' @importFrom vegan vegdist
#'
#' @examples
#' \dontrun{
#' GraphBeta("outputs/bact_baseline.rds", element = "TreatmentGroup")
#' }
#'
#' @export
#'

  GraphBeta <- function(baselines_dec,df, element, subset, subset_elements) {
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

    source("Colorpalette.R")
    df <- normalize_metadata_names(as.data.frame(df))
    element <- tolower(trimws(element))
    subset <- tolower(trimws(subset))

    if (grepl("bact", baselines_dec, ignore.case = TRUE)) {
      domain <- "Bacteria"
    } else if (grepl("arch", baselines_dec, ignore.case = TRUE)) {
      domain <- "Archaea"
    } else if (grepl("euk", baselines_dec, ignore.case = TRUE)) {
      domain <- "Eukaryota"
    } else {
      stop("Error: Domain not recognized. Please check the baseline_dec input.")
    }

    baselines_dec <- readRDS(baselines_dec)
    metadata <- normalize_metadata_names(data.frame(sample_data(baselines_dec)))

    if (!(subset %in% colnames(metadata))) {
      stop(paste("Subset column does not exist in metadata:", subset))
    }

    filtered_samples <- metadata[[subset]] %in% subset_elements
    baselines_dec <- prune_samples(filtered_samples, baselines_dec)
    metadata <- align_metadata_classes(df, normalize_metadata_names(data.frame(sample_data(baselines_dec))))

    if (!(element %in% colnames(metadata))) {
      stop(paste("Selected column does not exist in metadata:", element))
    }

    element_values <- metadata[[element]]
    max_unique_values <- max(1, ceiling(nrow(metadata) / 2))
    n_unique_element <- length(unique(stats::na.omit(element_values)))

    if (!is.factor(element_values) && n_unique_element > max_unique_values) {
      stop(
        paste0(
          "Selected beta variable '", element, "' has ", n_unique_element,
          " unique values after subsetting. Use a factor or choose a character/numeric column with at most ",
          max_unique_values, " unique values."
        )
      )
    }

    if (is.character(element_values)) {
      metadata[[element]] <- factor(element_values, levels = unique(stats::na.omit(element_values)))
    } else if (is.numeric(element_values)) {
      element_levels <- sort(unique(stats::na.omit(element_values)))
      metadata[[element]] <- factor(
        element_values,
        levels = element_levels,
        ordered = TRUE
      )
    }

    sample_data(baselines_dec) <- sample_data(metadata)
    wUF.ordu <- ordinate(baselines_dec, method = "PCoA", distance = "bray")
    # Calcola la matrice di distanze DOPO il filtro dei sample
    bray_dist <- phyloseq::distance(baselines_dec, method = "bray")
    sample_df <- data.frame(sample_data(baselines_dec))
    set.seed(2204)
    # Assicurati che sample_df sia ordinato coerentemente con bray_dist
    sample_df <- sample_df[match(labels(bray_dist), rownames(sample_df)), ]
    n_groups <- length(unique(stats::na.omit(sample_df[[element]])))
    pval_cat <- NA_real_

    if (n_groups >= 2) {
      fmla <- as.formula(paste("bray_dist ~", element))
      permanova_cat <- vegan::adonis2(fmla, data = sample_df)
      pval_cat <- signif(permanova_cat$`Pr(>F)`[1], 3)
    }

    baselines_dec <- phyloseq(otu_table(baselines_dec), tax_table(baselines_dec), sample_data(baselines_dec))

    color_values <- colorRampPalette(rev(colors_10))(max(1, n_groups))

    beta_graph <- plot_ordination(baselines_dec, wUF.ordu, type = "samples", color = element) +
      geom_point(size = 4) +
      theme_classic() +
      scale_color_manual(values = color_values) +
      guides(color = guide_legend(title = element)) +
      annotate(
        "text",
        x = Inf, y = Inf,
        label = if (is.na(pval_cat)) "PERMANOVA p = NA" else paste0("PERMANOVA p = ", pval_cat),
        hjust = 1.1, vjust = 1.5, size = 5
      ) +
      theme(
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
      )

    if (n_groups >= 2) {
      beta_graph <- beta_graph + stat_ellipse()
    }

    return(beta_graph)
  }
