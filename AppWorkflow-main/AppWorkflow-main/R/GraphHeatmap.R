#' @title Generate Annotated Heatmap from Phyloseq Object
#'
#' @description
#' This function creates a heatmap of normalized and z-scored abundances at the Genus-Species level from a phyloseq object, with sample and taxonomic annotations.
#' It saves the plot as a PDF and returns the ggplot version of the heatmap.
#'
#' @param physeq_object A string path to an RDS file containing a `phyloseq` object.
#' @param metadata A data frame containing sample metadata. Must include an `id` column matching sample names in the phyloseq object.
#' @param output_folder Output directory to save the heatmap PDF. Default is `"Image/Heatmap/"`.
#' @param metadata_vars A character vector of column names from the metadata to be used for heatmap annotation.
#' @param sample_split_var Optional metadata column name used to split/order samples on the X axis.
#'   Samples are ordered alphanumerically within each group.
#' @param sample_label_var Optional metadata column name used as displayed sample labels on the X axis.
#' @param palette_length Integer. Number of color breaks for the heatmap gradient. Default is 35.
#' @param color_palette A color palette for the heatmap gradient. Default is `RColorBrewer::brewer.pal(9, "RdBu")`.
#' @param phylum_palette A palette function to color phyla on the rows. Default is `viridisLite::viridis`.
#'
#' @return A `ggplot` object representing the annotated heatmap.
#'
#' @details
#' The function:
#' - Extracts OTU and taxonomic tables from the `phyloseq` object.
#' - Normalizes and z-scores abundance data.
#' - Filters and formats metadata and taxonomic data for annotation.
#' - Uses `pheatmap::pheatmap()` to generate a clustered heatmap.
#' - Converts it to a `ggplot` object and saves the figure as a PDF.
#'
#' The resulting heatmap includes:
#' - Row annotation for `Phylum` (with domain suffix).
#' - Column annotation for selected metadata variables.
#' - Conditional clustering based on the number of rows/columns.
#'
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom phyloseq tax_table otu_table sample_data
#' @importFrom RColorBrewer brewer.pal
#' @importFrom pheatmap pheatmap
#' @importFrom ggplotify as.ggplot
#' @importFrom ggplot2 ggsave
#'
#' @examples
#' \dontrun{
#' meta <- read.csv("input/metadata.csv")
#' GraphHeatmap(
#'   physeq_object = "Output/my_physeq.rds",
#'   metadata = meta,
#'   metadata_vars = c("Treatment", "Sex")
#' )
#' }
#'
#' @export
#physeq_object<-"/home/franky/Downloads/DAS_gc_treatment_limma_meta_2025-07-21.rds"
#metadata<- read.csv("input/reduced_data.csv")
#metadata_vars <- c("sex","gc_treatment")

GraphHeatmap <- function(physeq_object, 
                                    metadata, 
                                    output_folder = "Image/Heatmap/",
                                    metadata_vars,
                                    sample_split_var = NULL,
                                    sample_label_var = NULL,
                                    palette_length = 35,
                                    color_palette = RColorBrewer::brewer.pal(9, "RdBu"),
                                    phylum_palette = viridisLite::viridis) {
  
 # write.csv(metadata, file = file.path( "metadata_used.csv"), row.names = FALSE)
 # print(metadata_vars)
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  filename<-basename(physeq_object)
  physeq_object <- readRDS(physeq_object)
  # Taxa + OTU
  taxatables <- as.data.frame(tax_table(physeq_object)) %>%
    rownames_to_column("otu_id")
  
  otutables <- data.frame(otu_table(physeq_object)) %>%
    rownames_to_column("otu_id")
  
  combined_df <- full_join(taxatables, otutables, by = "otu_id") %>%
    mutate(Genus_species = paste(Genus, Species, sep = " "))
  colnames(combined_df) <- gsub("\\.", "-", colnames(combined_df))
  
  norm_data <- combined_df[, !(colnames(combined_df) %in% colnames(taxatables))]
  rownames(norm_data) <- combined_df$Genus_species
  norm_data <- norm_data[, -ncol(norm_data)]
  
  metadata_kingdom <- combined_df %>%
    select(Kingdom, Phylum, Genus, Genus_species)
  rownames(metadata_kingdom) <- metadata_kingdom$Genus_species
  metadata_kingdom <- metadata_kingdom %>% select(-Genus_species)
  
  norm_data_z <- t(scale(t(norm_data)))
  norm_data_z[is.na(norm_data_z) | is.nan(norm_data_z)] <- 0
  
  #metadata <- read.csv(metadata_path, header = TRUE, sep = ",", na.strings = c("", " ", "NA")) %>%
  metadata <- metadata %>%
  mutate(id = as.character(id))

  
  samples <- colnames(norm_data_z)
  metadata <- metadata[metadata$id %in% samples, ]
  metadata <- as.data.frame(metadata)
  rownames(metadata) <- metadata$id

  natural_order <- function(x) {
    x_chr <- as.character(x)
    x_low <- tolower(x_chr)
    has_num <- grepl("[0-9]+$", x_low, perl = TRUE)
    prefix <- ifelse(has_num, sub("(.*?)([0-9]+)$", "\\1", x_low, perl = TRUE), x_low)
    trailing_num <- suppressWarnings(as.numeric(ifelse(
      has_num,
      sub(".*?([0-9]+)$", "\\1", x_low, perl = TRUE),
      NA_character_
    )))
    order(prefix, is.na(trailing_num), trailing_num, x_low, x_chr)
  }

  natural_rank <- function(x) {
    ord <- natural_order(x)
    r <- integer(length(ord))
    r[ord] <- seq_along(ord)
    r
  }

  order_var <- "id"
  if (!is.null(sample_label_var) && nzchar(sample_label_var) && sample_label_var %in% colnames(metadata)) {
    order_var <- sample_label_var
  }

  order_vals <- as.character(metadata[[order_var]])
  order_vals[is.na(order_vals) | !nzchar(order_vals)] <- metadata$id[is.na(order_vals) | !nzchar(order_vals)]

  has_split_var <- !is.null(sample_split_var) && nzchar(sample_split_var) && sample_split_var %in% colnames(metadata)

  if (has_split_var) {
    split_vals <- as.character(metadata[[sample_split_var]])
    split_vals[is.na(split_vals)] <- "NA"

    ord_idx <- order(
      natural_rank(split_vals),
      natural_rank(order_vals),
      natural_rank(metadata$id)
    )

    ordered_ids <- metadata$id[ord_idx]
  } else {
    ordered_ids <- metadata$id[order(
      natural_rank(order_vals),
      natural_rank(metadata$id)
    )]
  }
  ordered_ids <- ordered_ids[ordered_ids %in% colnames(norm_data_z)]
  norm_data_z <- norm_data_z[, ordered_ids, drop = FALSE]
  metadata <- metadata[ordered_ids, , drop = FALSE]
  
  # Select only required metadata columns
  metadata_hm <- metadata[, c("id", metadata_vars),drop= FALSE]
  rownames(metadata_hm) <- metadata_hm$id
  metadata_hm <- metadata_hm[, -1, drop = FALSE]  

  gaps_col <- NULL
  if (has_split_var) {
    split_sequence <- as.character(metadata[[sample_split_var]])
    split_sequence[is.na(split_sequence)] <- "NA"
    run_lengths <- rle(split_sequence)$lengths
    if (length(run_lengths) > 1) {
      gaps_col <- cumsum(run_lengths)[-length(run_lengths)]
    }
  }

  labels_col <- ordered_ids
  if (!is.null(sample_label_var) && nzchar(sample_label_var) && sample_label_var %in% colnames(metadata)) {
    labels_col <- as.character(metadata[[sample_label_var]])
    labels_col[is.na(labels_col) | !nzchar(labels_col)] <- ordered_ids[is.na(labels_col) | !nzchar(labels_col)]
  }
  
  # Automatic factor conversion for categorical variables
  metadata_hm <- metadata_hm %>%
    mutate(across(where(is.character), ~ if (n_distinct(.) <= 10) factor(., levels = sort(unique(.))) else .)) %>%
    mutate(across(where(is.logical), as.factor)) %>%
    mutate(across(where(is.numeric), ~ if (n_distinct(.) <= 10) factor(., levels = sort(unique(.))) else .))
  
  my_colors <- colorRampPalette(c("blue", "white", "red"))(palette_length)
  # my_breaks <- c(seq(min(norm_data_z), 0, length.out = ceiling(palette_length / 2) + 1),
  #                seq(-1 / palette_length, max(norm_data_z), length.out = floor(palette_length / 2)))
  my_breaks <- seq(-(max(norm_data_z)), max(norm_data_z),length.out = palette_length + 1)
  metadata_kingdom <- metadata_kingdom %>% select(Phylum)
  colnames(metadata_kingdom) <- paste0("Phylum_", unique(combined_df$Kingdom))
  
 n_rows <- nrow(norm_data_z)
  n_cols <- ncol(norm_data_z) 
  cluster_rows_flag <- n_rows > 1
  cluster_cols_flag <- FALSE
  
  
  print(str(metadata_hm))

  heatmap <- pheatmap::pheatmap(norm_data_z,
                                annotation_col = metadata_hm,
                                annotation_row = metadata_kingdom,
                                labels_col = labels_col,
                                cluster_cols = cluster_cols_flag,
                                cluster_rows = cluster_rows_flag,
                                gaps_col = gaps_col,
                                col = my_colors,
                                breaks = my_breaks,
                                border_color = NA,
                                silent = TRUE)
  
  #pdf_file <- file.path( paste0(filename, ".pdf"))
  gg <- as.ggplot(heatmap)
  #height <- min(max(n_rows * 0.2, 6), 30)
  #width <- min(max(n_cols * 0.4, 6), 30)

  #ggsave(plot = gg, filename = pdf_file, height = height, width = width)
  return(gg)

}

