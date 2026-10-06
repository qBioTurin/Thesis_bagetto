#' @title Import and Preprocess MetaPhlAn Data for Decontamination
#'
#' @description
#' Imports a MetaPhlAn abundance table and associated metadata, performs threshold filtering, and writes filtered tables split by domain (Bacteria, Archaea, Eukaryota) for downstream decontamination or analysis.
#'
#' @param data_path Path to the MetaPhlAn input table (usually a `.txt` or `.tsv` file) containing clade-level relative abundances.
#' @param meta_path Path to the metadata CSV file corresponding to the samples in `data_path`.
#' @param output_dir Output directory where the filtered and separated files will be saved.
#'
#' @details
#' The function:
#' - Reads the MetaPhlAn relative abundance table and metadata.
#' - Cleans sample names and normalizes sample IDs.
#' - Filters out all features with relative abundance < 0.001 across all samples.
#' - Removes features with zero abundance across all samples.
#' - Saves three domain-specific filtered files:
#'   - `Bacteria_only.txt`
#'   - `Archaea_only.txt`
#'   - `Eukaryota_only.txt`
#' - Also saves a complete filtered table: `ALL_baseline_phylo.txt`.
#'
#' Metadata is type-converted for future integration with phyloseq or other tools. The function creates a subdirectory `META_DEC` within `output_dir` to save results.
#'
#' @return The function does not return an R object but saves filtered `.txt` files to disk.
#'
#' @import dplyr
#' @import readr
#'
#' @examples
#' \dontrun{
#' MetaphlanDataImportDecontam(
#'   data_path = "input/merged_abundance_table.txt",
#'   meta_path = "input/sample_metadata.csv",
#'   output_dir = "Output"
#' )
#' }
#'
#' @export

MetaphlanDataImportDecontam <- function(data_path, baselines_metadata, output_dir, remove_data_name = "",threshold) {
  if (is.list(data_path) && !is.null(data_path$datapath)) {
    data_path <- data_path$datapath
  }
  if (!is.character(data_path) || length(data_path) != 1 || !nzchar(data_path) || !file.exists(data_path)) {
    stop("MetaphlanDataImportDecontam: data_path must be a valid existing file path.")
  }

  data <- read.table(file = data_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

  colnames(data) <- gsub(remove_data_name, "", colnames(data))
  colnames(data) <- gsub("[^A-Za-z0-9]", "_", colnames(data))
  # remove all OTUs that have 0 counts in all columns
  # threshold <- 0.001
  data[data < threshold] <- 0 #non sono sicuro sia logico essendo abbondanze relative

  data <- data %>%
    mutate(sum = rowSums(across(where(is.numeric)))) %>%
    filter(sum > 0)

  data <- data[, -dim(data)[2]]
  output_folder <- paste0(output_dir, "/META_DEC/")
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  write.table(data, file = paste0(output_folder, "ALL_baseline_phylo.txt"), sep = "\t", quote = FALSE)
  # Filter rows that contain 'k__Bacteria'
  bacteria_data <- data %>%
    filter(grepl("k__Bacteria", clade_name))

  # Write the filtered data to a new file
  write.table(bacteria_data, file = paste0(output_folder, "Bacteria_only.txt"), sep = "\t", quote = FALSE)

  # Filter rows that contain 'k__Eukaryota'
  eukaryota_data <- data %>%
    filter(grepl("k__Eukaryota", clade_name))

  # Write the filtered data to a new file
  write.table(eukaryota_data, file = paste0(output_folder, "Eukaryota_only.txt"), sep = "\t", quote = FALSE)

  # Filter rows that contain 'k__Archaea'
  archaea_data <- data %>%
    filter(grepl("k__Archaea", clade_name))

  # Write the filtered data to a new file
  write.table(archaea_data, file = paste0(output_folder, "/Archaea_only.txt"), sep = "\t", quote = FALSE)
}
