#' @title Search and Clean TaxIDs from MetaPhlAn Output Files
#'
#' @description
#' Reads all `.txt` files in a specified folder containing MetaPhlAn taxonomic paths and taxids,
#' cleans missing or malformed taxid entries by extracting the last taxon name from the path,
#' and merges all data into a single tab-delimited file.
#' 
#' @param meta_folder Path to the folder containing MetaPhlAn output `.txt` files.
#' @param output_folder Path to the folder where the merged and cleaned taxid file will be saved.
#' @param all Logical flag (default TRUE). If TRUE, replaces missing or malformed taxids by extracting 
#' the last taxon from the taxonomic path. If FALSE, filters out rows with missing or malformed taxids.
#'
#' @return The path to the saved merged taxid file (a tab-delimited text file).
#'
#' @details
#' Each MetaPhlAn file is expected to have at least two columns: the taxonomic path and the taxid.
#' Missing taxids or taxids ending with '|' are cleaned or removed depending on the `all` parameter.
#' The cleaned data from all files are combined and deduplicated before saving.
#' 
#' The output file is named `taxid_searcher.txt` inside the `output_folder`.
#'
#' @examples
#' \dontrun{
#' merged_taxid_file <- MetaSearchTaxid(
#'   meta_folder = "MetaPhlAn_outputs/",
#'   output_folder = "Processed_taxids/",
#'   all = TRUE
#' )
#' }
#'
#' @export

MetaSearchTaxid <- function(meta_folder, output_folder, all = TRUE) {
    modDataPath <- function(path) {
        parts <- unlist(strsplit(path, split = "\\|"))
        last_part <- tail(parts, 1)
        last_part <- sub("^[a-zA-Z]+__", "", last_part)
        # keep underscores instead of converting them to spaces; also normalize any spaces to underscores
        last_part <- gsub("\\s+", "_", last_part, perl = TRUE)
        last_part <- trimws(last_part)
        return(last_part)
    }
    output_folder <- paste0(output_folder, "/TAXID_Searcher/")
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }
    output_file <- paste0(output_folder, "/taxid_searcher.txt")
    file_list <- list.files(meta_folder, pattern = "\\.txt$", full.names = TRUE)
    all_data <- list()

    for (file in file_list) {
        print(paste("Processing file:", file))
        # Check if the file has more than 5 lines
        if (length(readLines(file)) > 5) {
            data <- read_tsv(file, skip = 4, col_names = TRUE)
            data <- data[, 1:2]
            colnames(data) <- c("path", "taxid")

            data$taxid <- as.character(data$taxid)
            missing_taxid <- is.na(data$taxid) | data$taxid == "" | endsWith(data$taxid, "|")

            if (all) {
                data$taxid[missing_taxid] <- sapply(data$path[missing_taxid], modDataPath)
            } else {
                data <- data[!missing_taxid, ]
            }

            # Aggiorna solo se ci sono righe rimanenti dopo il filtro
            if (nrow(data) > 0) {
                valid_taxid <- !is.na(data$taxid) & data$taxid != "" & !endsWith(data$taxid, "|")
                data$taxid[valid_taxid] <- sub(".*\\|", "", data$taxid[valid_taxid])
            }

            all_data[[file]] <- data
        }
    }

    merged_data <- do.call(rbind, all_data)
    merged_data <- unique(merged_data)

    # dir.create("input", showWarnings = FALSE, recursive = TRUE)
    write.table(merged_data, file = output_file, sep = "\t", row.names = FALSE, quote = FALSE)

    return(output_file)
}
