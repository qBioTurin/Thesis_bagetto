#' @title Import and Process BIOM and Metadata Files
#'
#' @description
#' This function imports a BIOM file and a corresponding metadata CSV file, constructs a `phyloseq` object, assigns taxonomy and OTU tables,
#' builds a random phylogenetic tree, associates metadata, and saves `.rds` objects for each taxonomic kingdom (Bacteria, Archaea, Eukaryota).
#'
#' @param biom_path Path to the BIOM file.
#' @param meta_path Path to the metadata CSV file.
#' @param output_folder Path to the output directory where the `.rds` files will be saved.
#'
#' @return A character vector containing the sample IDs (column names) from the metadata.
#'
#' @details
#' The function removes the first column from the metadata (typically an index column), converts column types using `readr::type_convert()`,
#' and assigns standardized column names to the taxonomy table. It creates a random rooted phylogenetic tree using `ape::rtree()` to complete the `phyloseq` object.
#'
#' It then saves the full phyloseq object and subsets by kingdom:
#' - `ALL_baseline_phylo.rds`: full dataset
#' - `Bact_baseline_phylo.rds`: Bacteria only
#' - `Eukaryotes_baseline_phylo.rds`: Eukaryota only
#' - `Archaea_baseline_phylo.rds`: Archaea only
#'
#' @import phyloseq readr dplyr ape
#' @export


# KrakenDataImport_ambiguous <- function(biom_path, baselines_metadata, output_folder, remove_data_name = "") {
#   fill_taxa_gaps <- function(taxa_df) {
#     levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

#     for (i in 2:length(levels)) {
#       current <- levels[i]
#       previous <- levels[i - 1]

#       # find rows where current level is NA or ""
#       empty_idx <- which(is.na(taxa_df[[current]]) | taxa_df[[current]] == "")
#       # fill with "unclassified <previous level>"
#       for (j in empty_idx) {
#         prev_val <- taxa_df[[previous]][j]
#         if (grepl("unclassified", prev_val, ignore.case = TRUE)) {
#           taxa_df[[current]][j] <- prev_val
#         } else {
#           taxa_df[[current]][j] <- paste0("unclassified ", prev_val)
#         }
#       }
#     }
#     return(taxa_df)
#   }
#   baselines_metadata <- as_tibble(baselines_metadata)
#   # print(baselines_metadata)

#   baselines <- import_biom(biom_path)

#   # # Import Metadata
#   # baselines_metadata <- read.csv(meta_path,
#   #                                header = TRUE,
#   #                                sep = ",",
#   #                                na = c("", " ", "NA"),
#   #                                check.names = TRUE)
#   # baselines_metadata <- baselines_metadata[,-1]  # Rimuove la prima colonna

#   # Convert data types
#   # baselines_metadata <- readr::type_convert(baselines_metadata)

#   # Prep taxa table
#   baselines@tax_table@.Data <- substring(baselines@tax_table@.Data, 4)
#   colnames(baselines@tax_table@.Data) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
#   TAXA_baselines <- tax_table(baselines@tax_table@.Data)

#   # Prep OTU table
#   OTU_baselines <- otu_table(baselines@otu_table@.Data, taxa_are_rows = TRUE)

#   colnames(OTU_baselines) <- gsub(remove_data_name, "", colnames(OTU_baselines))

#   # Create phyloseq object
#   ALL_baselines <- phyloseq(OTU_baselines, TAXA_baselines)

#   otu <- data.frame(otu_table(ALL_baselines))
#   taxa <- data.frame(tax_table(ALL_baselines))

#   num_rows_with_empty <- sum(apply(taxa, 1, function(row) any(is.na(row) | row == "")))
#   if (num_rows_with_empty > 0) {
#     filled_taxa <- fill_taxa_gaps(as.data.frame(ALL_baselines@tax_table))
#     ALL_baselines@tax_table <- tax_table(as.matrix(filled_taxa))
#   }
#   # Create and add phylogenetic tree
#   TREE_baselines <- ape::rtree(ntaxa(ALL_baselines), rooted = TRUE, tip.label = taxa_names(ALL_baselines))
#   suppressMessages({
#     ALL_baselines <- phyloseq::merge_phyloseq(ALL_baselines, TREE_baselines)
#   })
#   # Associate metadata without filtering
#   sampledata <- baselines_metadata[baselines_metadata$id %in% colnames(ALL_baselines@otu_table), ]
#   sampledata <- sampledata %>% arrange(factor(id, levels = colnames(ALL_baselines@otu_table)))

#   sampledata <- baselines_metadata[baselines_metadata$id %in% colnames(ALL_baselines@otu_table), ]
#   ## This section needs to be removede here only for dataset
#   meta_ids <- baselines_metadata$id
#   sample_ids <- colnames(ALL_baselines@otu_table)
#   col_to_remove <- setdiff(sample_ids, meta_ids)
#   ALL_baselines@otu_table <- otu_table(ALL_baselines@otu_table[, !colnames(ALL_baselines@otu_table) %in% col_to_remove], taxa_are_rows = TRUE)
#   ##########################################################
#   baselines_sampledata <- sample_data(data.frame(sampledata,
#     row.names = sample_names(ALL_baselines@otu_table),
#     stringsAsFactors = FALSE
#   ))
# suppressMessages({
#   ALL_baselines <- merge_phyloseq(ALL_baselines, baselines_sampledata)
# })
#   # Save phyloseq object with all kingdoms
#   saveRDS(ALL_baselines, file = gsub(" ", "", paste(output_folder, "ALL_baseline_phylo.rds")))

#   # Subset and save by Kingdom
#   saveRDS(subset_taxa(ALL_baselines, Kingdom == "Bacteria"), file = gsub(" ", "", paste(output_folder, "Bact_baseline_phylo.rds")))
#   saveRDS(subset_taxa(ALL_baselines, Kingdom == "Eukaryota"), file = gsub(" ", "", paste(output_folder, "Eukaryotes_baseline_phylo.rds")))
#   saveRDS(subset_taxa(ALL_baselines, Kingdom == "Archaea"), file = gsub(" ", "", paste(output_folder, "Archaea_baseline_phylo.rds")))
#   return(colnames(baselines_metadata))
# }
# biom_path <- "/home/franky/Downloads/PRJNA/PRJNA_bracken92.biom"
# baselines_metadata <- read.csv("/home/franky/Downloads/PRJNA/metadata4app.csv",
#   header = TRUE,
#   sep = ",",
#   na = c("", " ", "NA"),
#   check.names = TRUE
# )
# remove_data_name <- "_bracken"
# output_folder <- "/home/franky/Desktop/PRJNA/RDS"
KrakenDataImport <- function(biom_path, baselines_metadata, output_folder, remove_data_name = "") {
  
    fill_taxa_gaps <- function(taxa_df) {
        levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
        for (i in 2:length(levels)) {
            current <- levels[i]
            previous <- levels[i - 1]
            empty_idx <- which(is.na(taxa_df[[current]]) | taxa_df[[current]] == "")
            for (j in empty_idx) {
                prev_val <- taxa_df[[previous]][j]
                if (grepl("unclassified", prev_val, ignore.case = TRUE)) {
                    taxa_df[[current]][j] <- prev_val
                } else {
                    taxa_df[[current]][j] <- paste0("unclassified ", prev_val)
                }
            }
        }
        return(taxa_df)
    }

    baselines_metadata <- tibble::as_tibble(baselines_metadata)
    library(phyloseq)
    # Import biom file
    baselines <- import_biom(biom_path)

    # Prepare the taxa table
    baselines@tax_table@.Data <- substring(baselines@tax_table@.Data, 4)
    colnames(baselines@tax_table@.Data) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    TAXA_baselines <- phyloseq::tax_table(baselines@tax_table@.Data)

    # Prepare OTU table
    OTU_baselines <- phyloseq::otu_table(baselines@otu_table@.Data, taxa_are_rows = TRUE)
    # Rimuovi stringa specifica e sostituisci caratteri speciali con _ nei nomi dei campioni
    colnames(OTU_baselines) <- gsub(remove_data_name, "", colnames(OTU_baselines))
    colnames(OTU_baselines) <- gsub("[^A-Za-z0-9]", "_", colnames(OTU_baselines))

    # Create phyloseq object
    ALL_baselines <- phyloseq::phyloseq(OTU_baselines, TAXA_baselines)

    otu <- data.frame(phyloseq::otu_table(ALL_baselines))
    taxa <- data.frame(phyloseq::tax_table(ALL_baselines))

    num_rows_with_empty <- sum(apply(taxa, 1, function(row) any(is.na(row) | row == "")))
    if (num_rows_with_empty > 0) {
        filled_taxa <- fill_taxa_gaps(as.data.frame(ALL_baselines@tax_table))
        ALL_baselines@tax_table <- phyloseq::tax_table(as.matrix(filled_taxa))
    }

    # Create phylogenetic tree
    TREE_baselines <- ape::rtree(phyloseq::ntaxa(ALL_baselines), rooted = TRUE, tip.label = phyloseq::taxa_names(ALL_baselines))
    suppressMessages({
        ALL_baselines <- phyloseq::merge_phyloseq(ALL_baselines, TREE_baselines)
    })

    # Associate metadata
    sampledata <- dplyr::filter(baselines_metadata, id %in% colnames(phyloseq::otu_table(ALL_baselines)))
    sampledata <- dplyr::arrange(sampledata, base::factor(id, levels = colnames(phyloseq::otu_table(ALL_baselines))))

    meta_ids <- baselines_metadata$id
    sample_ids <- colnames(phyloseq::otu_table(ALL_baselines))
    col_to_remove <- setdiff(sample_ids, meta_ids)
    ALL_baselines@otu_table <- phyloseq::otu_table(
        phyloseq::otu_table(ALL_baselines)[, !colnames(phyloseq::otu_table(ALL_baselines)) %in% col_to_remove],
        taxa_are_rows = TRUE
    )

    baselines_sampledata <- phyloseq::sample_data(
        data.frame(sampledata, row.names = phyloseq::sample_names(ALL_baselines@otu_table), stringsAsFactors = FALSE)
    )
    suppressMessages({
        ALL_baselines <- phyloseq::merge_phyloseq(ALL_baselines, baselines_sampledata)
    })

    # Save phyloseq object
    saveRDS(ALL_baselines, file = file.path(output_folder, "ALL_baseline_phylo.rds"))


    # Save Bacteria
    if (any(phyloseq::tax_table(ALL_baselines)[, "Kingdom"] == "Bacteria", na.rm = TRUE)) {
        bact <- phyloseq::subset_taxa(ALL_baselines, Kingdom == "Bacteria")
        if (inherits(bact, "phyloseq") && phyloseq::ntaxa(bact) > 0) {
            saveRDS(bact, file = file.path(output_folder, "Bact_baseline_phylo.rds"))
        } else {
            warning("RDS containing Bacteria not created due to lack of data")
        }
    } else {
        warning("No Bacteria kingdom found in tax_table; skipping Bact_baseline_phylo.rds")
    }

    # Save Eukaryota
    if (any(phyloseq::tax_table(ALL_baselines)[, "Kingdom"] == "Eukaryota", na.rm = TRUE)) {
        euk <- phyloseq::subset_taxa(ALL_baselines, Kingdom == "Eukaryota")
        if (inherits(euk, "phyloseq") && phyloseq::ntaxa(euk) > 0) {
            saveRDS(euk, file = file.path(output_folder, "Eukaryotes_baseline_phylo.rds"))
        } else {
            warning("RDS containing Eukaryota not created due to lack of data")
        }
    } else {
        warning("No Eukaryota kingdom found in tax_table; skipping Eukaryotes_baseline_phylo.rds")
    }

    # Save Archaea
    if (any(phyloseq::tax_table(ALL_baselines)[, "Kingdom"] == "Archaea", na.rm = TRUE)) {
        arch <- phyloseq::subset_taxa(ALL_baselines, Kingdom == "Archaea")
        if (inherits(arch, "phyloseq") && phyloseq::ntaxa(arch) > 0) {
            saveRDS(arch, file = file.path(output_folder, "Archaea_baseline_phylo.rds"))
        } else {
            warning("RDS containing Archaea not created due to lack of data")
        }
    } else {
        warning("No Archaea kingdom found in tax_table; skipping Archaea_baseline_phylo.rds")
    }

    return(colnames(baselines_metadata))
}



# biom_path <- "/home/franky/Downloads/MS_newscript+results/MS_final.biom"
# meta_path <- "/home/franky/Downloads/MS_newscript+results/metadata_mav.csv"
# output_folder <- "/home/franky/Desktop/prove_MS/RDS"
# baselines_metadata <- read.csv(meta_path,
#   header = TRUE,
#   sep = ",",
#   na = c("", " ", "NA"),
#   check.names = TRUE
# )

# KrakenDataImport(biom_path, baselines_metadata, output_folder)
