#' @title Remove Strain-Level Taxonomy and Clean Species Names in a Phyloseq Object
#' 
#' @description
#' This function processes a phyloseq object to remove the "Strain" taxonomic level if present,
#' cleans species names by removing genus name prefixes if duplicated, prunes taxa that do not
#' reach the species level, and saves the pruned phyloseq object to disk.
#' 
#' @param data A phyloseq object.
#' @param path Character string. Path to the directory where the pruned phyloseq object will be saved.
#' @param name Character string. Filename (including extension, e.g. "data_pruned.rds") to save the phyloseq object.
#' 
#' @return The pruned phyloseq object with strain-level removed and cleaned species names.
#' 
#' @details
#' - If the tax_table has fewer than 8 columns, it assumes strain information is missing and only removes taxa that do not reach species level.
#' - Species names starting with the genus name are cleaned by removing the genus prefix.
#' - The resulting phyloseq object is saved as an RDS file at the specified path with the given name.
#' 
#' @examples
#' \dontrun{
#' ps <- removeStrain(data = phyloseq_obj, path = "output/", name = "pruned_phyloseq.rds")
#' }
#' 
#' @import phyloseq
#' @export

removeStrain <- function(data, path, name) {
  if (!inherits(data, "phyloseq")) {
    stop("L'oggetto fornito non è un oggetto phyloseq.")
  }

  tax <- tax_table(data)
  if (ncol(tax) < 8) {
    warning("The tax_table does not contain strain information. Only entries not reaching the species level will be removed.")
  }

  tax_mat <- as.matrix(tax)

  # Rimuove il nome del genere dalla specie se presente
  if (ncol(tax_mat) >= 7) {
    genus <- tax_mat[, 6]
    species <- tax_mat[, 7]

    valid_idx <- !is.na(genus) & !is.na(species)
    
    for (i in which(valid_idx)) {
      genus_i <- genus[i]
      species_i <- species[i]
      if (startsWith(species_i, paste0(genus_i, " "))) {
        species[i] <- sub(paste0("^", genus_i, " "), "", species_i)
      }
    }

    tax_mat[, 7] <- species
    tax_table(data) <- tax_table(tax_mat)
  }

  keep <- !is.na(tax_mat[, 7]) & (ncol(tax_mat) < 8 | is.na(tax_mat[, 8]))
  taxa_to_keep <- taxa_names(data)[keep]
  data_pruned <- prune_taxa(taxa_to_keep, data)

  if (ncol(tax_table(data_pruned)) >= 8) {
    tax_table(data_pruned) <- tax_table(data_pruned)[, -8]
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  saveRDS(data_pruned, file = file.path(path, name))

  return(data_pruned)
}
