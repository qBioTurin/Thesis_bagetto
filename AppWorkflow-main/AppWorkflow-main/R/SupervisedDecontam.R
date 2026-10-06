#' Supervised Decontamination and Alpha Diversity Calculation
#'
#' This function performs supervised decontamination on microbiome phyloseq objects
#' for Bacteria, Archaea, and Eukaryota domains. It filters taxa based on a selected list
#' and taxonomic level (Phylum or Genus), calculates alpha diversity metrics,
#' removes strain-level information if using MetaPhlAn pipeline, and saves results.
#'
#' @param bacteria_selected Character vector of taxa to select for Bacteria domain.
#' @param archaea_selected Character vector of taxa to select for Archaea domain.
#' @param eukaryota_selected Character vector of taxa to select for Eukaryota domain.
#' @param bacteria_level Character string indicating taxonomic level for Bacteria ("Phylum" or "Genus").
#' @param archaea_level Character string indicating taxonomic level for Archaea ("Phylum" or "Genus").
#' @param eukaryota_level Character string indicating taxonomic level for Eukaryota ("Phylum" or "Genus").
#' @param pipeline Character string indicating which pipeline was used ("kraken" or "metaphlan").
#' @param output_folder Character string specifying the path where outputs will be saved.
#' @param bacteria_spike_taxids Character vector of bacterial taxids (species-level entries) to force-remove as spike-ins.
#'
#' @return Invisibly returns NULL. Results are saved as RDS and CSV files in the specified output folder.
#'
#' @details
#' - Reads phyloseq objects for each domain from the provided output folder.
#' - Filters taxa based on the specified taxonomic level and selected taxa list.
#' - For Kraken pipeline, applies an abundance threshold of 0.001 to remove low abundance taxa.
#' - Calculates alpha diversity metrics (Observed, Shannon, Simpson, Chao1) and merges with sample metadata.
#' - For MetaPhlAn pipeline, strain-level taxonomic information is removed from results.
#'
#' @examples
#' \dontrun{
#' SupervisedDecontam(
#'   bacteria_selected = c("Firmicutes", "Bacteroidetes"),
#'   archaea_selected = c("Methanobacteria"),
#'   eukaryota_selected = c("Fungi"),
#'   bacteria_level = "Phylum",
#'   archaea_level = "Phylum",
#'   eukaryota_level = "Phylum",
#'   pipeline = "kraken",
#'   output_folder = "/path/to/output/"
#' )
#' }
#'
#' @import phyloseq
#' @importFrom microbiome alpha
#' @importFrom vegan estimate_richness
#' @importFrom dplyr mutate filter across rowSums
#'
#' @export
SupervisedDecontam <- function(
    bacteria_selected, archaea_selected, eukaryota_selected,
    bacteria_level, archaea_level, eukaryota_level,
  pipeline, output_folder, decont_level = NULL,
  bacteria_spike_taxids = character(0)) {
  createAlpha <- function(baselines_dec, Domain, pipeline, output_folder) {
    alphaDir <- file.path(output_folder, "ALPHA")
    if (!dir.exists(alphaDir)) dir.create(alphaDir, recursive = TRUE)

    baselines_dec_metadata <- data.frame(baselines_dec@sam_data)

    if (pipeline == "kraken") {
      richness <- estimate_richness(baselines_dec,
        split = TRUE,
        measures = c("Observed", "Shannon", "Simpson", "Chao1")
      )
      richness <- data.frame(id = rownames(richness), richness)
      richness$id <- gsub("\\.", "-", richness$id)
      metadata <- merge(richness, baselines_dec_metadata, all.y = TRUE, by = "id")
      write.csv(metadata, file = file.path(alphaDir, paste0(Domain, "_alpha_metadata.csv")), row.names = FALSE)
    } else if (pipeline == "metaphlan") {
      tab <- microbiome::alpha(baselines_dec,
        index = c("Observed", "diversity_shannon", "diversity_gini_simpson", "Chao1")
      )
      tab <- data.frame(id = rownames(tab), tab)
      metadata <- merge(tab, baselines_dec_metadata, all.y = TRUE, by = "id")
      write.csv(metadata, file = file.path(alphaDir, paste0(Domain, "_meta_alpha_metadata.csv")), row.names = FALSE)
    }
  }

  decontam <- function(baselines_file, decontam_list, level, Domain, output_folder, pipeline) {
    baselines <- readRDS(baselines_file)
    if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

    if (ntaxa(baselines) == 0 || nsamples(baselines) == 0) {
      warning(paste("Baselines empty for", Domain))
      return(NULL)
    }

    tax <- data.frame(tax_table(baselines))

    # Remove user-selected spike-in taxids only for Bacteria, then apply manual selection.
    forced_taxids <- if (Domain == "Bacteria") as.character(bacteria_spike_taxids) else character(0)
    present_forced <- intersect(rownames(tax), forced_taxids)
    if (length(present_forced) > 0) {
      keep_taxa <- setdiff(rownames(tax), present_forced)
      if (length(keep_taxa) == 0) {
        warning(paste("All taxa are spike-ins for", Domain))
        return(NULL)
      }
      baselines <- prune_taxa(keep_taxa, baselines)
      tax <- data.frame(tax_table(baselines))
    }

    # Now filter taxa by specified level and selected taxa list (manual selection)
    if (level == "Phylum") {
      baselines_ds <- subset_taxa(baselines, !is.na(Phylum) & !Phylum %in% c("", "uncharacterized", "NA"))
      supervised_tax <- tax[tax$Phylum %in% decontam_list, , drop = FALSE]
    } else if (level == "Genus") {
      baselines_ds <- subset_taxa(baselines, !is.na(Genus) & !Genus %in% c("", "uncharacterized", "NA"))
      supervised_tax <- tax[tax$Genus %in% decontam_list, , drop = FALSE]
    } else {
      stop("Level must be 'Phylum' or 'Genus'")
    }

    if (nrow(supervised_tax) == 0) {
      warning(paste("No taxa found in", Domain))
      return(NULL)
    }

    supervised <- prune_taxa(rownames(supervised_tax), baselines)

    if (pipeline == "kraken" && !is.null(decont_level)) {
      rel <- data.frame(abundances(supervised, transform = "compositional"))
      rel[rel < decont_level] <- 0
      rel$sum <- rowSums(rel)
      rel <- rel[rel$sum > 0, ]
      if (nrow(rel) == 0) {
        warning(paste("All OTU removed for", Domain))
        return(NULL)
      }
      final <- prune_taxa(rownames(rel), supervised)
    } else {
      final <- supervised
    }

    if (ntaxa(final) == 0) {
      warning(paste("File empty for", Domain))
      return(NULL)
    }

    final_name <- if (pipeline == "metaphlan") {
      file.path(output_folder, paste0(Domain, "_Supervised_meta_decontam.rds"))
    } else {
      file.path(output_folder, paste0(Domain, "_Supervised_decontam.rds"))
    }

    saveRDS(final, file = final_name)
    return(final)
  }

  output_folderN <- ifelse(pipeline == "kraken",
    file.path(output_folder, "SUPERVISED_DEC"),
    file.path(output_folder, "META_SUPERVISED_DEC")
  )
  if (!dir.exists(output_folderN)) dir.create(output_folderN, recursive = TRUE)

  domains <- list(
    Bacteria = list(
      file = if (pipeline == "kraken") {
        file.path(output_folder, "DESEQ_RDS", "Bact_DeSeq.rds")
      } else {
        file.path(output_folder, "META_RDS", "Bacteria_meta_phylo.rds")
      },
      level = bacteria_level,
      list = bacteria_selected
    ),
    Archaea = list(
      file = if (pipeline == "kraken") {
        file.path(output_folder, "DESEQ_RDS", "Archaea_DeSeq.rds")
      } else {
        file.path(output_folder, "META_RDS", "Archaea_meta_phylo.rds")
      },
      level = archaea_level,
      list = archaea_selected
    ),
    Eukaryota = list(
      file = if (pipeline == "kraken") {
        file.path(output_folder, "DESEQ_RDS", "Eukaryota_DeSeq.rds")
      } else {
        file.path(output_folder, "META_RDS", "Eukaryota_meta_phylo.rds")
      },
      level = eukaryota_level,
      list = eukaryota_selected
    )
  )

  for (Domain in names(domains)) {
    dom <- domains[[Domain]]

    if (length(dom$list) == 0 || !file.exists(dom$file)) {
      message(paste("Skipping", Domain, "- no taxa selected or file not found"))
      next
    }

    result <- tryCatch(
      decontam(dom$file, dom$list, dom$level, Domain, output_folderN, pipeline),
      error = function(e) {
        message(paste("Error during decontam for", Domain, ":", e$message))
        return(NULL)
      }
    )

    if (!is.null(result)) {
      if (pipeline == "metaphlan") {
        result_no_strain <- removeStrain(result, output_folderN, paste0(Domain, "_Supervised_meta_no_strain.rds"))
        createAlpha(result_no_strain, Domain, pipeline, output_folder)
      } else {
        createAlpha(result, Domain, pipeline, output_folder)
      }
    } else {
      message(paste("Decontam not executed for", Domain))
    }
  }
  invisible(NULL)
}
