SupervisedDecontam <- function(
  bacteria_selected, archaea_selected, eukaryota_selected,
  bacteria_level, archaea_level, eukaryota_level,
  pipeline, output_folder, decont_level = NULL
) {
  
  createAlpha <- function(baselines_dec, Domain, pipeline, output_folder) {
    alphaDir <- file.path(output_folder, "ALPHA")
    if (!dir.exists(alphaDir)) dir.create(alphaDir, recursive = TRUE)

    baselines_dec_metadata <- data.frame(baselines_dec@sam_data)

    if (pipeline == "kraken") {
      richness <- estimate_richness(baselines_dec, split = TRUE, measures = c("Observed", "Shannon", "Simpson", "Chao1"))
      richness <- data.frame(id = rownames(richness), richness)
      richness$id <- gsub("\\.", "-", richness$id)
      metadata <- merge(richness, baselines_dec_metadata, all.y = TRUE, by = "id")
      write.csv(metadata, file = file.path(alphaDir, paste0(Domain, "_alpha_metadata.csv")), row.names = FALSE)
    } else if (pipeline == "metaphlan") {
      tab <- microbiome::alpha(baselines_dec, index = c("Observed", "diversity_shannon", "diversity_gini_simpson", "Chao1"))
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
    
    # Filter taxa by specified level and selected taxa list
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
      # Calculate relative abundance and filter low abundance taxa
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
                           file.path(output_folder, "META_SUPERVISED_DEC"))
  if (!dir.exists(output_folderN)) dir.create(output_folderN, recursive = TRUE)

  domains <- list(
    Bacteria = list(file = if (pipeline == "kraken") file.path(output_folder, "DESEQ_RDS", "Bact_DeSeq.rds") else file.path(output_folder, "META_RDS", "Bacteria_meta_phylo.rds"),
                    level = bacteria_level,
                    list = bacteria_selected),
    Archaea = list(file = if (pipeline == "kraken") file.path(output_folder, "DESEQ_RDS", "Archaea_DeSeq.rds") else file.path(output_folder, "META_RDS", "Archaea_meta_phylo.rds"),
                   level = archaea_level,
                   list = archaea_selected),
    Eukaryota = list(file = if (pipeline == "kraken") file.path(output_folder, "DESEQ_RDS", "Eukaryota_DeSeq.rds") else file.path(output_folder, "META_RDS", "Eukaryota_meta_phylo.rds"),
                     level = eukaryota_level,
                     list = eukaryota_selected)
  )

  for (Domain in names(domains)) {
    result <- decontam(domains[[Domain]]$file, domains[[Domain]]$list, domains[[Domain]]$level, Domain, output_folderN, pipeline)
    if (!is.null(result)) {
      createAlpha(baselines_dec = result, Domain = Domain, pipeline=pipeline, output_folder=output_folder)
      if (pipeline == "metaphlan") {
        removeStrain(result, output_folderN, paste0(Domain, "_Supervised_meta_no_strain.rds"))
      }
    } else {
      message(paste("Decontam not executed for", Domain))
    }
  }
  invisible(NULL)
}
bacteria_selected = c("Actinomycetota", "Bacillota", "Bacteroidota", "Campylobacterota", 
                "Chlamydiota", "Chloroflexota", "Lentisphaerota", "Mycoplasmatota", 
                "Pseudomonadota", "Spirochaetota", "Synergistota", "Verrucomicrobiota", 
                "Acidobacteriota", "Candidatus Saccharibacteria", "Elusimicriobiota", 
                "Gemmatimonadota", "Ignavibacteriota", "Myxococcota", "Nitrospirota",
                "Planctomycetota", "Thermodesulfobacteriota")

archaea_selected = c("Euryarchaeota", "Thermoproteota", "Candidatus Korarchaeota", "Nanoarchaeota")

eukaryota_selected= c(  "Acremonium",	"Agaricales",	"Agaricomycetes",	"Aspergillus",
                "Aureobasidium",	"Byssochlamys",	"Candida", "Cladosporium",
                "Claroideoglomus",	"Clavispora", "Cyberlindnera", "Debaryomyces",
                "Geotrichum", "Hanseniaspora", "Hypocreales",	"Itersonilia", "Kazachstania",
                "Kazchstania",	"Kluyveromyces",	"Kurtzmaniella",	"Lachancea",	"Lecanoromycetes",
                "Malassezia",	"Malasseziales", "Metschnikowia", "Ogataea", "Penicillium",	"Pichia",
                "Polyporales",	"Rhizoplaca",	"Rhodotorula",	"Saccharomycetales",	"Schizosaccharomyces",
                "Starmerella", "Torulaspora",	"Trametes",	"Vishniacozyma",	"Wickerhamomyces",
                "Yamadazyma", "Yarrowia", "Saccharomyces")
pipeline = "kraken"
output_folder = "/home/franky/Desktop/ilario/"

  bacteria_level = "Phylum"
  archaea_level = "Phylum"
  eukaryota_level = "Genus"

  decont_level = 0.00001
