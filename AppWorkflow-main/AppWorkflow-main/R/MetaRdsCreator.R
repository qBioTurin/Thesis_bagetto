#' @title Create Phyloseq Objects from MetaPhlAn Data and Save RDS Files
#'
#' @description
#' This function imports MetaPhlAn relative abundance data and sample metadata,
#' merges taxonomy with NCBI taxids, reconstructs a phyloseq object, and saves
#' full and domain-specific `.rds` files (Bacteria, Archaea, Eukaryota).
#' It also returns taxonomic identifiers at a specified level for supervised decontamination.
#'
#' @param metadata_path Path to the metadata CSV file with a column named `id` corresponding to sample IDs.
#' @param data_path Path to the MetaPhlAn abundance table (tab-delimited `.txt` or `.tsv` file).
#' @param taxid_path Path to a local table with `path` (full taxonomic path) and `taxid` columns for mapping.
#' @param out_dir Output directory to save the `.rds` files.
#' @param bacteria_level Taxonomic level to extract (e.g., `"Phylum"` or `"Genus"`) from the Bacteria domain.
#' @param archaea_level Taxonomic level to extract from the Archaea domain.
#' @param eukaryota_level Taxonomic level to extract from the Eukaryota domain.
#'
#' @details
#' Steps performed:
#' - Reads and preprocesses MetaPhlAn abundance table.
#' - Re-weights taxa using UNCLASSIFIED proportions.
#' - Parses and separates taxonomy into columns.
#' - Maps taxa to `taxid` using a local lookup table.
#' - Merges abundances with taxonomic annotations.
#' - Builds a `phyloseq` object with OTU table, taxonomy, tree, and sample metadata.
#' - Removes features with total abundance = 1 (often artifacts of MetaPhlAn normalization).
#' - Saves:
#'   - `All_meta_phylo.rds`
#'   - `Bacteria_meta_phylo.rds`
#'   - `Eukaryota_meta_phylo.rds`
#'   - `Archaea_meta_phylo.rds`
#'
#' Returns a list of vectors with taxa names (e.g., phyla or genera) for Bacteria, Archaea, and Eukaryota,
#' useful for supervised decontamination or taxonomic filtering.
#'
#' @return A named list of character vectors with unique taxon names for each domain:
#' \describe{
#'   \item{bacteria}{Vector of taxa (e.g. phyla) from Bacteria domain}
#'   \item{archaea}{Vector of taxa from Archaea domain}
#'   \item{eukaryota}{Vector of taxa from Eukaryota domain}
#' }
#'
#' @import dplyr
#' @import tibble
#' @import stringr
#' @import tidyr
#' @importFrom ape rtree
#' @importFrom phyloseq phyloseq otu_table tax_table sample_data merge_phyloseq subset_taxa prune_taxa taxa_names
#'
#' @examples
#' \dontrun{
#' levels_list <- MetaRdsCreator(
#'   metadata_path = "metadata.csv",
#'   data_path = "filtered_abundance.txt",
#'   taxid_path = "taxid_lookup.txt",
#'   out_dir = "Output",
#'   bacteria_level = "Phylum",
#'   archaea_level = "Phylum",
#'   eukaryota_level = "Phylum"
#' )
#' levels_list$bacteria
#' }
#'
#' @export

#  baselines_metadata <- read.csv("/home/franky/Downloads/PRJNA/metadata4app.csv")
# #baselines_metadata <- baselines_metadata[, -1]
MetaRdsCreator <- function(baselines_metadata, data_path, taxid_path, out_dir, bacteria_level, archaea_level, eukaryota_level, string_rem = "") {
  print(paste0("baseline_metadata: ", (baselines_metadata)))
  print(paste0("data_path: ", data_path))
  print(paste0("taxid_path: ", taxid_path))
  print(paste0("out_dir: ", out_dir))

  phylum_list <- function(phy, level) {
    tax_df <- as.data.frame(phy@tax_table)

    if (level == "Phylum") {
      out_list <- unique(na.omit(tax_df$Phylum))
    } else if (level == "Genus") {
      out_list <- unique(na.omit(tax_df$Genus))
    } else {
      stop("Livello tassonomico non supportato.")
    }

    return(out_list)
  }
  normalize_domain <- function(otu, reference_taxon) {
    mat <- as(otu, "matrix")
    if (!reference_taxon %in% rownames(mat)) {
      stop(paste("Taxon", reference_taxon, "non trovato nella OTU table"))
    }
    ref_values <- mat[reference_taxon, ]
    scaling_factors <- ifelse(ref_values != 0, 1 / ref_values, 0)
    mat_scaled <- sweep(mat, 2, scaling_factors, `*`)
    otu_out <- otu_table(mat_scaled, taxa_are_rows = taxa_are_rows(otu))
    return(otu_out)
  }


  baselines_metadata <- as.data.frame(baselines_metadata)

  # Carica tabella delle abbondanze
  df_old <- read.table(
    file = data_path,
    header = TRUE,
    sep = "\t",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Rimuovi la riga UNCLASSIFIED dalla tabella
  df_old <- df_old[df_old$clade_name != "UNCLASSIFIED", ]

  # TRY
  ####################################################################
  Bacteria_row <- df_old[df_old$clade_name == "k__Bacteria", ]
  if (nrow(Bacteria_row) == 0) {
    Bacteria_row <- df_old[1, , drop = FALSE]
    num_cols <- sapply(Bacteria_row, is.numeric)
    Bacteria_row[, num_cols] <- 0
    Bacteria_row$clade_name <- "k__Bacteria"
  }

  Archaea_row <- df_old[df_old$clade_name == "k__Archaea", ]
  if (nrow(Archaea_row) == 0) {
    Archaea_row <- df_old[1, , drop = FALSE]
    num_cols <- sapply(Archaea_row, is.numeric)
    Archaea_row[] <- 0
    Archaea_row$clade_name <- "k__Archaea"
  }

  Eukaryota_row <- df_old[df_old$clade_name == "k__Eukaryota", ]
  if (nrow(Eukaryota_row) == 0) {
    Eukaryota_row <- df_old[1, , drop = FALSE]
    num_cols <- sapply(Eukaryota_row, is.numeric)
    Eukaryota_row[] <- 0
    Eukaryota_row$clade_name <- "k__Eukaryota"
  }
  df_old$Domain <- sapply(strsplit(df_old$clade_name, "\\|"), function(x) {
    sub("k__", "", x[1])
  })
  # Unisci le righe Bacteria_row, Archaea_row ed Eukaryota_row in un'unica tabella
  domain_rows <- rbind(Bacteria_row, Archaea_row, Eukaryota_row)
  # Porta ogni colonna numerica (escluse le prime due colonne) a somma 100 per ciascuna riga
  num_cols <- sapply(domain_rows, is.numeric)

  # 2. Calcola il fattore di moltiplicazione per ciascuna colonna numerica
  factors <- sapply(names(domain_rows)[num_cols], function(col) {
    col_sum <- sum(domain_rows[[col]])
    if (col_sum == 0) {
      return(0) # o 0 se preferisci
    } else {
      return(100 / col_sum)
    }
  })

  # 3. Crea una riga con i fattori di moltiplicazione
  # Inizializza un vettore con tutti i valori NA
  new_row <- as.list(rep(NA, ncol(domain_rows)))
  names(new_row) <- names(domain_rows)

  # Inserisci i fattori nelle colonne numeriche
  for (col in names(factors)) {
    new_row[[col]] <- factors[[col]]
  }


  new_row$clade_name <- "Scaling_Factor"
  new_row$Domain <- NA

  num_cols <- sapply(domain_rows, is.numeric)
  factors <- sapply(names(domain_rows)[num_cols], function(col) {
    col_sum <- sum(domain_rows[[col]])
    if (col_sum == 0) {
      return(0)
    } else {
      return(100 / col_sum)
    }
  })

  # only numeric factors
  num_cols_df_old <- sapply(df_old, is.numeric)

  # all elements of df_old
  for (col in names(df_old)[num_cols_df_old]) {
    if (col %in% names(factors)) {
      factor <- factors[[col]]
      if (factor != 0) {
        df_old[[col]] <- df_old[[col]] * factor
      } else {
        # valori invariati
      }
    }
  }


  ####################################################################

  # unclassified_row <- df_old[df_old$clade_name == "UNCLASSIFIED", ]
  # Applica la formula a tutte le colonne numeriche (escludi)
  # for (col in colnames(df_old)[-1]) {
  #   df_old[[col]] <- as.numeric(df_old[[col]]) * (1 + as.numeric(unclassified_row[[col]]) / 100)
  # }
  df <- tibble(taxonomy = df_old[[1]])

  # Pulizia tassonomia
  df_clean <- df %>%
    mutate(taxonomy_clean = str_replace_all(taxonomy, "[a-z]__", "")) %>%
    separate(
      taxonomy_clean,
      into = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Strain"),
      sep = "\\|",
      fill = "right"
    )
  # Carica file locale con path -> taxid
  local_taxids <- read_tsv(
    taxid_path,
    col_names = TRUE
  )
  # Abbina con ricerca locale
  df_clean <- df_clean %>%
    left_join(local_taxids, by = c("taxonomy" = "path")) %>%
    dplyr::rename(TaxID = taxid)

  df_old <- df_old %>% dplyr::rename(taxonomy = clade_name)
  df_non_na <- df_clean %>% filter(!is.na(TaxID))
  final_df <- left_join(df_non_na, df_old, by = "taxonomy")

  final_df <- final_df %>% select(-c("Domain"))
  abundance_cols <- setdiff(colnames(final_df), c(
    "taxonomy", "Kingdom", "Phylum", "Class", "Order",
    "Family", "Genus", "Species", "Strain", "TaxID"
  ))

  final_df <- tibble::as_tibble(final_df)
  data <- final_df %>%
    group_by(TaxID) %>%
    summarise(across(all_of(abundance_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")



  taxon <- final_df %>%
    group_by(TaxID) %>%
    dplyr::slice(1) %>%
    dplyr::select(TaxID, Kingdom, Phylum, Class, Order, Family, Genus, Species, Strain)

  # Crea tabella delle abbondanze
  data <- data %>%
    column_to_rownames("TaxID") %>%
    as.data.frame()

  # Rimuovi colonne con tutti i valori NA
  # data <- data[, !colnames(data) %in% c("NEG", "POS"), drop = FALSE]
  data[] <- lapply(data, function(x) round(as.numeric(as.character(x)) / 100, 5))



  # Tabella tassonomica
  taxon <- as.data.frame(taxon)
  rownames(taxon) <- taxon$TaxID
  taxon <- taxon %>% select(-TaxID)

  # Crea oggetti phyloseq
  otu <- otu_table(as.matrix(data), taxa_are_rows = TRUE)
  colnames(otu) <- gsub(string_rem, "", colnames(otu))
  taxon[] <- lapply(taxon, function(x) {
    if (is.character(x)) gsub("_", " ", x) else x
  })
  tax <- tax_table(as.matrix(taxon))
  ALL_baselines_meta <- phyloseq(otu, tax)

  TREE_baselines_meta <- rtree(ntaxa(ALL_baselines_meta), rooted = TRUE, tip.label = taxa_names(ALL_baselines_meta))
  ALL_baselines_meta <- phyloseq::merge_phyloseq(ALL_baselines_meta, TREE_baselines_meta)
  otu_mat <- as(otu_table(ALL_baselines_meta), "matrix")
  taxa_sums <- rowSums(otu_mat)
  taxa_to_keep <- taxa_names(ALL_baselines_meta)[taxa_sums != 1]
  ALL_baselines_meta <- prune_taxa(taxa_to_keep, ALL_baselines_meta)
  otu <- ALL_baselines_meta@otu_table

  # Filtra i metadata
  
  colnames(otu_table(ALL_baselines_meta)) <- gsub("[^A-Za-z0-9]", "_", colnames(otu_table(ALL_baselines_meta)))
  baselines_metadata$id <- gsub("[^A-Za-z0-9]", "_", baselines_metadata$id)
  sample_ids_meta <- colnames(otu_table(ALL_baselines_meta))
  meta_ids_meta <- baselines_metadata$id

  rownames(baselines_metadata) <- baselines_metadata$id
  # print(class(baselines_metadata))
  # print(colnames(baselines_metadata))
  # print(str(baselines_metadata$id))

  # print(class(sample_ids_meta))
  # print(str(sample_ids_meta))

    cat("sample_ids_meta:", paste(sample_ids_meta, collapse=", "), "\n")
    cat("meta_ids_meta:", paste(meta_ids_meta, collapse=", "), "\n")


  sampledata <- baselines_metadata %>%
    filter(id %in% sample_ids_meta) %>%
    arrange(factor(id, levels = sample_ids_meta))
  # Associa sample_data
  baselines_sampledata <- sample_data(
    data.frame(
      sampledata,
      row.names = sampledata$id,
      stringsAsFactors = FALSE
    )
  )
  # Rimuovi colonne della OTU non presenti nei metadata
  missing_ids <- setdiff(sample_ids_meta, sampledata$id)
  if (length(missing_ids) > 0) {
    otu <- otu_table(otu[, !colnames(otu) %in% missing_ids], taxa_are_rows = TRUE)
    baselines_sampledata <- sample_data(
      data.frame(
        sampledata,
        row.names = sampledata$id,
        stringsAsFactors = FALSE
      )
    )
    ALL_baselines_meta <- phyloseq(otu, tax, TREE_baselines_meta, baselines_sampledata)
  }
  ALL_baselines_meta <- merge_phyloseq(ALL_baselines_meta, sample_data(baselines_metadata))


  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  saveRDS(
    ALL_baselines_meta,
    file = file.path(out_dir, "All_meta_phylo.rds")
  )

  if (any(tax_table(ALL_baselines_meta)[, "Kingdom"] == "Bacteria")) {
    onlyBacteria <- subset_taxa(ALL_baselines_meta, Kingdom == "Bacteria")
    onlyBacteria@otu_table <- normalize_domain(onlyBacteria@otu_table, "2")
    saveRDS(onlyBacteria, file = file.path(out_dir, "Bacteria_meta_phylo.rds"))
  }

  if (any(tax_table(ALL_baselines_meta)[, "Kingdom"] == "Eukaryota")) {
    onlyEukaryota <- subset_taxa(ALL_baselines_meta, Kingdom == "Eukaryota")
    onlyEukaryota@otu_table <- normalize_domain(onlyEukaryota@otu_table, "2759")
    saveRDS(onlyEukaryota, file = file.path(out_dir, "Eukaryota_meta_phylo.rds"))
  }

  if (any(tax_table(ALL_baselines_meta)[, "Kingdom"] == "Archaea")) {
    onlyArchaea <- subset_taxa(ALL_baselines_meta, Kingdom == "Archaea")
    onlyArchaea@otu_table <- normalize_domain(onlyArchaea@otu_table, "2157")
    saveRDS(onlyArchaea, file = file.path(out_dir, "Archaea_meta_phylo.rds"))
  }

  # Verifica e crea i livelli tassonomici solo se il dominio esiste
  if (any(tax_table(ALL_baselines_meta)[, "Kingdom"] == "Bacteria")) {
    bact_level <- phylum_list(subset_taxa(ALL_baselines_meta, Kingdom == "Bacteria"), bacteria_level)
  } else {
    bact_level <- character(0)
    message("Nessun taxon Bacteria trovato.")
  }

  if (any(tax_table(ALL_baselines_meta)[, "Kingdom"] == "Archaea")) {
    arch_level <- phylum_list(subset_taxa(ALL_baselines_meta, Kingdom == "Archaea"), archaea_level)
  } else {
    arch_level <- character(0)
    message("Nessun taxon Archaea trovato.")
  }

  if (any(tax_table(ALL_baselines_meta)[, "Kingdom"] == "Eukaryota")) {
    euk_level <- phylum_list(subset_taxa(ALL_baselines_meta, Kingdom == "Eukaryota"), eukaryota_level)
  } else {
    euk_level <- character(0)
    message("Nessun taxon Eukaryota trovato.")
  }

  return(list(list_of_decontam = (list(
    bacteria = bact_level,
    archaea = arch_level,
    eukaryota = euk_level
  ))))
}
