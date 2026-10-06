###########################################################################################################################
### MAIN FUNCTION
###########################################################################################################################
#' Process BIOM and Metadata Files with Decontamination and Phyloseq Conversion
#'
#' This function processes microbiome data using a Kraken pipeline. It imports
#' a BIOM file and associated metadata, performs DESeq2-based supervised decontamination,
#' and saves the results as `.rds` files.
#'
#' @param biom_file File input object (from `shiny::fileInput`) representing the BIOM file.
#' @param metadata_file File input object representing the metadata CSV file.
#' @param Bacteria_level Integer. Taxonomic level to retain for Bacteria (e.g., 6 for Genus).
#' @param Archaea_level Integer. Taxonomic level to retain for Archaea.
#' @param Eukaryota_level Integer. Taxonomic level to retain for Eukaryota.
#' @param selectDesign Character. The column in the metadata used as experimental design.
#' @param out_dir Character. Base output directory path where results will be saved.
#'
#' @return A list containing:
#' \describe{
#'   \item{list_of_decontam}{A list of filtered phyloseq objects after supervised DESeq2 decontamination.}
#'   \item{tab_list}{The imported phyloseq object list created from the raw data.}
#' }
#'
#' @seealso \code{\link{KrakenDataImport}}, \code{\link{KrakenDeSeq}}, \code{\link{createFolder}}

process_file <- function(biom_file, metadata_file, Bacteria_level = NULL, Archaea_level = NULL, Eukaryota_level = NULL, selectDesign = NULL, out_dir, do_normalization = FALSE, tab_list) {
  output_folder <- paste0(out_dir, "RDS/")
  output_folderD <- paste0(out_dir, "DESEQ_RDS/")
  createFolder(output_folderD)
  allPath <- paste0(output_folder, "/ALL_baseline_phylo.rds")
  list_of_decontam <- KrakenDeSeq(allPath, output_folderD, Bacteria_level, Archaea_level, Eukaryota_level, selectDesign, do_normalization)


  return(list(list_of_decontam = list_of_decontam, tab_list = tab_list))
}

#' Create a Dynamic Checkbox Group Input
#'
#' This function creates a `checkboxGroupInput` UI element only if the choices are valid.
#' It pre-selects values that match the globally defined `selected_genera` vector.
#'
#' @param id Character. The input ID to assign to the checkbox group.
#' @param label Character. The label to display above the checkbox group.
#' @param choices Character vector. The available choices to display as checkboxes.
#'
#' @return A `shiny` UI object (checkbox group) or `NULL` if `choices` is empty.
#'
#' @note This function depends on a global variable `selected_genera` being defined.
#'
#' @seealso \code{\link[shiny]{checkboxGroupInput}}

createCheckboxGroup <- function(id, label, choices) {
  if (is.null(choices) || length(choices) == 0) {
    return(NULL)
  }

  virtualSelectInput(
    id, label,
    choices = choices,
    selected = choices[choices %in% selected_genera],
    multiple = TRUE,
    showValueAsTags = TRUE,
    clearButton = TRUE,
    search = TRUE
  )
}

#' Create Differential Abundance Results using LIMMA
#'
#' This function performs differential abundance analysis using LIMMA
#' on decontaminated microbiome data, based on the specified pipeline (`kraken` or `metaphlan`).
#' It merges the results into a single file for downstream analysis.
#'
#' @param analisys Character. Type of analysis performed (e.g., "disease_vs_control").
#' @param sub_category Character. Name of the metadata column used for subsetting (e.g., "Treatment").
#' @param sub_names Character vector. Values within `sub_category` to subset (e.g., c("Treated", "Control")).
#' @param program Character. Method to use: `"limma"`.
#' @param kingdom Integer. Kingdom index: 1 = Archaea, 2 = Bacteria, 3 = Eukaryota.
#' @param pipeline Character. Pipeline used: `"kraken"` or `"metaphlan"`.
#' @param output_folder Character. Path to the output root folder.
#'
#' @return The path to the merged result file as returned by `DasMerge()`.
#'
#' @seealso \code{\link{DASLimma}}, \code{\link{DasMerge}}
run_das_analysis <- function(
    analisys,
    sub_category,
    sub_names,
    program,
    kingdom,
    pipeline,
    output_folder,
    metadata,
    discriminants = NULL, sign = "q_value", model_filter = "both") {
  kingdom <- as.integer(kingdom)
  print("metadata in FUN:")
  print(metadata)
  # ---- paths -------------------------------------------------
  out_maaslin <- file.path(output_folder, "MAASLIN3")
  out_ancom <- file.path(output_folder, "ANCOMBC2")
  out_merge <- file.path(output_folder, "MERGE_DAS")

  dir.create(out_merge, recursive = TRUE, showWarnings = FALSE)

  # ---- pipeline paths ---------------------------------------
  if (pipeline == "kraken") {
    baselines_dec <- c(
      file.path(output_folder, "SUPERVISED_DEC/Archaea_Supervised_decontam.rds"),
      file.path(output_folder, "SUPERVISED_DEC/Bacteria_Supervised_decontam.rds"),
      file.path(output_folder, "SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")
    )
    metadata_path <- c(
      file.path(output_folder, "ALPHA/Archaea_alpha_metadata.csv"),
      file.path(output_folder, "ALPHA/Bacteria_alpha_metadata.csv"),
      file.path(output_folder, "ALPHA/Eukaryota_alpha_metadata.csv")
    )
  } else {
    baselines_dec <- c(
      file.path(output_folder, "META_SUPERVISED_DEC/Archaea_Supervised_meta_no_strain.rds"),
      file.path(output_folder, "META_SUPERVISED_DEC/Bacteria_Supervised_meta_no_strain.rds"),
      file.path(output_folder, "META_SUPERVISED_DEC/Eukaryota_Supervised_meta_no_strain.rds")
    )
    metadata_path <- c(
      file.path(output_folder, "ALPHA/Archaea_meta_alpha_metadata.csv"),
      file.path(output_folder, "ALPHA/Bacteria_meta_alpha_metadata.csv"),
      file.path(output_folder, "ALPHA/Eukaryota_meta_alpha_metadata.csv")
    )
  }

  # ---- DAS runs ---------------------------------------------
  maaslin_df <- NULL
  ancombc_df <- NULL

  if (grepl("maaslin", program, ignore.case = TRUE)) {
    maaslin_path <- DASMaaslin3(
      baselines_dec[kingdom],
      metadata,
      analisys,
      discriminants,
      sub_category,
      sub_names,
      out_maaslin,
      sign,
      model_filter = model_filter
    )
    maaslin_df <- read.csv(maaslin_path)
  }

  if (grepl("ancom", program, ignore.case = TRUE)) {
    ancom_path <- AncomBC2_run(
      baselines_dec[kingdom],
      metadata,
      analisys,
      sub_category,
      sub_names,
      out_ancom,
      discriminants, sign
    )
    print(paste("ANCOM path:", ancom_path))

    # check se il CSV è vuoto
    ancombc_df <- tryCatch(
      read.csv(ancom_path),
      error = function(e) {
        warning("Could not read ANCOMBC CSV, returning empty data frame")
        return(data.frame())
      }
    )
    print("ANCOM df:")
    print(ancombc_df)
  }

  # ---- merge + filter phyloseq -------------------------------
  final_rds <- file.path(
    out_merge,
    paste0(
      "DAS_",
      program, "_",
      find_domain(baselines_dec[kingdom]), "_",
      analisys,
      Sys.Date(), ".rds"
    )
  )

  return(
    filter_phyloseq_by_das(
      rds_path = baselines_dec[kingdom],
      maaslin_df = maaslin_df,
      ancombc_df = ancombc_df,
      output_rds = final_rds,
      sub_category = sub_category,
      sub_name = sub_names
    )
  )
}


create_soi <- function(analisys, sub_category, sub_names, program, kingdom, pipeline, metadata, output_folder) {
  # Percorsi di output
  output_folder_limma <- paste0(output_folder, "/LIMMA/")
  output_folder_merge <- paste0(output_folder, "/MERGE_DAS/")

  kingdom <- as.integer(kingdom)
  if (!dir.exists(output_folder_merge)) dir.create(output_folder_merge, recursive = TRUE)

  # File path per pipeline
  if (pipeline == "kraken") {
    baselines_dec <- c(
      paste0(output_folder, "/SUPERVISED_DEC/Archaea_Supervised_decontam.rds"),
      paste0(output_folder, "/SUPERVISED_DEC/Bacteria_Supervised_decontam.rds"),
      paste0(output_folder, "/SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")
    )
    metadata_path <- c(
      paste0(output_folder, "/ALPHA/Archaea_alpha_metadata.csv"),
      paste0(output_folder, "/ALPHA/Bacteria_alpha_metadata.csv"),
      paste0(output_folder, "/ALPHA/Eukaryota_alpha_metadata.csv")
    )
  } else if (pipeline == "metaphlan") {
    baselines_dec <- c(
      paste0(output_folder, "/META_SUPERVISED_DEC/Archaea_Supervised_meta_no_strain.rds"),
      paste0(output_folder, "/META_SUPERVISED_DEC/Bacteria_Supervised_meta_no_strain.rds"),
      paste0(output_folder, "/META_SUPERVISED_DEC/Eukaryota_Supervised_meta_no_strain.rds")
    )
    metadata_path <- c(
      paste0(output_folder, "/ALPHA/Archaea_meta_alpha_metadata.csv"),
      paste0(output_folder, "/ALPHA/Bacteria_meta_alpha_metadata.csv"),
      paste0(output_folder, "/ALPHA/Eukaryota_meta_alpha_metadata.csv")
    )
  } else {
    stop("Pipeline not recognized. Use 'kraken' or 'metaphlan'.")
  }

  metadata_hm <- read.csv(metadata_path[kingdom],
    header = TRUE,
    sep = ",",
    na = c("", " ", "NA"),
    check.names = TRUE
  )
  domain <- find_domain(baselines_dec[kingdom])

  # --- LIMMA ---
  limma_df <- tryCatch(
    {
      if (grepl("limma", program, ignore.case = TRUE)) {
        limma_path <- DASLimma(
          baselines_dec[kingdom], metadata_path[kingdom],
          analisys, sub_category, sub_names,
          output_folder_limma
        )
        read.csv(limma_path)
      } else {
        stop("Skipping LIMMA")
      }
    },
    error = function(e) {
      message("Warning: LIMMA failed - ", e$message)
      ids <- c(metadata_hm$id)
      names <- c("X", ids)
      as.data.frame(matrix(ncol = length(names), nrow = 0, dimnames = list(NULL, names)))
    }
  )
  # --- Merge finale ---
  final_output <- gsub(" ", "", paste0(
    output_folder_merge,"SOI_",program,"_",domain, "_",
    analisys, ".rds"
  ))

  # Limita la lunghezza del nome file (mantenendo path e estensione)
  max_filename_length <- 40
  dir_path <- dirname(final_output)
  filename <- basename(final_output)

  if (nchar(filename) > max_filename_length) {
    extension <- ".rds"
    max_name_length <- max_filename_length - nchar(extension)
    filename <- paste0(substr(filename, 1, max_name_length), extension)
    final_output <- file.path(dir_path, filename)
  }

  return(SOIMerge(
    baselines_dec[kingdom], limma_df,
    final_output, sub_category, sub_names
  ))
}

find_domain <- function(path) {
  domain <- "Unknown"
  if (grepl("bact", path, ignore.case = TRUE)) {
    domain <- "bacteria"
  } else if (grepl("euk", path, ignore.case = TRUE)) {
    domain <- "eukaryote"
  } else if (grepl("arch", path, ignore.case = TRUE)) {
    domain <- "archaea"
  }
  return(domain)
}

sub_names_creator <- function(sub_names) {
  if (length(sub_names) > 1) {
    subname_paste <- paste(sub_names, collapse = "_")
  } else {
    subname_paste <- sub_names
  }
  return(subname_paste)
}
change_type <- function(variable, df, levels, colTypes, ord_factor) {
  column_class <- colTypes[variable]
  col_data <- df[[variable]]

  if (column_class == "character") {
    new_col <- as.character(col_data)
  } else if (column_class == "date") {
    new_col <- suppressWarnings(as.Date(col_data, format = "%Y-%m-%d"))
    if (all(is.na(new_col))) {
      new_col <- suppressWarnings(as.Date(col_data, format = "%d/%m/%Y"))
    }
    if (all(is.na(new_col))) {
      new_col <- suppressWarnings(as.Date(col_data, format = "%m/%d/%Y"))
    }
  } else if (column_class == "numeric") {
    new_col <- suppressWarnings(as.numeric(col_data))
  } else if (column_class == "factor") {
    if (ord_factor[variable]) {
      # Assicura che levels sia un vettore atomico, non una lista
      factor_levels <- levels[[variable]]
      if (is.list(factor_levels)) {
        factor_levels <- unlist(factor_levels)
      }
      new_col <- factor(col_data,
        ordered = TRUE,
        levels = factor_levels
      )
    } else {
      new_col <- as.factor(col_data)
    }
  } else if (column_class == "logic") {
    if (any(is.na(suppressWarnings(as.logical(levels[[variable]]))))) {
      new_col <- as.factor(col_data)
    } else {
      new_col <- suppressWarnings(as.logical(col_data))
    }
  } else {
    # fallback di sicurezza se qualcosa va storto
    new_col <- as.character(col_data)
  }

  return(new_col)
}

callback <- c(
  "var colnames = table.columns().header().to$().map(function() {",
  "return this.innerHTML;",
  "}).get();",
  "Shiny.onInputChange('colnames', colnames);",
  "table.on('dblclick.dt', 'thead th', function(e) {",
  "var $th = $(this);",
  "var index = $th.index();",
  "var colname = $th.text(), newcolname = colname;",
  "var $input = $('<input type=\"text\">')",
  "$input.val(colname);",
  "$th.empty().append($input);",
  "$input.on('change', function() {",
  "newcolname = $input.val();",
  "if(newcolname != colname){",
  "$(table.column(index).header()).text(newcolname);",
  "colnames[index] = newcolname;",
  "Shiny.onInputChange('colnames', colnames);",
  "}",
  "$input.remove();",
  "}).on('blur', function() {",
  "$(table.column(index).header()).text(newcolname);",
  "$input.remove();",
  "});",
  "})"
)

golden <- (1 + sqrt(5)) / 2
