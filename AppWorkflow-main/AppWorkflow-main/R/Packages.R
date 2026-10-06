#add 
# CRAN
cran_packages <- c(
  "dplyr", "ggplot2", "ggsignif", "patchwork", "tidyr", "MetBrewer",
  "ggbreak", "tibble", "readr", "pheatmap", "ggpubr", "grid", "leaps",
  "UpSetR", "NbClust", "ggfortify", "factoextra", "cluster",
  "gridExtra", "shiny", "bslib", "bsicons", "shinyjs", "shinyBS",
  "shinybusy", "shinyalert", "waiter", "stringr", "ggplotify", "shinyFiles",
  "pak", "fs", "datamods","tidyverse","shinyWidgets","DT","sortable","remotes","devtools",
  "shinyauthr", "sodium", "dotenv",
  "Cairo","MicrobiomeStat", "topicmodels","tidytext","cowplot","numbers"
)

optional_cran_packages <- c()
optional_github_packages <- c()

# Install dependencies only when explicitly enabled.
running_in_docker <- file.exists("/.dockerenv")
install_deps_enabled <-
  tolower(Sys.getenv("MICROWORKFLOW_INSTALL_DEPS", "false")) %in% c("1", "true", "yes")

# Bioconductor
bioc_packages <- c(
  "phyloseq", "ape", "Biostrings", "DESeq2", "microbiome",
  "limma", "biomformat","curatedMetagenomicData"
)

# Funzioni per installare pacchetti mancanti
install_if_missing_cran <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tryCatch(
      install.packages(pkg, repos = "https://cloud.r-project.org"),
      error = function(e) {
        if (pkg %in% optional_cran_packages) {
          warning(sprintf("Optional package '%s' could not be installed: %s", pkg, conditionMessage(e)))
        } else {
          stop(e)
        }
      }
    )
  }
}

ensure_min_version_cran <- function(pkg, min_version, type = "source") {
  installed <- rownames(installed.packages())
  needs_install <- !pkg %in% installed
  if (!needs_install) {
    current_version <- as.character(utils::packageVersion(pkg))
    needs_install <- utils::compareVersion(current_version, min_version) < 0
  }
  if (needs_install) {
    if (pkg %in% loadedNamespaces()) {
      try(unloadNamespace(pkg), silent = TRUE)
    }
    install.packages(pkg, repos = "https://cloud.r-project.org", type = type)
  }
}

install_if_missing_bioc <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager", repos = "https://cloud.r-project.org")
    BiocManager::install(pkg, ask = FALSE, update = FALSE)
  }
}

# Pacchetti GitHub
github_packages <- list(
  phyloseq = "joey711/phyloseq",
  ggstream = "davidsjoberg/ggstream",
  MoMAColors= "BlakeRMills/MoMAColors",
  ldatuning = "nikita-moor/ldatuning",
  LinDA = "zhouhj1994/LinDA",
  maaslin3 = "biobakery/maaslin3"
)

# Installed via pak in order: gghalves first so microbiomeutilities dep is satisfied
custom_github_packages <- c(
  "erocoar/gghalves",
  "microsud/microbiomeutilities",
  "hrbrmstr/ggalt",
  "hrbrmstr/waffle"
)

if (!requireNamespace("pak", quietly = TRUE)) {
  if (install_deps_enabled) {
    install.packages("pak", repos = "https://cloud.r-project.org")
  }
}

if (install_deps_enabled) {
  # Installa prima CRAN e Bioconductor per evitare che pak debba scaricare
  # molte dipendenze Bioc durante l'installazione dei pacchetti GitHub.
  invisible(lapply(cran_packages, install_if_missing_cran))
  invisible(lapply(bioc_packages, install_if_missing_bioc))
}

install_github_packages <- function(pkgs) {
  for (pkg in names(pkgs)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch(
        pak::pak(pkgs[[pkg]]),
        error = function(e) {
          if (pkg %in% optional_github_packages) {
            warning(sprintf("Optional GitHub package '%s' could not be installed: %s", pkg, conditionMessage(e)))
          } else {
            stop(e)
          }
        }
      )
    }
  }
}

if (install_deps_enabled) {
  install_github_packages(github_packages)
}

install_custom_github_packages <- function(repos) {
  for (repo in repos) {
    pkg <- basename(repo)
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch(
        pak::pak(repo),
        error = function(e) {
          if (pkg %in% optional_github_packages) {
            warning(sprintf("Optional GitHub package '%s' could not be installed: %s", pkg, conditionMessage(e)))
          } else {
            stop(e)
          }
        }
      )
    }
  }
}

if (install_deps_enabled) {
  install_custom_github_packages(custom_github_packages)
}

# MaAsLin3 currently calls mirai::info(), which is only available in newer mirai releases.
# Re-apply this after GitHub installs as pak may otherwise resolve an older mirror snapshot.
if (install_deps_enabled) {
  ensure_min_version_cran("nanonext", "1.7.0", type = "source")
  ensure_min_version_cran("mirai", "2.5.0", type = "source")
}

main_packages <- unique(c(names(github_packages), cran_packages, bioc_packages))
suppressMessages(invisible(lapply(main_packages, function(p) {
  try(library(p, character.only = TRUE), silent = TRUE)
})))

if (!install_deps_enabled) {
  missing_pkgs <- main_packages[!vapply(main_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_pkgs) > 0) {
    stop(
      paste0(
        "Missing required packages: ",
        paste(missing_pkgs, collapse = ", "),
        ". Run with MICROWORKFLOW_INSTALL_DEPS=true once to install them "
        , "(for example: Sys.setenv(MICROWORKFLOW_INSTALL_DEPS='true'); source('R/Packages.R'))."
      )
    )
  }
}
