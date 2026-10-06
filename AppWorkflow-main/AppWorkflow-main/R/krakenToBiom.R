
library(jsonlite)
reports <- list.files("/home/franky/Desktop/krakenToBiom/input/", pattern = "\\.report$", full.names = TRUE)


kraken_reports_to_biom <- function(input_dir = ".", output_file = "kraken_aggregated.biom") {
  # 1. Trova tutti i report Kraken
  reports <- list.files(input_dir, pattern = "\\.report$", full.names = TRUE)
  if (length(reports) == 0) stop("Nessun file .report trovato nella cartella")

  message("Trovati ", length(reports), " report Kraken")

  # 2. Parsing funzione singolo file
  parse_kraken <- function(file) {
    df <- read.table(file, sep = "\t", header = FALSE, quote = "", fill = TRUE, stringsAsFactors = FALSE)
    colnames(df) <- c("percent", "reads_clade", "reads_taxon", "rank_code", "ncbi_taxid", "name")

    df %>%
      mutate(
        sample = tools::file_path_sans_ext(basename(file)),
        rank_code = trimws(rank_code),
        name = trimws(name)
      ) %>%
      filter(rank_code %in% c("D","P","C","O","F","G","S","S1","S2"))
  }

  # 3. Leggi tutti i report
  merged <- do.call(rbind, lapply(reports, parse_kraken))

  # 4. Tabella taxa × campioni
  mat <- merged %>%
    select(name, sample, reads_clade) %>%
    pivot_wider(names_from = sample, values_from = reads_clade, values_fill = 0)

  # 5. Converti in formato BIOM "sparse"
  taxa <- mat$name
  samples <- colnames(mat)[-1]
  data_sparse <- list()
  mat_data <- as.matrix(mat[,-1])

  for (i in seq_along(taxa)) {
    for (j in seq_along(samples)) {
      val <- mat_data[i, j]
      if (val != 0) {
        data_sparse[[length(data_sparse) + 1]] <- list(i - 1, j - 1, val)
      }
    }
  }

  biom_json <- list(
    id = NULL,
    format = "Biological Observation Matrix 1.0.0",
    format_url = "http://biom-format.org",
    type = "Taxon table",
    generated_by = "custom R script (Kraken2 compatible)",
    date = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    matrix_type = "sparse",
    matrix_element_type = "int",
    shape = c(length(taxa), length(samples)),
    rows = lapply(taxa, function(t) list(id = t, metadata = NULL)),
    columns = lapply(samples, function(s) list(id = s, metadata = NULL)),
    data = data_sparse
  )

  # 6. Scrivi il file .biom
  write(toJSON(biom_json, auto_unbox = TRUE, pretty = TRUE), output_file)
  message("Creato file BIOM: ", output_file)
}
kraken_reports_to_biom(
  input_dir = "/home/franky/Desktop/krakenToBiom/input/",
  output_file = "/home/franky/Desktop/krakenToBiom/output/kraken_aggregated.biom"
)
# Leggi e visualizza il file BIOM creato
biom_data <- fromJSON("/home/franky/Desktop/krakenToBiom/output/kraken_aggregated.biom")
str(biom_data)