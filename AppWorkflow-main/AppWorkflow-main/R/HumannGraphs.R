#to do ricordati di modificare id con la colonna interessata

HumannGraphs <- function(humann_data, metadata, input_folder, column_to_use,
                         main_el, pathway_to_search, plot_name,
                         docker_image = "franky2204/humann3.9:latest",
                         additional_rule = NULL) {
    # Controllo immagine docker
    ensure_docker_container(docker_image)

    # Caricamento tabella HUMAnN
    data <- read_tsv(humann_data)
    colnames(data)<-gsub("-", "_", colnames(data))

    # Controllo metadata
    cols_to_keep <- c("id", column_to_use)
    missing_cols <- setdiff(cols_to_keep, colnames(metadata))
    if (length(missing_cols) > 0) {
        stop("Colonne mancanti nei metadata: ", paste(missing_cols, collapse = ", "))
    }

    metadata <- metadata[, cols_to_keep, drop = FALSE]

    # Trasformazione HUMAnN
    data2 <- as.data.frame(t(data))
    data2$id <- rownames(data2)
    data2$id <- sub("_[^_]*$", "", data2$id)

    merged_df <- merge(data2, metadata, by = "id", all.x = TRUE)
    merged_df$"#" <- "#"

    data3 <- as.data.frame(t(merged_df))

    metadata_rows <- intersect(rownames(data3), column_to_use)
    for (row_name in metadata_rows) {
        data3[row_name, 1] <- row_name
    }
    data3["id", 1] <- "id"

    id_row <- data3["id", , drop = FALSE]
    metadata_row_data <- data3[metadata_rows, , drop = FALSE]
    hash_row <- data3["#", , drop = FALSE]
    other_rows <- data3[!rownames(data3) %in% c("id", metadata_rows, "#"), , drop = FALSE]

    data3 <- rbind(id_row, metadata_row_data, hash_row, other_rows)

    # Scrittura file
    processed_file <- file.path(input_folder, "processed_data.tsv")
    write_tsv(data3, processed_file, col_names = FALSE)

    # Cartella output
    out_dir <- file.path(input_folder, "humann_graphs_output")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    # File grafico
    output_png <- paste0(plot_name, ".png")

    # Comando Docker
    cmd <- sprintf(
        "docker run -v %s:/input_files/ %s bash -c 'humann_barplot --input /input_files/processed_data.tsv --output /input_files/humann_graphs_output/%s --focal-metadata %s --focal-feature %s %s'",
        normalizePath(input_folder),
        docker_image,
        output_png,
        main_el,
        pathway_to_search,
        ifelse(is.null(additional_rule), "", additional_rule)
    )

    message("Eseguo Docker:\n", cmd)
    system(cmd)
}

# pathway_to_search <- c("P124-PWY", "P163-PWY", "P41-PWY", "P461-PWY", "PWY-8086", "PWY-5100", "P163-PWY")
# for (pathway in pathway_to_search) {
#     HumannGraphs(
#         humann_data = humann_data,
#         metadata = metadata,
#         input_folder = input_folder,
#         column_to_use = column_to_use,
#         main_el = main_el,
#         pathway_to_search = pathway,
#         plot_name = paste0("humann_pathway_coverage_", pathway, "_sorted_sum"),
#         additional_rule = "--sort sum"
#     )

#     HumannGraphs(
#         humann_data = humann_data,
#         metadata = metadata,
#         input_folder = input_folder,
#         column_to_use = column_to_use,
#         main_el = main_el,
#         pathway_to_search = pathway,
#         plot_name = paste0("humann_pathway_abundance_", pathway, "_sorted_sum_metadata_logstack"),
#         additional_rule = "--sort sum metadata --scaling logstack"
#     )

#     HumannGraphs(
#         humann_data = humann_data,
#         metadata = metadata,
#         input_folder = input_folder,
#         column_to_use = column_to_use,
#         main_el = main_el,
#         pathway_to_search = pathway,
#         plot_name = paste0("humann_pathway_abundance_", pathway, "_sorted_braycurtis_metadata_logstack_as_genera_remove_zeros"),
#         additional_rule = "--sort braycurtis --scaling logstack --as-genera --remove-zeros"
#     )
# }
