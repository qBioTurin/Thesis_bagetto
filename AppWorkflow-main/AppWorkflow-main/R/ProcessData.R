detect_pipeline <- function(folder_input) {
    files_report <- list.files(folder_input, pattern = "\\.(report|txt)$", full.names = FALSE)

    if (length(files_report) == 0) {
        stop("Nessun file .report o .txt trovato nella cartella di input")
    }

    message("File trovati nella cartella di input:")
    print(files_report)

    file_extensions <- tools::file_ext(files_report)

    if (all(file_extensions == "txt")) {
        message("File MetaPhlAn detected")
        return("metaphlan")
    }

    if (all(file_extensions == "report")) {
        message("File Kraken detected")
        return("kraken")
    }

    stop("Mixed file types found")
}

build_pipeline_command <- function(parent_dir, last_folder, pipeline_name, image_name, folder_input_path, output_folder, output_file, working_root) {
    running_in_docker <- file.exists("/.dockerenv")

    if (running_in_docker) {
        # Copy the input to a writable workspace so the preprocessing scripts
        # never try to write next to a read-only mounted dataset.
        inner_cmd <- if (pipeline_name == "kraken") {
            paste0(
                "INPUT_DIR=", shQuote(folder_input_path), "; ",
                "WORK_ROOT=/shared_results/preprocess_work; ",
                "WORK_DIR=\"$WORK_ROOT/", pipeline_name, "/", last_folder, "\"; ",
                "INPUT_COPY=\"$WORK_DIR/input\"; ",
                "OUTPUT_COPY=\"$WORK_DIR/output\"; ",
                "OUTPUT_FILE=\"$OUTPUT_COPY/kraken_merged.biom\"; ",
                "mkdir -p \"$INPUT_COPY\" \"$OUTPUT_COPY\" && ",
                "cp -a \"$INPUT_DIR\"/. \"$INPUT_COPY\"/ && ",
                "bash /scripts/kingdom_rename.sh \"$INPUT_COPY\" && ",
                "kraken-biom \"$INPUT_COPY\"/*.report --output \"$OUTPUT_FILE\" --fmt json"
            )
        } else {
            paste0(
                "INPUT_DIR=", shQuote(folder_input_path), "; ",
                "WORK_ROOT=/shared_results/preprocess_work; ",
                "WORK_DIR=\"$WORK_ROOT/", pipeline_name, "/", last_folder, "\"; ",
                "INPUT_COPY=\"$WORK_DIR/input\"; ",
                "OUTPUT_COPY=\"$WORK_DIR/output\"; ",
                "OUTPUT_FILE=\"$OUTPUT_COPY/merged_metaphlan.txt\"; ",
                "mkdir -p \"$INPUT_COPY\" \"$OUTPUT_COPY\" && ",
                "cp -a \"$INPUT_DIR\"/. \"$INPUT_COPY\"/ && ",
                "merge_metaphlan_tables.py \"$INPUT_COPY\"/*.txt > \"$OUTPUT_FILE\""
            )
        }

        return(paste(
            "docker run --rm --volumes-from microworkflow-app",
            image_name,
            "bash -lc",
            shQuote(inner_cmd)
        ))
    }

    # Container-internal paths (working_root is mounted as /shared_results inside the container)
    container_input_dir  <- paste0("/shared_results/", pipeline_name, "/", last_folder, "/input")
    container_output_dir <- paste0("/shared_results/", pipeline_name, "/", last_folder, "/output")

    if (pipeline_name == "kraken") {
        inner_cmd <- paste0(
            "mkdir -p ", shQuote(container_input_dir), " ", shQuote(container_output_dir), " && ",
            "cp -a /input_files/", last_folder, "/. ", shQuote(container_input_dir), "/ && ",
            "bash /scripts/kingdom_rename.sh ", shQuote(container_input_dir), " && ",
            "kraken-biom ", shQuote(container_input_dir),
            "/*.report --output ", shQuote(paste0(container_output_dir, "/kraken_merged.biom")),
            " --fmt json"
        )
    } else {
        inner_cmd <- paste0(
            "mkdir -p ", shQuote(container_input_dir), " ", shQuote(container_output_dir), " && ",
            "cp -a /input_files/", last_folder, "/. ", shQuote(container_input_dir), "/ && ",
            "merge_metaphlan_tables.py ", shQuote(container_input_dir),
            "/*.txt > ", shQuote(paste0(container_output_dir, "/merged_metaphlan.txt"))
        )
    }

    paste(
        "docker run --rm -v",
        paste0(shQuote(parent_dir), ":/input_files:ro"),
        "-v",
        paste0(shQuote(working_root), ":/shared_results"),
        image_name,
        "bash -lc",
        shQuote(inner_cmd)
    )
}

#' @title Preprocess Kraken or MetaPhlAn Reports with Docker
#'
#' @description
#' This function detects whether the input folder contains Kraken/Bracken `.report`
#' files or MetaPhlAn `.txt` files, builds the corresponding Docker command, and
#' generates a merged output file in a pipeline-specific output directory.
#'
#' @param folder_input Character string. Path to a directory containing either
#'   only `.report` files (Kraken/Bracken) or only `.txt` files (MetaPhlAn).
#' @param image_name Character string. Docker image to use for preprocessing.
#'   Default is `"microworkflow/pipeline-tools:latest"`.
#' @param execute Logical. If `TRUE`, execute the Docker command. If `FALSE`,
#'   only return the generated command and resolved output paths.
#'
#' @return A named list with:
#' \itemize{
#'   \item `pipeline_name`: detected pipeline (`"kraken"` or `"metaphlan"`)
#'   \item `output_folder`: absolute path to the writable output directory
#'   \item `output_file`: absolute path to the expected merged output file
#'   \item `cmd`: full Docker command used for processing
#'   \item `status`: exit status from `system(cmd)` (only when `execute = TRUE`)
#' }
#'
#' @details
#' - The input is copied to a writable workspace before processing.
#' - Inside Docker, the writable workspace lives under `/shared_results/preprocess_work`.
#' - Outside Docker, the writable workspace lives under `tempdir()`.
#' - For Kraken inputs, the function runs `kingdom_rename.sh` before `kraken-biom`.
#' - For MetaPhlAn inputs, the function runs `merge_metaphlan_tables.py`.
#' - The Docker image must already be available locally.
#'
#' @examples
#' \dontrun{
#' # Execute preprocessing
#' res <- run_preprocess_pipeline("/path/to/input_folder")
#'
#' # Preview command without executing
#' preview <- run_preprocess_pipeline("/path/to/input_folder", execute = FALSE)
#' preview$cmd
#' preview$output_file
#' }
#'
#' @export
run_preprocess_pipeline <- function(folder_input,
                                    image_name = "microworkflow/pipeline-tools:latest",
                                    execute = TRUE) {
    running_in_docker <- file.exists("/.dockerenv")
    folder_input_path <- normalizePath(folder_input, winslash = "/", mustWork = FALSE)

    if (!dir.exists(folder_input_path)) {
        stop("La cartella di input non esiste: ", folder_input_path)
    }

    pipeline_name <- detect_pipeline(folder_input_path)
    ensure_docker_container(image_name)

    parent_dir <- dirname(folder_input_path)
    last_folder <- basename(folder_input_path)
    working_root <- if (running_in_docker) {
        "/shared_results/preprocess_work"
    } else {
        file.path(tempdir(), "microworkflow_preprocess_work")
    }
    output_folder <- file.path(working_root, pipeline_name, last_folder, "output")
    output_file <- if (pipeline_name == "kraken") {
        file.path(output_folder, "kraken_merged.biom")
    } else {
        file.path(output_folder, "merged_metaphlan.txt")
    }

    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }

    cmd <- build_pipeline_command(
        parent_dir = parent_dir,
        last_folder = last_folder,
        pipeline_name = pipeline_name,
        image_name = image_name,
        folder_input_path = folder_input_path,
        output_folder = output_folder,
        output_file = output_file,
        working_root = working_root
    )

    if (!execute) {
        return(list(
            pipeline_name = pipeline_name,
            output_folder = output_folder,
            output_file = output_file,
            cmd = cmd
        ))
    }

    status <- system(cmd)

    if (!identical(status, 0L)) {
        stop(
            paste0(
                "Preprocess pipeline failed (exit status ", status, "). Command: ",
                cmd
            )
        )
    }

    if (!file.exists(output_file)) {
        stop(
            paste0(
                "Preprocess completed but output file was not created: ",
                output_file
            )
        )
    }

    list(
        pipeline_name = pipeline_name,
        output_folder = output_folder,
        output_file = output_file,
        cmd = cmd,
        status = status
    )
}

