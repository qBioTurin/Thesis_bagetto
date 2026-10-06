# Set of function for humann3 normalization and processing

ensure_docker_image_humann3 <- function(container_name) {
    # Check Docker availability
    if (system2("docker", "--version", stdout = FALSE, stderr = FALSE) != 0) {
        stop("Docker not installed or not working.")
    }

    # Check if the image exists locally first
    existing_image <- tryCatch(
        system2("docker", c("images", "-q", container_name), stdout = TRUE, stderr = TRUE),
        error = function(e) character(0)
    )

    image_exists <- length(existing_image) > 0 && any(nzchar(existing_image))

    if (image_exists) {
        message("Image '", container_name, "' found locally.")
        return(invisible(list(exists = FALSE, pulled = FALSE, image_exists = TRUE)))
    }

    message("Image '", container_name, "' not found locally. Attempting 'docker pull'...")

    # Try to pull the image
    pull_status <- tryCatch(
        system2("docker", c("pull", container_name), stdout = NULL, stderr = NULL),
        error = function(e) 1L
    )

    if (!is.numeric(pull_status) || pull_status != 0L) {
        warning("docker pull failed for: ", container_name)
        return(invisible(list(exists = FALSE, pulled = FALSE, image_exists = FALSE)))
    }

    message("docker pull succeeded for: ", container_name, ".")
    invisible(list(exists = FALSE, pulled = TRUE, image_exists = TRUE))
}

# Normalize HUMAnN3 tables

norm_table <- function(input_folder, output_folder, docker_image = "franky2204/humann3.9:latest") {
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }
    if (!dir.exists(input_folder)) {
        stop("Input folder does not exist: ", input_folder)
    }
    files <- list.files(input_folder, pattern = "\\.tsv$", full.names = TRUE)
    if (length(files) == 0) {
        message("No tsv found in  ", input_folder)
        return(invisible(NULL))
    }

    status <- ensure_docker_image_humann3(docker_image)
    if (!status$image_exists) {
        stop("Docker image not available: ", docker_image)
    }

    # Create temporary folder inside input_folder
    output_folder_norm <- file.path(input_folder, "norm_table")
    if (!dir.exists(output_folder_norm)) {
        dir.create(output_folder_norm, recursive = TRUE)
    }
    cmd <- paste0(
        "docker run -v ", normalizePath(input_folder), ":/input_files/ ",
        docker_image, " bash -c '",
        "mkdir -p /input_files/norm_table; ",
        "for f in /input_files/*.tsv; do ",
        "humann_renorm_table --input \"$f\" --output \"/input_files/norm_table/$(basename \"${f%.tsv}-cpm.tsv\")\" --units cpm --update-snames; ",
        "done'"
    )

    system(cmd)

    # Move all normalized files to the final output folder
    norm_files <- list.files(output_folder_norm, full.names = TRUE)
    file.rename(norm_files, file.path(output_folder, basename(norm_files)))

    # Remove the temporary folder
    unlink(output_folder_norm, recursive = TRUE)

    return(invisible(NULL))
}
# input_folder <- "/media/franky/dati/University_data/unitor/unitor_8_kraken_humann/genefamilies_data"
# output_folder <- "/media/franky/dati/University_data/unitor/unitor_8_kraken_humann/genefamilies_data_norm"
# norm_table(input_folder, output_folder)

# Merge HUMAnN3 tables, it function by using wildcards to identify the type of table to merge(pattern)
# patterns is a named vector where the name is the type of table and the value is the pattern to search for
merge_humann_tables <- function(input_folder,
                                output_folder,
                                docker_image = "franky2204/humann3.9:latest",
                                patterns = c(
                                    families = "genefamilies",
                                    pathabundance = "pathabundance",
                                    pathcoverage = "pathcoverage"
                                )) {
    if (!dir.exists(input_folder)) {
        stop("Input folder does not exist: ", input_folder)
    }
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }

    status <- ensure_docker_image_humann3(docker_image)
    if (!status$image_exists) {
        stop("Docker image not available: ", docker_image)
    }
    for (typ in names(patterns)) {
        pat <- patterns[[typ]]
        files_found <- list.files(input_folder, pattern = pat, full.names = TRUE)

        if (length(files_found) == 0) {
            message("No files found for type: ", typ, " (pattern: ", pat, ")")
            next
        }

        output_name <- paste0("merged_", typ, ".tsv")

        cmd <- paste0(
            "docker run -v ", normalizePath(input_folder), ":/input_files ",
            docker_image, " bash -c '",
            "humann_join_tables --input /input_files ",
            "--output /input_files/", output_name, " ",
            "--file_name ", pat, "'"
        )

        message("Execute humann for type: ", typ)
        system(cmd)

        src <- file.path(input_folder, output_name)
        dest <- file.path(output_folder, output_name)

        if (file.exists(src)) {
            file.rename(src, dest)
            message("Saved: ", dest)
        } else {
            warning("No output file found for type: ", typ)
        }
    }

    message("Merge completed. Files saved in: ", normalizePath(output_folder))
    invisible(NULL)
}
# input_folder <- "/media/franky/dati/University_data/unitor/unitor_8_kraken_humann/genefamilies_data_norm"
# output_folder <- "/media/franky/dati/University_data/unitor/unitor_8_kraken_humann/merged_tables"
# merge_humann_tables(input_folder, output_folder)


# Regroup HUMAnN3 tables by name and uses wildcard to identify the type of table to regroup but
# only genefamilies(not suitable for pathabundance or pathcoverage))
regroup_name_humann <- function(
    input_folder, output_folder, docker_name = "franky2204/humann3.9:latest",
    pattern_src = "genefamilies", grouptype = "uniref90_rxn") {
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }

    files <- list.files(input_folder, pattern = pattern_src, full.names = TRUE)
    if (length(files) == 0) {
        message("No file ", pattern_src, " found in ", input_folder)
        return(invisible(NULL))
    }


    status <- ensure_docker_image_humann3(docker_name)
    if (!status$image_exists) {
        stop("Docker image not available: ", docker_name)
    }

    output_folder_regrouped <- file.path(input_folder, "regrouped_tables")
    if (!dir.exists(output_folder_regrouped)) {
        dir.create(output_folder_regrouped, recursive = TRUE)
    }

    cmd <- paste0(
        "docker run --rm -v ", normalizePath(input_folder), ":/input_files/ ",
        docker_name, " bash -c '",
        "mkdir -p /input_files/regrouped_tables; ",
        "for f in /input_files/*genefamilies*.tsv; do ",
        "echo Processing $f; ",
        "humann_regroup_table --input \"$f\" ",
        "--output \"/input_files/regrouped_tables/$(basename \"${f%.tsv}-regrouped.tsv\")\" ",
        "--groups ", grouptype, "; ",
        "done'"
    )

    system(cmd)

    regrouped_files <- list.files(output_folder_regrouped, full.names = TRUE)
    if (length(regrouped_files) > 0) {
        file.rename(regrouped_files, file.path(output_folder, basename(regrouped_files)))
    }

    unlink(output_folder_regrouped, recursive = TRUE)

    message("Reorganization completed for genefamilies files.")
    return(invisible(NULL))
}


# input_folder <- "/home/franky/Desktop/humann/norm_tables/"
# output_folder <- "/home/franky/Desktop/humann/regrouped_tables/"
# regroup_name_humann(input_folder, output_folder)

# Rename HUMAnN3 tables using a specific database
# pattern_src is used to identify the type of table to rename
# names_db is the database to use for renaming, e.g., "metacyc-rxn", "uniref90-rxn"
rename_humann_tables <- function(input_folder,
                                 output_folder,
                                 docker_name = "franky2204/humann3.9:latest",
                                 pattern_src = "genefamilies",
                                 names_db = "metacyc-rxn") {
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }

    files <- list.files(input_folder, pattern = pattern_src, full.names = TRUE)
    if (length(files) == 0) {
        message("No file ", pattern_src, " found in ", input_folder)
        return(invisible(NULL))
    }

    status <- ensure_docker_image_humann3(docker_name)
    if (!status$image_exists) {
        stop("Docker image not available: ", docker_name)
    }

    temp_folder <- file.path(input_folder, "renamed_temp")
    if (!dir.exists(temp_folder)) {
        dir.create(temp_folder, recursive = TRUE)
    }

    cmd <- paste0(
        "docker run --rm -v ", normalizePath(input_folder), ":/input_files/ ",
        docker_name, " bash -c '",
        "mkdir -p /input_files/renamed_temp; ",
        "for f in /input_files/*", pattern_src, "*.tsv; do ",
        "echo Processing $f; ",
        "humann_rename_table --input \"$f\" ",
        "--output \"/input_files/renamed_temp/$(basename \"${f%.tsv}-named.tsv\")\" ",
        "--names ", names_db, "; ",
        "done'"
    )

    system(cmd)

    renamed_files <- list.files(temp_folder, full.names = TRUE)
    if (length(renamed_files) > 0) {
        file.rename(renamed_files, file.path(output_folder, basename(renamed_files)))
        unlink(temp_folder, recursive = TRUE)
        message("Renamed files saved in ", normalizePath(output_folder))
    } else {
        warning("No files were renamed.")
    }

    invisible(NULL)
}

# input_folder <- "/home/franky/Desktop/humann/regrouped_tables/"
# output_folder <- "/home/franky/Desktop/humann/renamed_tables/"
# rename_humann_tables(input_folder, output_folder)

# ########### merge final table for graphs
# merge_humann_tables(
#     input_folder = "/home/franky/Desktop/humann/renamed_tables/",
#     output_folder = "/home/franky/Desktop/humann/final_merged_tables/", patterns = c(
#         families = "genefamilies"
#     )
# )
############## function for graphs


# Wrapper to run the full HUMAnN processing sequence used by the Shiny app
# This function executes the requested steps in order and returns a list with
# results (TRUE on success or the caught error object on failure) for each step.
run_humann_pipeline <- function(input_folder, base_output, docker_image = "franky2204/humann3.9:latest") {
    if (missing(input_folder) || missing(base_output)) stop("input_folder and base_output are required")

    # Ensure base output exists
    base_output <- file.path(base_output)
    dir.create(base_output, recursive = TRUE, showWarnings = FALSE)

    results <- list()

    # Step 1: norm_table -> base_output/norm
    out_norm1 <- file.path(base_output, "norm")
    dir.create(out_norm1, recursive = TRUE, showWarnings = FALSE)
    results$norm1 <- tryCatch(
        {
            norm_table(input_folder, out_norm1, docker_image)
            TRUE
        },
        error = function(e) e
    )

    # Step 2: merge_humann_tables (input = out_norm1, output = base_output/merge_tables)
    out_merge <- file.path(base_output, "merge_tables")
    dir.create(out_merge, recursive = TRUE, showWarnings = FALSE)
    results$merge <- tryCatch(
        {
            merge_humann_tables(out_norm1, out_merge, docker_image = docker_image)
            TRUE
        },
        error = function(e) e
    )

    # Step 3: regroup_name_humann (input = out_norm1, output = base_output/regroup)
    out_regroup <- file.path(base_output, "regroup")
    dir.create(out_regroup, recursive = TRUE, showWarnings = FALSE)
    results$regroup <- tryCatch(
        {
            regroup_name_humann(out_norm1, out_regroup, docker_name = docker_image)
            TRUE
        },
        error = function(e) e
    )

    # Step 4: rename_humann_tables (input = out_regroup, output = base_output/renamed)
    out_renamed <- file.path(base_output, "renamed")
    dir.create(out_renamed, recursive = TRUE, showWarnings = FALSE)
    results$rename <- tryCatch(
        {
            rename_humann_tables(out_regroup, out_renamed, docker_name = docker_image)
            TRUE
        },
        error = function(e) e
    )

    # Step 5: norm_table on renamed files (input = out_renamed, output = base_output/renamed_grouped)
    out_renorm2 <- file.path(base_output, "renamed_grouped")
    dir.create(out_renorm2, recursive = TRUE, showWarnings = FALSE)
    results$norm2 <- tryCatch(
        {
            norm_table(out_renamed, out_renorm2, docker_image)
            TRUE
        },
        error = function(e) e
    )

    # Step 6: merge final tables for graphs (only genefamilies)
    out_final_merge <- file.path(base_output, "final_merged_tables")
    dir.create(out_final_merge, recursive = TRUE, showWarnings = FALSE)
    results$final_merge <- tryCatch(
        {
            merge_humann_tables(
                out_renorm2,
                out_final_merge,
                docker_image = docker_image,
                patterns = c(families = "genefamilies")
            )
            TRUE
        },
        error = function(e) e
    )

    invisible(results)
}
