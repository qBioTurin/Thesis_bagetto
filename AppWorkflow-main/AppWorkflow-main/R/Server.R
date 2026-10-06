server <- function(input, output, session) {
    # output_directory <- reactiveVal("C:/Users/fpant/Desktop/delete/")
    #######################################
    ### REACTIVES ####
    require_startup_password <- tolower(Sys.getenv("MICROWORKFLOW_REQUIRE_PASSWORD", "false")) %in% c("1", "true", "yes")
    default_disable_auth <- if (require_startup_password) "false" else "true"
    disable_auth <- tolower(Sys.getenv("MICROWORKFLOW_DISABLE_AUTH", default_disable_auth)) %in% c("1", "true", "yes")
    auth_enabled <- !disable_auth
    auth_user <- Sys.getenv("MICROWORKFLOW_AUTH_USER", "admin")
    auth_password_hash <- Sys.getenv("MICROWORKFLOW_AUTH_PASSWORD_HASH", "")
    auth_credentials_file <- Sys.getenv("MICROWORKFLOW_AUTH_CREDENTIALS_FILE", "")

    load_auth_base <- function() {
        if (nzchar(auth_credentials_file)) {
            if (!file.exists(auth_credentials_file)) {
                stop("MICROWORKFLOW_AUTH_CREDENTIALS_FILE points to a missing file.")
            }

            credentials_df <- readr::read_csv(
                auth_credentials_file,
                show_col_types = FALSE,
                col_types = readr::cols(.default = readr::col_character())
            )

            required_columns <- c("user", "password_hash")
            missing_columns <- setdiff(required_columns, names(credentials_df))
            if (length(missing_columns) > 0) {
                stop(
                    paste0(
                        "Credential file must contain columns: ",
                        paste(required_columns, collapse = ", ")
                    )
                )
            }

            return(tibble::tibble(
                user = credentials_df[["user"]],
                password = credentials_df[["password_hash"]]
            ))
        }

        if (!nzchar(auth_password_hash)) {
            stop(
                paste0(
                    "Set MICROWORKFLOW_AUTH_PASSWORD_HASH or MICROWORKFLOW_AUTH_CREDENTIALS_FILE ",
                    "before starting the app."
                )
            )
        }

        tibble::tibble(
            user = auth_user,
            password = auth_password_hash
        )
    }

    if (auth_enabled) {
        user_base <- load_auth_base()

        logout_init <- NULL
        credentials <- shinyauthr::loginServer(
            id = "login",
            data = user_base,
            user_col = "user",
            pwd_col = "password",
            sodium_hashed = TRUE,
            log_out = reactive(logout_init())
        )

        logout_init <- shinyauthr::logoutServer(
            id = "logout",
            active = reactive(credentials()$user_auth)
        )
    } else {
        local_auth <- reactiveVal(FALSE)
        observeEvent(input$local_enter, {
            local_auth(TRUE)
        })
        credentials <- reactive(list(user_auth = local_auth()))
        logout_init <- reactive(NULL)
    }

    observe({
        if (isTRUE(credentials()$user_auth)) {
            shinyjs::addClass("auth_shell", "app-mode")
            shinyjs::show("app_content")
            shinyjs::hide("auth_entry_panel")
            shinyjs::hide("auth_login_brand")
        } else {
            shinyjs::removeClass("auth_shell", "app-mode")
            shinyjs::hide("app_content")
            shinyjs::show("auth_entry_panel")
            shinyjs::show("auth_login_brand")
        }
    })

    output$pipeline_type_ui <- reactive({
        pipeline_type()
    })
    outputOptions(output, "pipeline_type_ui", suspendWhenHidden = FALSE)

    rv <- reactiveValues(
        remove_dn = NULL,
        tab_list = NULL,
        row_selected = NULL,
        pathway_file_location = NULL,
        rds_obj = NULL,
        soi_obj = NULL,
        pathway_fit_out = NULL,
        pathway_array_path = NULL,
        humann_graph_file = NULL,
        output_directory = NULL,
        current_page = 1,
        total_pages = 3,
        contam_list = NULL,
        AllAlright = FALSE,
        fileUploaded = FALSE,
        df_pre = NULL,
        df_post = NULL, # variable to substitute the df
        variables = NULL,
        ncol = NULL,
        colTypes = NULL,
        levels = NULL,
        ntype_ord = NULL,
        ntype_facets = NULL,
        ord_factor = NULL,
        palettes = NULL,
        plot_n = 1,
        plot = NULL,
        heatmap_plot = NULL,
        rds_path = NULL,
        title = NULL,
        description = NULL,
        idColumn = NULL,
        topic_modeling_p1 = NULL,
        topic_modeling_p2 = NULL,
        topic_modeling_p3 = NULL,
        preprocessed_file = NULL,
        pipeline_name = NULL,
        selected_input_dir = NULL,
        metadata_loaded = FALSE,
        unlocked_tabs = c("pre_process"),
        last_allowed_tab = "pre_process"
    )

    tab_order <- c(
        "pre_process", "load_data", "normalization", "supervised",
        "graphs", "das_soi", "leaps",
        "topic_modeling", "humann_processing", "humann_graphs"
    )
    always_unlocked_tabs <- c("pre_process", "humann_processing")

    set_unlocked_tabs <- function(tabs) {
        tabs_clean <- unique(intersect(c(always_unlocked_tabs, tabs), tab_order))
        rv$unlocked_tabs <- tabs_clean
        current_last_allowed <- isolate(rv$last_allowed_tab)
        if (!(current_last_allowed %in% tabs_clean)) {
            rv$last_allowed_tab <- tabs_clean[[length(tabs_clean)]]
        }
    }

    unlock_all_tabs <- function() {
        set_unlocked_tabs(tab_order)
    }

    show_cat_waiter <- function(message = "Working...", cat_type = "typing") {
        cat_gif <- if (identical(cat_type, "error")) "gif/error_cat.gif" else "gif/cat_typing.gif"
        has_facts <- exists("data_about_cats") && length(data_about_cats) > 0
        initial_fact <- if (has_facts) sample(data_about_cats, 1) else NULL
        facts_json <- if (has_facts) {
            escaped <- gsub("\\\\", "\\\\\\\\", data_about_cats)
            escaped <- gsub('"', '\\\\"', escaped)
            paste0('["', paste(escaped, collapse = '\",\"'), '"]')
        } else {
            NULL
        }
        onload_rotate <- if (!is.null(facts_json)) {
            paste0(
                "(function(){",
                "var el=document.getElementById('cat-fact-text');",
                "if(!el){return;}",
                "var facts=JSON.parse(el.getAttribute('data-facts')||'[]');",
                "if(!facts.length){return;}",
                "if(window.__catFactInterval){clearInterval(window.__catFactInterval);}",
                "var prev=el.textContent;",
                "window.__catFactInterval=setInterval(function(){",
                "var currentEl=document.getElementById('cat-fact-text');",
                "if(!currentEl){clearInterval(window.__catFactInterval);window.__catFactInterval=null;return;}",
                "var next=prev;",
                "if(facts.length===1){next=facts[0];}",
                "else{while(next===prev){next=facts[Math.floor(Math.random()*facts.length)];}}",
                "currentEl.textContent=next;",
                "prev=next;",
                "},5000);",
                "})();"
            )
        } else {
            NULL
        }
        waiter_show(
            html = tagList(
                tags$div(
                    class = "waiter-overlay-content",
                    tags$img(
                        src = cat_gif,
                        alt = "Loading",
                        onload = onload_rotate,
                        class = "waiter-cat-img"
                    ),
                    tags$div(message, class = "waiter-message"),
                    if (!is.null(initial_fact)) tags$div(
                        class = "waiter-fact",
                        tags$span("Did you know? ", class = "waiter-fact-label"),
                        tags$span(
                            initial_fact,
                            id = "cat-fact-text",
                            `data-facts` = facts_json
                        )
                    )
                )
            ),
            color = "rgba(26, 46, 37, 0.80)"   # forest-dark overlay
        )
    }

    reset_after_metadata_edit <- function() {
        rv$remove_dn <- NULL
        rv$tab_list <- NULL
        rv$row_selected <- NULL
        rv$rds_obj <- NULL
        rv$soi_obj <- NULL
        rv$heatmap_plot <- NULL
        rv$title <- NULL
        rv$description <- NULL
        rv$table <- NULL
        rv$idColumn <- NULL
        rv$df_post <- NULL
        rv$AllAlright <- FALSE
        rv$ord_factor <- NULL
        rv$ntype_ord <- NULL
        rv$ntype_facets <- NULL
        rv$palettes <- NULL
        rv$preprocessed_file <- NULL
        rv$pipeline_name <- NULL
        rv$selected_input_dir <- NULL
        rv$output_directory <- NULL
        rv$contam_list <- NULL
        rv$plot <- NULL
        rv$plot_n <- 1
        rv$metadata_loaded <- FALSE
        set_unlocked_tabs(c("pre_process"))

        updateNavbarPage(session, "main_nav", selected = "pre_process")
        updateVirtualSelect("var", selected = character(0))
    }

    apply_tab_locks_ui <- function() {
        unlocked_tabs <- isolate(rv$unlocked_tabs)
        if (is.null(unlocked_tabs) || length(unlocked_tabs) == 0) {
            unlocked_tabs <- c("pre_process")
        }

        all_tabs_js <- paste(sprintf("'%s'", tab_order), collapse = ",")
        unlocked_tabs_js <- paste(sprintf("'%s'", unlocked_tabs), collapse = ",")

        shinyjs::runjs(paste0(
            "(function(){",
            "var allTabs=[", all_tabs_js, "];",
            "var unlocked=[", unlocked_tabs_js, "];",
            "allTabs.forEach(function(tab){",
            "var isUnlocked = unlocked.indexOf(tab) !== -1;",
            "var $a = $('a[data-value=\"' + tab + '\"]');",
            "var $li = $a.parent('li');",
            "if($li.length){",
            "if(isUnlocked){$li.removeClass('tab-locked');}else{$li.addClass('tab-locked');}",
            "}",
            "});",
            "})();"
        ))
    }

    session$onFlushed(function() {
        set_unlocked_tabs(c("pre_process"))
        updateNavbarPage(session, "main_nav", selected = "pre_process")
        apply_tab_locks_ui()
    }, once = TRUE)

    observe({
        req(rv$unlocked_tabs)
        apply_tab_locks_ui()
    })

    output$metadataLoaded <- reactive({
        isTRUE(rv$metadata_loaded)
    })
    outputOptions(output, "metadataLoaded", suspendWhenHidden = FALSE)

    observeEvent(input$main_nav, {
        req(input$main_nav)
        if (!(input$main_nav %in% rv$unlocked_tabs)) {
            showNotification("Step locked. Complete the previous steps first.", type = "warning")
            updateNavbarPage(session, "main_nav", selected = rv$last_allowed_tab)
            return()
        }
        rv$last_allowed_tab <- input$main_nav
    }, ignoreInit = TRUE)

    observeEvent(input$load, {
        req(input$load != 0)
        set_unlocked_tabs(c("pre_process", "load_data"))
        updateNavbarPage(session, "main_nav", selected = "load_data")
    }, ignoreInit = TRUE)

    observeEvent(input$editMetadata, {
        reset_after_metadata_edit()
        showNotification("Metadata editing re-enabled. Steps from Load Data onward were reset.", type = "message")
    })
    # metadata_df <- reactive({
    #     req(input$Metadata)
    #     read.csv(input$Metadata$datapath, header = TRUE, sep = ",")
    # })

    selected_input_dir <- reactive({
        req(input$Biom)
        selected_dir <- parseDirPath(volumes, input$Biom)
        req(length(selected_dir) > 0)
        selected_dir
    })

    detected_pipeline <- reactive({
        selected_dir <- selected_input_dir()

        candidate_files <- list.files(
            selected_dir,
            pattern = "\\.(report|txt)$",
            full.names = TRUE,
            ignore.case = TRUE
        )

        if (length(candidate_files) == 0) {
            showNotification("No .report or .txt file found in selected directory.", type = "error")
            return(NULL)
        }

        file_extensions <- tolower(tools::file_ext(candidate_files))

        if (all(file_extensions == "report")) {
            return("kraken")
        }

        if (all(file_extensions == "txt")) {
            return("metaphlan")
        }

        showNotification("Mixed file types found. Use a directory with only .report or only .txt files.", type = "error")
        NULL
    })

    data_df <- reactive({
        req(rv$preprocessed_file)
        ext <- tolower(tools::file_ext(rv$preprocessed_file))

        if (ext == "biom") {
            return(read_biom(rv$preprocessed_file))
        }

        if (ext == "txt") {
            return(read.table(rv$preprocessed_file, header = TRUE, sep = "\t"))
        }

        showNotification("Unsupported preprocessed file type.", type = "error")
        NULL
    })

    pipeline_type <- reactive({
        if (!is.null(rv$pipeline_name)) {
            return(rv$pipeline_name)
        }

        pipe <- detected_pipeline()
        req(pipe)

        if (pipe == "kraken") {
            showNotification("Pipeline selected: Kraken")
            return("kraken")
        }

        if (pipe == "metaphlan") {
            showNotification("Pipeline selected: MetaPhlAn")
            return("metaphlan")
        }

        showNotification("Unsupported pipeline type in selected directory.", type = "error")
        NULL
    })
    #### Find Volumes####
    RabAnalyser.getVolumes <- function(exclude, from = "~", custom_name = "Home") {
        osSystem <- Sys.info()["sysname"]
        userHome <- path_expand(from) # Get the user's home directory

        if (osSystem == "Darwin") {
            # volumes <- fs::dir_ls(userHome)
            # names(volumes) <- basename(volumes)
            volumes <- userHome
            names(volumes) <- basename(volumes)
        } else if (osSystem == "Linux") {
            running_in_docker <- file.exists("/.dockerenv")

            # Start from root to keep a generic fallback.
            volumes <- c(setNames("/", "Root"))

            # Prefer explicit host mounts when app runs in Docker.
            if (running_in_docker) {
                docker_mounts <- c(
                    HostHome = "/host_home",
                    HostUserProfile = "/host_userprofile",
                    HostC = "/host_c",
                    AppData = "/opt/app/data",
                    RunMedia = "/run/media",
                    Mnt = "/mnt",
                    Media = "/media"
                )
                docker_mounts <- docker_mounts[dir.exists(docker_mounts)]
                volumes <- c(volumes, docker_mounts)
            }

            # Add common mount points for external drives
            common_mounts <- c("/media", "/mnt", "/run/media")
            for (mount_point in common_mounts) {
                if (dir.exists(mount_point)) {
                    tryCatch(
                        {
                            subdirs <- list.dirs(mount_point, recursive = FALSE, full.names = TRUE)
                            if (length(subdirs) > 0) {
                                names(subdirs) <- basename(subdirs)
                                volumes <- c(volumes, subdirs)
                            }
                        },
                        error = function(e) {
                            # Silently skip if permission denied
                        }
                    )
                }
            }

            # Also add user home as convenience
            volumes <- c(volumes, setNames(userHome, custom_name))
            volumes <- volumes[!duplicated(volumes)]
        } else if (osSystem == "Windows") {
            userHome <- gsub("\\\\", "/", userHome) # Convert Windows path format
            volumes <- c(setNames(userHome, custom_name))

            # Check for mounted drives inside user home (e.g., OneDrive, Network Drives)
            possible_drives <- fs::dir_ls(userHome, type = "directory")
            names(possible_drives) <- basename(possible_drives)
            volumes <- c(volumes, possible_drives)
        } else {
            stop("unsupported OS")
        }

        if (!is.null(exclude)) {
            volumes <- volumes[!names(volumes) %in% exclude]
        }

        return(volumes)
    }
    #### SHINYJS ####
    shinyjs::hide("options_panel_k")
    shinyjs::hide("options_panel_m")
    shinyjs::disable("download_das")
    shinyjs::disable("downloadSOI")
    observe({
        req(pipeline_type())
        if (pipeline_type() == "kraken") {
            shinyjs::show("options_panel_k")
            shinyjs::hide("options_panel_m")
        } else {
            shinyjs::hide("options_panel_k")
            shinyjs::show("options_panel_m")
        }
    })



    volumes <- RabAnalyser.getVolumes(exclude = NULL)
    running_in_docker <- file.exists("/.dockerenv")
    shared_results_dir <- if (running_in_docker) {
        "/shared_results"
    } else {
        local_shared <- file.path(getwd(), "shared_results")
        if (!dir.exists(local_shared)) {
            dir.create(local_shared, recursive = TRUE, showWarnings = FALSE)
        }
        local_shared
    }

    rv$output_directory <- paste0(normalizePath(shared_results_dir, mustWork = FALSE), "/")

    detect_existing_processed_data <- function(base_dir) {
        if (is.null(base_dir) || !nzchar(base_dir) || !dir.exists(base_dir)) {
            return(list(available = FALSE, pipeline = NULL))
        }

        domains <- c("Bacteria", "Archaea", "Eukaryota")

        kraken_pairs <- vapply(domains, function(domain) {
            supervised_exists <- file.exists(file.path(base_dir, "SUPERVISED_DEC", paste0(domain, "_Supervised_decontam.rds")))
            alpha_exists <- file.exists(file.path(base_dir, "ALPHA", paste0(domain, "_alpha_metadata.csv")))
            supervised_exists && alpha_exists
        }, logical(1))

        metaphlan_pairs <- vapply(domains, function(domain) {
            supervised_exists <- any(file.exists(c(
                file.path(base_dir, "META_SUPERVISED_DEC", paste0(domain, "_Supervised_meta_decontam.rds")),
                file.path(base_dir, "META_SUPERVISED_DEC", paste0(domain, "_Supervised_meta_no_strain.rds"))
            )))
            alpha_exists <- file.exists(file.path(base_dir, "ALPHA", paste0(domain, "_meta_alpha_metadata.csv")))
            supervised_exists && alpha_exists
        }, logical(1))

        n_kraken <- sum(kraken_pairs)
        n_metaphlan <- sum(metaphlan_pairs)

        if (n_kraken == 0 && n_metaphlan == 0) {
            return(list(available = FALSE, pipeline = NULL))
        }

        if (n_kraken >= n_metaphlan) {
            return(list(available = TRUE, pipeline = "kraken"))
        }

        list(available = TRUE, pipeline = "metaphlan")
    }

    output$subset_name_DAS <- renderUI({
        req(rv$df_post, input$element_selected_das)
        vals <- unique(rv$df_post[[input$element_selected_das]])
        virtualSelectInput(
            "subset_name_DAS",
            "Subset elements (DAS)",
            choices = vals,
            selected = vals,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })
    observeEvent(input$uploadData, {
        req(rv$df_post, rv$output_directory, input$idColumn)
        colnames(rv$df_post) <- tolower(colnames(rv$df_post))

        existing_processed <- detect_existing_processed_data(rv$output_directory)

        selected_dir <- tryCatch(
            {
                parseDirPath(volumes, input$Biom)
            },
            error = function(e) {
                character(0)
            }
        )
        has_input_dir <- length(selected_dir) > 0

        if (!has_input_dir && !existing_processed$available) {
            showNotification(
                "Select Data directory, or use an Output directory that already contains Supervised decontam + at least one ALPHA metadata file.",
                type = "error"
            )
            return()
        }

        if (!has_input_dir && existing_processed$available) {
            rv$pipeline_name <- existing_processed$pipeline
            rv$preprocessed_file <- NULL
            showNotification(
                paste0(
                    "Existing processed data detected in Output directory (",
                    rv$pipeline_name,
                    "). Data directory is not required."
                ),
                type = "message"
            )
        }

        selected_id_column <- input$idColumn
        if (!(selected_id_column %in% colnames(rv$df_post))) {
            showNotification(
                paste0("Selected column for Data/Metadata match '", selected_id_column, "' not found in metadata."),
                type = "error"
            )
            return()
        }

        if (!identical(selected_id_column, "id")) {
            # If an existing 'id' column is present, preserve it to avoid collisions.
            if ("id" %in% colnames(rv$df_post)) {
                new_original <- "id_original"
                if (new_original %in% colnames(rv$df_post)) {
                    suffix <- 1L
                    while (paste0(new_original, "_", suffix) %in% colnames(rv$df_post)) suffix <- suffix + 1L
                    new_original <- paste0(new_original, "_", suffix)
                }
                names(rv$df_post)[names(rv$df_post) == "id"] <- new_original
                showNotification(paste0("Existing 'id' column renamed to '", new_original, "'."), type = "message")
            }

            names(rv$df_post)[names(rv$df_post) == selected_id_column] <- "id"
            showNotification(paste0("Column '", selected_id_column, "' renamed to 'id'."), type = "message")
        } else {
            showNotification("ID column already set to 'id' — no column rename needed.", type = "message")
        }

        # Normalize and validate IDs used for matching data and metadata.
        rv$df_post$id <- as.character(rv$df_post$id)
        rv$df_post$id <- trimws(rv$df_post$id)
        rv$df_post$id <- gsub("[^A-Za-z0-9]", "_", rv$df_post$id)
        rv$df_post$id <- gsub("_+", "_", rv$df_post$id)
        rv$df_post$id <- gsub("^_|_$", "", rv$df_post$id)

        empty_id_idx <- which(is.na(rv$df_post$id) | rv$df_post$id == "")
        if (length(empty_id_idx) > 0) {
            showNotification(
                paste0(
                    "Metadata ID check failed: ",
                    length(empty_id_idx),
                    " empty/invalid IDs after normalization. Please fix the selected ID column."
                ),
                type = "error"
            )
            return()
        }

        duplicated_ids <- unique(rv$df_post$id[duplicated(rv$df_post$id)])
        if (length(duplicated_ids) > 0) {
            showNotification(
                paste0(
                    "Metadata ID check failed: duplicated IDs found after normalization (e.g. ",
                    paste(utils::head(duplicated_ids, 5), collapse = ", "),
                    ")."
                ),
                type = "error",
                duration = NULL
            )
            return()
        }

        rv$idColumn <- "id"

        if (has_input_dir) {
            preprocess_res <- tryCatch(
                {
                    run_preprocess_pipeline(folder_input = selected_dir, execute = TRUE)
                },
                error = function(e) {
                    show_cat_waiter(
                        message = paste("Preprocessing failed:", conditionMessage(e)),
                        cat_type = "error"
                    )
                    shinyjs::delay(2500, waiter_hide())
                    showNotification(paste("Preprocessing failed:", conditionMessage(e)), type = "error")
                    NULL
                }
            )

            if (is.null(preprocess_res) || is.null(preprocess_res$output_file)) {
                return()
            }

            rv$pipeline_name <- preprocess_res$pipeline_name
            rv$preprocessed_file <- preprocess_res$output_file

            if (is.null(data_df())) {
                show_cat_waiter(
                    message = "Preprocessing completed but output file is not readable.",
                    cat_type = "error"
                )
                shinyjs::delay(2500, waiter_hide())
                showNotification("Preprocessing completed but output file is not readable.", type = "error")
                return()
            }
        }

        # Optional: check rv$table (metadata) if needed
        # if (is.null(rv$table)) {
        #     showNotification("Please upload a valid metadata file.", type = "error")
        #     return()
        # }
        rds_dir <- file.path(rv$output_directory, "RDS/")
        if (!dir.exists(rds_dir)) {
            ok <- tryCatch(
                {
                    dir.create(rds_dir, recursive = TRUE, showWarnings = FALSE)
                    TRUE
                },
                error = function(e) {
                    showNotification(paste("Unable to create RDS folder:", conditionMessage(e)), type = "error")
                    FALSE
                }
            )
            if (ok) showNotification(paste("Created folder:", rds_dir), type = "message")
        }
        current_pipeline <- pipeline_type()
        if (current_pipeline == "kraken" && !is.null(rv$preprocessed_file)) {
            res <- tryCatch(
                {
                    KrakenDataImport(rv$preprocessed_file, rv$df_post, rds_dir, input$remove_data_name)
                },
                error = function(e) {
                    showNotification(paste("Errore nell'importazione Kraken:", conditionMessage(e)), type = "error")
                    NULL
                }
            )
            rv$tab_list <- res
        } else {
            rv$remove_dn <- input$remove_data_name
        }

        existing_after_upload <- detect_existing_processed_data(rv$output_directory)
        if (existing_after_upload$available) {
            unlock_all_tabs()
        } else {
            set_unlocked_tabs(c("pre_process", "load_data", "normalization"))
            updateNavbarPage(session, "main_nav", selected = "normalization")
        }

        showNotification("Data uploaded successfully!", type = "message")
    })

    # Collega il bottone alla scelta della directory
    shinyDirChoose(input, "Biom", roots = volumes, session = session)
    if (!running_in_docker) {
        shinyDirChoose(input, "output_dir", roots = volumes, session = session)
    }

    # HUMAnN folder chooser for processing
    shinyDirChoose(input, "humann_input_dir", roots = volumes, session = session)
    # Pathway fit_out save folder chooser
    shinyDirChoose(input, "pathway_fitout_dir", roots = volumes, session = session)

    output$meta_folder <- renderPrint({
        req(selected_input_dir())
        selected_input_dir()
    })
    output$output_dir <- renderPrint({
        req(rv$output_directory)
        rv$output_directory
    })
    output$output_directory <- renderPrint({
        req(rv$output_directory)
        rv$output_directory
    })
    output$output_dir_selector_ui <- renderUI({
        if (running_in_docker) {
            tags$p(
                "Running in Docker: output directory is automatically set to /shared_results.",
                style = "margin-top: 10px;"
            )
        } else {
            withTooltip(
                shinyDirButton(
                    "output_dir",
                    "Working Directory", "Please select a folder",
                    class = "refined-button"
                ),
                Output_directory_set
            )
        }
    })
    output$download_shared_results_ui <- renderUI({
        if (running_in_docker) {
            downloadButton("download_shared_results", "Download Shared Results")
        } else {
            NULL
        }
    })
    output$pathway_fitout_dir_text <- renderText({
        req(input$pathway_fitout_dir)
        parseDirPath(volumes, input$pathway_fitout_dir)
    })
    output$humann_input_folder <- renderPrint({
        req(input$humann_input_dir)
        parseDirPath(volumes, input$humann_input_dir)
    })
    observeEvent(input$output_dir, {
        req(!running_in_docker)
        req(input$output_dir)
        path <- parseDirPath(volumes, input$output_dir)
        rv$output_directory <- paste0(path, "/")
    })

    sanitize_filename_part <- function(x) {
        if (is.null(x) || length(x) == 0) return("unknown")
        x <- as.character(x[1])
        x <- trimws(x)
        if (!nzchar(x)) return("unknown")
        x <- gsub("[^A-Za-z0-9._-]+", "-", x)
        x <- gsub("-+", "-", x)
        x <- gsub("(^-+|-+$)", "", x)
        if (!nzchar(x)) "unknown" else x
    }

    get_pipeline_suffix <- function() {
        pipe <- tryCatch(pipeline_type(), error = function(e) NULL)
        paste0("_", sanitize_filename_part(if (is.null(pipe)) "pipeline" else pipe))
    }

    get_single_minus_suffix <- function(all_values, selected_values) {
        if (is.null(all_values) || is.null(selected_values)) return("")
        all_chr <- as.character(stats::na.omit(all_values))
        sel_chr <- as.character(stats::na.omit(selected_values))
        if (length(all_chr) == 0 || length(sel_chr) == 0) return("")
        removed <- setdiff(unique(all_chr), unique(sel_chr))
        if (length(removed) == 1) {
            return(paste0("_minus_", sanitize_filename_part(removed)))
        }
        ""
    }

    output$download_shared_results <- downloadHandler(
        filename = function() {
            paste0(
                "shared_results",
                get_pipeline_suffix(),
                "_",
                format(Sys.time(), "%Y%m%d_%H%M%S"),
                ".tar.gz"
            )
        },
        content = function(file) {
            shared_dir <- if (running_in_docker) "/shared_results" else normalizePath(shared_results_dir, mustWork = FALSE)
            if (!dir.exists(shared_dir)) {
                stop("Shared results directory not found. Check the /shared_results Docker mount.")
            }

            owd <- getwd()
            on.exit(setwd(owd), add = TRUE)
            setwd(dirname(shared_dir))

            utils::tar(
                tarfile = file,
                files = basename(shared_dir),
                compression = "gzip",
                tar = "internal"
            )
        }
    )

    observeEvent(input$Biom, {
        rv$preprocessed_file <- NULL
        rv$pipeline_name <- NULL
        rv$selected_input_dir <- selected_input_dir()
    })

    observeEvent(input$file, {
        if (isTRUE(rv$metadata_loaded) || !is.null(rv$df_post)) {
            reset_after_metadata_edit()
        }
    }, ignoreInit = TRUE)

    # Start HUMAnN processing sequence when user clicks the button
    observeEvent(input$startHumann, {
        req(input$humann_input_dir)
        print(paste0("cartella passata: ", input$humann_input_dir))
        print(paste0("volumes: ", names(volumes)))

        # Determine folders - parseDirPath returns a character vector, need to build full path
        input_dir_parsed <- parseDirPath(volumes, input$humann_input_dir)
        print(paste0("input_dir_parsed: ", input_dir_parsed))
        print(paste0("length input_dir_parsed: ", length(input_dir_parsed)))

        if (length(input_dir_parsed) == 0) {
            showNotification("Please select a valid input folder", type = "error")
            return()
        }

        input_folder <- file.path(input_dir_parsed)
        print(paste0("input_folder final: ", input_folder))
        print("DEBUG: About to determine base_out...")
        # prefer humann_output_dir if provided, otherwise use global rv
        base_out <- file.path(input_folder, "HUMANN_OUTPUT")
        print(paste0("DEBUG: base_out = ", base_out))

        print("DEBUG: About to create directory...")
        dir.create(base_out, recursive = TRUE, showWarnings = FALSE)
        print("DEBUG: Directory created, showing waiter...")

        show_cat_waiter("HUMAnN processing in progress...")
        print("DEBUG: Waiter shown, defining notify_step...")
        notify_step <- function(msg, type = "message") showNotification(msg, type = type)

        # Debug: show what paths we're using
        notify_step(paste0("DEBUG: input_folder = ", input_folder))
        notify_step(paste0("DEBUG: base_out = ", base_out))

        tryCatch({
            # 1) First normalization: input_folder -> base_out/norm
            out_norm1 <- file.path(base_out, "norm")
            dir.create(out_norm1, recursive = TRUE, showWarnings = FALSE)
            notify_step(paste0("HUMAnN: running norm_table on ", input_folder, " -> ", out_norm1))

            # Check if humann3Norm functions are available
            if (!exists("norm_table")) {
                stop("norm_table function not found. Please source R/humann3Norm.R")
            }

            norm_table(input_folder, out_norm1)
            notify_step("HUMAnN: norm_table completed")

            # 2) merge tables: input = out_norm1, output = base_out/merge_tables
            out_merge <- file.path(base_out, "merge_tables")
            dir.create(out_merge, recursive = TRUE, showWarnings = FALSE)
            notify_step(paste0("HUMAnN: running merge_humann_tables on ", out_norm1, " -> ", out_merge))
            merge_humann_tables(out_norm1, out_merge)
            notify_step("HUMAnN: merge_humann_tables completed")

            # 3) regroup_name_humann: input = out_norm1, output = base_out/regroup
            out_regroup <- file.path(base_out, "regroup")
            dir.create(out_regroup, recursive = TRUE, showWarnings = FALSE)
            notify_step(paste0("HUMAnN: running regroup_name_humann on ", out_norm1, " -> ", out_regroup))
            regroup_name_humann(out_norm1, out_regroup)

            # 4) rename_humann_tables: input = out_regroup, output = base_out/renamed
            out_renamed <- file.path(base_out, "renamed")
            dir.create(out_renamed, recursive = TRUE, showWarnings = FALSE)
            notify_step(paste0("HUMAnN: running rename_humann_tables on ", out_regroup, " -> ", out_renamed))
            rename_humann_tables(out_regroup, out_renamed)

            # 5) redo normalization on renamed tables: input = out_renamed, output = base_out/renamed_grouped
            out_renorm2 <- file.path(base_out, "renamed_grouped")
            dir.create(out_renorm2, recursive = TRUE, showWarnings = FALSE)
            notify_step(paste0("HUMAnN: running norm_table on renamed tables ", out_renamed, " -> ", out_renorm2))
            norm_table(out_renamed, out_renorm2)

            notify_step("HUMAnN processing finished", "message")
        }, error = function(e) {
            showNotification(paste("Error during HUMAnN processing:", conditionMessage(e)), type = "error", duration = NULL)
        }, finally = {
            waiter_hide()
        })
    })
    #### UPLOAD FILE ####
    output$biom_file_contents <- renderText({
        if (!is.null(rv$preprocessed_file)) {
            return(paste("Preprocessed file:", basename(rv$preprocessed_file)))
        }

        selected_dir <- tryCatch(
            {
                parseDirPath(volumes, input$Biom)
            },
            error = function(e) {
                character(0)
            }
        )

        if (length(selected_dir) > 0) {
            return(paste("Data directory selected:", selected_dir))
        }

        existing_processed <- detect_existing_processed_data(rv$output_directory)
        if (existing_processed$available) {
            return(
                paste0(
                    "Data directory not selected. Existing ",
                    existing_processed$pipeline,
                    " Supervised+ALPHA files detected in Output directory."
                )
            )
        }

        "Data directory not selected."
    })

    output$design_input_dynamic <- renderUI({
        req(rv$df_post, pipeline_type())

        colnames_data <- names(rv$df_post)[
            sapply(rv$df_post, function(x) is.factor(x) || is.ordered(x))
        ]

        tags$div(
            style = "width: 100%; overflow: hidden;",
          selectInput(
    "selectDesign",
    "Select element for Design:",
    choices = setNames(colnames_data, colnames_data),
    width = "100%",
    selectize = FALSE
)
        )
    })

    observeEvent(input$startDecontam, {
        req(pipeline_type(), rv$df_post)
        if (pipeline_type() == "kraken") {
            show_cat_waiter("Normalization in progress...")
            rv$contam_list <- process_file(
                list(name = basename(rv$preprocessed_file), datapath = rv$preprocessed_file), rv$df_post,
                input$Bacteria_level, input$Archaea_level, input$Eukaryota_level,
                input$selectDesign, rv$output_directory, input$do_normalization, rv$tab_list
            )
        } else if (pipeline_type() == "metaphlan") {
            show_cat_waiter("Normalization in progress...")

            data_path <- rv$preprocessed_file
            if (is.list(data_path) && !is.null(data_path$datapath)) {
                data_path <- data_path$datapath
            }
            if (!is.character(data_path) || length(data_path) != 1 || !nzchar(data_path) || !file.exists(data_path)) {
                waiter_hide()
                showNotification(
                    "MetaPhlAn normalization error: preprocessed input file not found. Please run Upload Data again.",
                    type = "error",
                    duration = NULL
                )
                return()
            }

            # Run preprocessing on the uploaded MetaPhlAn table. Pass the processed
            # metadata (rv$df_post) rather than a file path for metadata.
            MetaphlanDataImportDecontam(
                data_path = data_path,
                baselines_metadata = rv$df_post,
                output_dir = rv$output_directory,
                remove_data_name = rv$remove_dn,
                threshold = input$decont_level_metaphlan
            )

            # The input directory selected at the beginning is used for taxid lookup.
            metaphlan_folder <- selected_input_dir()
            taxid_path <- MetaSearchTaxid(metaphlan_folder, rv$output_directory)

            # MetaphlanDataImportDecontam writes merged/filtered tables into
            # rv$output_directory/META_DEC/. Use the merged table as input for
            # MetaRdsCreator instead of the original upload path.
            merged_table_path <- file.path(rv$output_directory, "META_DEC", "ALL_baseline_phylo.txt")

            rv$contam_list <- MetaRdsCreator(
                baselines_metadata = rv$df_post,
                data_path = merged_table_path,
                taxid_path = taxid_path,
                out_dir = file.path(rv$output_directory, "META_RDS"),
                bacteria_level = input$Bacteria_level_meta,
                archaea_level = input$Archaea_level_meta,
                eukaryota_level = input$Eukaryota_level_meta,
                string_rem = rv$remove_dn
            )
        }

        waiter_hide()
        set_unlocked_tabs(c("pre_process", "load_data", "normalization", "supervised"))
        updateNavbarPage(session, "main_nav", selected = "supervised")
        showNotification("Normalization complete", type = "message")
    })

    # Extract unique taxa from an existing RDS at the requested taxonomic level
    load_taxa_from_rds <- function(rds_path, level) {
        if (!file.exists(rds_path)) return(character(0))
        tryCatch({
            obj <- readRDS(rds_path)
            tax <- data.frame(phyloseq::tax_table(obj))
            col <- if (level %in% colnames(tax)) level else NULL
            if (is.null(col)) return(character(0))
            sort(unique(na.omit(tax[[col]])))
        }, error = function(e) character(0))
    }

    load_bacteria_spike_taxid_choices <- function(rds_path) {
        if (!file.exists(rds_path)) return(character(0))
        tryCatch({
            obj <- readRDS(rds_path)
            tax <- data.frame(phyloseq::tax_table(obj))
            taxid <- rownames(tax)
            if (length(taxid) == 0) return(character(0))

            species <- if ("Species" %in% colnames(tax)) as.character(tax$Species) else rep(NA_character_, nrow(tax))
            genus <- if ("Genus" %in% colnames(tax)) as.character(tax$Genus) else rep(NA_character_, nrow(tax))
            species_label <- ifelse(
                !is.na(species) & nzchar(species),
                ifelse(!is.na(genus) & nzchar(genus), paste(genus, species), species),
                "unknown species"
            )

            labels <- paste0(taxid, " | ", species_label)
            names(taxid) <- labels
            taxid[order(labels)]
        }, error = function(e) character(0))
    }

    # Reactive: paths of the 3 kingdom RDS for the current pipeline
    kingdom_rds_paths <- reactive({
        req(rv$output_directory, pipeline_type())
        if (pipeline_type() == "kraken") {
            list(
                Bacteria  = file.path(rv$output_directory, "DESEQ_RDS", "Bact_DeSeq.rds"),
                Archaea   = file.path(rv$output_directory, "DESEQ_RDS", "Archaea_DeSeq.rds"),
                Eukaryota = file.path(rv$output_directory, "DESEQ_RDS", "Eukaryota_DeSeq.rds")
            )
        } else {
            list(
                Bacteria  = file.path(rv$output_directory, "META_RDS", "Bacteria_meta_phylo.rds"),
                Archaea   = file.path(rv$output_directory, "META_RDS", "Archaea_meta_phylo.rds"),
                Eukaryota = file.path(rv$output_directory, "META_RDS", "Eukaryota_meta_phylo.rds")
            )
        }
    })

    # Real-time checkbox update: read taxa directly from existing RDS, no re-normalization
    observeEvent(
        list(
            input$Bacteria_level, input$Archaea_level, input$Eukaryota_level,
            input$Bacteria_level_meta, input$Archaea_level_meta, input$Eukaryota_level_meta
        ),
        {
            paths <- tryCatch(kingdom_rds_paths(), error = function(e) NULL)
            if (is.null(paths)) return()
            pipe <- pipeline_type()
            bact_lev  <- if (pipe == "kraken") input$Bacteria_level  else input$Bacteria_level_meta
            arch_lev  <- if (pipe == "kraken") input$Archaea_level   else input$Archaea_level_meta
            euk_lev   <- if (pipe == "kraken") input$Eukaryota_level else input$Eukaryota_level_meta

            rv$contam_list$list_of_decontam$bacteria  <- load_taxa_from_rds(paths$Bacteria,  bact_lev)
            rv$contam_list$list_of_decontam$archaea   <- load_taxa_from_rds(paths$Archaea,   arch_lev)
            rv$contam_list$list_of_decontam$eukaryota <- load_taxa_from_rds(paths$Eukaryota, euk_lev)
        },
        ignoreInit = TRUE
    )
    ######### Manual Decontamination#########
    observeEvent(input$startManual, {
        req(pipeline_type())

        show_cat_waiter("Manual decontamination in progress...")

        if (pipeline_type() == "kraken") {
            bacteria_lev <- input$Bacteria_level
            archaea_lev <- input$Archaea_level
            eukaryota_lev <- input$Eukaryota_level
        } else {
            bacteria_lev <- input$Bacteria_level_meta
            archaea_lev <- input$Archaea_level_meta
            eukaryota_lev <- input$Eukaryota_level_meta
        }

        # Selezioni opzionali
        bacteria_sel <- if (!is.null(input$bacteria_selected)) input$bacteria_selected else character(0)
        archaea_sel <- if (!is.null(input$archaea_selected)) input$archaea_selected else character(0)
        eukaryota_sel <- if (!is.null(input$eukaryota_selected)) input$eukaryota_selected else character(0)
        bacteria_spike_taxids <- if (!is.null(input$bacteria_spike_taxids)) input$bacteria_spike_taxids else character(0)

        SupervisedDecontam(
            bacteria_selected = bacteria_sel,
            archaea_selected = archaea_sel,
            eukaryota_selected = eukaryota_sel,
            bacteria_level = bacteria_lev,
            archaea_level = archaea_lev,
            eukaryota_level = eukaryota_lev,
            pipeline = pipeline_type(),
            output_folder = rv$output_directory,
            decont_level = input$decont_level,
            bacteria_spike_taxids = bacteria_spike_taxids
        )

        waiter_hide()
        unlock_all_tabs()
        showNotification("Manual decontamination complete", type = "message")
        updateCollapse(session, "acc",
            open = "Alpha+Beta Diversity plus Stackbar",
            close = "Supervised Decontamination"
        )
    })


    observeEvent(input$selectRows, {
        req(input$element_selected, rv$df_post)

        row_selected(input$element_selected)

        valid_choices <- intersect(input$element_selected, colnames(rv$df_post))
        updateSelectInput(session, "row_selected", choices = valid_choices)
    })

    createCheckboxGroup <- function(id, label, choices, pre_list) {
        if (is.null(choices) || length(choices) == 0) {
            return(NULL)
        }

        # Ordina le scelte in ordine alfanumerico
        sorted_choices <- sort(choices)

        if (is.null(pre_list) || length(pre_list) == 0) {
            virtualSelectInput(
                id,
                label,
                choices = sorted_choices,
                multiple = TRUE,
                showValueAsTags = TRUE,
                clearButton = TRUE,
                search = TRUE
            )
        } else {
            virtualSelectInput(
                id,
                label,
                choices = sorted_choices,
                selected = sorted_choices[sorted_choices %in% pre_list],
                multiple = TRUE,
                showValueAsTags = TRUE,
                clearButton = TRUE,
                search = TRUE
            )
        }
    }
    createSelectInput <- function(id, label, choices) {
        if (is.null(choices) || length(choices) == 0) {
            return(NULL)
        }

        selectInput(
            id,
            label,
            choices = choices,
            selected = choices[1]
        )
    }

    output$checkBoxBacteria <- renderUI({
        req(rv$contam_list)
        createCheckboxGroup(
            "bacteria_selected", "Bacteria",
            rv$contam_list$list_of_decontam$bacteria, human_phylaB
        )
    })

    output$bacteriaSpikeTaxidUi <- renderUI({
        paths <- tryCatch(kingdom_rds_paths(), error = function(e) NULL)
        choices <- if (is.null(paths)) character(0) else load_bacteria_spike_taxid_choices(paths$Bacteria)

        if (length(choices) == 0) {
            return(tags$p(
                style = "font-size: 0.82em; color: #888; margin-bottom: 0;",
                "No bacterial taxid available yet. Run normalization first."
            ))
        }

        virtualSelectInput(
            "bacteria_spike_taxids",
            "Taxid to remove:",
            choices = choices,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })

    output$checkBoxArchaea <- renderUI({
        req(rv$contam_list, pipeline_type())
        createCheckboxGroup(
            "archaea_selected", "Archaea",
            rv$contam_list$list_of_decontam$archaea, human_phylaA
        )
    })

    output$checkBoxEukaryota <- renderUI({
        req(rv$contam_list, pipeline_type())
        createCheckboxGroup(
            "eukaryota_selected", "Eukaryota",
            rv$contam_list$list_of_decontam$eukaryota, human_genus
        )
    })

    output$checkBoxHeatmap <- renderUI({
        req(rv$contam_list)
        createCheckboxGroup(
            "das_elements",
            "Elements:", rv$contam_list$tab_list, NULL
        )
    })

    output$selectEl_graphs <- renderUI({
        req(rv$contam_list)
        createCheckboxGroup(
            "element_selected_graphs",
            "Elements", rv$contam_list$tab_list, ""
        )
    })
    output$selectEl_soi <- renderUI({
        req(rv$df_post)
        createSelectInput(
            "selectCol_soi",
            "Discriminant", colnames(rv$df_post)
        )
    })
    output$selectEl_das_pathway <- renderUI({
        req(rv$df_post)
        createSelectInput(
            "element_selected_das_pathway",
            "Discriminant:", colnames(rv$df_post)
        )
    })


    output$setIdColumn <- renderUI({
        req(rv$df_post)
        createSelectInput(
            "idColumn",
            "Select ID Column:",
            colnames(rv$df_post)
        )
    })

    output$selectDiscr_dss <- renderUI({
        req(rv$df_post)
        createSelectInput("discr_element", "Discriminant:", colnames(rv$df_post))
    })

    output$subSetDAS <- renderUI({
        req(rv$df_post)
        tagList(
            selectInput("subset", "Subset:", choices = colnames(rv$df_post)),
            uiOutput("subsetElements")
        )
    })
    output$subSetDAS_pathway <- renderUI({
        req(rv$df_post)
        tagList(
            selectInput("subset_pathway", "Subset:", choices = colnames(rv$df_post)),
            uiOutput("subsetElements_pathway")
        )
    })



    output$leapsUi <- renderUI({
        req(rv$df_post)
        tagList(
            selectInput(
                "Col_subset",
                "Column to do subset:",
                choices = colnames(rv$df_post)
            ),
            uiOutput("leapsUi2"),
            virtualSelectInput(
                "leaps_predictors",
                "Select predictors:",
                choices = colnames(rv$df_post),
                multiple = TRUE,
                showValueAsTags = TRUE,
                clearButton = TRUE,
                search = TRUE
            )
        )
    })



    # UI secondaria: checkbox con i valori unici della colonna selezionata
    output$leapsUi2 <- renderUI({
        req(input$Col_subset)

        selected_col <- input$Col_subset
        unique_values <- unique(rv$df_post[[selected_col]])

        virtualSelectInput(
            "leaps_factors",
            label = paste("Elements to maintain for", selected_col),
            choices = unique_values,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })


    output$select_discrDAS <- renderUI({
        req(rv$df_post)
        vals <- names(rv$df_post)
        virtualSelectInput("discr_element", "Confounders", choices = vals,
            multiple = TRUE, showValueAsTags = TRUE, clearButton = TRUE, search = TRUE)
    })
    output$select_discrDAS_pathway <- renderUI({
        req(rv$df_post)
        vals <- names(rv$df_post)
        virtualSelectInput("discr_element_pathway", "Confounders", choices = vals,
            multiple = TRUE, showValueAsTags = TRUE, clearButton = TRUE, search = TRUE)
    })

    output$subsetElements <- renderUI({
        req(rv$df_post, input$subset)

        vals <- unique(rv$df_post[[input$subset]])

        virtualSelectInput(
            "subset_name",
            "Subset elements:",
            choices = vals,
            selected = vals,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })
    output$subsetElements_pathway <- renderUI({
        req(rv$df_post, input$subset_pathway)

        vals <- unique(rv$df_post[[input$subset_pathway]])

        virtualSelectInput(
            "subset_name_pathway",
            "Subset elements:",
            choices = vals,
            selected = vals,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })


    output$DiscrElements <- renderUI({
        # Require the metadata
        req(rv$df_post, rv$colTypes)

        # Choices should be the column names of rv$df_post that are factors
        choices <- names(rv$colTypes)[rv$colTypes == "factor"]
        choices <- intersect(choices, colnames(rv$df_post))
        if (!is.null(input$element_selected_dss) && input$element_selected_dss %in% choices) {
            choices <- setdiff(choices, input$element_selected_dss)
        }

        virtualSelectInput("discr_name", "Confounders",
            choices = choices,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })

    # observeEvent(input$subset, {
    #     req(rv$table, input$subset)

    #     updateCheckboxGroupInput(session, "subset_name",
    #         label = "Subset elements",
    #         choices = unique(rv$table[[input$subset]])
    #     )
    # })
    # ################################## TRY TO FIX THE PROBLEM
    output$Elements_Soi <- renderUI({
        req(rv$df_post, input$selectColEL_soi)

        vals <- unique(rv$df_post[[input$selectColEL_soi]])

        virtualSelectInput(
            "subset_name_soi",
            "Subset elements:",
            choices = vals,
            selected = vals,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })
    # ################################
    # Update discriminator choices when the selected DAS element or metadata changes
    observeEvent(list(input$element_selected_dss, rv$df_post, rv$colTypes), {
        req(rv$df_post, rv$colTypes)
        choices <- names(rv$colTypes)[rv$colTypes == "factor"]
        choices <- intersect(choices, colnames(rv$df_post))
        if (!is.null(input$element_selected_dss) && input$element_selected_dss %in% choices) {
            choices <- setdiff(choices, input$element_selected_dss)
        }

        updateVirtualSelect("discr_name",
            choices = choices
        )
    })

    selectedData <- reactive({
        req(input$xcol, input$ycol)
        iris[, c(input$xcol, input$ycol)]
    })
    ######### DAS ####
    observeEvent(input$createSOI, {
        req(
            input$selectCol_soi,
            input$selectColEL_soi, input$subset_name_soi, input$run_soi, input$kingdomSOI, pipeline_type(), rv$df_post
        )

        selected_el <- input$selectCol_soi
        selected_subset <- input$selectColEL_soi
        selected_subset_elements <- input$subset_name_soi
        program <- input$run_soi
        kingdom <- input$kingdomSOI
        metadata <- rv$df_post
        show_cat_waiter("DAS analysis in progress...")
        pipeline <- pipeline_type()
        discriminants <- input$discr_element
        SOI <- create_soi(
            selected_el, selected_subset,
            selected_subset_elements, program, kingdom, pipeline,
            metadata, rv$output_directory
        )

        rv$soi_obj <- SOI
        shinyjs::enable("downloadSOI")

        waiter_hide()
        showNotification("DAS analysis complete", type = "message")
    })
    observeEvent(input$createDAS, {
        print(str(list(
            selectEl_das = input$element_selected_das,
            subSetDAS = input$subset,
            subset_name = input$subset_name,
            run_das = input$run_das,
            pipeline = pipeline_type(),
            df_post = rv$df_post,
            kingdomDAS = input$kingdomDAS
        )))
        req(
            input$element_selected_das,
            input$subset, input$subset_name, input$run_das, input$kingdomDAS, pipeline_type(), rv$df_post, input$significance_metric_das,
            input$maaslin_model_filter
        )
        analisys <- input$element_selected_das
        sub_category <- input$subset
        selected_el <- input$selectEl_das
        sub_names <- input$subset_name
        program <- input$run_das
        pipeline <- pipeline_type()
        metadata <- rv$df_post
        kingdom <- input$kingdomDAS
        discriminants <- input$discr_element
        output_folder <- rv$output_directory
        sign <- input$significance_metric_das
        maaslin_model_filter <- input$maaslin_model_filter

        show_cat_waiter("DAS analysis in progress...")
        tryCatch(
            {
                DAS <- run_das_analysis(
                    analisys, sub_category, sub_names, program, kingdom, pipeline, output_folder,
                    metadata,
                    discriminants = discriminants, sign = sign, model_filter = maaslin_model_filter
                )

                rv$rds_obj <- DAS
                # Salva su disco temporaneo per l'heatmap integrata
                tmp <- tempfile(fileext = ".rds")
                saveRDS(DAS, tmp)
                rv$rds_path <- tmp
                shinyjs::enable("download_das")
                showNotification("DAS analysis complete — puoi ora creare l'Heatmap", type = "message")
            },
            error = function(e) {
                rv$rds_obj <- NULL
                showNotification(
                    paste("Error during DAS analysis:", conditionMessage(e)),
                    type = "error",
                    duration = NULL
                )
                cat("[ERROR] DAS analysis:", conditionMessage(e), "\n")
            },
            finally = {
                waiter_hide()
            }
        )
    })

    output$download_das <- downloadHandler(
        filename = function() {
            minus_suffix <- ""
            if (!is.null(input$subset) && !is.null(input$subset_name) && !is.null(rv$df_post) && input$subset %in% colnames(rv$df_post)) {
                minus_suffix <- get_single_minus_suffix(rv$df_post[[input$subset]], input$subset_name)
            }
            paste0(
                "DAS_",
                sanitize_filename_part(input$element_selected_das),
                "_",
                sanitize_filename_part(input$run_das),
                get_pipeline_suffix(),
                minus_suffix,
                "_",
                Sys.Date(),
                ".rds"
            )
        },
        content = function(file) {
            saveRDS(rv$rds_obj, file)
        }
    )
    output$downloadSOI <- downloadHandler(
        filename = function() {
            paste0(
                "SOI_",
                sanitize_filename_part(input$element_selected_soi),
                "_",
                sanitize_filename_part(input$run_soi),
                get_pipeline_suffix(),
                "_",
                Sys.Date(),
                ".rds"
            )
        },
        content = function(file) {
            saveRDS(rv$soi_obj, file)
        }
    )

    output$contamTable <- renderTable({
        req(rv$contam_list)
        rv$contam_list
    })
    #### ALPHA DIVERSITY ####
    output$setVarAlpha <- renderUI({
        req(rv$df_post, rv$colTypes)
        factor_vars <- tolower(names(rv$colTypes)[rv$colTypes == "factor"])
        selectInput("row_selected_alpha", "Variable",
            choices = factor_vars
        )
    })

    alphaPlot <- reactive({
        req(input$Domain, input$alpha, input$row_selected_alpha, pipeline_type(), input$subset_alpha, input$subset_name_alpha)

        domain <- input$Domain
        pipe <- pipeline_type()

        metadata_alpha <- switch(domain,
            "Archaea" = if (pipe == "kraken") {
                read.csv(paste0(rv$output_directory, "ALPHA/Archaea_alpha_metadata.csv"))
            } else {
                read.csv(paste0(rv$output_directory, "ALPHA/Archaea_meta_alpha_metadata.csv"))
            },
            "Bacteria" = if (pipe == "kraken") {
                read.csv(paste0(rv$output_directory, "ALPHA/Bacteria_alpha_metadata.csv"))
            } else {
                read.csv(paste0(rv$output_directory, "ALPHA/Bacteria_meta_alpha_metadata.csv"))
            },
            "Eukaryota" = if (pipe == "kraken") {
                read.csv(paste0(rv$output_directory, "ALPHA/Eukaryota_alpha_metadata.csv"))
            } else {
                read.csv(paste0(rv$output_directory, "ALPHA/Eukaryota_meta_alpha_metadata.csv"))
            },
            {
                showNotification("Invalid domain selected", type = "error")
                return(NULL)
            }
        )

        colnames(metadata_alpha) <- tolower(trimws(colnames(metadata_alpha)))
        alpha_col <- tolower(input$alpha)
        selected_col <- tolower(input$row_selected_alpha)
        sub_col <- tolower(input$subset_alpha)

        if (pipeline_type() == "metaphlan" && alpha_col == "shannon") {
            alpha_col <- "diversity_shannon"
        } else if (pipeline_type() == "metaphlan" && alpha_col == "simpson") {
            alpha_col <- "diversity_gini_simpson"
        }

        if (!(selected_col %in% colnames(metadata_alpha))) {
            showNotification(paste(
                "Selected column", selected_col,
                "does not exist in data!"
            ), type = "error")
            return(NULL)
        }

        if (!((alpha_col %in% colnames(metadata_alpha)) || sub_col %in% colnames(metadata_alpha))) {
            showNotification(
                paste(
                    "Selected alpha diversity index",
                    alpha_col, "does not exist in data!"
                ),
                type = "error"
            )
            return(NULL)
        }
        GraphAlpha(rv$df_post,metadata_alpha,alpha_col, selected_col, sub_col, input$subset_name_alpha)
    })


    output$AlphaPlot <- renderPlot({
        req(alphaPlot())
        alphaPlot()
    })

    output$plotAlpha <- downloadHandler(
        filename = function() {
            minus_suffix <- ""
            if (!is.null(input$subset_alpha) && !is.null(input$subset_name_alpha) && !is.null(rv$df_post) && input$subset_alpha %in% colnames(rv$df_post)) {
                minus_suffix <- get_single_minus_suffix(rv$df_post[[input$subset_alpha]], input$subset_name_alpha)
            }
            paste0(
                "Alpha_",
                sanitize_filename_part(input$Domain),
                "_",
                sanitize_filename_part(input$alpha),
                get_pipeline_suffix(),
                minus_suffix,
                ".pdf"
            )
        },
        content = function(file) {
            ggsave(file, plot = alphaPlot(), device = "pdf", width = 8, height = 6.5)
        }
    )

    #### BETA DIVERSITY ####
    output$setVarBeta <- renderUI({
        req(rv$df_post, rv$colTypes)
        n_samples <- nrow(rv$df_post)
        max_unique_values <- max(1, ceiling(n_samples / 2))
        eligible_types <- c("factor", "character", "numeric")
        eligible_vars <- names(rv$colTypes)[rv$colTypes %in% eligible_types]
        beta_vars_display <- tolower(eligible_vars)

        beta_vars <- beta_vars_display[vapply(eligible_vars, function(var_name) {
            column_values <- rv$df_post[[var_name]]

            if (rv$colTypes[[var_name]] == "factor") {
                return(TRUE)
            }

            unique_values <- unique(stats::na.omit(column_values))
            length(unique_values) <= max_unique_values
        }, logical(1))]

        selectInput("row_selected_beta", "Variable",
            choices = beta_vars
        )
    })

    betaPlot <- reactive({
        req(input$DomainBeta, input$row_selected_beta, pipeline_type(), input$subset_beta, input$subset_name_beta)

        data <- switch(input$DomainBeta,
            "Archaea" = if (pipeline_type() == "kraken") {
                paste0(rv$output_directory, "SUPERVISED_DEC/Archaea_Supervised_decontam.rds")
            } else {
                paste0(rv$output_directory, "META_SUPERVISED_DEC/Archaea_Supervised_meta_decontam.rds")
            },
            "Bacteria" = if (pipeline_type() == "kraken") {
                paste0(rv$output_directory, "SUPERVISED_DEC/Bacteria_Supervised_decontam.rds")
            } else {
                paste0(rv$output_directory, "META_SUPERVISED_DEC/Bacteria_Supervised_meta_decontam.rds")
            },
            "Eukaryota" = if (pipeline_type() == "kraken") {
                paste0(rv$output_directory, "SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")
            } else {
                paste0(rv$output_directory, "META_SUPERVISED_DEC/Eukaryota_Supervised_meta_decontam.rds")
            }
        )
        GraphBeta(data, rv$df_post, input$row_selected_beta, input$subset_beta, input$subset_name_beta)
    })

    output$BetaPlot <- renderPlot({
        req(betaPlot())
        betaPlot()
    })

    output$plotBeta <- downloadHandler(
        filename = function() {
            minus_suffix <- ""
            if (!is.null(input$subset_beta) && !is.null(input$subset_name_beta) && !is.null(rv$df_post) && input$subset_beta %in% colnames(rv$df_post)) {
                minus_suffix <- get_single_minus_suffix(rv$df_post[[input$subset_beta]], input$subset_name_beta)
            }
            paste0(
                "Beta_",
                sanitize_filename_part(input$DomainBeta),
                get_pipeline_suffix(),
                minus_suffix,
                ".pdf"
            )
        },
        content = function(file) {
            ggsave(file, plot = betaPlot(), device = "pdf", width = 8, height = 6)
        }
    )

    #### STACKBAR ####
    output$DomainUI <- renderUI({
        selectInput(
            inputId = "DomainSb",
            label = "Select Domain",
            choices = c("Bacteria", "Archaea", "Eukaryota")
        )
    })

    output$LevelSbUI <- renderUI({
        req(input$DomainSb, pipeline_type())
        if (pipeline_type() == "kraken") {
            selectInput(
                inputId = "LevelSb",
                label = "Select Level",
                choices = c("Species", "Genus"),
                selected = "Species"
            )
        } else {
            selectInput(
                inputId = "LevelSb",
                label = "Select Level",
                choices = c("Strain", "Species", "Genus"),
                selected = "Species"
            )
        }
    })
    output$StackBin <- renderUI({
        req(pipeline_type())
        if (pipeline_type() != "kraken") {
            selectInput(
                inputId = "StackBin",
                label = "Bin in stackbar?",
                choices = c("TRUE", "FALSE"),
                selected = c("FALSE")
            )
        } else {
            return(NULL)
        }
    })

    output$DivisionSbUI <- renderUI({
        req(rv$df_post, rv$colTypes)
        n_samples <- nrow(rv$df_post)
        max_unique_values <- max(1, ceiling(n_samples / 2))
        eligible_types <- c("factor", "character", "numeric")
        eligible_vars <- names(rv$colTypes)[rv$colTypes %in% eligible_types]
        division_vars_display <- tolower(eligible_vars)

        division_vars <- division_vars_display[vapply(eligible_vars, function(var_name) {
            column_values <- rv$df_post[[var_name]]

            if (rv$colTypes[[var_name]] == "factor") {
                return(TRUE)
            }

            unique_values <- unique(stats::na.omit(column_values))
            length(unique_values) <= max_unique_values
        }, logical(1))]

        selectInput("row_selected_stack", "Variable",
            choices = division_vars
        )
    })

    output$selectEl_stack <- renderUI({
        req(rv$df_post)
        createSelectInput("selectEl_stack", "Elements:", colnames(rv$df_post))
    })


    observeEvent(input$createLeaps, {
        # use the correct input id (selectInput was created as "Col_subset")
        req(input$Col_subset, input$leaps_factors, input$leaps_predictors, input$leaps_filename)
        showNotification("Starting LEAPS analysis, this may take a while...", type = "message")
        if (pipeline_type() == "kraken") {
            midfolder <- "SUPERVISED_DEC/"
            file_name <- ""
        } else if (pipeline_type() == "metaphlan") {
            midfolder <- "META_SUPERVISED_DEC/"
            file_name <- "meta_"
        }
        Bacteria <- paste0(rv$output_directory, midfolder, "Bacteria_Supervised_", file_name, "decontam.rds")
        Archaea <- paste0(rv$output_directory, midfolder, "Archaea_Supervised_", file_name, "decontam.rds")
        Eukaryota <- paste0(rv$output_directory, midfolder, "Eukaryota_Supervised_", file_name, "decontam.rds")
        output_folder <- paste0(rv$output_directory, "LEAPS/")
        row_sel <- as.character(input$Col_subset)
        els_row <- as.character(input$leaps_factors)
        predictors <- as.character(input$leaps_predictors)
        file_name <- as.character(input$leaps_filename)
        if (!dir.exists(output_folder)) {
            ok <- tryCatch(
                {
                    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
                    TRUE
                },
                error = function(e) {
                    showNotification(paste("Unable to create output folder:", conditionMessage(e)), type = "error")
                    FALSE
                }
            )
            if (ok) {
                showNotification(paste("Created output folder:", output_folder), type = "message")
            }
        }
        print(input$leaps_filename)
        # LEAPS signature is (bact, archea, euk, predictors, row_sel, els_row)
        # pass the selected column name (input$Col_subset) and the selected factor values
        LEAPS(bact_path = Bacteria, archea_path = Archaea, euk_path = Eukaryota, predictors = predictors, row_sel = row_sel, els_row = els_row, file_name = file_name, output_folder = output_folder, pipeline_type = pipeline_type())
        showNotification("LEAPS analysis complete", type = "message")
    })
    observeEvent(input$createTopic, {
        return(NULL)
    })

    stackPlot <- reactive({
        req(input$DomainSb, input$LevelSb, input$row_selected_stack, pipeline_type(), input$selectEl_stack, input$subset_stack, input$subset_name_stack)
        subset <- input$subset_stack
        subset_elements <- input$subset_name_stack
        data <- switch(input$DomainSb,
            "Bacteria" = if (pipeline_type() == "kraken") {
                paste0(rv$output_directory, "SUPERVISED_DEC/Bacteria_Supervised_decontam.rds")
            } else {
                paste0(rv$output_directory, "META_SUPERVISED_DEC/Bacteria_Supervised_meta_decontam.rds")
            },
            "Archaea" = if (pipeline_type() == "kraken") {
                paste0(rv$output_directory, "SUPERVISED_DEC/Archaea_Supervised_decontam.rds")
            } else {
                paste0(rv$output_directory, "META_SUPERVISED_DEC/Archaea_Supervised_meta_decontam.rds")
            },
            "Eukaryota" = if (pipeline_type() == "kraken") {
                paste0(rv$output_directory, "SUPERVISED_DEC/Eukaryota_Supervised_decontam.rds")
            } else {
                paste0(rv$output_directory, "META_SUPERVISED_DEC/Eukaryota_Supervised_meta_decontam.rds")
            },
            {
                showNotification("Invalid domain selected", type = "error")
                return(NULL)
            }
        )
        bin <- FALSE
        if (pipeline_type() == "metaphlan" && input$StackBin == "TRUE") {
            bin <- TRUE
        }
        res <- tryCatch(
            {
                GraphStackedBar(data,rv$df_post, input$LevelSb, input$row_selected_stack, bin, subset, subset_elements)
            },
            error = function(e) {
                showNotification(paste("Error creating stacked bar:", conditionMessage(e)), type = "error", duration = NULL)
                return(NULL)
            }
        )
        return(res)
    })

    output$StackPlot <- renderPlot({
        req(stackPlot())
        stackPlot()
    })

    output$plotStack <- downloadHandler(
        filename = function() {
            minus_suffix <- ""
            if (!is.null(input$subset_stack) && !is.null(input$subset_name_stack) && !is.null(rv$df_post) && input$subset_stack %in% colnames(rv$df_post)) {
                minus_suffix <- get_single_minus_suffix(rv$df_post[[input$subset_stack]], input$subset_name_stack)
            }
            paste0(
                "Stack_",
                sanitize_filename_part(input$DomainSb),
                "_",
                sanitize_filename_part(input$LevelSb),
                get_pipeline_suffix(),
                minus_suffix,
                ".pdf"
            )
        },
        content = function(file) {
            ggsave(file, plot = stackPlot(), device = "pdf", width = 16, height = 9) ### to_modify
        }
    )

    #### HEATMAPS ####
    output$checkBoxHeatmap <- renderUI({
        req(rv$df_post)
        virtualSelectInput(
            "checkBoxHeatmap",
            "Select metadata variables for annotation:",
            choices = setdiff(names(rv$df_post), "id"),
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })

    output$heatmapSplitVarUi <- renderUI({
        req(rv$df_post)
        split_choices <- sort(setdiff(names(rv$df_post), "id"))
        selectInput(
            "heatmap_split_var",
            "Divide patients by:",
            choices = c("None" = "", split_choices),
            selected = ""
        )
    })

    output$heatmapLabelVarUi <- renderUI({
        req(rv$df_post)
        label_choices <- sort(setdiff(names(rv$df_post), "id"))
        selectInput(
            "heatmap_label_var",
            "Show names on X axis from:",
            choices = c("id" = "id", label_choices),
            selected = "id"
        )
    })



    observeEvent(input$createHeatmap, {
        req(input$checkBoxHeatmap, rv$df_post)
        # Usa il file caricato manualmente se disponibile, altrimenti quello prodotto da Run DAS
        if (!is.null(input$DAS) && !is.null(input$DAS$datapath)) {
            physeq_path <- input$DAS$datapath
        } else {
            req(rv$rds_path)
            physeq_path <- rv$rds_path
        }
        metadata <- rv$df_post
        vars <- input$checkBoxHeatmap
        show_cat_waiter("Heatmap in progress...")
        tryCatch(
            {
                plot <- GraphHeatmap(
                    physeq_path,
                    metadata,
                    metadata_vars = vars,
                    sample_split_var = input$heatmap_split_var,
                    sample_label_var = input$heatmap_label_var
                )
                rv$heatmap_plot <- plot
            },
            error = function(e) {
                showNotification(paste("Error creating heatmap:", conditionMessage(e)), type = "error", duration = NULL)
                cat("[ERROR] Heatmap:", conditionMessage(e), "\n")
            },
            finally = {
                waiter_hide()
            }
        )
    })

    output$HeatmapPlot <- renderPlot({
        req(rv$heatmap_plot)
        rv$heatmap_plot
    })

    output$HeatmapDownload <- downloadHandler(
        filename = function() {
            das_label <- if (!is.null(input$DAS) && !is.null(input$DAS$name)) {
                sanitize_filename_part(tools::file_path_sans_ext(input$DAS$name))
            } else {
                paste0("DAS_heatmap_", Sys.Date())
            }
            paste0(das_label, "_heatmap", get_pipeline_suffix(), ".pdf")
        },
        content = function(file) {
            req(rv$heatmap_plot)
            ggsave(filename = file, plot = rv$heatmap_plot, width = 20, height = 16)
        }
    )
    output$download_topic1 <- downloadHandler(
        filename = function() {
            paste0("Topic_Modeling_immage1", get_pipeline_suffix(), "_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            req(rv$topic_modeling_p1)
            ggsave(file, plot = rv$topic_modeling_p1$p1, device = "pdf", width = 12, height = 9)
        }
    )
    output$download_topic2 <- downloadHandler(
        filename = function() {
            paste0("Topic_Modeling_immage2", get_pipeline_suffix(), "_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            req(rv$topic_modeling_p2)
            ggsave(file, plot = rv$topic_modeling_p2$p2, device = "pdf", width = 9, height = 16)
        }
    )
    output$download_topic3 <- downloadHandler(
        filename = function() {
            paste0("Topic_Modeling_immage3", get_pipeline_suffix(), "_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            req(rv$topic_modeling_p3)
            ggsave(file, plot = rv$topic_modeling_p3$p3, device = "pdf", width = 9, height = 16)
        }
    )
    output$download_topic4 <- downloadHandler(
        filename = function() {
            paste0("Topic_Modeling_immage4", get_pipeline_suffix(), "_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            req(rv$topic_modeling_p3)
            ggsave(file, plot = rv$topic_modeling_p3$p4, device = "pdf", width = 9, height = 16)
        }
    )
    # save in the reactive value the defauts it gives to the variables
    inferColTypes <- function(df) {
        vapply(seq_len(ncol(df)), function(i) {
            col_data <- df[[i]]
            col_data_no_na <- na.omit(col_data)
            n_unique <- length(unique(col_data_no_na))
            as_num <- suppressWarnings(as.numeric(col_data_no_na))
            all_numeric <- all(!is.na(as_num)) && length(as_num) > 0
            date_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%Y/%m/%d")
            as_date_list <- lapply(date_formats, function(fmt) {
                suppressWarnings(as.Date(col_data_no_na, format = fmt))
            })
            all_date <- any(sapply(as_date_list, function(x) {
                all(!is.na(x)) && length(x) > 0
            }))
            has_letters <- any(grepl("[A-Za-z]", col_data_no_na))
            if (n_unique < 5) {
                "factor"
            } else if (all_numeric && !has_letters) {
                "numeric"
            } else if (all_date && !has_letters) {
                "date"
            } else {
                "character"
            }
        }, character(1))
    }

    normalize_metadata_names <- function(df) {
        names(df) <- tolower(trimws(names(df)))
        df
    }



    # condition to show the table adjustments buttons
    output$fileUploaded <- reactive({
        return(!is.null(input$file))
    })
    outputOptions(output,
        "fileUploaded",
        suspendWhenHidden = FALSE
    )

    # load del dataframe
    observeEvent(list(input$file, input$file1, input$header, input$sep, input$quote), {
        if (req(!rv$AllAlright)) {
            req(input$file)
            df <- NULL
            ext <- tools::file_ext(input$file$name)
            if (ext == "csv" && input$manually %% 2 == 0) {
                df <- read.csv(input$file$datapath,
                    header = FALSE,
                    fill = TRUE,
                    stringsAsFactors = FALSE
                )
                df[df == ""] <- NA
                df <- normalize_metadata_names(df)
                rv$df_pre <- df
                rv$variables <- colnames(df)
                rv$ncol <- ncol(df)
                rv$levels <- vector("list", ncol(df))
                rv$colTypes <- inferColTypes(df)
                names(rv$colTypes) <- rv$variables
            } else if (ext == "tsv" && input$manually %% 2 == 0) {
                df <- read.table(input$file$datapath,
                    header = FALSE,
                    # delim="\t",
                    fill = TRUE,
                    stringsAsFactors = FALSE
                )
                df[df == ""] <- NA
                df <- normalize_metadata_names(df)

                rv$df_pre <- df
                rv$variables <- colnames(df)
                rv$ncol <- ncol(df)
                rv$levels <- vector("list", ncol(df))
                rv$colTypes <- inferColTypes(df)
                names(rv$colTypes) <- rv$variables
            } else {
                tryCatch(
                    {
                        df <- read.delim(input$file$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote
                        )
                        df[df == ""] <- NA
                        df <- normalize_metadata_names(df)

                        rv$df_pre <- df
                        rv$variables <- colnames(df)
                        rv$ncol <- ncol(df)
                        rv$levels <- vector("list", ncol(df))
                        rv$colTypes <- inferColTypes(df)
                        names(rv$colTypes) <- rv$variables
                    },
                    warning = function(cond) {
                        message("Here's the original error message:")
                        message(conditionMessage(cond))
                        df <- read.table(input$file$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote
                        )
                    },
                    error = function(cond) {
                        message("There is something wrong:")
                        message(conditionMessage(cond))
                    }
                )
            }
            req(input$file1)
            df <- NULL
            ext <- tools::file_ext(input$file$name)
            if (ext == "csv" && input$manually %% 2 == 0) {
                df <- read.csv(input$file1$datapath,
                    header = FALSE,
                    fill = TRUE,
                    stringsAsFactors = FALSE
                )
                df[df == ""] <- NA
                df <- normalize_metadata_names(df)
                rv$df_pre <- df
                rv$variables <- colnames(df)
                rv$ncol <- ncol(df)
                rv$levels <- vector("list", ncol(df))
                rv$colTypes <- inferColTypes(df)
                names(rv$colTypes) <- rv$variables
            } else if (ext == "tsv" && input$manually %% 2 == 0) {
                df <- read.table(input$file1$datapath,
                    header = FALSE,
                    # delim="\t",
                    fill = TRUE,
                    stringsAsFactors = FALSE
                )
                df[df == ""] <- NA
                df <- normalize_metadata_names(df)

                rv$df_pre <- df
                rv$variables <- colnames(df)
                rv$ncol <- ncol(df)
                rv$levels <- vector("list", ncol(df))
                rv$colTypes <- inferColTypes(df)
                names(rv$colTypes) <- rv$variables
            } else {
                tryCatch(
                    {
                        df <- read.delim(input$file1$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote
                        )
                        df[df == ""] <- NA
                        df <- normalize_metadata_names(df)

                        rv$df_pre <- df
                        rv$variables <- colnames(df)
                        rv$ncol <- ncol(df)
                        rv$levels <- vector("list", ncol(df))
                        rv$colTypes <- inferColTypes(df)
                        names(rv$colTypes) <- rv$variables
                    },
                    warning = function(cond) {
                        message("Here's the original error message:")
                        message(conditionMessage(cond))
                        df <- read.table(input$file$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote
                        )
                    },
                    error = function(cond) {
                        message("There is something wrong:")
                        message(conditionMessage(cond))
                    }
                )
            }
        }
    })

    # show table
    output$df_table <- renderDataTable({
        req(rv$df_pre)
        df_display <- rv$df_pre
        df_display[is.na(df_display)] <- "NA"
        DT::datatable(
            df_display,
            callback = JS(callback),
            rownames = FALSE,
            options = list(
                dom = "t",
                ordering = FALSE,
                scrollX = TRUE,
                pageLength = 20,
                autoWidth = TRUE
            ),
            escape = FALSE
        )
    })

    # aggiornamento nomi variabili
    observeEvent(input$colnames, {
        if (req(!rv$AllAlright)) {
            req(input$file)
            if (all(req(input$colnames) != "")) {
                rv$variables <- tolower(trimws(input$colnames))
                names(rv$colTypes) <- rv$variables
            }
        }
    })

    # crea Picker
    output$colTypeInputs <- renderUI({
        req(input$file)
        columnNames <- rv$variables
        tagList(
            lapply(1:rv$ncol, function(i) {
                col_data <- rv$df_pre[[i]]
                n_unique <- length(unique(col_data))

                # Heuristic per determinare il tipo
                col_data_no_na <- col_data[!is.na(col_data)]

                if (
                    length(col_data_no_na) > 0 &&
                        all(!is.na(suppressWarnings(as.numeric(col_data_no_na)))) &&
                        all(!grepl("[A-Za-z]", col_data_no_na))
                ) {
                    defaultEl <- "numeric"
                } else if (n_unique <= 3) {
                    defaultEl <- "factor"
                } else {
                    defaultEl <- "character"
                }

                # Se il tipo è factor, lascialo vuoto ("")
                selectedValue <- if (!is.null(rv$colTypes) &&
                    length(rv$colTypes) >= i &&
                    rv$colTypes[i] != "") {
                    rv$colTypes[i]
                } else if (defaultEl == "factor") {
                    ""
                } else {
                    defaultEl
                }

                pickerInput(
                    inputId = paste0("colType_", i),
                    label = paste0(columnNames[i], ": "),
                    choices = c("", "character", "numeric", "factor", "date"),
                    selected = selectedValue,
                    inline = FALSE
                )
            })
        )
    })


    # aggiornamento tipi colonne
    observeEvent(
        {
            req(rv$ncol)
            lapply(seq_len(rv$ncol), function(i) input[[paste0("colType_", i)]])
        },
        {
            if (req(!rv$AllAlright)) {
                req(input$file)
                req(input$changed)

                last_input <- input$changed

                i <- which(last_input == sapply(
                    seq_len(rv$ncol),
                    function(i) paste0("colType_", i)
                ))

                if (length(i) != 0) {
                    colTypeInput <- input[[last_input]]
                    rv$colTypes[i] <- colTypeInput

                    if (colTypeInput == "factor") {
                        var <- rv$variables[i]
                        n_fact <- rv$df_pre %>%
                            pull(i) %>%
                            unique() %>%
                            length()

                        if (n_fact <= 10) {
                            levels_ord <- rv$levels[[i]]
                            if (is.null(levels_ord)) {
                                levels_non_ord <- unique(na.omit(rv$df_pre[, i]))
                            } else {
                                levels_non_ord <- NULL
                            }

                            showModal(
                                modalDialog(
                                    bucket_list(
                                        header = "Do the factors have a preferential order? (i.e. they are time steps)?",
                                        group_name = paste0("bucket_ord_", last_input),
                                        orientation = "horizontal",
                                        add_rank_list(
                                            text = "Unordered factor detected",
                                            labels = levels_non_ord,
                                            input_id = paste0("factor_nonord_", i)
                                        ),
                                        add_rank_list(
                                            text = "Correct order",
                                            labels = levels_ord,
                                            input_id = paste0("factor_ord_", i)
                                        )
                                    ),
                                    footer = tagList(
                                        shinyjs::useShinyjs(),
                                        actionButton(paste0("submit_nonord_", i), "Order is not meaningful"),
                                        actionButton(paste0("submit_ord_", i), "Save with this order")
                                    )
                                )
                            )
                        } else {
                            shinyalert(
                                title = "Not quite the choice",
                                text = paste(
                                    "It appears that your column contains ",
                                    n_fact, " different levels when considered as a factor, whereas we can only handle a maximum of 10. \n",
                                    "You may have inadvertently selected the wrong column type. However, if this isn't the case and the number of values doesn't align with your expectations, consider reviewing your table for any discrepancies and reloading it. \n",
                                    "Please bear in mind that the system distinguishes values even if they differ only by a capitalization, spacing, or a single letter.",
                                    collapse = ""
                                ),
                                type = "info",
                                imageUrl = "https://media4.giphy.com/media/v1.Y2lkPTc5MGI3NjExOWlwYWY2bDYxd2tocjRzaHlwN2Z6ZmJmd2JlNjRld3Q3Z3piMjVpMSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/JIX9t2j0ZTN9S/giphy.gif",
                                imageHeight = 200,
                                imageWidth = 200,
                                confirmButtonText = "OK",
                                confirmButtonCol = "#19323C",
                                size = "m"
                            )
                            rv$colTypes[i] <- ""
                        }
                    }
                }
            }
        }
    )

    # salva ordine factor
    observeEvent(
        {
            req(rv$ncol)
            reactiveValuesToList(input)[grep("^submit_ord_", names(input))]
        },
        {
            if (req(!rv$AllAlright)) {
                req(input$file)
                last_input <- input$changed

                i <- which(last_input == sapply(
                    seq_len(rv$ncol),
                    function(j) paste0("submit_ord_", j)
                ))

                if (length(i) != 0 && req(input[[paste0("submit_ord_", i)]]) != 0) {
                    rv$levels[[i]] <- req(input[[paste0("factor_ord_", i)]])
                    removeModal()
                }
            }
        }
    )


    # risalva factor non ord
    observeEvent(
        {
            req(rv$ncol)
            reactiveValuesToList(input)[grep("^submit_nonord_", names(input))]
        },
        {
            if (req(!rv$AllAlright)) {
                req(input$file)
                last_input <- input$changed

                i <- which(last_input == sapply(
                    seq_len(rv$ncol),
                    function(j) paste0("submit_nonord_", j)
                ))

                if (length(i) != 0) {
                    rv$levels[i] <- list(NULL)
                    removeModal()
                }
            }
        }
    )
    # attiva load se tutti campi fillati
    observe({
        toggleState(
            "load",
            !is.null(rv$df_pre) & all(rv$colTypes != "") & all(!is.null(rv$colTypes))
        )
        req(input$file, rv$ncol)
        lapply(seq_len(rv$ncol), function(col) {
            toggleState(
                paste0("submit_ord_", col),
                length(input[[paste0("factor_nonord_", col)]]) == 0
            )
        })
    })

    # Sistemazione df
    observeEvent(input$load, {
        req(input$load != 0, rv$df_pre, rv$variables, rv$colTypes, rv$levels)
        colnames(rv$df_pre) <- rv$variables
        names(rv$levels) <- rv$variables
        rv$ord_factor <- !sapply(rv$levels, is.null)
        names(rv$ord_factor) <- rv$variables
        rv$levels[rv$colTypes == "factor" & !rv$ord_factor] <- lapply(
            rv$df_pre %>%
                dplyr::select(
                    all_of(
                        rv$variables[
                            which(rv$colTypes == "factor" & rv$ord_factor == FALSE)
                        ]
                    )
                ),
            function(x) {
                unique(na.omit(x))
            }
        )
        rv$colTypes[rv$colTypes == "factor" & rv$ord_factor & sapply(rv$df_pre, function(v) {
            length(unique(v))
        }) == 2] <- "logic"

        rv$ntype_ord <- lapply(
            rv$levels[rv$colTypes == "factor" & rv$ord_factor],
            length
        )
        rv$ntype_facets <- lapply(
            rv$levels[rv$colTypes == "factor" & !rv$ord_factor],
            length
        )

        rv$ndata <- nrow(rv$df_pre)
        # Crea df_post assicurandosi che le colonne siano vettori e non liste
        df_list <- lapply(rv$variables,
            change_type,
            df = rv$df_pre,
            levels = rv$levels,
            colTypes = rv$colTypes,
            ord_factor = rv$ord_factor
        )
        names(df_list) <- rv$variables
        # Usa data.frame invece di as_tibble per evitare colonne-lista
        # data.frame controlla automaticamente che ogni colonna sia un vettore atomico
        rv$df_post <- as_tibble(data.frame(df_list, check.names = FALSE, stringsAsFactors = FALSE))
        rv$df_post <- as_tibble(normalize_metadata_names(as.data.frame(rv$df_post)))
        rv$variables <- colnames(rv$df_post)
        names(rv$colTypes) <- rv$variables

        if (any(sapply(rv$df_post, function(col) {
            return(all(sapply(col, is.na)))
        }))) {
            wrong_type <- which(sapply(rv$df_post, function(col) {
                return(all(sapply(col, is.na)))
            }))
            shinyalert(
                title = "Warning!",
                text = paste("Incorrectly assigned variable type for:", paste(names(wrong_type), collapse = ", "), "\nPlease select another variable type."),
                type = "warning",
                imageUrl = "https://media4.giphy.com/media/v1.Y2lkPTc5MGI3NjExOWlwYWY2bDYxd2tocjRzaHlwN2Z6ZmJmd2JlNjRld3Q3Z3piMjVpMSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/JIX9t2j0ZTN9S/giphy.gif",
                imageHeight = 200,
                imageWidth = 200,
                confirmButtonText = "OK",
                confirmButtonCol = "#19323C"
            )
            rv$AllAlright <- FALSE
            rv$metadata_loaded <- FALSE
            rv$levels[!rv$ord_factor] <- list(NULL)
            rv$colTypes[rv$colTypes == "logic"] <- "factor"
            rv$colTypes[names(wrong_type)] <- ""
        } else {
            rv$AllAlright <- TRUE
            rv$metadata_loaded <- TRUE
            rv$colTypes <- factor(rv$colTypes,
                levels = c("character", "logic", "factor", "numeric", "date")
            )
            names(rv$colTypes) <- rv$variables

            # Gestione palette fattori ordinati
            n_needed_ord <- sum(rv$ord_factor)
            if (n_needed_ord > 0 && length(rv$ntype_ord) > 0) {
                recycled_palettes_gradient <- rep(possible_palettes_gradient, length.out = n_needed_ord)
                recycled_ntypes_ord <- rep(rv$ntype_ord, length.out = n_needed_ord)

                palettes_factord <- mapply(
                    moma.colors,
                    recycled_palettes_gradient,
                    n = recycled_ntypes_ord,
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE
                )
                if (length(rv$colTypes[rv$ord_factor & rv$colTypes == "factor"]) > 0) {
                    names(palettes_factord) <- names(rv$colTypes)[rv$ord_factor]
                }
            } else {
                palettes_factord <- NULL
            }

            # Gestione palette fattori non ordinati
            idx <- which(rv$colTypes == "factor")
            n_needed <- sum(!rv$ord_factor[idx])

            if (n_needed > 0 && length(rv$ntype_facets) > 0) {
                recycled_palettes <- rep(possible_palettes_facets, length.out = n_needed)
                recycled_ntypes <- rep(rv$ntype_facets, length.out = n_needed)

                palettes_factnonord <- mapply(
                    moma.colors,
                    recycled_palettes,
                    n = recycled_ntypes,
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE
                )
            } else {
                palettes_factnonord <- NULL
            }

            if (length(rv$colTypes[!rv$ord_factor & rv$colTypes == "factor"]) > 0) {
                names(palettes_factnonord) <- names(rv$colTypes)[!rv$ord_factor & rv$colTypes == "factor"]
            }

            n_logic <- sum(rv$colTypes == "logic")
            if (n_logic > 0) {
                palettes_logic <- possible_palettes_neutral[1:n_logic]
                if (length(rv$colTypes[rv$colTypes == "logic"]) > 0) {
                    names(palettes_logic) <- names(rv$colTypes)[rv$colTypes == "logic"]
                }
            } else {
                palettes_logic <- NULL
            }

            rv$palettes <- append(append(palettes_factnonord, palettes_factord), palettes_logic)
        }
    })

    output$AllAlright <- reactive({
        return(rv$AllAlright)
    })

    outputOptions(output,
        "AllAlright",
        suspendWhenHidden = FALSE
    )

    # crea le variabili da mostrare per la scelta
    output$variables_input <- renderUI({
        if (isTRUE(rv$metadata_loaded)) {
            req(rv$variables, rv$colTypes)
            tagList(
                virtualSelectInput(
                    inputId = "var",
                    label = "Chose the variables you want to explore",
                    choices = rv$variables[rv$colTypes != "character"],
                    multiple = TRUE,
                    showValueAsTags = TRUE,
                    clearButton = TRUE,
                    search = TRUE
                )
            )
        }
    })

    # aggiorna il numero del plot da mostrare
    observeEvent(input$refresh, {
        rv$plot_n <- rv$plot_n + 1
    })

    # aggiorna le variabili da mostrare
    observeEvent(input$var, {
        var <- names(sort(rv$colTypes[input$var]))
        var[rv$colTypes[var] == "factor"] <- names(sort(rv$ord_factor[names(rv$colTypes[var][rv$colTypes[var] == "factor"])]))
        if (length(var) == 1) {
            var <- as.character(input$var[1])

            if (rv$colTypes[var] == "logic") {
                rv$plot <- list(
                    barplot_logic(rv$df_post, var, rv$palettes),
                    pie_logic(rv$df_post, var, rv$palettes)
                )
                rv$table <- rv$df_post %>% dplyr::count(!!sym(var))
                if (all(!is.na(rv$df_post[[var]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var) %>%
                        t()
                    rownames(rv$table) <- "Number \n of samples"
                } else {
                    rv$table <- rv$table %>%
                        filter(n) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var]], "NA")
                    rownames(rv$table) <- "Number \n of samples"
                }
                rv$title <- list(
                    paste("Bar plot showing the distribution of data by", var),
                    paste("Pie chart showing the distribution of data by", var)
                )
                rv$description <- list(
                    "The height of each bar corresponds to the number of samples contained in the group. Choose barplots for precise comparisons and pie charts for illustrating proportions in a whole.",
                    "Each slice represents a category and its size is proportionial to the number of samples contained in the group. Choose barplots for precise comparisons and pie charts for illustrating proportions in a whole."
                )
            } else if (rv$colTypes[var] == "factor" & !rv$ord_factor[var]) {
                message(">>> [DEBUG] Variabile selezionata: ", var)
                message(">>> class(var): ", paste(class(var), collapse = ", "))
                message(">>> is.character(var): ", is.character(var))
                message(">>> length(var): ", length(var))

                if (!is.null(rv$df_post[[var]])) {
                    message(">>> str(df[[var]]):")
                    str(rv$df_post[[var]])
                    message(">>> head(df[[var]]): ", paste(head(rv$df_post[[var]]), collapse = ", "))
                } else {
                    warning(">>> [ATTENZIONE] df_post[[var]] è NULL")
                }

                rv$plot <- list(
                    waffle_factor_nonord(rv$df_post, var, rv$palettes, rv$ndata, rv$levels),
                    barplot_factor_nonord(rv$df_post, var, rv$palettes, rv$levels)
                )
                rv$table <- rv$df_post %>% dplyr::count(!!sym(var))
                if (all(!is.na(rv$df_post[[var]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var) %>%
                        t()
                    rownames(rv$table) <- "Number \n of samples"
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(n) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var]], "NA")
                    rownames(rv$table) <- "Number \n of samples"
                }
                rv$title <- list(
                    paste("Waffle chart showing the distribution of data subdivided by", var),
                    paste("Bar plot showing the distribution of data subdivided by", var)
                )
                rv$description <- list(
                    "Each square represents a unit and the colors are assigned according to the belonging to groups of samples, making it easy to see proportions at a glance.",
                    "The length of each bar corresponds to the number of samples contained in the group. Choose barplots for precise comparisons among groups."
                )
            } else if (rv$colTypes[var] == "factor" & rv$ord_factor[var]) {
                rv$plot <- list(
                    barplot_factor_ord_x(rv$df_post, var, rv$palettes),
                    barplot_factor_ord_stack(rv$df_post, var, rv$palettes)
                )
                rv$table <- rv$df_post %>% dplyr::count(!!sym(var))
                if (all(!is.na(rv$df_post[[var]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var) %>%
                        t()
                    rownames(rv$table) <- "Number \n of samples"
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(n) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var]], "NA")
                    rownames(rv$table) <- "Number \n of samples"
                }
                rv$title <- list(
                    paste("Bar plot showing the distribution of data subdivided by", var),
                    paste("Bar plot showing the distribution of data subdivided by", var, "stucked")
                )
                rv$description <- list(
                    "The height of each bar corresponds to the number of samples contained in the group. Choose side-by-syde barplots for precise comparisons among groups.",
                    "The height of each bar corresponds to the number of samples contained in the group. Choose stucked barplots to show proportions between groups."
                )
            } else if (rv$colTypes[var] == "numeric") {
                rv$plot <- list(
                    hist_numeric(rv$df_post, var),
                    density_numeric(rv$df_post, var, rv$palettes),
                    boxplot_numeric(rv$df_post, var, rv$palettes)
                )
                rv$table <- data.frame(
                    as.array(
                        summary(rv$df_post %>% dplyr::select(.data[[var]]) %>% pull())
                    )
                ) %>%
                    column_to_rownames("Var1") %>%
                    t()
                rownames(rv$table) <- "values"
                rv$title <- list(
                    paste("Histogram of", var, "values"),
                    paste("Density of", var, "values"),
                    paste("Boxplot of", var, "values")
                )
                rv$description <- list(
                    "Each bar's height reflects the number of samples with values lying in that bin. If values are discrete, histograms are ideal, expecially for smaller sample sizes, since they show a more accurate representation of the distribution.",
                    "Density plots provide a smoothed estimate of the distribution of the values between samples, especially useful with larger datasets. If you need to see the nuances of the distribution, such as small peaks and valleys, density plots are preferable.",
                    "Concise summary of a dataset's distribution, focusing on central tendency and variability. Median, quartiles, and potential outliers can be seen."
                )
            }
        } else if (length(var) == 2) {
            var1 <- as.character(var[1])
            var2 <- as.character(var[2])

            if (rv$colTypes[var1] == "logic" & rv$colTypes[var2] == "logic") {
                rv$plot <- list(
                    barplot_logic_logic(rv$df_post, var1, var2, rv$palettes),
                    barplot_logic_logic(rv$df_post, var2, var1, rv$palettes)
                )
                rv$table <- rv$df_post %>%
                    count(.data[[var1]], .data[[var2]]) %>%
                    pivot_wider(names_from = var2, values_from = n)

                if (all(!is.na(rv$df_post[[var1]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var1)
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(-all_of(var1)) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var1]], "NA")
                }
                rv$title <- list(
                    paste("Barplot of", var1, "colored according to", var2),
                    paste("Barplot of", var2, "colored according to", var1)
                )
                rv$description <- list(
                    paste("The plot represents the number of samples in each factor of", var1, "divided in two colors according to", var2),
                    paste("The plot represents the number of samples in each factor of", var2, "divided in two colors according to", var1)
                )
            } else if (rv$colTypes[var1] == "logic" &
                rv$colTypes[var2] == "factor" & !rv$ord_factor[var2]) {
                rv$plot <- list(
                    barplot_logic_factnonord_stack(rv$df_post, var1, var2, rv$palettes),
                    barplot_logic_factnonord_dodge(rv$df_post, var1, var2, rv$palettes),
                    barplot_logic_factnonord_stack(rv$df_post, var2, var1, rv$palettes),
                    barplot_logic_factnonord_dodge(rv$df_post, var2, var1, rv$palettes)
                )
                rv$table <- rv$df_post %>%
                    dplyr::count(.data[[var1]], .data[[var2]]) %>%
                    pivot_wider(names_from = var2, values_from = n)
                if (all(!is.na(rv$df_post[[var1]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var1)
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(-all_of(var1)) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var1]], "NA")
                }
                rv$title <- list(
                    paste("Barplot of", var1, "colored according to", var2, "stacked"),
                    paste("Barplot of", var1, "colored according to", var2, "dodged"),
                    paste("Barplot of", var2, "colored according to", var1, "stacked"),
                    paste("Barplot of", var2, "colored according to", var1, "dodged")
                )
                rv$description <- list(
                    paste("The plot represents the number of samples in each factor of", var1, "divided in two colors according to", var2),
                    paste("The plot represents the number of samples in each factor of", var1, "divided in two colors according to", var2),
                    paste("The plot represents the number of samples in each factor of", var2, "divided in two colors according to", var1),
                    paste("The plot represents the number of samples in each factor of", var2, "divided in two colors according to", var1)
                )
            } else if (rv$colTypes[var1] == "logic" &
                rv$colTypes[var2] == "factor" & rv$ord_factor[var2]) {
                rv$plot <- list(
                    barplot_logic_factord_stack(rv$df_post, var1, var2, rv$palettes),
                    barplot_logic_factord_dodge(rv$df_post, var1, var2, rv$palettes),
                    barplot_logic_factord_stack(rv$df_post, var2, var1, rv$palettes),
                    barplot_logic_factord_dodge(rv$df_post, var2, var1, rv$palettes)
                )
                rv$table <- rv$df_post %>%
                    dplyr::count(.data[[var1]], .data[[var2]]) %>%
                    pivot_wider(names_from = var2, values_from = n)
                if (all(!is.na(rv$df_post[[var1]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var1)
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(-all_of(var1)) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var1]], "NA")
                }
                rv$title <- list(
                    paste("Barplot of", var1, "colored according to", var2, "stacked"),
                    paste("Barplot of", var1, "colored according to", var2, "dodged"),
                    paste("Barplot of", var2, "colored according to", var1, "stacked"),
                    paste("Barplot of", var2, "colored according to", var1, "dodged")
                )
            } else if (rv$colTypes[var1] == "logic" &
                rv$colTypes[var2] == "numeric") {
                rv$plot <- list(
                    density_logic_num(rv$df_post, var1, var2, rv$palettes),
                    boxplot_logic_num(rv$df_post, var1, var2, rv$palettes),
                    histogram_logic_num_stack(rv$df_post, var1, var2, rv$palettes),
                    histogram_logic_num_identity(rv$df_post, var1, var2, rv$palettes)
                )
                rv$table <- data.frame(
                    do.call(
                        rbind,
                        tapply(
                            rv$df_post %>% dplyr::select(.data[[var2]]) %>% pull(),
                            rv$df_post %>% dplyr::select(.data[[var1]]) %>% pull(),
                            summary
                        )
                    )
                )
                rv$title <- list(
                    paste("Ridgeline plot of", var2, "according to", var1),
                    paste("Boxplots of", var2, "subdivided according to", var1),
                    paste("Histograms of", var2, "colored according to", var1, "stucked"),
                    paste("Histograms of", var2, "subdivided and colored according to", var1)
                )
            } else if (rv$colTypes[var1] == "factor" & !rv$ord_factor[var1] &
                rv$colTypes[var2] == "factor" & !rv$ord_factor[var2]) {
                rv$plot <- list(
                    barplot_factornonord_factornonord(rv$df_post, var1, var2, rv$palettes),
                    barplot_factornonord_factornonord(rv$df_post, var2, var1, rv$palettes)
                )
                rv$title <- list(
                    paste("Barplot of", var1, "colored according to", var2),
                    paste("Barplot of", var2, "colored according to", var1)
                )
                rv$table <- rv$df_post %>%
                    dplyr::count(.data[[var1]], .data[[var2]]) %>%
                    pivot_wider(names_from = var2, values_from = n)
                if (all(!is.na(rv$df_post[[var1]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var1)
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(-all_of(var1)) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var1]], "NA")
                }
            } else if (rv$colTypes[var1] == "factor" & !rv$ord_factor[var1] &
                rv$colTypes[var2] == "factor" & rv$ord_factor[var2]) {
                rv$plot <- list(
                    barplot_factornonord_factorord(rv$df_post, var1, var2, rv$palettes),
                    barplot_factorord_factornonord(rv$df_post, var1, var2, rv$palettes)
                )
                rv$title <- list(
                    paste("Barplot of", var1, "colored according to", var2, "stacked"),
                    paste("Barplot of", var2, "colored according to", var1, "stacked")
                )
                rv$table <- rv$df_post %>%
                    dplyr::count(.data[[var1]], .data[[var2]]) %>%
                    pivot_wider(names_from = var2, values_from = n)
                if (all(!is.na(rv$df_post[[var1]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var1)
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(-all_of(var1)) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var1]], "NA")
                }
            } else if (rv$colTypes[var1] == "factor" & !rv$ord_factor[var1] &
                rv$colTypes[var2] == "numeric") {
                rv$plot <- list(
                    density_factornonord_num(rv$df_post, var1, var2, rv$palettes),
                    histogram_factornonord_num_identity(rv$df_post, var1, var2, rv$palettes),
                    histogram_factornonord_num_stack(rv$df_post, var1, var2, rv$palettes),
                    boxplot_factornonord_num(rv$df_post, var1, var2, rv$palettes)
                )
                rv$title <- list(
                    paste("Ridgeline plot of", var2, "according to", var1),
                    paste("Histograms of", var2, "subdivided and colored according to", var1),
                    paste("Histograms of", var2, "colored according to", var1, "stacked"),
                    paste("Boxplots of", var2, "subdivided according to", var1)
                )
                rv$table <- data.frame(
                    do.call(
                        rbind,
                        tapply(
                            rv$df_post %>% dplyr::select(.data[[var2]]) %>% pull(),
                            rv$df_post %>% dplyr::select(.data[[var1]]) %>% pull(),
                            summary
                        )
                    )
                )
            } else if (rv$colTypes[var1] == "factor" & rv$ord_factor[var1] &
                rv$colTypes[var2] == "factor" & rv$ord_factor[var2]) {
                rv$plot <- list(
                    barplot_factord_factord_dodge(rv$df_post, var1, var2, rv$palettes),
                    barplot_factord_factord_dodge(rv$df_post, var2, var1, rv$palettes),
                    barplot_factord_factord_stack(rv$df_post, var1, var2, rv$palettes),
                    barplot_factord_factord_stack(rv$df_post, var2, var1, rv$palettes)
                )
                rv$title <- list(
                    paste("Barplot of", var1, "colored according to", var2, "dodged"),
                    paste("Barplot of", var2, "colored according to", var1, "dodged"),
                    paste("Barplot of", var1, "colored according to", var2, "stacked"),
                    paste("Barplot of", var2, "colored according to", var1, "stacked")
                )
                rv$table <- rv$df_post %>%
                    dplyr::count(.data[[var1]], .data[[var2]]) %>%
                    pivot_wider(names_from = var2, values_from = n)
                if (all(!is.na(rv$df_post[[var1]]))) {
                    rv$table <- rv$table %>%
                        column_to_rownames(var1)
                } else {
                    rv$table <- rv$table %>%
                        dplyr::select(-all_of(var1)) %>%
                        t()
                    colnames(rv$table) <- c(rv$levels[[var1]], "NA")
                }
            } else if (rv$colTypes[var1] == "factor" & rv$ord_factor[var1] &
                rv$colTypes[var2] == "numeric") {
                rv$plot <- list(
                    density_factorord_num(rv$df_post, var1, var2, rv$palettes),
                    boxplot_factorord_num(rv$df_post, var1, var2, rv$palettes)
                )
                rv$title <- list(
                    paste("Ridgeline plot of", var2, "according to", var1),
                    paste("Boxplots of", var2, "subdivided according to", var1)
                )
                rv$table <- data.frame(
                    do.call(
                        rbind,
                        tapply(
                            rv$df_post %>% dplyr::select(.data[[var2]]) %>% pull(),
                            rv$df_post %>% dplyr::select(.data[[var1]]) %>% pull(),
                            summary
                        )
                    )
                )
            } else if (rv$colTypes[var1] == "numeric" & rv$colTypes[var2] == "numeric") {
                rv$plot <- list(
                    scatter_num_num(rv$df_post, var1, var2, rv$palettes),
                    scatter_num_num(rv$df_post, var2, var1, rv$palettes)
                )
                rv$title <- list(
                    paste("Scatterplot of", var2, "vs", var1),
                    paste("Scatterplot of", var1, "vs", var2)
                )
                rv$table <- data.frame(
                    do.call(
                        rbind,
                        tapply(
                            rv$df_post %>%
                                dplyr::select(all_of(c(var1, var2))) %>%
                                pivot_longer(c(var1, var2)) %>%
                                dplyr::select(value) %>%
                                pull(),
                            rv$df_post %>%
                                dplyr::select(all_of(c(var1, var2))) %>%
                                pivot_longer(c(var1, var2)) %>%
                                dplyr::select(name) %>%
                                pull(),
                            summary
                        )
                    )
                )
            }
        } else {
            rv$plot <- NULL
            rv$title <- NULL
            rv$table <- NULL
        }
    })

    # scrive titolo
    output$Title <- renderText({
        return(rv$title[[numbers::mod(rv$plot_n, length(rv$title)) + 1]])
    })

    # scrive descrizione
    output$Description <- renderText({
        return(rv$description[[numbers::mod(rv$plot_n, length(rv$plot)) + 1]])
    })

    # plotta grafici
    output$distPlot <- renderPlot(
        {
            return(rv$plot[[numbers::mod(rv$plot_n, length(rv$plot)) + 1]])
        },
        bg = "transparent"
    )

    # plotta grafico dopo
    output$distPlot_next <- renderPlot(
        {
            return(rv$plot[[numbers::mod((rv$plot_n + 1), length(rv$plot)) + 1]] +
                theme(
                    legend.position = "none",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks = element_blank()
                ))
        },
        bg = "transparent"
    )

    # printa tabella
    output$Table <- renderTable(
        {
            return(rv$table)
        },
        rownames = TRUE
    )


    # output$downloadPlot <- downloadHandler(
    #     filename = function() {
    #         paste("plot-", paste(input$var, collapse = "_"), ".png", sep = "")
    #     },
    #     content = function(con) {
    #         ggsave(
    #             con,
    #             plot = rv$plot[[numbers::mod(rv$plot_n, length(rv$plot)) + 1]],
    #         )
    #     }
    # )
    ################## start
    output$subset_alpha <- renderUI({
        req(rv$df_post)
        tagList(
            selectInput("subset_alpha", "Subset:", choices = colnames(rv$df_post)),
            uiOutput("subsetElements_alpha")
        )
    })
    output$subsetElements_alpha <- renderUI({
        req(rv$df_post, input$subset_alpha)
        vals <- unique(rv$df_post[[input$subset_alpha]])
        virtualSelectInput(
            "subset_name_alpha",
            "Subset elements",
            choices = vals,
            selected = vals,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })

    output$subset_beta <- renderUI({
        req(rv$df_post)
        tagList(
            selectInput("subset_beta", "Subset:", choices = colnames(rv$df_post)),
            uiOutput("subsetElements_beta")
        )
    })
    output$subsetElements_beta <- renderUI({
        req(rv$df_post, input$subset_beta)
        vals <- unique(rv$df_post[[input$subset_beta]])
        virtualSelectInput(
            "subset_name_beta",
            "Subset elements",
            choices = vals,
            selected = vals,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })

    output$subset_stack <- renderUI({
        req(rv$df_post)
        tagList(
            selectInput("subset_stack", "Subset:", choices = colnames(rv$df_post)),
            uiOutput("subsetElements_stack")
        )
    })
    output$subsetElements_stack <- renderUI({
        req(rv$df_post, input$subset_stack)
        vals <- unique(rv$df_post[[input$subset_stack]])
        virtualSelectInput(
            "subset_name_stack",
            "Subset elements",
            choices = vals,
            selected = vals,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })


    output$select_var_topic <- renderUI({
        req(rv$df_post)
        tagList(
            selectInput(
                "var_topic",
                "Variable to do topic modeling:",
                choices = colnames(rv$df_post)
            ),
            uiOutput("discriminants_var_topic"),
            selectInput(
                "subset_colon_topic",
                "Variable to subset:",
                choices = colnames(rv$df_post)
            ),
            uiOutput("subsetElements_colon_topic")
        )
    })
    output$discriminants_var_topic <- renderUI({
        req(input$var_topic)

        selected_col <- input$var_topic
        unique_values <- setdiff(colnames(rv$df_post), selected_col)
        virtualSelectInput(
            "var_topic_elements",
            label = paste("Discriminants:"),
            choices = unique_values,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })
    output$subsetElements_colon_topic <- renderUI({
        req(input$subset_colon_topic)

        selected_col <- input$subset_colon_topic
        unique_values <- unique(rv$df_post[[selected_col]])

        virtualSelectInput(
            "subset_colon_elements",
            label = paste("Elements to maintain for", selected_col),
            choices = unique_values,
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })
    output$show_topic_plot_p1 <- reactive({
        !is.null(rv$topic_modeling_p1$p1)
    })
    outputOptions(output, "show_topic_plot_p1", suspendWhenHidden = FALSE)

    output$show_topic_plot_p2 <- reactive({
        !is.null(rv$topic_modeling_p2$p2)
    })
    outputOptions(output, "show_topic_plot_p2", suspendWhenHidden = FALSE)
    output$show_topic_plot_p3 <- reactive({
        !is.null(rv$topic_modeling_p3$p3)
    })
    outputOptions(output, "show_topic_plot_p3", suspendWhenHidden = FALSE)
    output$show_topic_plot_p4 <- reactive({
        !is.null(rv$topic_modeling_p3$p4)
    })
    outputOptions(output, "show_topic_plot_p4", suspendWhenHidden = FALSE)

    # UI dinamica per ciascuna pagina
    output$topic_page_ui <- renderUI({
        tags$div(
            style = "width: 100%;",
            tabsetPanel(
                type = "pills",
                tabPanel(
                    "Page 1",
                    tagList(
                        h4("Page 1: Select topic modeling parameters"),
                        fluidRow(
                            column(
                                6,
                                tags$div(
                                    style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: left;",
                                    tags$h4("Topic Modeling Parameters"),
                                    selectInput(
                                        inputId = "DomainTopic",
                                        label = "Seleziona dominio:",
                                        choices = c("Bacteria", "Archaea", "Eukaryota"),
                                        selected = "Bacteria"
                                    ),
                                    selectInput(
                                        "tax_rank",
                                        "Seleziona il rank tassonomico:",
                                        choices = if (pipeline_type() == "kraken") {
                                            c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
                                        } else {
                                            c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Strain")
                                        },
                                        selected = "Genus"
                                    ),
                                    uiOutput("select_var_topic"),
                                    shiny::div(
                                        actionButton(
                                            "startTopicStep1",
                                            "Execute topic first step",
                                            icon = icon("play"),
                                            class = "refined-button"
                                        ),
                                        downloadButton(
                                            "download_topic1",
                                            "Download results",
                                            icon = icon("download"),
                                            class = "refined-button"
                                        )
                                    )
                                )
                            ),
                            conditionalPanel(
                                condition = "output.show_topic_plot_p1 == true",
                                column(
                                    6,
                                    tags$div(
                                        style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: center;",
                                        tags$h4("Topic Modeling Diagnostics"),
                                        plotOutput("topic_plot_p1")
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Page 2",
                    tagList(
                        h4("Page 2: Choose K parameter"),
                        fluidRow(
                            column(
                                6,
                                tags$div(
                                    style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: left;",
                                    tags$h4("K parameter selection"),
                                    sliderInput(
                                        inputId = "topic_k",
                                        label = "Numero di topic (1-100):",
                                        min = 1,
                                        max = 100,
                                        value = 10,
                                        step = 1
                                    ),
                                    shiny::div(
                                        actionButton(
                                            "startTopicStep2",
                                            "Execute topic step 2",
                                            icon = icon("play"),
                                            class = "refined-button"
                                        ),
                                        downloadButton(
                                            "download_topic2",
                                            "Download results",
                                            icon = icon("download"),
                                            class = "refined-button"
                                        )
                                    )
                                )
                            ),
                            conditionalPanel(
                                condition = "output.show_topic_plot_p2 == true",
                                column(
                                    6,
                                    tags$div(
                                        style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: center;",
                                        tags$h4("Topic Modeling Diagnostics"),
                                        plotOutput("topic_plot_p2")
                                    )
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Page 3",
                    tagList(
                        h4("Page 3: Topic Number"),
                        fluidRow(
                            column(
                                6,
                                tags$div(
                                    style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: left;",
                                    tags$h4("Topic Display Options"),
                                    sliderInput(
                                        inputId = "topic_number",
                                        label = "Numero di topic da mostrare:",
                                        min = 1,
                                        max = 100,
                                        value = 10,
                                        step = 1
                                    ),
                                    actionButton(
                                        "startTopicStep3",
                                        "Execute topic step 3",
                                        icon = icon("play"),
                                        class = "refined-button"
                                    ),
                                    downloadButton(
                                        "download_topic3",
                                        "Download graph 3",
                                        icon = icon("download"),
                                        class = "refined-button"
                                    ),
                                    downloadButton(
                                        "download_topic4",
                                        "Download graph 4",
                                        icon = icon("download"),
                                        class = "refined-button"
                                    )
                                )
                            ),
                            conditionalPanel(
                                condition = "output.show_topic_plot_p3 == true && output.show_topic_plot_p4 == true",
                                column(
                                    6,
                                    tags$div(
                                        style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: center;",
                                        tags$h4("Topic Modeling Diagnostics"),
                                        plotOutput("topic_plot_p3"),
                                        plotOutput("topic_plot_p4")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    })

    observeEvent(input$humann_graph, {
        req(input$humann_graph)
        rv$humann_graph_file <- tryCatch(
            {
                readr::read_tsv(input$humann_graph$datapath, show_col_types = FALSE)
            },
            error = function(e) {
                showNotification(paste("Unable to read HUMAnN graph file:", conditionMessage(e)), type = "error")
                NULL
            }
        )
    })

    output$humann_graph_file_text <- renderPrint({
        req(rv$humann_graph_file)
        utils::head(rv$humann_graph_file)
    })

    observe({
        if (!is.null(rv$pathway_array_path)) {
            updateSelectizeInput(
                session, 
                "pathway_search",
                choices = rv$pathway_array_path,
                server = TRUE
            )
        }
    })

        output$selectEl_das <- renderUI({
        req(rv$df_post)
        selectInput(
            "element_selected_das",
            "Discriminant:",
            choices = colnames(rv$df_post)
        )
    })

    # UI per selezionare main element (singola colonna)
    output$selectEl_das_pathway_graph_main <- renderUI({
        req(rv$df_post)
        selectInput(
            "main_el_pathway_graph",
            "Main Element (focal-metadata):",
            choices = colnames(rv$df_post)
        )
    })

    # UI per selezionare colonne da usare (multiple colonne)
    output$selectEl_das_pathway_graph_sub <- renderUI({
        req(rv$df_post)
        virtualSelectInput(
            "column_to_use_pathway_graph",
            "Columns to use:",
            choices = colnames(rv$df_post),
            selected = colnames(rv$df_post),
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })

    observeEvent(input$start_pathway_searcher, {
        message("[DEBUG] start_pathway_searcher triggered")
        message("[DEBUG] input$element_selected_das_pathway: ", input$element_selected_das_pathway)
        message("[DEBUG] input$discr_element_pathway: ", paste(input$discr_element_pathway, collapse = ", "))
        message("[DEBUG] rv$output_directory: ", rv$output_directory)
        message("[DEBUG] input$humann_graph$datapath: ", input$humann_graph$datapath)
        req(rv$df_post, input$humann_graph$datapath, input$element_selected_das_pathway, rv$output_directory)
        tryCatch(
            {
                output <- paste0(rv$output_directory, "/MAASLIN_PATHWAY_SEARCHER/")
                conf <- input$discr_element_pathway

                if (is.null(conf) || length(conf) == 0) {
                    conf <- NULL
                }
                dir.create(output, recursive = TRUE, showWarnings = FALSE)
                result <- PathwaySearcher(
                    data = input$humann_graph$datapath,
                    metadata = rv$df_post,
                    main_disc = input$element_selected_das_pathway,
                    discr_els = conf,
                    output_folder = output
                )
                rv$pathway_fit_out <- result$fit_out
                rv$pathway_array_path <- result$array_path
                rv$pathway_file_location <- input$humann_graph$datapath

                showModal(modalDialog(
                    title = "Save fit_out",
                    shinyDirButton("pathway_fitout_dir", "Folder", "Select a folder"),
                    textOutput("pathway_fitout_dir_text"),
                    textInput("pathway_fitout_name", "File name", value = "fit_out.tsv", width = "100%"),
                    footer = tagList(
                        actionButton("save_pathway_fitout", "Save"),
                        modalButton("Cancel")
                    ),
                    easyClose = TRUE
                ))

                showNotification("Pathway searcher completed.", type = "message")
            },
            error = function(e) {
                showNotification(paste("Error in PathwaySearcher:", conditionMessage(e)), type = "error")
                cat("[ERROR] PathwaySearcher:", conditionMessage(e), "\n")
            }
        )
    })

    # UI per selezionare main element (singola colonna)
    output$selectEl_das_pathway_graph_main <- renderUI({
        req(rv$df_post)
        selectInput(
            "main_el_pathway_graph",
            "Main Element (focal-metadata):",
            choices = colnames(rv$df_post)
        )
    })

    # UI per selezionare colonne da usare (multiple colonne)
    output$selectEl_das_pathway_graph_sub <- renderUI({
        req(rv$df_post)
        virtualSelectInput(
            "column_to_use_pathway_graph",
            "Columns to use:",
            choices = colnames(rv$df_post),
            selected = colnames(rv$df_post),
            multiple = TRUE,
            showValueAsTags = TRUE,
            clearButton = TRUE,
            search = TRUE
        )
    })

    # Observer per eseguire HumannGraphs
    observeEvent(input$start_humann_graphs, {
        req(
            rv$pathway_file_location,
            rv$df_post,
            input$pathway_search,
            input$main_el_pathway_graph,
            input$column_to_use_pathway_graph
        )

        show_cat_waiter("Pathway graph generation in progress...")

        tryCatch(
            {
                # Preparazione output folder
                output_folder <- file.path(rv$output_directory, "humann_graphs")
                dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

                # Raccolta additional rules
                additional_rules <- c()
                
                # Aggiungi regole dal checkbox
                if (!is.null(input$pathway_variables)) {
                    additional_rules <- c(additional_rules, input$pathway_variables)
                }
                
                # Aggiungi sorting (solo se non è "none")
                if (!is.null(input$pathway_variables_sort) && input$pathway_variables_sort != "none") {
                    additional_rules <- c(additional_rules, input$pathway_variables_sort)
                }
                
                # Aggiungi scaling (solo se non è "none")
                if (!is.null(input$pathway_variables_scaling) && input$pathway_variables_scaling != "none") {
                    additional_rules <- c(additional_rules, input$pathway_variables_scaling)
                }
                
                # Combina tutte le regole
                additional_rule_string <- if (length(additional_rules) > 0) {
                    paste(additional_rules, collapse = " ")
                } else {
                    NULL
                }

                # Nome del plot
                plot_name <- paste0(
                    "pathway_",
                    gsub("[^A-Za-z0-9_-]", "_", input$pathway_search),
                    "_",
                    format(Sys.time(), "%Y%m%d_%H%M%S")
                )

                message("[DEBUG] Calling HumannGraphs with:")
                message("  - humann_data: ", rv$pathway_file_location)
                message("  - main_el: ", input$main_el_pathway_graph)
                message("  - column_to_use: ", paste(input$column_to_use_pathway_graph, collapse = ", "))
                message("  - pathway: ", input$pathway_search)
                message("  - additional_rules: ", additional_rule_string)

                # Chiama HumannGraphs
                HumannGraphs(
                    humann_data = rv$pathway_file_location,
                    metadata = rv$df_post,
                    input_folder = output_folder,
                    column_to_use = input$column_to_use_pathway_graph,
                    main_el = input$main_el_pathway_graph,
                    pathway_to_search = input$pathway_search,
                    plot_name = plot_name,
                    additional_rule = additional_rule_string
                )

                showNotification(
                    paste0("HumannGraphs completed! Plot saved: ", plot_name, ".png"),
                    type = "message",
                    duration = 10
                )
            },
            error = function(e) {
                showNotification(
                    paste("Error during HumannGraphs:", conditionMessage(e)),
                    type = "error",
                    duration = NULL
                )
            }
        )
        
        waiter_hide()
    })

    observeEvent(input$save_pathway_fitout, {
        req(rv$pathway_fit_out, input$pathway_fitout_dir, input$pathway_fitout_name)
        if (!nzchar(input$pathway_fitout_name)) {
            showNotification("Please provide a valid file name.", type = "error")
            return()
        }

        dir_path <- parseDirPath(volumes, input$pathway_fitout_dir)
        if (length(dir_path) == 0 || !nzchar(dir_path)) {
            showNotification("Please select a valid folder.", type = "error")
            return()
        }

        file_name <- input$pathway_fitout_name
        if (!grepl("\\.tsv$", file_name, ignore.case = TRUE)) {
            file_name <- paste0(file_name, ".tsv")
        }

        save_path <- file.path(dir_path, file_name)

        ok <- tryCatch(
            {
                utils::write.table(rv$pathway_fit_out,
                    file = save_path,
                    sep = "\t",
                    row.names = FALSE,
                    quote = FALSE
                )
                TRUE
            },
            error = function(e) {
                showNotification(paste("Unable to save fit_out:", conditionMessage(e)), type = "error")
                FALSE
            }
        )

        if (ok) {
            showNotification(paste("fit_out saved to:", save_path), type = "message")
            removeModal()
        }
    })

    output$humann_graph_file_contents <- renderUI({
        req(rv$humann_graph_file)
        tagList(
            h4("Contents of the HUMAnN graph file:"),
            verbatimTextOutput("humann_graph_file_text")
        )
    })

    observeEvent(input$startTopicStep1, {
        cat("[DEBUG] startTopicStep1 triggered\n")
        cat("[DEBUG] input$var_topic:", input$var_topic, "\n")
        cat("[DEBUG] input$var_topic_elements:", paste(input$var_topic_elements, collapse = ", "), "\n")
        cat("[DEBUG] input$subset_colon_topic:", input$subset_colon_topic, "\n")
        cat("[DEBUG] input$subset_colon_elements:", paste(input$subset_colon_elements, collapse = ", "), "\n")

        if (is.null(input$var_topic) || is.null(input$subset_colon_topic) || is.null(input$subset_colon_elements)) {
            showNotification("Compila tutti i campi per avviare la topic modeling!", type = "error")
            return(NULL)
        }

        rds_path <- paste0(rv$output_directory, "/SUPERVISED_DEC/", input$DomainTopic, "_Supervised_decontam.rds")
        cat("[DEBUG] rds_path:", rds_path, "\n")
        tryCatch(
            {
                rv$topic_modeling_p1 <- topic_modeling_p1(
                    rds_path = rds_path,
                    metadata = rv$df_post,
                    main_el = input$var_topic,
                    discr_els = input$var_topic_elements,
                    categorySel = input$subset_colon_topic,
                    elements = input$subset_colon_elements,
                    rank = input$tax_rank
                )
                showNotification("Topic modeling step 1 completato!", type = "message")
            },
            error = function(e) {
                showNotification(paste("Errore in topic_modeling_p1:", conditionMessage(e)), type = "error")
                cat("[ERROR] topic_modeling_p1:", conditionMessage(e), "\n")
                rv$topic_modeling_p1 <- NULL
            }
        )
    })
    observeEvent(input$startTopicStep2, {
        cat("[DEBUG] startTopicStep2 triggered\n")
        req(rv$topic_modeling_p1, input$topic_k)

        tryCatch(
            {
                rv$topic_modeling_p2 <- topic_modeling_p2(rv$topic_modeling_p1$ps_g, input$topic_k, rv$topic_modeling_p1$count_matrix, rv$topic_modeling_p1$main_el, rv$df_post, rv$topic_modeling_p1$formulaInUse)
                showNotification("Topic modeling step 2 completed!", type = "message")
            },
            error = function(e) {
                showNotification(paste("Error in topic_modeling_p2:", conditionMessage(e)), type = "error")
                cat("[ERROR] topic_modeling_p2:", conditionMessage(e), "\n")
                rv$topic_modeling_p2 <- NULL
            }
        )
    })
    observeEvent(input$startTopicStep3, {
        cat("[DEBUG] startTopicStep3 triggered\n")
        req(rv$topic_modeling_p2, input$topic_number)

        tryCatch(
            {
                rv$topic_modeling_p3 <- topic_modeling_p3(
                    rv$topic_modeling_p2$b_df, rv$topic_modeling_p2$ps_g,
                    input$topic_number, rv$topic_modeling_p2$formulaInUse
                )
                showNotification("Topic modeling step 3 completed!", type = "message")
            },
            error = function(e) {
                showNotification(paste("Error in topic_modeling_p3:", conditionMessage(e)), type = "error")
                cat("[ERROR] topic_modeling_p3:", conditionMessage(e), "\n")
                rv$topic_modeling_p3 <- NULL
            }
        )
    })
    # Mostra il grafico solo dopo l'esecuzione
    output$topic_plot_p1 <- renderPlot({
        req(rv$topic_modeling_p1$p1)
        rv$topic_modeling_p1$p1
    })
    output$topic_plot_p2 <- renderPlot({
        req(rv$topic_modeling_p2$p2)
        rv$topic_modeling_p2$p2
    })

    output$topic_plot_p3 <- renderPlot({
        req(rv$topic_modeling_p3$p3)
        rv$topic_modeling_p3$p3
    })
    output$topic_plot_p4 <- renderPlot({
        req(rv$topic_modeling_p3$p4)
        rv$topic_modeling_p3$p4
    })

    ################## end
    session$onSessionEnded(function() {
        stopApp()
    })
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("combined_", Sys.Date(), ".jpg")
        },
        content = function(file) {
            # Recupera il grafico corrente
            plot_index <- numbers::mod(rv$plot_n, length(rv$plot)) + 1
            req(rv$plot[[plot_index]])
            req(rv$table)

            # Converte la tabella data.frame in un grob
            table_grob <- tableGrob(rv$table)

            # Combina grafico e tabella
            combined_plot <- rv$plot[[plot_index]] / table_grob +
                plot_layout(heights = c(4, 1))
            # Salva in un unico file
            ggsave(file, plot = combined_plot, device = "jpg", width = 10, height = 13, dpi = 300)
        }
    )
}
