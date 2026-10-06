running_in_docker <- file.exists("/.dockerenv")
require_startup_password <- tolower(Sys.getenv("MICROWORKFLOW_REQUIRE_PASSWORD", "false")) %in% c("1", "true", "yes")
default_disable_auth <- if (require_startup_password) "false" else "true"
disable_auth <- tolower(Sys.getenv("MICROWORKFLOW_DISABLE_AUTH", default_disable_auth)) %in% c("1", "true", "yes")
auth_enabled <- !disable_auth

ui <- fluidPage(
  theme_QBio,
  tags$head(
    tags$script(
      "$(document).ready(function() {
        const tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')) # nolint
        const tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
          return new bootstrap.Tooltip(tooltipTriggerEl)
        })
      });"
    ),
    tags$script(
      "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
    ),
    # All visual styles are in QBio_Theme.css — no inline overrides needed.
  ),
  tags$div(
    id = "auth_shell",
    class = "auth-shell",
    tags$div(
      id = "auth_login_brand",
      tags$img(
        src = "image/title_old.png",
        alt = "MicroWorkflow",
        class = "auth-login-title-img"
      )
    ),
    tags$div(
      id = "auth_entry_panel",
      class = "auth-login-panel",
      tags$img(
        src = "image/title_new.png",
        alt = "MicroWorkflow",
        class = "auth-login-cat"
      ),
      if (auth_enabled) {
        shinyauthr::loginUI(
          id = "login",
          title = "",
          user_title = "Username",
          pass_title = "Password",
          login_title = "Entra"
        )
      } else {
        tagList(
          tags$p("Local mode: no authentication required."),
          actionButton(
            "local_enter",
            label = "Entra",
            class = "btn btn-primary"
          )
        )
      }
    ),
    shinyjs::hidden(
      tags$div(
        id = "app_content",
        class = "app-shell",
        tags$div(
          class = "auth-toolbar",
          shinyauthr::logoutUI(id = "logout", label = "Esci")
        ),
        headerPanel(),
        useShinyjs(),
        use_waiter(),
        navbarPage(
          id = "main_nav",
          title = "",
          collapsible = TRUE,
          # inverse = TRUE,
          tabPanel(
            value = "pre_process",
            title = HTML("⚙️⚙️<br>Pre-process."),
            conditionalPanel(
              "!output.metadataLoaded",
              fluidRow(
                tags$div(
                  fileInput("file", "Upload metadata",
                    multiple = FALSE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
                  ),
                  title = "Upload the CSV file to preprocess",
                  `data-bs-toggle` = "tooltip",
                  `data-bs-placement` = "right"
                ),
                conditionalPanel(
                  "output.fileUploaded",
                  sidebarLayout(
                    sidebarPanel(
                      actionButton("manually",
                        label = "Manually adjust",
                        styleclass = "secondary",
                        style = "margin:1em;"
                      ),
                      conditionalPanel(
                        condition = "input.manually !=0",
                        tags$div(
                          style = "display: flex; align-items: center; gap: 8px;",
                          materialSwitch(
                            inputId = "header",
                            label = "Header",
                            value = TRUE,
                            status = "default"
                          ),
                          withTooltip(
                            tags$span(
                              icon("question-circle"),
                              style = "cursor: help;"
                            ),
                            Manually_adj
                          )
                        ),
                        textInput("sep",
                          "Separator",
                          value = ",",
                          width = "100px"
                        ),
                        radioButtons("quote",
                          "Quote",
                          choices = c(
                            None = "",
                            "Double Quote" = '"',
                            "Single Quote" = "'"
                          ),
                          selected = '"'
                        )
                      ),
                      uiOutput("colTypeInputs"),
                      width = 3,
                      style = "margin:1em;"
                    ),
                    mainPanel(
                      DTOutput("df_table")
                    )
                  )
                ),
                shinyjs::useShinyjs(),
                actionButton("load",
                  label = "Load",
                  styleclass = "default",
                  style = "margin:1em;"
                ),
                align = "center"
              )
            ),
            conditionalPanel(
              "output.metadataLoaded",
              h1("Preprocessing"),
              fluidRow(
                column(
                  uiOutput("variables_input"),
                  offset = 1,
                  width = 3
                ),
                column(
                  fluidRow(
                    wellPanel(
                      plotOutput(outputId = "distPlot"),
                      downloadButton("downloadData", "Download"),
                      actionButton("refresh", "Other", class = "red-button"),
                      actionButton("editMetadata", "Edit metadata", class = "refined-button")
                    ),
                    wellPanel(tableOutput(outputId = "Table"),
                      class = "fit-to-content"
                    )
                  ),
                  width = 7
                )
              )
            ),
          ),
          tabPanel(
            value = "load_data",
            title = HTML("📂<br>Kraken/Meta."),
            fluidRow(
              column(
                width = 10,
                offset = 1,
                tags$div(
                  style = "display: flex;
              flex-direction: column; align-items: center;",
                  tags$div(
                    style = "width: 100%; max-width: 860px; padding: 12px 0;",
                    tags$h3("Upload Inputs"),
                    uiOutput("setIdColumn"),
                    withTooltip(
                      shinyDirButton(
                        "Biom",
                        "Raw Biom Data", "Please select a folder",
                        class = "refined-button"
                      ),
                      Select_data_type
                    ),
                    verbatimTextOutput("biom_file_contents"),
                    textInput("remove_data_name",
                      "Data/metadata name alignment",
                      value = "_final_unmapped_"
                    ),
                    uiOutput("output_dir_selector_ui"),
                    verbatimTextOutput("output_dir"),
                    uiOutput("download_shared_results_ui"),
                    actionButton(
                      "uploadData", "Upload Data",
                      icon = icon("play"),
                      styleclass = "default",
                      style = "margin:1em;",
                      class = "refined-button"
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "normalization",
            title = HTML("🧹<br>Normaliz."),
            fluidRow(
              column(
                width = 10,
                offset = 1,
                tags$div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  tags$div(
                    style = "width: 100%; max-width: 860px; padding: 12px 0;",
                    tags$div(
                      id = "options_panel_k",
                      style = "overflow: visible !important; padding: 12px 0;",
                      tags$h3("Normalization Options"),
                      tags$div(
                        style = "display: flex; justify-content: center; margin-bottom: 12px;",
                        materialSwitch(
                          inputId = "do_normalization",
                          label = "Normalize data?",
                          value = TRUE,
                          status = "primary"
                        )
                      ),
                      conditionalPanel(
                        condition = "input.do_normalization == true",
                        tags$div(
                          style = "overflow: visible !important;",
                          uiOutput("design_input_dynamic")
                        )
                      )
                    ),
                    tags$div(
                      id = "options_panel_m",
                      style = "padding: 12px 0;",
                      tags$h3("Normalization Options"),
                      sliderInput("decont_level_metaphlan", "Decontamination Level:",
                        min = 0, max = 0.05,
                        value = 0.01, step = 0.001
                      )
                    ),
                    tags$div(
                      style = "display: flex; justify-content: center; margin-top: 16px;",
                      actionButton("startDecontam",
                        "Start Decontamination",
                        icon = icon("play"),
                        class = "refined-button"
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "supervised",
            title = HTML("🧪<br>Decontam."),
            tags$div(
              style = "padding: 16px;",
              fluidRow(
                column(
                  width = 4, offset = 4,
                  conditionalPanel(
                    condition = "output.pipeline_type_ui == 'kraken'",
                    sliderInput("decont_level", "Decontamination Level:",
                      min = 0, max = 0.05,
                      value = 0.01, step = 0.001
                    )
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 4,
                  card(
                    card_header(tags$span(icon("bacterium"), " Bacteria", style = "font-weight:600;")),
                    card_body(
                      conditionalPanel(
                        condition = "output.pipeline_type_ui == 'kraken'",
                        radioButtons("Bacteria_level", "Taxonomic level", choices = c("Phylum", "Genus"), inline = TRUE)
                      ),
                      conditionalPanel(
                        condition = "output.pipeline_type_ui == 'metaphlan'",
                        radioButtons("Bacteria_level_meta", "Taxonomic level", choices = c("Phylum", "Genus"), inline = TRUE)
                      ),
                      uiOutput("checkBoxBacteria"),
                      tags$hr(),
                      tags$p(
                        style = "font-size: 0.85em; color: #666; margin-bottom: 6px;",
                        "Species-level spike-in removal (by taxid)"
                      ),
                      uiOutput("bacteriaSpikeTaxidUi")
                    )
                  )
                ),
                column(
                  width = 4,
                  card(
                    card_header(tags$span(icon("circle"), " Archaea", style = "font-weight:600;")),
                    card_body(
                      conditionalPanel(
                        condition = "output.pipeline_type_ui == 'kraken'",
                        radioButtons("Archaea_level", "Taxonomic level", choices = c("Phylum", "Genus"), inline = TRUE)
                      ),
                      conditionalPanel(
                        condition = "output.pipeline_type_ui == 'metaphlan'",
                        radioButtons("Archaea_level_meta", "Taxonomic level", choices = c("Phylum", "Genus"), inline = TRUE)
                      ),
                      uiOutput("checkBoxArchaea")
                    )
                  )
                ),
                column(
                  width = 4,
                  card(
                    card_header(tags$span(icon("leaf"), " Eukaryota", style = "font-weight:600;")),
                    card_body(
                      conditionalPanel(
                        condition = "output.pipeline_type_ui == 'kraken'",
                        radioButtons("Eukaryota_level", "Taxonomic level", choices = c("Phylum", "Genus"), selected = "Genus", inline = TRUE)
                      ),
                      conditionalPanel(
                        condition = "output.pipeline_type_ui == 'metaphlan'",
                        radioButtons("Eukaryota_level_meta", "Taxonomic level", choices = c("Phylum", "Genus"), selected = "Genus", inline = TRUE)
                      ),
                      uiOutput("checkBoxEukaryota")
                    )
                  )
                )
              ),
              tags$div(
                style = "display: flex; justify-content: center; margin-top: 20px;",
                actionButton("startManual",
                  "Decontamination",
                  icon = icon("flask"),
                  class = "refined-button"
                )
              )
            )
          ),
          tabPanel(
            value = "graphs",
            title = HTML("📊<br>Graphs"),
            # Pills row — centred
            tags$div(
              style = "width: 100%; padding: 16px 0 8px 0;",
              tabsetPanel(
                type = "pills",
                tabPanel(
                  "Alpha Diversity",
                  fluidRow(
                    style = "width: 100%; margin: 24px 0 0 0;",
                    column(
                      4,
                      tags$div(
                        style = "max-width: 640px; margin: 0 auto; text-align: left;",
                        selectInput("Domain", "Domain",
                          choices = c("Archaea", "Bacteria", "Eukaryota"),
                          selected = "Bacteria"
                        ),
                        selectInput("alpha", "Alpha metric",
                          choices = c("Shannon", "Simpson", "Observed")
                        ),
                        uiOutput("setVarAlpha"),
                        uiOutput("selectEl_alpha"),
                        uiOutput("subset_alpha")
                      )
                    ),
                    column(
                      8,
                      tags$div(
                        style = "width: 100%;",
                        plotOutput("AlphaPlot", width = "100%", height = "720px")
                      ),
                      tags$div(
                        style = "display: flex; justify-content: center; margin-top: 12px;",
                        downloadButton("plotAlpha", "Download")
                      )
                    )
                  )
                ),
                tabPanel(
                  "Beta Diversity",
                  fluidRow(
                    style = "width: 100%; margin: 24px 0 0 0;",
                    column(
                      4,
                      tags$div(
                        style = "max-width: 640px; margin: 0 auto; text-align: left;",
                        selectInput("DomainBeta", "Domain",
                          choices = c("Archaea", "Bacteria", "Eukaryota"),
                          selected = "Bacteria"
                        ),
                        uiOutput("setVarBeta"),
                        uiOutput("selectEl_beta"),
                        uiOutput("subset_beta")
                      )
                    ),
                    column(
                      8,
                      tags$div(
                        style = "width: 100%;",
                        plotOutput("BetaPlot", width = "100%", height = "720px")
                      ),
                      tags$div(
                        style = "display: flex; justify-content: center; margin-top: 12px;",
                        downloadButton("plotBeta", "Download")
                      )
                    )
                  )
                ),
                tabPanel(
                  "Stacked Bar",
                  fluidRow(
                    style = "width: 100%; margin: 24px 0 0 0;",
                    column(
                      4,
                      tags$div(
                        style = "max-width: 640px; margin: 0 auto; text-align: left;",
                        uiOutput("DomainUI"),
                        uiOutput("LevelSbUI"),
                        uiOutput("DivisionSbUI"),
                        uiOutput("StackBin"),
                        uiOutput("selectEl_stack"),
                        uiOutput("subset_stack")
                      )
                    ),
                    column(
                      8,
                      tags$div(
                        style = "width: 100%;",
                        plotOutput("StackPlot", width = "100%", height = "720px")
                      ),
                      tags$div(
                        style = "display: flex; justify-content: center; margin-top: 12px;",
                        downloadButton("plotStack", "Download")
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "das_soi",
            title = HTML("🧬<br>DAS"),
            fluidRow(
              column(
                6,
                tags$div(
                  style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: left;",
                  tags$h3("DAS Options"),
                  selectInput("kingdomDAS", "Kingdom",
                    choices = c("Bacteria" = 2, "Archaea" = 1, "Eukaryota" = 3)
                  ),
                  selectInput("run_das", "Programs to run",
                    choices = c("Maaslin3")
                  ),
                  selectInput(
                    "maaslin_model_filter",
                    "DA species source (all_results.tsv$model)",
                    choices = c(
                      "Both (abundance + prevalence)" = "both",
                      "Abundance only" = "abundance",
                      "Prevalence only" = "prevalence"
                    ),
                    selected = "both"
                  ),
                  selectInput("significance_metric_das", "Significance Metric",
                    choices = c("p_value", "q_value")
                  ),
                  uiOutput("selectEl_das"),
                  uiOutput("subSetDAS"),
                  uiOutput("select_discrDAS"),
                  tags$div(
                    style = "display: flex; gap: 10px; margin-top: 12px; flex-wrap: wrap;",
                    actionButton("createDAS", "Run DAS",
                      icon = icon("rocket"), class = "refined-button"
                    ),
                    downloadButton("download_das",
                      "Download (.rds)",
                      class = "refined-button"
                    )
                  )
                )
              ),
              column(
                6,
                tags$div(
                  style = "width: 100%; max-width: 560px; margin: 0 auto; padding: 12px 0; text-align: left;",
                  tags$h3("Heatmap Options"),
                  tags$p(
                    class = "text-muted",
                    style = "font-size:0.82em; margin-bottom:8px;",
                    "Il file RDS viene caricato automaticamente dopo il Run DAS, oppure carica un file manualmente."
                  ),
                  tags$div(
                    fileInput("DAS", "DAS File (opzionale — sovrascrive Run DAS)"),
                    title = "Select the RDS file with DAS results to create the heatmap",
                    `data-bs-toggle` = "tooltip",
                    `data-bs-placement` = "right"
                  ),
                  uiOutput("checkBoxHeatmap"),
                  uiOutput("heatmapSplitVarUi"),
                  uiOutput("heatmapLabelVarUi"),
                  tags$div(
                    style = "display: flex; gap: 10px; margin-top: 12px; flex-wrap: wrap;",
                    actionButton("createHeatmap", "Create Heatmap",
                      icon = icon("fire"), class = "refined-button"
                    ),
                    downloadButton("HeatmapDownload", "Download",
                      class = "refined-button"
                    )
                  )
                )
              )
            ),
            # ── bottom: full-width heatmap output ─────────────────────────────
            fluidRow(
              column(
                12,
                tags$div(
                  style = "width: 100%; margin-top: 8px;",
                  plotOutput("HeatmapPlot", height = "700px")
                )
              )
            )
          ),
          tabPanel(
            value = "leaps",
            title = HTML("📈<br>Leaps"),
            fluidRow(
              column(
                width = 10,
                offset = 1,
                tags$div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  tags$div(
                    style = "width: 100%; max-width: 860px; padding: 12px 0;",
                    tags$h3("LEAPS Options"),
                    uiOutput("leapsUi"),
                    textInput("leaps_filename",
                      "Leaps filename:",
                      value = "LEAPS_results"
                    ),
                    actionButton("createLeaps", "Run",
                      icon = icon("rocket"), class = "refined-button"
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "topic_modeling",
            title = HTML("🗂️<br>Topic Model."),
            fluidRow(
              column(
                width = 10,
                offset = 1,
                tags$div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  tags$div(
                    style = "width: 100%; max-width: 1100px; padding: 12px 0;",
                    tags$h3("Topic Modeling Options"),
                    uiOutput("topic_page_ui")
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "humann_processing",
            title = HTML("⚗️<br>Pre-Humann"),
            fluidRow(
              column(
                width = 10,
                offset = 1,
                tags$div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  tags$div(
                    style = "width: 100%; max-width: 860px; padding: 12px 0;",
                    tags$h3("Pre-Humann"),
                    shinyDirButton(
                      "humann_input_dir",
                      "HUMAnN input folder", "Please select a folder"
                    ),
                    verbatimTextOutput("humann_input_folder"),
                    actionButton("startHumann", "Start HUMAnN Processing",
                      icon = icon("play"), class = "refined-button"
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "humann_graphs",
            title = HTML("📉<br>Humann Graph"),
            fluidRow(
              column(
                width = 10,
                offset = 1,
                tags$div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  tags$div(
                    style = "width: 100%; max-width: 1100px; padding: 12px 0;",
                    tabsetPanel(
                      type = "pills",
                      tabPanel(
                        "Page 1",
                        tags$div(
                          style = "width: 100%; padding: 20px 0;",
                          tags$div(
                            style = "width: 100%; min-height: 600px; padding: 12px 0;",
                        tags$h3("Humann Graphs Options"),
                        tags$div(
                          fileInput("humann_graph", "Choose HUMAnN Processed Data File",
                            multiple = FALSE,
                            accept = c(
                              "text/tsv",
                              "text/tab-separated-values,text/plain",
                              ".tsv"
                            )
                          ),
                          title = "Upload the TSV file with processed HUMAnN results",
                          `data-bs-toggle` = "tooltip",
                          `data-bs-placement` = "right"
                        ),
                        uiOutput("humann_graph_file_contents"),
                        uiOutput("selectEl_das_pathway"),
                        uiOutput("subSetDAS_pathway"),
                        uiOutput("select_discrDAS_pathway"),
                        actionButton("start_pathway_searcher", "Start Pathway Searcher",
                          icon = icon("play"), class = "refined-button"
                        )
                      )
                    )
                  ),
                      tabPanel(
                        "Page 2",
                        tags$div(
                          style = "width: 100%; padding: 20px 0;",
                          tags$div(
                            style = "width: 100%; min-height: 600px; padding: 12px 0;",
                        tags$h3("Humann Graphs Options - Page 2"),
                        selectizeInput(
                          "pathway_search",
                          label = "Seleziona pathway",
                          choices = NULL,
                          options = list(
                            placeholder = "Digita il nome della pathway...",
                            maxOptions = 1000,
                            onInitialize = I("function() { this.focus(); }")
                          )
                        ),
                        uiOutput("selectEl_das_pathway_graph_main"),
                        uiOutput("selectEl_das_pathway_graph_sub"),
                        virtualSelectInput(
                          "pathway_variables",
                          "Select Variables",
                          choices = c(
                            "remove zeroes" = "--remove-zeros",
                            "view at genera level" = "--as-genera"
                          ),
                          multiple = TRUE,
                          showValueAsTags = TRUE,
                          clearButton = TRUE,
                          search = FALSE
                        ),
                        radioButtons(
                          "pathway_variables_sort",
                          "Select how to sort",
                          choices = c(
                            "None" = "none",
                            "sort by sum" = "--sort sum",
                            "sort by braycurtis" = "--sort braycurtis",
                            "sort by dominant" = "--sort dominant",
                            "sort by braycurtis weighted" = "--sort braycurtis_w"
                          ),
                          selected = "none"
                        ),
                        radioButtons(
                          "pathway_variables_scaling",
                          "Select how to scale",
                          choices = c(
                            "None" = "none",
                            "logstack" = "--scaling logstack",
                            "original" = "--scaling original",
                            "totalsum" = "--scaling totalsum"
                          ),
                          selected = "none"
                        ),
                        actionButton("start_humann_graphs",
                          "Generate Pathway Graph",
                          icon = icon("chart-bar"),
                          class = "refined-button",
                          style = "margin-top: 20px;"
                        )
                      )
                    )
                  )
                    )
                  )
                )
              )
            )
          )
        ),
        footerPanel()
      )
    )
  )
)
