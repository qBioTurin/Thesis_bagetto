# Template for the Mirri Shiny app
This app is designed to run on Unix-based systems (Linux or macOS); running it on Windows is not recommended because some features—such as automatic downloading of Docker images—are not supported. For best results and maximum compatibility, use a Unix-like environment.
## Requirements
- R (version compatible with the packages used by the app).
- RStudio or Code with Rstudio extension (recommended).
- Docker (required only if you use the provided containers; recommended on Linux/macOS).
- On Linux/Windows you may need additional system packages to compile some R dependencies (e.g., development tools).

## Download
In a terminal, run:

```bash
mkdir projectdirectory
cd projectdirectory
git clone https://github.com/qBioTurin/ShinyMirriTemplate.git
```

## Launching the app
1. Open RStudio and set the project working directory to the project root (`AppWorkflow`).
2. Open and run `inst/Shiny/app.R`. The script will attempt to install any missing R packages and then start the app.

Notes:
- The automatic function that detects the project root does not always work—work in progress.
- Automatic download of Docker images is supported on Linux and macOS only. Windows users must install the images/containers manually.
- macOS: there may be issues that have not been verified (no test machine available).
- On the first run the app may crash. If that happens, restart R/RStudio and run the steps in order again.

## Using the app
1. On the first tab, upload your metadata file (CSV/TSV/TXT) and explore it using the Univariate and Bivariate plots.
2. If column names appear as V1, V2, ...:
  - Click the "Manually adjust" control in the UI.
  - Toggle the "Has header" (or "Header") switch off and then on to force re-detection of the header row.
  - If column names are still wrong, verify the delimiter (comma/semicolon/tab) in the import options or open the file in a text editor to confirm the first row contains the column names.
2. On the second tab, upload your data files (`.biom` for Kraken, `.txt` for MetaPhlAn) and choose an output folder.

- These two steps are required each session. If the chosen output folder already contains results, subsequent operations are optional.
- When running the full workflow for the first time, follow the steps in order and expect possible instability; restarting usually resolves transient issues.