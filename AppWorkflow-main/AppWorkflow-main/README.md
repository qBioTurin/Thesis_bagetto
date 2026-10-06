# MicroWorkflow (AppWorkflow)

## 1. Description / Overview

MicroWorkflow is an R/Shiny application for post-processing and interactive analysis of metagenomics outputs from Kraken/Bracken, MetaPhlAn, and HUMAnN.

The application is designed for microbiome and metagenomics researchers who need an end-to-end interface for:

- metadata-driven exploratory analysis,
- normalization and decontamination,
- diversity and composition visualization,
- differential analysis of taxa and pathways,
- generation of analysis-ready RDS and publication-ready plots.

Supported pipelines:

- Kraken/Bracken (via BIOM input)
- MetaPhlAn (merged abundance table plus per-sample files for taxid recovery)
- HUMAnN (Docker-automated processing and pathway visualization)

## 2. Features

- Interactive metadata import with manual parsing controls (header, separator, quote style).
- Per-column metadata typing with support for character, numeric, factor, and date variables.
- Automated pipeline detection from file extensions in the selected raw data directory (`.report` → Kraken/Bracken, `.txt` → MetaPhlAn).
- Fully automated raw data preprocessing via Docker (`run_preprocess_pipeline()`): `kingdom_rename.sh` + `kraken-biom` for Kraken, `merge_metaphlan_tables.py` for MetaPhlAn.
- Kraken and MetaPhlAn preprocessing to domain-specific phyloseq objects.
- Threshold-based filtering and taxonomic-level control for decontamination.
- Supervised manual decontamination by domain (Bacteria, Archaea, Eukaryota).
- Alpha diversity plots (Shannon, Simpson, Observed) with significance annotations.
- Beta diversity PCoA (Bray-Curtis) with PERMANOVA p-value display.
- Stacked barplots for top taxa with optional MetaPhlAn BIN inclusion.
- Differential analysis workflows:
  - DAS: MaAsLin3, ANCOM-BC2, or combined outputs.
  - SOI: LEfSe, limma, or combined outputs.
- Heatmap generation from DAS/SOI RDS outputs with metadata annotations.
- HUMAnN processing pipeline automation through Docker.
- Pathway differential analysis (MaAsLin3) and pathway barplot generation.
- Export of key visual outputs to PDF and analysis artifacts to RDS/TSV/CSV.

## 3. System Requirements

### Operating system

- Recommended: Linux or macOS.
- Windows: supported only with caution. Docker-dependent modules (LEfSe and HUMAnN workflows) are often less reliable on native Windows setups. WSL2 is strongly recommended for Windows users.

### Software

- R: 4.5.x (project developed with R 4.5).
- Docker: required for LEfSe and HUMAnN steps.
- Python environment/tools (for upstream preprocessing):
  - MetaPhlAn utilities (for `merge_metaphlan_tables.py`)
  - kraken-biom (for BIOM conversion)

### Typical system-level build dependencies for R packages

Many R dependencies require C/C++ toolchains and common development libraries (for SSL, CURL, XML, and font/text rendering). Ensure your system can build source packages when binaries are unavailable.

## 4. Installation

### Step 1. Clone the repository

```bash
git clone git@github.com:qBioTurin/AppWorkflow.git
cd AppWorkflow
```

### Step 2. Install R dependencies

The project includes `R/Packages.R`, which installs missing CRAN, Bioconductor, and GitHub packages.

```r
source("R/Packages.R")
```

Behavior of `R/Packages.R`:

- checks for required packages,
- installs missing packages from CRAN/Bioconductor,
- installs selected GitHub packages (including MaAsLin3).

Notes:

- first installation can be long,
- internet access is required,
- some packages may compile from source.

### Step 3. Ensure Docker is available

Verify Docker before using LEfSe or HUMAnN pages:

```bash
docker --version
```

The app expects/pulls Docker images used in code paths, including:

- `microworkflow/pipeline-tools:latest` — unified pipeline tools image (Kraken/Bracken, MetaPhlAn, DB-classification script)
- `franky2204/lefse:latest`
- `franky2204/humann3.9:latest`

### Step 4. Prepare upstream inputs

**This step is now fully automated by the app.** You only need to prepare a directory of raw output files:

- **Kraken/Bracken**: a folder containing `.report` files produced by Bracken.
- **MetaPhlAn**: a folder containing per-sample `.txt` output files.

When you click **Upload Data** in the Load Data tab, the app automatically:

1. Detects the pipeline from the file extensions (`.report` → Kraken, `.txt` → MetaPhlAn).
2. Runs the appropriate preprocessing sequence inside the `microworkflow/pipeline-tools:latest` Docker container:
   - **Kraken**: runs `kingdom_rename.sh` to fix rank codes, then `kraken-biom` to produce `kraken_merged/kraken_merged.biom`.
   - **MetaPhlAn**: runs `merge_metaphlan_tables.py` to produce `metaphlan_merged/merged_metaphlan.txt`.
3. Passes the generated file into the downstream analysis pipeline.

Output files are written alongside the selected input directory:

```
<parent_of_selected_dir>/
  kraken_merged/kraken_merged.biom     # Kraken output
  metaphlan_merged/merged_metaphlan.txt  # MetaPhlAn output
```

#### Background: kingdom_rename.sh

Recent versions of Kraken and Bracken changed how taxonomy rank information is represented, but `kraken-biom` was not updated accordingly. The `kingdom_rename.sh` script from the [Bio-Tools repository](https://github.com/franky2204/Bio-Tools) fixes this by setting the rank code field (field 4) to `D` (domain) for kingdom-level taxonomy IDs:

- `2157` — Archaea
- `2` — Bacteria
- `2759` — Eukaryota
- `10239` — Viruses

This step runs automatically inside the Docker container; no manual invocation is needed.

## 5. Running the Application

Primary repository entry point:

```r
source("inst/Shiny/app.R")
```

Package entry point (after package installation):

```r
MicroWorkflow.run()
```

Legacy/local wrapper usage (if your environment defines an `App.R` launcher):

```r
source("App.R")
```

Default runtime configuration in the repository app script:

- host: `0.0.0.0`
- port: `3838`

Expected behavior:

- the Shiny interface starts in your browser,
- you proceed tab-by-tab through metadata upload, taxonomic input processing, decontamination, and downstream analyses.
- the interface uses a guided workflow: tabs are progressively unlocked as prerequisite steps are completed.
- `Humann Processing` remains available independently from the main metagenomics workflow.

## 6. Workflow Overview

1. Upload and type metadata.
2. After metadata `Load`, the app unlocks **Load Data**.
3. Select the raw data directory (Kraken `.report` files or MetaPhlAn per-sample `.txt` files) and click **Upload Data** — preprocessing runs automatically via Docker.
4. Alternatively, if your output folder already contains at least one valid domain pair of Supervised decontamination + ALPHA metadata, you can skip **Data directory** and upload directly from existing outputs.
5. If alpha/supervised outputs are already available, the downstream analysis tabs are unlocked immediately.
6. Otherwise, unlock and run **Normalization** first.
7. After normalization, unlock and run **Supervised Decontamination**.
8. After supervised decontamination, the remaining downstream tabs are unlocked.
9. (Optional) Run HUMAnN processing and pathway analysis/visualization at any time.

## 7. Detailed Usage

### Metadata Upload and Preprocessing

What metadata is:

- sample-level annotations used to define groups, covariates, subsets, and confounders (for example phenotype, timepoint, treatment, age).

File expectations:

- table format loaded through the first tab,
- include a unique sample identifier column named `id` (or converted to `id` in your workflow),
- avoid ambiguous duplicated IDs.

Manual adjustments:

- Header toggle: indicates whether first row contains column names.
- Separator: allows non-comma delimiters.
- Quote: none, single quote, or double quote handling.

Column type handling:

- `character`: free-text fields.
- `numeric`: quantitative variables.
- `factor`: categorical variables (unordered or ordered).
- `date`: date-formatted fields.

Practical note:

- after pressing `Load`, metadata are used as the reference for the ongoing workflow.
- if you need to change them, use `Edit metadata` in the Pre-process tab.
- this safely resets all steps that depend on the current metadata, starting from `Load Data` onward, so downstream results are not mixed with outdated sample annotations.

### Data Upload (Kraken / MetaPhlAn)

Input selection:

- Click **Data directory (optional)** and select the folder containing your raw pipeline outputs:
  - Kraken/Bracken: folder with `.report` files.
  - MetaPhlAn: folder with per-sample `.txt` files.
- The pipeline type is auto-detected from the file extensions.
- Click **Upload Data** to trigger automated preprocessing via Docker (`microworkflow/pipeline-tools:latest`).

Guided availability of tabs:

- before metadata are loaded, the main downstream tabs are locked.
- after metadata `Load`, only **Load Data** is unlocked in the main workflow.
- after **Upload Data**:
  - if existing alpha + supervised outputs are detected, the downstream tabs are unlocked;
  - otherwise, only **Normalization** is unlocked.
- after **Start Decontamination**, **Supervised Decontamination** is unlocked.
- after **Start Manual Decontam.**, all remaining downstream tabs are unlocked.
- **Humann Processing** is always available and is not blocked by the main workflow state.

Optional direct load from existing outputs:

- You can skip **Data directory** if **Output directory** already contains processed files.
- The app accepts this mode when it finds at least one valid domain pair (Bacteria, Archaea, or Eukaryota):
  - Kraken pair: `SUPERVISED_DEC/<Domain>_Supervised_decontam.rds` + `ALPHA/<Domain>_alpha_metadata.csv`
  - MetaPhlAn pair: `META_SUPERVISED_DEC/<Domain>_Supervised_meta_decontam.rds` (or `_meta_no_strain.rds`) + `ALPHA/<Domain>_meta_alpha_metadata.csv`
- In this case, preprocessing is skipped and the app uses the detected pipeline from existing files.

Preprocessing output (auto-generated next to the selected directory):

- Kraken: `kraken_merged/kraken_merged.biom`
- MetaPhlAn: `metaphlan_merged/merged_metaphlan.txt`

The generated file is then automatically loaded into the downstream analysis pipeline. No separate MetaPhlAn folder selection is required — the same directory is used for taxid recovery.

ID matching requirements:

- metadata IDs must match sample IDs in the processed abundance data after name cleaning.

Naming/cleaning behavior:

- the app can remove a user-defined suffix/prefix using `Remove data name`.
- special characters are frequently normalized to underscores in internal processing.

### Normalization and Decontamination

- Kraken path includes optional normalization and taxonomic-level choices (Phylum/Genus by domain).
- MetaPhlAn path applies threshold filtering and domain table generation before RDS creation.
- Manual supervised decontamination allows explicit taxa selection via domain-specific checkboxes.

### Diversity Analysis

Alpha diversity metrics:

- Shannon: entropy-based richness/evenness index.
- Simpson: dominance-sensitive diversity index.
- Observed: observed feature richness count.

Beta diversity:

- Bray-Curtis distance,
- PCoA visualization,
- PERMANOVA p-value shown on the plot.

Subsetting:

- alpha, beta, and stacked bar analyses support metadata-driven sample subset selection.

Export:

- alpha/beta/stacked bar plots are downloadable as PDF.

### Visualization

Stacked barplots:

- top taxa composition by selected taxonomic level,
- grouped/faceted by metadata variable,
- supports subset filtering.

MetaPhlAn BIN option:

- BIN-like strain labels (for example entries containing `GB`) can be included or excluded.
- enabling BIN inclusion is useful for exploratory views but can reduce interpretability.

### Differential Analysis (DAS / SOI)

Current app implementation:

- DAS module:
  - MaAsLin3,
  - ANCOM-BC2,
    
- SOI module:
  - LEfSe,
  - limma,
  - or LEfSe+limma intersection/merge.

Statistical concepts:

- p-value: raw significance probability under the null hypothesis.
- q-value: multiple-testing adjusted value (false discovery control).

Model terms:

- discriminant: main variable of interest used to compare groups.
- confounder/discriminant covariate: additional variables included to adjust effects.

Outputs:

- differential results are filtered and exported as RDS objects for downstream steps.

### Heatmap Generation

Input:

- DAS/SOI RDS object loaded in the Heatmap tab.

Customization:

- select metadata columns for annotation tracks.

Export:

- heatmap can be downloaded from the app interface.

### HUMAnN Processing

HUMAnN processing is Docker-automated in the app.

Input expectations:

- a folder containing HUMAnN TSV outputs.

Processing sequence (automated):

1. normalize tables,
2. merge HUMAnN tables,
3. regroup gene families,
4. rename features,
5. renormalize renamed outputs,
6. produce merged final tables for graphing.

Outputs are written under an auto-created `HUMANN_OUTPUT` directory in the selected input folder.

### Pathway Analysis

Pathway differential analysis:

- uses MaAsLin3 on a selected HUMAnN processed table.

Visualization:

- pathway barplots generated through HUMAnN plotting commands in Docker,
- configurable options include:
  - remove zeros,
  - view at genera level,
  - sorting (`sum`, `braycurtis`, `dominant`, `braycurtis_w`),
  - scaling (`logstack`, `original`, `totalsum`).

### Graph Types and Options (Complete Reference)

This section describes every graph currently exposed by the Shiny interface and the meaning of each option.

#### A. Preprocessing Exploratory Graphs (first tab)

Graph purpose:

- quick exploratory visualization of metadata distributions before taxonomic analyses.

Available graph families (automatically chosen by variable type):

- categorical variables: bar chart and pie chart.
- numeric/date variables: univariate and bivariate summary plots generated by the preprocessing module.

Options:

- selected variable(s): determines which metadata column(s) are summarized.
- refresh/other plot: cycles to an alternative chart for the same variable set.
- download: exports the currently displayed exploratory plot.

#### B. Alpha Diversity Graph

Graph purpose:

- boxplot-based comparison of within-sample diversity across groups.

Options:

- Domain:
  - selects which decontaminated domain RDS is used (`Bacteria`, `Archaea`, `Eukaryota`).
- Alpha metric:
  - `Shannon`: entropy-style diversity combining richness and evenness.
  - `Simpson`: dominance-sensitive diversity (higher values indicate more even communities, depending on representation).
  - `Observed`: count of observed taxa/features.
- Variable:
  - grouping metadata column shown on the x-axis.
- Subset column:
  - metadata column used to restrict the dataset before plotting.
- Subset elements:
  - selected values from the subset column to keep.
- Download:
  - exports a PDF of the alpha plot.

Displayed statistics:

- pairwise significance annotations are shown across groups (with `NS`, `*`, `**`, `***` labels based on p-value thresholds used in code).

#### C. Beta Diversity Graph

Graph purpose:

- ordination of between-sample differences (PCoA on Bray-Curtis distances).

Options:

- Domain:
  - selects domain-specific decontaminated phyloseq object.
- Variable:
  - metadata column used to color/group samples.
- Subset column:
  - metadata field used to pre-filter samples.
- Subset elements:
  - retained levels in the subset column.
- Download:
  - exports the beta diversity figure as PDF.

Displayed statistics:

- PERMANOVA p-value is printed on the plot to summarize association between community composition and selected grouping variable.

#### D. Stacked Bar Graph

Graph purpose:

- relative abundance composition plot of top taxa (top 10 plus `Other`).

Options:

- Domain:
  - target biological domain (`Bacteria`, `Archaea`, `Eukaryota`).
- Level:
  - taxonomic aggregation level (for example `Genus`, `Species`; `Strain` available for MetaPhlAn workflows).
- Grouping variable:
  - metadata field used for faceting/clustering displayed bars.
- Show BIN (MetaPhlAn mode):
  - when enabled, keeps BIN-like strain labels (including less-resolved entries such as `GB` groups).
  - when disabled, excludes those entries for cleaner interpretation.
- Subset column:
  - metadata field used to filter samples before plotting.
- Subset elements:
  - selected values retained from subset column.
- Download:
  - exports stacked barplot as PDF.

#### E. Heatmap Graph

Graph purpose:

- abundance heatmap of taxa retained in DAS/SOI result object, with sample and feature annotations.

Options:

- DAS file:
  - input RDS file used as heatmap data source.
- Metadata annotation variables:
  - selected metadata columns shown as annotation tracks.
- Create heatmap:
  - renders the heatmap in-app.
- Download:
  - exports the rendered heatmap.

#### F. HUMAnN Pathway Graph (Humann Graphs page)

Graph purpose:

- pathway-level barplot generation using HUMAnN plotting commands executed in Docker.

Required setup options:

- pathway selector:
  - target pathway ID/name to visualize.
- Main element (focal metadata):
  - metadata variable defining the primary grouping in the barplot.
- Columns to use:
  - metadata columns included in processed graph input.

Graph customization options:

- remove zeroes (`--remove-zeros`):
  - omits zero-valued entries to simplify plots and reduce clutter.
- view at genera level (`--as-genera`):
  - aggregates stratified pathway contributions at the genus level.
- sorting:
  - `None`: keep default order.
  - `sum` (`--sort sum`): sort by total abundance.
  - `braycurtis` (`--sort braycurtis`): sort by Bray-Curtis similarity.
  - `dominant` (`--sort dominant`): sort by dominant contributors.
  - `braycurtis_w` (`--sort braycurtis_w`): weighted Bray-Curtis sorting.
- scaling:
  - `None`: no additional scaling.
  - `logstack` (`--scaling logstack`): logarithmic stacking for improved visibility of small components.
  - `original` (`--scaling original`): preserve original scale.
  - `totalsum` (`--scaling totalsum`): normalize bars to total sum.

Output:

- pathway graphs are written as PNG files in the HUMAnN graph output folder under the selected project output directory.

## 8. Notes / Limitations

- Linux/macOS environments are recommended for full feature reliability.
- Docker is mandatory for LEfSe and HUMAnN modules.
- Sample IDs must be consistent across metadata and abundance inputs.
- Metadata become effectively immutable in-session after the initial `Load` step.
- Input type detection for taxonomic data is extension-based (`.biom` vs `.txt`).
- Several workflows rely on file/folder naming conventions and generated intermediate directories.
