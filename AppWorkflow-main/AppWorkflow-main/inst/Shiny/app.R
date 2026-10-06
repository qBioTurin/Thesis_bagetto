root_candidates <- unique(c(
	getwd(),
	normalizePath(file.path(getwd(), ".."), mustWork = FALSE),
	normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE),
	"/opt/app"
))

project_root <- root_candidates[
	vapply(
		root_candidates,
		function(p) file.exists(file.path(p, "R", "utilities.R")),
		logical(1)
	)
][1]

if (is.na(project_root) || !nzchar(project_root)) {
	stop("Unable to locate project root containing R/utilities.R")
}

setwd(normalizePath(project_root, mustWork = TRUE))

##packages and functions
source("R/utilities.R")
source("R/Packages.R")

# Load .env file when running locally (outside Docker)
env_file <- file.path(project_root, ".env")
if (file.exists(env_file) && requireNamespace("dotenv", quietly = TRUE)) {
	dotenv::load_dot_env(file = env_file)
}

source("R/function.R")
source("R/DescriptionList.R")
source("info/CAT.R")
#######
##pre-post
source("R/Univar_functs.R")
source("R/Bivar_functs.R")
##Graphics
source("Palettes.R")
source("Colorpalette.R")
#######
##pre-kraken
source("R/ProcessData.R")
source("R/KrakenDataImport.R")
source("R/KrakenDeSeq.R")
#######
##pre-Meta
source("R/MetaphlanDataImportDecontam.R")
source("R/MetaSearchTaxid.R")
source("R/MetaRdsCreator.R")
source("R/removeStrain.R")
#######
##Supervised Decontamination
source("R/SupervisedDecontam.R")
#######
##graphs
source("R/GraphAlpha.R")
source("R/GraphBeta.R")
source("R/GraphStackedBar.R")
source("R/GraphHeatmap.R")
########
##DAS
source("R/DasMerge.R")
source("R/DASMaaslin3.R")
########
##LEAPS
source("R/LEAPS.R")
source("R/FunctionLRM.R")
##Topic Models
source("R/Topic_modeling.R")
########
###humann
source("R/humann3Norm.R")
source("R/PathwaySearcher.R")
source("R/HumannGraphs.R")
source("ShinyMirriTemplate/ThemeShiny_QBio.R")
source("info/CAT.R")
source("R/UI.R")
source("R/Server.R")





app_dir <- normalizePath(getwd(), mustWork = FALSE)
gif_candidates <- unique(c(
	file.path(app_dir, "gif"),
	file.path(app_dir, "..", "gif"),
	system.file("gif", package = "MicroWorkflow")
))
gif_dir <- gif_candidates[nzchar(gif_candidates) & dir.exists(gif_candidates)][1]
if (!is.na(gif_dir) && nzchar(gif_dir)) {
	shiny::addResourcePath("gif", normalizePath(gif_dir, mustWork = TRUE))
}

image_candidates <- unique(c(
	file.path(app_dir, "image"),
	file.path(app_dir, "..", "image"),
	system.file("image", package = "MicroWorkflow")
))
image_dir <- image_candidates[nzchar(image_candidates) & dir.exists(image_candidates)][1]
if (!is.na(image_dir) && nzchar(image_dir)) {
	shiny::addResourcePath("image", normalizePath(image_dir, mustWork = TRUE))
}

options(shiny.maxRequestSize = 100 * 1024^2)

app <- shinyApp(ui = ui, server = server)
runApp(app, port = 3838, host = "0.0.0.0")
