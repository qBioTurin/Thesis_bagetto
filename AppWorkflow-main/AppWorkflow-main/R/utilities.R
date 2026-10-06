createFolder <- function(folder_path) {
  # Controlla se la cartella esiste
  if (!dir.exists(folder_path)) {
    # Crea la cartella
    dir.create(folder_path, recursive = TRUE)
    message("Folder created: ", folder_path)
  } else {
    message("The folder already exists: ", folder_path)
  }
}

ensure_docker_container <- function(image_name) {
  if (system2("docker", "--version", stdout = FALSE, stderr = FALSE) != 0) {
    stop("Docker non è installato o non funziona.")
  }

  existing_image <- system2(
    "docker",
    c("images", "-q", image_name),
    stdout = TRUE,
    stderr = TRUE
  )

  if (length(existing_image) > 0 && nzchar(existing_image)) {
    message("Immagine Docker trovata: ", image_name)
    return(invisible(TRUE))
  }

  stop("L'immagine Docker NON è presente localmente: ", image_name)
}

extrapolateSpecies<-function(path,outputFolder){
  rds1 <- readRDS(path)
  rds1tax <- tax_table(rds1)
  species_list <- rds1tax[, "Species"]
  write.table(species_list, "species_list.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.csv(species_list, gsub(" ","",paste(outputFolder,"species_list.csv")), row.names = FALSE, quote = FALSE)
}
# Increase the file upload size limit
# Manual selection Bacteria
##################
# manual selection using Encyclopedia of Life (https://eol.org/), Taxonomuy Browser and Pubmed (Phylum name AND human)
## Phyla associated with human:
# Actinomycetota/actinobacteria
# Bacillota/Firmicutes
# Bacteroidota/Bacteroidetes
# Campylobacterota
# Chlamydiota
# Chloroflexota
# Lentisphaerota
# Mycoplasmatota/Mycoplasma/Tenericutes
# Pseudomonadota
# Spirochaetota
# Synergistota
# Verrucomicrobiota

## Phyla associated with human in only few ref: 
# Acidobacteriota
# Candidatus Saccharibacteria
# Elusimicriobiota
# Gemmatimonadota
# Ignavibacteriota
# Myxococcota
# Nitrospirota
# Planctomycetota
# Thermodesulfobacteriota

levelsB=c(  "Bifidobacterium adolescentis  ","Collinsella aerofaciens  ","Segatella copri  "
,"Phocaeicola dorei  ","Bifidobacterium longum  ","Faecalibacterium prausnitzii  ",
"Agathobacter rectalis  " ,"Bacteroides uniformis  ","Phocaeicola vulgatus  ", 
"Blautia wexlerae  ","Other" )
levelsA=c("Methanomicrobium antiquum  ", "Methanosarcina barkeri  ", 
"Methanococcus maripaludis  ", "Methanobrevibacter smithii  ", 
"Methanosphaera sp. BMS  ", "Methanimicrococcus sp. Hf6  ", "Methanobrevibacter sp. TLL-48-HuF1  ", 
"Methanolobus uncultured Methanolobus sp.  ", "Methanoregula uncultured Methanoregula sp.  ", 
"Haloferax volcanii  ","Other" )
levelsE=c( "Candida albicans  ", "Saccharomyces cerevisiae  ", "Aspergillus fumigatus  ", 
"Torulaspora globosa  ", "Malassezia japonica  ", "Saccharomyces mikatae  ", 
"Aspergillus nidulans  ", "Penicillium oxalicum  ", 
"Schizosaccharomyces pombe  ", "Aspergillus puulaauensis  ", "Other")


####List of non contaminant

human_phylaB = c("Actinomycetota", "Bacillota", "Bacteroidota", "Campylobacterota", 
                "Chlamydiota", "Chloroflexota", "Lentisphaerota", "Mycoplasmatota", 
                "Pseudomonadota", "Spirochaetota", "Synergistota", "Verrucomicrobiota", 
                "Acidobacteriota", "Candidatus Saccharibacteria", "Elusimicriobiota", 
                "Gemmatimonadota", "Ignavibacteriota", "Myxococcota", "Nitrospirota",
                "Planctomycetota", "Thermodesulfobacteriota")

human_phylaA = c("Euryarchaeota", "Thermoproteota", "Candidatus Korarchaeota", "Nanoarchaeota")

human_genus= c(  "Acremonium",	"Agaricales",	"Agaricomycetes",	"Aspergillus",
                "Aureobasidium",	"Byssochlamys",	"Candida", "Cladosporium",
                "Claroideoglomus",	"Clavispora", "Cyberlindnera", "Debaryomyces",
                "Geotrichum", "Hanseniaspora", "Hypocreales",	"Itersonilia", "Kazachstania",
                "Kazchstania",	"Kluyveromyces",	"Kurtzmaniella",	"Lachancea",	"Lecanoromycetes",
                "Malassezia",	"Malasseziales", "Metschnikowia", "Ogataea", "Penicillium",	"Pichia",
                "Polyporales",	"Rhizoplaca",	"Rhodotorula",	"Saccharomycetales",	"Schizosaccharomyces",
                "Starmerella", "Torulaspora",	"Trametes",	"Vishniacozyma",	"Wickerhamomyces",
                "Yamadazyma", "Yarrowia", "Saccharomyces")

# ===== TOOLTIP HELPER FUNCTIONS =====
# Funzione generica helper per aggiungere tooltip a qualsiasi elemento Shiny
# Prende la descrizione da DescriptionList.R
# Usa tags$span (inline) per non interferire con il layout dei bottoni
withTooltip <- function(element, descriptionVar, placement = "right") {
  tags$span(
    element,
    title = descriptionVar,
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = placement
  )
}





