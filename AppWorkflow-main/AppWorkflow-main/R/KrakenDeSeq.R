#' @title Differential Abundance Analysis with DESeq2 on Decontaminated Phyloseq Data
#' 
#' @description
#' Performs DESeq2 differential abundance analysis on a combined phyloseq object,
#' normalizes counts, subsets by kingdoms (Bacteria, Archaea, Eukaryota),
#' and returns unique taxa at specified taxonomic levels for each kingdom.
#' 
#' @param all_deco Character. Path to the input `.rds` file containing the combined phyloseq object.
#' @param output_folder Character. Path to the folder where DESeq2 processed `.rds` files will be saved.
#' @param levelBact Character. Taxonomic level for bacteria (e.g., "Phylum", "Genus") to extract unique taxa.
#' @param levelArchaea Character. Taxonomic level for archaea (e.g., "Phylum", "Genus") to extract unique taxa.
#' @param levelEuk Character. Taxonomic level for eukaryotes (e.g., "Phylum", "Genus") to extract unique taxa.
#' @param select_design Character. The column name in sample metadata to use as the design formula for DESeq2 (e.g., "treatment").
#' 
#' @return A named list containing vectors of unique taxa names for bacteria, archaea, and eukaryota at the specified levels.
#' 
#' @details
#' The function:
#' \itemize{
#'   \item Reads the combined phyloseq object.
#'   \item Converts the OTU table to counts suitable for DESeq2.
#'   \item Runs DESeq2 differential abundance analysis with Wald test and parametric fit.
#'   \item Normalizes counts and reconstructs a phyloseq object with normalized data.
#'   \item Saves kingdom-specific phyloseq objects (Bacteria, Archaea, Eukaryota) as `.rds` files.
#'   \item Extracts unique taxa at the requested taxonomic levels from each kingdom.
#' }
#' 
#' @import phyloseq DESeq2 dplyr
#' 
#' @export


KrakenDeSeq<- function(all_deco,output_folder,levelBact=NULL,levelArchaea=NULL,levelEuk=NULL,select_design=NULL,do_normalization){
    phylum_list <- function(phy,level){
    if(level=="Phylum"){
        phylum_list <- phy@tax_table %>%
        as.data.frame() %>%
        pull(Phylum) %>%
        unique()
    }   else if(level=="Genus"){
        phylum_list <- phy@tax_table %>%
        as.data.frame() %>%
        pull(Genus) %>%
        unique()
    }
        return(phylum_list)
    }
    #all_deco="Output/RDS/ALL_baseline_phylo.rds"
    ALL_baselines = readRDS(all_deco)

    # Convert otu table to dataframe
    #prova_df = as.data.frame(prova@otu_table)
    ALL_baselines_df = as.data.frame(ALL_baselines@otu_table)

    if (do_normalization==TRUE){
        coldata_filt = as.data.frame(ALL_baselines@sam_data)
        design_formula <- as.formula(paste("~", select_design))
        # Convert into a DESeq Dataset
        ALL_baselines_df <-DESeqDataSetFromMatrix(
            countData = round(ALL_baselines_df),
            colData = coldata_filt,
            design = design_formula
        )
        # Normalize ALL_baselines_DDS
        ALL_baselines_DDS = DESeq(ALL_baselines_df, test="Wald", fitType="parametric")
        normalized_counts <- counts(ALL_baselines_DDS, normalized = TRUE)
    } else {
        # Usa direttamente la matrice delle conte
        normalized_counts <- as.matrix(ALL_baselines@otu_table)
    }
    # Round to integer the results and create OTU
    OTU_ds <- round(otu_table(normalized_counts, taxa_are_rows = TRUE))

    # Create new phyloseq
    #####################
    ALL_baselines_ds = phyloseq(OTU_ds, ALL_baselines@tax_table, ALL_baselines@sam_data)

    # Save normalized data for ALL kingdoms phyloseq
    ################################################
    saveRDS(ALL_baselines_ds, file = gsub(" ","",paste(output_folder,"ALL_DeSeq.rds"))) 


    # Subset by Kingdom
    ###################
    if ("Bacteria" %in% ALL_baselines@tax_table[, "Kingdom"]) {
        bact_baselines_ds = subset_taxa(ALL_baselines_ds, Kingdom == "Bacteria")
        bact_level <- phylum_list(bact_baselines_ds,levelBact)
        saveRDS(bact_baselines_ds, file = gsub(" ","",paste(output_folder,"Bact_DeSeq.rds")))
    }else {
        bact_level <- NULL
    }
    # Subset Archaea
    if("Archaea" %in% ALL_baselines@tax_table[, "Kingdom"]) {
        archaea_baselines_ds = subset_taxa(ALL_baselines_ds, Kingdom == "Archaea")
        arch_level <- phylum_list(archaea_baselines_ds,levelArchaea)
        saveRDS(archaea_baselines_ds, file = gsub(" ","",paste(output_folder,"Archaea_DeSeq.rds"))) 
    } else {
        arch_level <- NULL
    }

    # Subset Eukaryotes
    if("Eukaryota" %in% ALL_baselines@tax_table[, "Kingdom"]) {
        euk_baselines_ds = subset_taxa(ALL_baselines_ds, Kingdom == "Eukaryota")
        euk_level <- phylum_list(euk_baselines_ds,levelEuk)
        saveRDS(euk_baselines_ds, file = gsub(" ","",paste(output_folder,"Eukaryota_DeSeq.rds")))
    } else {
        euk_level <- NULL
    }
   return(list(
    bacteria = bact_level, 
    archaea = arch_level, 
    eukaryota = euk_level
  ))
}

