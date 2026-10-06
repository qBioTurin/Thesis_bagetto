###First page(metadata upload)
Manually_adj<-"If colnames shown as v1, v2 ecc enter here and select header switch"
Separator_adj<-"Select the separator used in your file"
Quote_adj<-"Select the quote used in your file"
Type<-"Select the type of your column id default not correct, if you want to switch from factor no ord to ord please select another type the swith again to factor"
####Second page(data upload)
Select_data_type<-"Upload a biom or a txt file containing the results of the chosen pipeline."
Select_id_column<-"Select the column containing the ID of your samples"
Remove_data_name<-"Remove the name of your data file from the sample IDs ususally *bracken for kraken and *output for metaphlan"
Output_directory_set<-"Set the output directory where all the results will be saved"
####Third page(unsup_decontam)
normalize_data<-"Select if you want to normalize your data before decontamination(using Deseq2 normalization)"
decontamination_level<-"Select the taxonomic level at which you want to perform the decontamination"
design_element<-"Select the variable in your metadata that you want to use for the normalization"
####Fourth page(Supervised Decontamination)
decontamination_level<-"Select the minimum abundance level in order to classify a taxa as non contaminant"
manual_ceck<-"Check the taxa that are NOT contaminants according to your knowledge"
####Fifth page(Alpha graphs)
alpha_metric<-"Alpha diversity: Measures the variety of species within a single sample or community. It quantifies how many different microbial species are present and their relative abundance. Common metrics include Shannon index, Simpson index, and observed species count."
Shannon_desc <- "Shannon: Measures both richness and evenness of species in a sample. Higher values indicate more diversity."
Simpson_desc <- "Simpson: Reflects the probability that two individuals randomly selected from a sample belong to the same species. Lower values indicate higher diversity."
Observed_desc <- "Observed: Counts the number of unique species (or features) observed in the sample."
Beta_desc <- "Beta diversity: Measures the differences in species composition between samples. It helps to assess how similar or different communities are across samples."

Subset_column<-"Select the variable in your metadata that you want to use to subset your data for the analysis"
Subset_elements<-"Select the elements of the variable in your metadata that you want to use to subset your data for the analysis"

Select_levels<-"Select the taxonomic levels you want to include in the stackbar plot"
Select_grouping_variable<-"Select the variable in your metadata that you want to use to group your data in the stackbar plot"

####Sixth page(DAS/SOI)
Maaslin3_desc <- "Maaslin3: A tool for identifying associations between microbial taxa and clinical/environmental variables using mixed-effects models."
AncomBC2_desc <- "ANCOM-BC2: Analysis of Composition of Microbiomes with Bias Correction 2. Tests for differentially abundant taxa while accounting for compositional bias."
Lefse_desc <- "LEfSe (Linear Discriminant Analysis Effect Size): Identifies taxa that are statistically different between classes and estimates their effect size."
Limma_desc <- "Limma: A statistical method for identifying differentially abundant features using linear models and empirical Bayes moderation."
P_value_desc <- "P-value: The probability of observing the data given the null hypothesis. Lower values indicate stronger evidence of association."
Q_value_desc <- "Q-value: Adjusted p-value that controls the false discovery rate (FDR) across multiple tests. Use this for multiple comparisons."
Main_discriminant_desc <- "Main discriminant: The taxa with the highest effect size or statistical significance that best distinguish between groups."
Discriminant_desc <- "Discriminant: Taxa that are significantly different between groups and can be used to distinguish between them."

