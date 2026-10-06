# # ## RDS, formula, subset,subset_el

#  rds_path <- "/home/franky/Desktop/PRJNA/SUPERVISED_DEC/Bacteria_Supervised_decontam.rds"
#  metadata <- read.csv("/home/franky/Downloads/PRJNA/metadata4app.csv")
#  categorySel <- "sex"
#  elements <- c("Female", "Male")
#  main_el <- "diagnosis"
# discr_els <- NULL
# k_value <- 18
# rank <- "Genus"
# aggingere taxrank
topic_modeling_p1 <- function(rds_path, metadata, main_el, discr_els=NULL, categorySel, elements, rank) {
  create_formula <- function(main_el, discr_els) {
    if (!is.null(discr_els) && length(discr_els) > 0) {
      formulaInUse <- paste("~", main_el, "+", paste(discr_els, collapse = "+"))
    } else {
      formulaInUse <- paste("~", main_el)
    }
    return(formulaInUse)
  }
  formulaInUse <- create_formula(main_el, discr_els)
  ps <- readRDS(rds_path) #### usa un RDS
  #metadata <- read.csv(metadata, header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
  # keep only metadata rows whose categorySel value is in 'elements'
  meta <- metadata[as.character(metadata[[categorySel]]) %in% as.character(elements), , drop = FALSE]
  # keep only samples whose categorySel value is in 'elements'
  ps <- prune_samples(as.character(sample_data(ps)[[categorySel]]) %in% as.character(elements), ps)

  (ps_g <- tax_glom(ps, taxrank = rank))

  taxa_names(ps_g) <- tax_table(ps_g)[, 6]
  # head(taxa_names(ps_g))

  count_matrix <- data.frame(t(data.frame(otu_table(ps_g)))) ### conte, giusto??

  result <- ldatuning::FindTopicsNumber(
    count_matrix,
    topics = seq(from = 3, to = 50, by = 3),
    metrics = c("CaoJuan2009", "Deveaud2014"),
    method = "VEM",
    control = list(seed = 243),
    mc.cores = 12,
    verbose = TRUE
  )


  my_plot <- function(values) {
    if ("LDA_model" %in% names(values)) {
      values <- values[!names(values) %in% c("LDA_model")]
    }

    columns <- base::subset(values, select = 2:ncol(values))
    values <- base::data.frame(
      values["topics"],
      base::apply(columns, 2, function(column) {
        scales::rescale(column, to = c(0, 1), from = range(column))
      })
    )

    values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
    values$group <- values$variable %in% c("Griffiths2004", "Deveaud2014")
    values$group <- base::factor(values$group, levels = c(FALSE, TRUE), labels = c("Minimize", "Maximize"))

    library(ggplot2)
    library(rlang)

    p <- ggplot(values, aes(x = .data[["topics"]], y = .data[["value"]], group = .data[["variable"]])) +
      geom_line() +
      geom_point(aes(shape = .data[["variable"]]), size = 3) +
      guides(size = "none", shape = guide_legend(title = "Metrics:")) +
      scale_x_continuous(breaks = values$topics) +
      labs(x = "\nNumber of Topics", y = NULL) +
      facet_grid(group ~ .) +
      theme_bw() %+replace% theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey70"),
        panel.grid.minor.x = element_blank(),
        legend.key = element_blank(),
        strip.text.y = element_text(angle = 90),
        legend.position = "bottom"
      )

    return(p)
  }
  p1 <- my_plot(result)
  #ggsave("/home/franky/Downloads/send_To_dora/kraken/topic_number_selection_161125.png", plot = p1, width = 6, height = 8, dpi = 300)
  return(list(ps_g = ps_g, count_matrix = count_matrix, p1 = p1, main_el = main_el, formulaInUse = formulaInUse))
}
k_value <- 18

topic_modeling_p2 <- function(ps_g, k_value, count_matrix, main_el, meta, formulaInUse) {
  cat("[DEBUG] Checking count_matrix before LDA...\n")

  # Converti in matrice numerica vera
  count_matrix <- as(otu_table(ps_g), "matrix")
  count_matrix <- t(count_matrix) # righe = documenti, colonne = taxa
  count_matrix <- as.data.frame(count_matrix)

  # Assicurati che tutto sia numerico
  count_matrix[] <- lapply(count_matrix, function(x) as.numeric(as.character(x)))

  # Sostituisci NAs con 0
  count_matrix[is.na(count_matrix)] <- 0

  # Filtra righe/colonne vuote
  count_matrix <- count_matrix[rowSums(count_matrix) > 0, , drop = FALSE]
  count_matrix <- count_matrix[, colSums(count_matrix) > 0, drop = FALSE]

  cat("[DEBUG] count_matrix cleaned. Dim:", paste(dim(count_matrix), collapse = "x"), "\n")

  # Conversione in matrice
  count_matrix <- as.matrix(count_matrix)

  # Verifica finale
  if (anyNA(count_matrix)) {
    stop("[FATAL] Still contains NAs after cleaning — check OTU table source!")
  }


  lda_k36 <- LDA(count_matrix, k = k_value, method = "VEM", control = list(seed = 243))


  b_df <- data.frame(tidy(lda_k36, matrix = "beta"))

  g_df <- data.frame(tidy(lda_k36, matrix = "gamma")) %>%
    arrange(document, topic)

  head(b_df)
  head(g_df)

  lib_size_df <- phyloseq::sample_sums(ps_g) %>%
    tibble::enframe(name = "document", value = "read_count")


  # ensure g_df has the needed columns
  stopifnot(all(c("document", "topic", "gamma") %in% colnames(g_df)))

  tm_df <- lib_size_df %>%
    left_join(g_df, by = "document") %>%
    mutate(topic_count = round(read_count * gamma, 0)) %>%
    select(document, topic, topic_count) %>%
    pivot_wider(
      names_from = topic,
      values_from = topic_count,
      values_fill = 0
    ) %>%
    # prefix topic columns; keep 'document' untouched
    rename_with(.cols = -document, .fn = ~ paste0("Topic_", .)) %>%
    # explicitly use tibble version to avoid masking issues
    tibble::column_to_rownames(var = "document") %>%
    t() %>%
    as.data.frame()

  (ps_topic_g <- phyloseq(
    sample_data(ps_g),
    otu_table(tm_df, taxa_are_rows = TRUE)
  ))

  #meta = data.frame(sample_data(ps_topic_g))

  formula <- paste0("~ ", main_el)
  linda <- LinDA::linda(
    otu.tab = data.frame(otu_table(ps_topic_g)), meta = meta,
    formula = formula, alpha = 0.05, winsor.quan = 0.97,
    prev.cut = 0.3, lib.cut = 0, n.cores = 1
  )
  main_el <- names(linda$output)[1]
  linda_res_df <- data.frame(linda$output[[main_el]]) %>%##risolvere
    dplyr::arrange(padj) %>%
    rownames_to_column(var = "Topic")

  fdr_linda <- linda_res_df %>%
    mutate(reject = ifelse(padj < 0.05, "Yes", "No")) %>%
    separate(Topic, into = c("t", "t_n"), remove = FALSE, convert = TRUE) %>%
    mutate(Topic = gsub("_", " ", Topic)) %>%
    arrange(desc(t_n)) %>%
    mutate(Topic = factor(Topic, levels = Topic)) # MS: no significative padj (not even close); signif in healthy!!

  (p2 <- ggplot(data = fdr_linda, aes(x = Topic, y = log2FoldChange, fill = reject)) +
    geom_col(alpha = .8) +
    labs(y = "\nLog2 Fold-Change", x = "") +
    theme(
      axis.text.x = element_text(color = "black", size = 8),
      axis.text.y = element_text(color = "black", size = 8),
      axis.title.y = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8)
    ) +
    coord_flip() +
    scale_fill_manual(values = c("grey", "dodgerblue")) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme(legend.position = "none"))

  # check topic 1
  # Selecting topic four as an example, we can use the information in the per-topic-per-word probabilities matrix to examine which genera have the highest probabilities of assignment to this topic/community type.
  ggsave("/home/franky/Downloads/send_To_dora/kraken/log2_fold_change_topics161125.png", plot = p2, width = 6, height = 12, dpi = 300)
  return(list(p2 = p2, b_df = b_df, ps_g = ps_g, formulaInUse = formulaInUse))
}

topic_number <- 6
topic_modeling_p3 <- function(b_df, ps_g, topic_number, formulaInUse) {
  b_plot <- b_df %>%
    dplyr::filter(topic == topic_number) %>% ### choose topic
    arrange(desc(beta)) %>%
    slice_head(n = 20) %>%
    arrange(desc(term)) %>%
    mutate(term = factor(term, levels = term))

  (p3 <- ggplot(data = b_plot, aes(x = beta, y = term, color = term)) +
    geom_point(aes(size = beta)) +
    labs(x = paste0("\nTopic ", topic_number, ": Genus-Topic Probabilities (beta)"), y = "") +
    theme(legend.position = "none"))

  # Selecting topic five as an example, we can use the information in the per-topic-per-word probabilities matrix to examine which genera have the highest probabilities of assignment to this topic/community type.
  # Below we can see that Segatella has the highest probability of assignment followed by several others with lower values.
  linda_g_all <- LinDA::linda(
    otu.tab = data.frame(otu_table(ps_g)), meta = data.frame(sample_data(ps_g)),
    formula = formulaInUse, alpha = 0.05, winsor.quan = 0.97,
    prev.cut = 0, lib.cut = 0, n.cores = 1
  )
  main_el <- names(linda_g_all$output)[1]
  linda_res_g_df <- data.frame(linda_g_all$output[[main_el]]) %>%
    dplyr::arrange(padj) %>%
    rownames_to_column(var = "Genus")

  keep_g <- b_plot$term

  log2_df <- linda_res_g_df %>%
    filter(Genus %in% keep_g) %>%
    arrange(desc(Genus)) %>%
    mutate(Genus = factor(Genus, levels = Genus))

  (p4 <- ggplot(data = log2_df, aes(x = Genus, y = log2FoldChange, fill = Genus)) +
    geom_col(alpha = .8) +
    labs(y = "\nLog2 Fold-Change", x = "") +
    theme(
      axis.text.x = element_text(color = "black", size = 8),
      axis.text.y = element_text(color = "black", size = 8),
      axis.title.y = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8)
    ) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme(legend.position = "none"))
  return(list(p3 = p3, p4 = p4))
  ggsave("/home/franky/Downloads/send_To_dora/kraken/topic_6_genus_probabilities_log2fc161125.png", plot = p4, width = 6, height = 8, dpi = 300)
  ggsave("/home/franky/Downloads/send_To_dora/kraken/topic_6_genus_probabilities161125.png", plot = p3, width = 6, height = 8, dpi = 300)
}

# transversal <- topic_modeling_p1(rds_path, metadata, main_el, discr_els, categorySel, elements, rank)

#  transversal$p1
# graphs=topic_modeling_p2(ps_g = transversal$ps_g,k_value = 48, 
#   count_matrix = transversal$count_matrix, main_el = transversal$main_el)

#  graphs$p2
#  graphs2=topic_modeling_p3(b_df = graphs$b_df, ps_g = graphs$ps_g, 
#   topic_number=46, formulaInUse=transversal$formulaInUse)
#  graphs2$p3
#  graphs2$p4














# ps = readRDS(rds_path)
# ps= subset_samples(ps, gc_treatment == "positive")
# (ps_g <- tax_glom(ps, taxrank = "Genus"))

# taxa_names(ps_g) <- tax_table(ps_g)[, 6]
# head(taxa_names(ps_g))

# count_matrix <- data.frame(t(data.frame(otu_table(ps_g)))) ###conte, giusto??

# result <- ldatuning::FindTopicsNumber(
#   count_matrix,
#   topics = seq(from = 3, to = 50, by = 3),
#   metrics = c("CaoJuan2009", "Arun2010"),
#   method = "VEM",
#   control = list(seed = 243),
#   mc.cores = 4,
#   verbose = TRUE
# )

# (p1 <- my_plot(result))
