#' @title Boxplot of Alpha Diversity by Metadata Group
#'
#' @description
#' Creates a `ggplot2` boxplot of a selected alpha diversity index grouped by a categorical metadata variable.
#' If the grouping variable has exactly two levels, a significance annotation is added using `geom_signif`.
#'
#' @param df A data frame containing alpha diversity values and grouping metadata.
#' @param alpha A string. The name of the column in `df` containing the alpha diversity index to be plotted.
#' @param row_selected A string. The name of the grouping variable in `df` to use on the x-axis.
#'
#' @return A `ggplot` object representing the boxplot of alpha diversity grouped by the selected variable.
#'
#' @details
#' If `row_selected` has exactly two unique values, normality is evaluated in each group using
#' the Shapiro-Wilk test. If both groups are normally distributed (p > 0.05), a t-test is used;
#' otherwise a Wilcoxon test is used. The significance comparison is displayed with `geom_signif`.
#' Significance labels are:
#' \itemize{
#'   \item \code{p < 0.049}: "* (p)"
#'   \item \code{0.049 ≤ p < 0.055}: "NS. (p)"
#'   \item \code{p ≥ 0.055}: "NS."
#' }
#'
#' The plot uses custom fill colors for two-group comparisons and a cleaner classic theme.
#'
#' @import ggplot2
#' @importFrom ggsignif geom_signif
#'
#' @examples
#' \dontrun{
#' GraphAlpha(df = alpha_div_df, alpha = "Shannon", row_selected = "TreatmentGroup")
#' }
#'
#' @export

GraphAlpha <- function(df,df_alpha, alpha, row_selected, col_sub, sub_name) {
  normalize_metadata_names <- function(x) {
    names(x) <- tolower(trimws(names(x)))
    x
  }

  align_metadata_classes <- function(source_df, target_df) {
    common_cols <- intersect(names(source_df), names(target_df))

    for (col_name in common_cols) {
      source_col <- source_df[[col_name]]
      target_col <- target_df[[col_name]]

      if (is.factor(source_col)) {
        target_df[[col_name]] <- factor(
          target_col,
          levels = levels(source_col),
          ordered = is.ordered(source_col)
        )
      } else if (inherits(source_col, "Date")) {
        target_df[[col_name]] <- as.Date(target_col)
      } else if (is.numeric(source_col)) {
        target_df[[col_name]] <- suppressWarnings(as.numeric(target_col))
      } else if (is.logical(source_col)) {
        target_df[[col_name]] <- as.logical(target_col)
      } else if (is.character(source_col)) {
        target_df[[col_name]] <- as.character(target_col)
      }
    }

    target_df
  }

  df <- normalize_metadata_names(as.data.frame(df))
  df_alpha <- normalize_metadata_names(as.data.frame(df_alpha))
  alpha <- tolower(trimws(alpha))
  row_selected <- tolower(trimws(row_selected))
  col_sub <- tolower(trimws(col_sub))

  df_alpha <- align_metadata_classes(df, df_alpha)

  # Genera comparazioni secondo ordine "vicini + salti"
  generate_comparisons <- function(groups) {
    n <- length(groups)
    comps <- list()
    for (gap in 1:(n - 1)) { # gap = distanza tra gli indici
      for (i in 1:(n - gap)) {
        comps[[length(comps) + 1]] <- c(groups[i], groups[i + gap])
      }
    }
    return(comps)
  }

  df_alpha_filtered <- df_alpha[df_alpha[[col_sub]] %in% sub_name, ]
  # Filtro NA subito dopo il subset
  df_alpha_filtered <- dplyr::filter(
    df_alpha_filtered,
    !is.na(.data[[alpha]]),
    !is.na(.data[[row_selected]])
  )
  # Ordine gruppi: rispetto ai livelli del factor
  if (is.factor(df_alpha_filtered[[row_selected]])) {
    groups <- levels(droplevels(df_alpha_filtered[[row_selected]]))
  } else {
    groups <- unique(as.character(df_alpha_filtered[[row_selected]]))
  }
  n_groups <- length(groups)
  signif_test <- "wilcox.test"
  signif_test_args <- list(exact = FALSE)
  test_label <- NULL
  test_p_value <- NA_real_

  # If exactly two groups are present, choose test based on per-group normality.
  if (n_groups == 2) {
    valid_subset <- df_alpha_filtered[!is.na(df_alpha_filtered[[alpha]]) & !is.na(df_alpha_filtered[[row_selected]]), ]
    group_values <- split(valid_subset[[alpha]], valid_subset[[row_selected]])
    normality_p <- sapply(group_values, function(x) {
      x <- x[is.finite(x)]
      # Più conservativo: almeno 5 valori
      if (length(x) < 5 || length(unique(x)) < 3) {
        return(NA_real_)
      }
      stats::shapiro.test(x)$p.value
    })

    if (!anyNA(normality_p) && all(normality_p > 0.05)) {
      signif_test <- "t.test"
      signif_test_args <- list()
    }

    g1 <- group_values[[1]]
    g2 <- group_values[[2]]
    g1 <- g1[is.finite(g1)]
    g2 <- g2[is.finite(g2)]

    if (signif_test == "t.test") {
      test_label <- "Test: t-test"
      test_p_value <- tryCatch(stats::t.test(g1, g2)$p.value, error = function(e) NA_real_)
    } else {
      test_label <- "Test: Wilcoxon"
      test_p_value <- tryCatch(stats::wilcox.test(g1, g2, exact = FALSE)$p.value, error = function(e) NA_real_)
    }

  }

  # Palette robusta
  colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_groups)
  p <- ggplot(df_alpha_filtered, aes(x = .data[[row_selected]], y = .data[[alpha]])) +
    geom_boxplot(aes(fill = .data[[row_selected]]), na.rm = TRUE) +
    labs(x = row_selected, y = paste("Diversity Index:", alpha), fill = "") +
    theme_classic() +
    scale_x_discrete() +
    scale_fill_manual(values = colors) +
    theme(
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      strip.text = element_text(size = 20),
      legend.position = "none"
    )

  # Aggiungi geom_signif solo se ci sono almeno 2 gruppi
  if (n_groups >= 2) {
    comparisons <- generate_comparisons(groups)
    y_max <- max(df_alpha_filtered[[alpha]], na.rm = TRUE)
    y_range <- diff(range(df_alpha_filtered[[alpha]], na.rm = TRUE))
    if (y_range == 0) {
      y_range <- abs(y_max) * 0.1 + 1
    }
    offset <- 0.1 * y_range
    y_positions <- y_max + seq_along(comparisons) * offset

    p <- p + geom_signif(
      comparisons = comparisons,
      y_position = y_positions,
      test = signif_test,
      test.args = signif_test_args,
      map_signif_level = function(p) {
        sapply(p, function(x) {
          if (is.na(x)) {
            "NA"
          } else if (x < 0.001) {
            paste0("*** (", signif(x, 2), ")")
          } else if (x < 0.01) {
            paste0("** (", signif(x, 2), ")")
          } else if (x <= 0.05) {
            paste0("* (", signif(x, 2), ")")
          } else if (x < 0.1) {
            paste0("NS. (", signif(x, 2), ")")
          } else {
            "NS."
          }
        })
      },
      textsize = 6
    )

    if (!is.null(test_label)) {
      p_label <- if (is.na(test_p_value)) {
        paste0(test_label, " (p=NA)")
      } else {
        paste0(test_label, " (p=", signif(test_p_value, 3), ")")
      }

      p <- p +
        labs(caption = p_label) +
        theme(
          plot.caption = element_text(size = 15, hjust = 0.5, vjust = 1),
          plot.caption.position = "plot",
          plot.margin = margin(10, 10, 25, 10)
        )
    }
  }

  return(p)
}
