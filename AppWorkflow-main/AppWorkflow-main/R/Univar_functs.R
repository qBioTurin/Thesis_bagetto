library(waffle)
my_theme <- ggplot2::theme_void() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
    plot.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
    legend.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
    legend.box.background = ggplot2::element_rect(fill = "transparent", color = "transparent")
  )



barplot_logic <- function(df, var, palettes) {
  ggplot2::ggplot(df) +
    geom_bar(aes(x = !!sym(var)),
      fill = palettes[var][[1]]
    ) +
    coord_flip() +
    my_theme +
    theme(
      axis.text.y = element_text(family = "PT Sans", size = 15)
    )
}

pie_logic <- function(df, var, palettes) {
  # Usa una sintassi alternativa che funziona meglio con fattori
  # Usa table() che è più robusta con fattori e poi converti in dataframe
  counted <- df %>%
    dplyr::select(!!sym(var)) %>%
    dplyr::pull(1) %>%
    table() %>%
    as.data.frame() %>%
    setNames(c(var, "n")) %>%
    as_tibble()
  
  ggplot2::ggplot(counted) +
    geom_bar(
      aes(
        x = "",
        y = n,
        fill = !!sym(var)
      ),
      stat = "identity", width = 1
    ) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = palettes[var][[1]]) +
    my_theme +
    theme(
      legend.text = element_text(family = "PT Sans", size = 15),
      legend.title = element_blank()
    )
}

waffle_factor_nonord <- function(df, var, palettes, ndata, type_facets) {
  library(rlang)
  df <- as_tibble(df)
  golden_ratio <- (1 + sqrt(5)) / 2

  #print(paste0("class df : ",class(df)))
  #print(paste0("variabili names: ",names(df)))
  #print(paste("ndata:", ndata, "var:", var, "type facets:", paste(type_facets[[var]], collapse = ", ")))

  counts <- df %>% dplyr::count(!!sym(var))
  print(counts)

  ggplot2::ggplot(counts) +
    waffle::geom_waffle(
      aes(values = n, fill = !!sym(var)),
      n_rows = round(sqrt(ndata / golden_ratio))
    ) +
    scale_fill_manual(
      name = as.character(var),
      values = palettes[[var]],
      labels = type_facets[[var]]
    ) +
    coord_equal() +
    my_theme +
    theme(
      legend.text = element_text(family = "PT Sans", size = 15),
      legend.title = element_blank()
    )
}



barplot_factor_nonord <- function(df, var, palettes, type_facets) {
  ggplot2::ggplot(df) +
    geom_bar(
      aes(
        y = .data[[var]],
        fill = .data[[var]]
      )
    ) +
    scale_fill_manual(
      name = as.character(var),
      values = palettes[var][[1]],
      labels = type_facets[var]
    ) +
    my_theme +
    theme(
      legend.position = "none",
      axis.text.y = element_text(family = "PT Sans", size = 15)
    )
}

barplot_factor_ord_x <- function(df, var, palettes) {
  ggplot2::ggplot(df) +
    geom_bar(aes(
      x = .data[[var]],
      fill = .data[[var]]
    )) +
    scale_fill_manual(values = palettes[var][[1]]) +
    my_theme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(family = "PT Sans", size = 15)
    )
}

barplot_factor_ord_stack <- function(df, var, palettes) {
  ggplot2::ggplot(df) +
    geom_bar(
      aes(
        x = 0,
        fill = .data[[var]]
      ),
      position = "stack",
      width = 1
    ) +
    scale_fill_manual(values = palettes[var][[1]]) +
    scale_x_continuous(limits = c(-1, 1)) +
    my_theme +
    theme(
      legend.text = element_text(family = "PT Sans", size = 15),
      legend.title = element_blank()
    )
}

hist_numeric <- function(df, var, palettes) {
  ggplot2::ggplot(df) +
    geom_histogram(aes(x = .data[[var]])) +
    my_theme +
    theme(
      panel.grid.major.y = element_line(color = "grey", linewidth = 0.1),
      panel.grid.minor.y = element_line(color = "grey", linewidth = 0.1),
      axis.text = element_text(family = "PT Sans", size = 15),
    )
}

density_numeric <- function(df, var, palettes) {
  ggplot2::ggplot(df) +
    geom_density(aes(x = .data[[var]])) +
    my_theme +
    theme(
      axis.text.x = element_text(family = "PT Sans", size = 15),
    )
}

boxplot_numeric <- function(df, var, palettes) {
  ggplot2::ggplot(df) +
    geom_boxplot(aes(x = .data[[var]])) +
    my_theme +
    theme(
      axis.text.x = element_text(family = "PT Sans", size = 15),
      panel.grid.major.x = element_line(color = "grey", linewidth = 0.1),
      panel.grid.minor.x = element_line(color = "grey", linewidth = 0.1)
    )
}
