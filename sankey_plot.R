#importo dataset
merged <- read.csv2("C:/AURI/Thesis_bagetto/input/merged.csv")
merged_ceci<- read.csv2("C:/AURI/Thesis_bagetto/input/merged_ceci.csv")
merged_MAV <- read.csv2("C:/AURI/Thesis_bagetto/input/merged_MAV.csv")

# SANKEY PLOT TOTALE MAV E TEC
# Fasce EDSS in formato long, troncate al primo NA
sankey_long <- merged_ceci %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      EDSS_T0 >= 6 ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      EDSS_T6 >= 6 ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      EDSS_T12 >= 6 ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      EDSS_T18 >= 6 ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      EDSS_T24 >= 6 ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      EDSS_T30 >= 6 ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      EDSS_T36 >= 6 ~ "≥6"
    )
  ) %>%
  pivot_longer(-ID, names_to = "time", values_to = "band") %>%
  mutate(time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36"))) %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  filter(cumsum(is.na(band)) == 0) %>%   # tiene le osservazioni fino al primo NA (escluso)
  ungroup()

# Link tra timepoint consecutivi
links <- sankey_long %>%
  group_by(ID) %>%
  mutate(
    source = paste0(time, "_", band),
    target = lead(paste0(time, "_", band))
  ) %>%
  ungroup() %>%
  filter(!is.na(target)) %>%        # l'ultima osservazione di ogni paziente non genera link
  count(source, target, name = "value")

# Creazione nodi
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  separate(name, into = c("time", "edss"), sep = "_", remove = FALSE) %>%
  mutate(
    group = case_when(
      grepl("≥6", edss)    ~ "≥6",
      grepl("3-5.5", edss) ~ "3-5.5",
      grepl("<3", edss)    ~ "<3"
    ),
    time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36")),
    group_order = factor(group, levels = c("≥6","3-5.5","<3"))
  ) %>%
  arrange(time, group_order) %>%
  mutate(name = paste(time, edss, sep = "_")) %>%
  select(name, group)

# Controllo rapido
print(table(nodes$group))

# Rimappatura indici JS
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Assegna gruppo ai link in base al nodo di destinazione
links <- links %>%
  mutate(group = nodes$group[target + 1])

# Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)

sankey_plot


# SANKEY PLOT TEC
# Fasce EDSS in formato long, troncate al primo NA
sankey_long <- merged %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      EDSS_T0 >= 6 ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      EDSS_T6 >= 6 ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      EDSS_T12 >= 6 ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      EDSS_T18 >= 6 ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      EDSS_T24 >= 6 ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      EDSS_T30 >= 6 ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      EDSS_T36 >= 6 ~ "≥6"
    )
  ) %>%
  pivot_longer(-ID, names_to = "time", values_to = "band") %>%
  mutate(time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36"))) %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  filter(cumsum(is.na(band)) == 0) %>%   # tiene le osservazioni fino al primo NA (escluso)
  ungroup()

# Link tra timepoint consecutivi
links <- sankey_long %>%
  group_by(ID) %>%
  mutate(
    source = paste0(time, "_", band),
    target = lead(paste0(time, "_", band))
  ) %>%
  ungroup() %>%
  filter(!is.na(target)) %>%        # l'ultima osservazione di ogni paziente non genera link
  count(source, target, name = "value")

# Creazione nodi
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  separate(name, into = c("time", "edss"), sep = "_", remove = FALSE) %>%
  mutate(
    group = case_when(
      grepl("≥6", edss)    ~ "≥6",
      grepl("3-5.5", edss) ~ "3-5.5",
      grepl("<3", edss)    ~ "<3"
    ),
    time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36")),
    group_order = factor(group, levels = c("≥6","3-5.5","<3"))
  ) %>%
  arrange(time, group_order) %>%
  mutate(name = paste(time, edss, sep = "_")) %>%
  select(name, group)

# Controllo rapido
print(table(nodes$group))

# Rimappatura indici JS
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Assegna gruppo ai link in base al nodo di destinazione
links <- links %>%
  mutate(group = nodes$group[target + 1])

# Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)

sankey_plot

# SANKEY PLOT MAV
# Fasce EDSS in formato long, troncate al primo NA
sankey_long <- merged_MAV %>%
  transmute(
    ID,
    T0 = case_when(
      EDSS_T0 < 3 ~ "<3",
      EDSS_T0 < 6 ~ "3-5.5",
      EDSS_T0 >= 6 ~ "≥6"
    ),
    T6 = case_when(
      EDSS_T6 < 3 ~ "<3",
      EDSS_T6 < 6 ~ "3-5.5",
      EDSS_T6 >= 6 ~ "≥6"
    ),
    T12 = case_when(
      EDSS_T12 < 3 ~ "<3",
      EDSS_T12 < 6 ~ "3-5.5",
      EDSS_T12 >= 6 ~ "≥6"
    ),
    T18 = case_when(
      EDSS_T18 < 3 ~ "<3",
      EDSS_T18 < 6 ~ "3-5.5",
      EDSS_T18 >= 6 ~ "≥6"
    ),
    T24 = case_when(
      EDSS_T24 < 3 ~ "<3",
      EDSS_T24 < 6 ~ "3-5.5",
      EDSS_T24 >= 6 ~ "≥6"
    ),
    T30 = case_when(
      EDSS_T30 < 3 ~ "<3",
      EDSS_T30 < 6 ~ "3-5.5",
      EDSS_T30 >= 6 ~ "≥6"
    ),
    T36 = case_when(
      EDSS_T36 < 3 ~ "<3",
      EDSS_T36 < 6 ~ "3-5.5",
      EDSS_T36 >= 6 ~ "≥6"
    )
  ) %>%
  pivot_longer(-ID, names_to = "time", values_to = "band") %>%
  mutate(time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36"))) %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  filter(cumsum(is.na(band)) == 0) %>%   # tiene le osservazioni fino al primo NA (escluso)
  ungroup()

# Link tra timepoint consecutivi
links <- sankey_long %>%
  group_by(ID) %>%
  mutate(
    source = paste0(time, "_", band),
    target = lead(paste0(time, "_", band))
  ) %>%
  ungroup() %>%
  filter(!is.na(target)) %>%        # l'ultima osservazione di ogni paziente non genera link
  count(source, target, name = "value")

# Creazione nodi
nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  separate(name, into = c("time", "edss"), sep = "_", remove = FALSE) %>%
  mutate(
    group = case_when(
      grepl("≥6", edss)    ~ "≥6",
      grepl("3-5.5", edss) ~ "3-5.5",
      grepl("<3", edss)    ~ "<3"
    ),
    time = factor(time, levels = c("T0","T6","T12","T18","T24","T30","T36")),
    group_order = factor(group, levels = c("≥6","3-5.5","<3"))
  ) %>%
  arrange(time, group_order) %>%
  mutate(name = paste(time, edss, sep = "_")) %>%
  select(name, group)

# Controllo rapido
print(table(nodes$group))

# Rimappatura indici JS
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Assegna gruppo ai link in base al nodo di destinazione
links <- links %>%
  mutate(group = nodes$group[target + 1])

# Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 12,
  nodeWidth = 25,
  colourScale = JS("d3.scaleOrdinal().domain(['≥6','3-5.5','<3']).range(['#E74C3C','#F1C40F','#2ECC71'])"),
  nodePadding = 20,
  iterations = 0,
  sinksRight = FALSE
)

sankey_plot

