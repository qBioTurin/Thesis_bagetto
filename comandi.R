tinytex::install_tinytex()


edss_long <- merged %>%
  select(ID, starts_with("EDSS_T")) %>%
  pivot_longer(
    cols = starts_with("EDSS_T"), 
    names_to = "timepoint", 
    values_to = "EDSS"
  ) %>%
  filter(!is.na(EDSS)) %>%
  mutate(timepoint = factor(timepoint, 
                            levels = paste0("EDSS_T", c(0, 6, 12, 18, 24, 30, 36)),
                            labels = paste0("T", c(0, 6, 12, 18, 24, 30, 36))))
#creo grafico
ggplot(edss_long, aes(x = timepoint, y = EDSS, group = ID)) +
  geom_line(aes(color = ID), alpha = 0.6, size = 0.6) + 
  #jitter
  geom_point(aes(color = ID), alpha = 0.6, size = 1.2, position = position_jitter(width = 0.1, height = 0, seed = 42)) +
  #aggiungo etichette ID
  geom_text_repel(data = edss_long %>% 
                    group_by(ID) %>% 
                    filter(as.numeric(timepoint) == max(as.numeric(timepoint))),
                  aes(label = ID, color = ID), size = 3, fontface = "bold", nudge_x = 0.2, segment.alpha = 0, show.legend = FALSE) +
  #Linea media
  stat_summary(aes(group = 1), fun = mean, geom = "line", size = 1.3, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "point", size = 2.5, color = "black", shape = 18) + 
  #correzione colori per averli più distinti
  scale_color_manual(values = as.vector(pals::polychrome(length(unique(edss_long$ID)))), guide = "none") +
  labs(title = "EDSS Progression Over Time", 
       subtitle = "Black: cohort average | Coloured: each TEC patient", 
       x = "Timepoints", y = "EDSS value" ) + 
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), 
        axis.title = element_text(face = "bold"), 
        plot.title = element_text(face = "bold", size = 14), 
        plot.subtitle = element_text(color = "grey30"),
        plot.margin = margin(10, 40, 10, 10)) #aggiungo spazio per gli ID
