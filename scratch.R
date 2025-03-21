
ggplot(sf_plot_avg) + 
  geom_point(aes (x = Date, y = F_avg, color = species)) + 
  facet_wrap(~plot, ncol = 1, scales = "fixed") + 
  labs(y = "Avg Sap Flux Density", x = "Date", title = "Sap Flux Density Averaged Daily, 11 AM - 12 PM")

sf_plot_clean <- sf_plot_avg %>% 
  filter(Date %in% dates_with_good_weather$date)

roll_length = 10

sf_rollmean <- sf_plot_clean %>% 
  clean_names() %>% 
  ungroup() %>% 
  group_by(plot, species) %>% 
  #filter(species == "Be") %>% 
  mutate(f_roll = zoo::rollmean(f_avg, roll_length, fill = NA)) %>% 
  mutate(doy = yday(date),
         year = year(date)) 

sf_rollmean %>%
  ggplot(aes(x = doy, color = as.factor(year))) +
  geom_point(aes(y = f_avg), alpha = 0.2) + 
  geom_line(aes(y = f_roll)) + 
  facet_wrap(species~plot, 
             scales = "free_y", 
             nrow = 3)

sf_filtered <- sf_rollmean %>% 
  filter(year == 2023) 

write_csv(sf_filtered, "data/250310_2023_sapflow.csv")

sf_filtered %>% 
  ggplot(aes(date, color = plot)) + 
  geom_point(aes(y = f_avg), alpha = 0.1) +
  geom_line(aes(y = f_roll)) +
  geom_vline(xintercept = as_date(flood1), linetype = "dashed") + 
  facet_wrap(~species, ncol = 1) + 
  labs(x = "", y = "Sapflux (cm/s)", color = "") + 
  theme(legend.background = element_blank(), 
        legend.position = "bottom")
ggsave("figures/X_2023_sapflow.png", width = 5, height = 7)


first_flood = as_date("2023-06-06")
## Note that vegetation metrics were taken on 6-13, so let's use that as our window
##

plot_by_species <- function(spp){
  
  sf_plot_clean %>% 
    clean_names() %>% 
    #filter(species == spp) %>% 
    filter(date >= first_flood - days(7)) %>% 
    filter(date <= first_flood + days(7)) %>% 
    mutate(period = case_when(date < first_flood ~ "0_preflood", 
                              date >= first_flood ~ "2_postflood")) %>% 
    mutate(condition2 = as.factor(case_when(period == "0_preflood" ~ "Preflood", 
                                            period == "2_postflood" ~ "Postflood"))) %>% 
    mutate(condition2 = fct_relevel(condition2, "Preflood")) %>% 
    ggplot(aes(condition2, f_avg, color = condition2, fill = condition2)) + 
    geom_boxplot(alpha = 0.6, show.legend = F) +
    geom_jitter(width = 0.2, show.legend = F) + 
    facet_wrap(~plot, nrow = 1, scales = "free") + 
    geom_tukey(where = "whisker") +
    #stat_compare_means() +
    labs(x = "", y = "Sapflux") + 
    theme_linedraw(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "gray90"), 
      panel.grid.minor = element_line(color = "gray90"),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(face = "bold", color = "black"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)
    ) + 
    scale_fill_viridis_d(option = "mako", end = 0.8) + 
    scale_color_viridis_d(option = "mako", end = 0.8)
}

plot_by_species("Tulip Poplar")




sf_plot_clean %>% 
  clean_names() %>% 
  filter(date >= first_flood - days(7)) %>% 
  filter(date <= first_flood + days(7)) %>% 
  mutate(period = ifelse(date < first_flood, "0_preflood", "1_postflood")) %>% 
  ggplot(aes(period, f_avg, fill = period)) + 
  geom_boxplot() + 
  facet_wrap(species~plot, nrow = 1, scales = "free") + 
  geom_tukey(where = "whisker") 