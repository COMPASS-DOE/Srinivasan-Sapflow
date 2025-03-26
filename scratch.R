
tulips <- data %>%
  mutate(Date = date(TIMESTAMP),
         Hour = hour(TIMESTAMP)) %>%
  filter(Species == "Tulip Poplar")

tulips23 <- tulips %>% 
  filter(Year == 2023) 

values <- seq(0,28,2)

tulips23 %>%
  filter(Fd < 30) %>%
ggplot() +
  geom_point(aes(x = Date, y = Fd, , color = ID), alpha = 0.2) +
  geom_hline(yintercept = values) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W") +
  facet_wrap(Plot~., 
             scales = "free_x", ncol = 1) + theme_classic()

winter <- tulips %>%
  filter(week(Date) < 11 & Fd < 2 | week(Date) > 46 & Fd < 2)
shoulder <- tulips %>%
  filter(week(Date) %in% c(11, 12, 13, 14, 15) & Fd < 8 |
         week(Date) %in% c(46, 45, 44, 43) & Fd < 12)
growing <- tulips %>%
  filter(week(Date) > 15 & week(Date) < 46 & Fd < 24)

tulips_filtered <- bind_rows(winter, shoulder, growing)

roll_length = 28

sf_rollmean <- tulips_filtered %>%
  group_by(ID, Date) %>%
  filter(Hour == 13) %>%
  mutate(Year = as.factor(Year)) %>%
  ungroup() %>%
  group_by(Year, Plot, ID) %>%
  arrange(Date) %>%
  mutate(f_roll = zoo::rollmean(Fd, roll_length,
                                align = "center",
                                fill = NA))

tulips_midday <- tulips_filtered %>%
  ungroup() %>%
  mutate(doy = yday(Date),
         Year = as.factor(Year)) %>%
  filter(Hour == 13.0) %>%
  group_by(Year, Date, doy, Plot, ID, Hour) %>%
  summarize(midday = mean(Fd, na.rm = TRUE))

ggplot() +
  geom_jitter(data = tulips_midday,
            aes(x = doy, y = midday, , color = Year), alpha = 0.25) +
  geom_line(data = sf_rollmean,
            aes(x = yday(Date), y = f_roll, color = Year)) +
  facet_wrap(Plot~ID, 
             scales = "free_x", ncol = 6) + theme_classic()

tulips_midday %>%
  left_join(sf_rollmean) %>%
  filter(ID %in% c("C1", "C3", "F3", "F4", "S2", "S3")) %>%
ggplot() +
  geom_jitter(aes(x = doy, y = midday, , color = Year), alpha = 0.25) +
  geom_line(aes(x = doy, y = f_roll, color = Year)) +
  facet_wrap(Plot~ID, 
             scales = "free_x", ncol = 2) + theme_classic()



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