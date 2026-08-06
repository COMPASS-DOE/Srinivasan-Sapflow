
wx_precip %>%
  mutate(Date = date(TIMESTAMP),
         Year = year(Date),
         Month = month(Date),
         Week = week(Date),
         precipBulk = replace_na(precip, 0)) -> clean

clean %>%
  group_by(Year, Month) %>%
  summarise(monthly_precip = sum(precipBulk)) -> monthly


ggplot(monthly, aes(Month, monthly_precip, color = as.factor(Year), group = Year)) +
  geom_point() + geom_line()

monthly_cum <- monthly %>%
  arrange(Year, Month) %>%
  group_by(Year) %>%
  mutate(cum_precip = cumsum(monthly_precip))

ggplot(monthly_cum, aes(x = Month, y = cum_precip, color = factor(Year))) +
  geom_line(size = 1.1) +
  geom_point() +
  labs(
    title = "Cumulative Monthly Precipitation",
    x = "Month",
    y = "Cumulative Precipitation (mm)",
    color = "Year"
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme_bw(base_size = 14)
