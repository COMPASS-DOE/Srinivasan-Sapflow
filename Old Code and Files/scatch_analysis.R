
library(tidyverse)

base <- lmer(sqrt(F_avg) ~ BA*Plot + 
             (1|ID:Year) - 1,
           data = tsb_2)
AIC(base) -> aic_base

base_alt <- glmer(sqrt(F_avg) ~ BA*Plot + 
                    (1|ID:Year) - 1,
                  data = tsb_2, family = gaussian)
AIC(base_alt) -> aic_base_alt

ec <- lmer(sqrt(F_avg) ~ BA*Plot + soil_ec_avg + 
              (1|ID:Year) - 1,
            data = tsb_2)
AIC(ec) -> aic_ec

vwc <- lmer(sqrt(F_avg) ~ BA*Plot + soil_ec_avg + soil_vwc_avg + 
            (1|ID:Year) - 1,
          data = tsb_2)
AIC(vwc) -> aic_vwc


soil_stuff_only <- lmer(sqrt(F_avg) ~ soil_ec_avg + soil_vwc_avg +
                          (1|ID:Year) - 1,
                data = tsb_2)
AIC(soil_stuff_only) -> aic_ss_only

anova(base, ec)
anova(base, vwc)
anova(vwc, soil_stuff_only)

variable <- c("base", "base_glmer", "+ EC", "+ EC + VWC", "EC + VWC only")
AIC <- c(aic_base, aic_base_alt, aic_ec, aic_vwc, aic_ss_only)
BACI <- c("w/ BACI", "w/ BACI", "w/ BACI", "w/ BACI", "w/o BACI")

AIC <- data.frame(variable, AIC, BACI)

AIC %>%
  mutate(variable =
           fct_relevel(variable,
                       "base", "base_glmer",
                       "+ EC", "+ EC + VWC",
                        "EC + VWC only")) %>%
ggplot(aes(variable, AIC + 17875, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() + 
  labs(title = 'AIC Model Comparison',
       x = 'Model', y = 'AIC Value') +
  theme_light()
