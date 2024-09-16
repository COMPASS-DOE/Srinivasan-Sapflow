
set.seed(123)
nochange <- function() rnorm(50, mean = 1, sd = 0.25)
change <- function() rnorm(50, mean = 1.15, sd = 0.25)

library(dplyr)
library(car)

baci <- bind_rows(
  tibble(case = "control_B", year = 1951:2000, value = nochange()),
  tibble(case = "impact_B", year = 1951:2000, value = nochange()),
  tibble(case = "control_A", year = 2001:2050, value = nochange()),
  #tibble(case = "+t", x_control = nochange(), x_treatment = change()),
  tibble(case = "impact_A", year = 2001:2050, value = nochange())
)

baci %>%
  separate(case, into = c("CI", "BA"), sep = "_") -> baci2


ggplot(baci2, aes(year, value, color = CI, shape = BA)) + geom_point(size = 3) 

test_baci <- glmer(value ~ BA + CI + BA*CI + (1|year),
                   data = baci2, family = gaussian)

Anova(test_baci, type = "III")  
