
library(dplyr)
library(tidyr)
library(car)
library(lme4)
library(ggplot2)

#generate dummy data for baseline year
set.seed(123)
nochange_yr1 <- function() rnorm(50, mean = 1, sd = 0.25)

baci <- bind_rows(
  tibble(case = "control_B", time = 1:50, value = nochange_yr1()),
  tibble(case = "impact_B", time = 1:50, value = nochange_yr1()),
  tibble(case = "control_B", time = 51:100, value = nochange_yr1()),
  tibble(case = "impact_B", time = 51:100, value = nochange_yr1())
)

baci %>%
  separate(case, into = c("CI", "BA"), sep = "_") %>%
  mutate(year = 2021) -> baci_yr1

#and for a first treatment year
nochange_yr2 <- function() rnorm(50, mean = 1.15, sd = 0.25)
change_yr2 <- function() rnorm(50, mean = 1.35, sd = 0.25)

baci2 <- bind_rows(
  tibble(case = "control_B", time = 1:50, value = nochange_yr2()),
  tibble(case = "impact_B", time = 1:50, value = nochange_yr2()),
  tibble(case = "control_A", time = 51:100, value = nochange_yr2()),
  tibble(case = "impact_A", time = 51:100, value = change_yr2())
)

baci2 %>%
  separate(case, into = c("CI", "BA"), sep = "_") %>%
  mutate(year = 2022) -> baci_yr2

two_years <- bind_rows(baci_yr1, baci_yr2)

ggplot(two_years, aes(time, value, color = CI, shape = BA)) +
  geom_point(size = 3) + facet_wrap(.~year)

test_baci <- lmer(value ~ BA + CI + BA*CI + (1|year),
                   data = two_years)

#one year of before data
#one year of treatment data
#significant CI term,
#and significant interaction
Anova(test_baci, type = "III")

#let's add another year of data, with a marginal difference
nochange_yr3 <- function() rnorm(50, mean = 1.05, sd = 0.25)
change_yr3 <- function() rnorm(50, mean = 1.15, sd = 0.25)

baci3 <- bind_rows(
  tibble(case = "control_B", time = 1:50, value = nochange_yr3()),
  tibble(case = "impact_B", time = 1:50, value = nochange_yr3()),
  tibble(case = "control_A", time = 51:100, value = nochange_yr3()),
  tibble(case = "impact_A", time = 51:100, value = change_yr3())
)

baci3 %>%
  separate(case, into = c("CI", "BA"), sep = "_") %>%
  mutate(year = 2023) -> baci_yr3

three_years <- bind_rows(two_years, baci_yr3)

ggplot(three_years, aes(time, value, color = CI, shape = BA)) +
  geom_point(size = 3) + facet_wrap(.~year)

bigger_test_baci <- lmer(value ~ BA + CI + BA*CI + (1|year),
                  data = three_years)

#one year of before data
#two years of treatment data
#significant CI term,
#marginally significant interaction
Anova(bigger_test_baci, type = "III")
