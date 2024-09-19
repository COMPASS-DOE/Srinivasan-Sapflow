#Using GLMM BACI Analysis method from Pardini et. al. (2018)
#as an initial template to analyze TEMPEST sapflow data 

library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

##From Pardini code: 
# List of packages to be used in the R session
.packages = c("lme4", "AICcmodavg", "MuMIn", "pbkrtest",
              "parallel", "data.table", "blmeco", "lsmeans")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Attach packages
sapply(.packages, require, character.only=TRUE)


#read in TEMPEST sapflow data
#tmp_full <- readRDS("Full_21_24.rds")

full_data %>%
  filter(month(TIMESTAMP) %in% c(5,6,7)) %>%
  group_by(ID, date(TIMESTAMP)) %>%
  mutate(peak = max(`F`)) %>%
  ggplot(aes(hour(TIMESTAMP))) + geom_histogram()


#TEMPEST EVENTS
tempest_events <- bind_rows(
  tibble( Year = 2021, flood_start = "2021-06-12", flood_end = "2021-06-12"), #average date = June 12
  tibble( Year = 2022, flood_start = "2022-06-22", flood_end = "2022-06-22"), #June 22
  tibble( Year = 2023, flood_start = "2023-06-06", flood_end = "2023-06-07"), #June 6, 7
  tibble( Year = 2024, flood_start = "2024-06-11", flood_end = "2024-06-13")) #June 11, 12, 13

tempest_events %>%
  mutate(flood_start = ymd(flood_start),
         flood_end = ymd(flood_end)) -> events

#window of data to look at
window <- days(15)

#Tidy up our data: 
full_data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
         Date < data_end & Date > flood_end,
         Hour < 14, Hour >= 11, 
         Plot != "Freshwater",
         Species == "Tulip Poplar") %>%
  ungroup() -> tulip_salt

#Obtain average F for 11 am - 2 pm period: 
tulip_salt %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  filter(`F` < 6e-06) %>% #remove outliers, ggplot below for confirmation
  group_by(Date, Plot, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n()) %>%
  ungroup()-> tsb_1

#[tulip_salt$`F` > 6e-06,]
ggplot(tulip_salt[tulip_salt$`F` < 6e-06,],
       aes(Date, `F`, color = ID)) +
  geom_point() + facet_grid(Plot~Year, scales = "free")

ggplot(tsb_1,
       aes(Date, F_avg, color = ID, shape = BA)) +
  geom_point() + facet_grid(Plot~Year, scales = "free")


tsb_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) -> tsb_2

ggplot(tsb_2, aes(Day, F_avg, color = ID, shape = BA)) +
  geom_point(size = 2.5) + facet_grid(Plot~Year, scales = "free")


# Fit model without the interaction of random effects (Site and Year)
Cand.set <- list()

#model.noint
Cand.set[[1]] <- glmer(F_avg ~ BA + Plot  + BA*Plot +
                         (1|ID) + (1|Year),
                       data = tsb_2, family = gaussian)
#model.int 
Cand.set[[2]] <- glmer(F_avg ~ BA + Plot + BA*Plot +
                         (1|ID) + (1|Year) +
                         (1|ID:Year),
                       data = tsb_2, family = gaussian)



AIC.res.table <- aictab(cand.set = list(Cand.set[[1]], Cand.set[[2]]), 
                        modnames = paste0("Cand.set_", c(1,2)), 
                        second.ord = TRUE)
AIC.res.table

# Model selection based on AICc:
#   
#   K      AICc Delta_AICc AICcWt Cum.Wt   Res.LL
# Cand.set_2 8 -26340.29       0.00      1      1 13178.22
# Cand.set_1 7 -26142.40     197.89      0      1 13078.26

#Suggests that the second model, with the interaction of ID and year
#is the better match for our data

#dispersion control? not necessary for normally distributed data

#check assumptions of normality
qqnorm(residuals(model.int), 
       main = "Q-Q plot - residuals")
qqline(residuals(model.int), col="red")

# inspecting the random effects (see also Bolker, 2009 - supp 1)
qqnorm(unlist(ranef(model.int)), 
       main = "Q-Q plot, random effects")
qqline(unlist(ranef(model.int)), col="red")

# fitted vs residuals
scatter.smooth(fitted(model.int), 
               residuals(model.int, type="pearson"),
               main="fitted vs residuals",
               xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

#definitely a little right skewed

library(e1071)
current_skewness <- skewness(tsb_2$F_avg)
print(paste("Current Skewness:", current_skewness))

# Logarithmic Transformation
log_transformed_response <- log(tsb_2$F_avg + 1)

# Square Root Transformation
sqrt_transformed_response <- sqrt(tsb_2$F_avg)

# Inverse Transformation
inv_transformed_response <- 1 / tsb_2$F_avg

log_skewness <- skewness(log_transformed_response)
sqrt_skewness <- skewness(sqrt_transformed_response)
inv_skewness <- skewness(inv_transformed_response)

print(paste("Log Transformation Skewness:", log_skewness))
print(paste("Square Root Transformation Skewness:", sqrt_skewness))
print(paste("Inverse Transformation Skewness:", inv_skewness))

ggplot(tsb_2, aes(sqrt(F_avg))) + geom_histogram() +
  facet_grid(Plot~.)

#Square Root Transformed normality check

model.int_sr <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot +
           (1|ID) + (1|Year) +
           (1|ID:Year),
         data = tsb_2, family = gaussian)

#check assumptions of normality
qqnorm(residuals(model.int_sr), 
       main = "Q-Q plot - residuals")
qqline(residuals(model.int), col="red")

# inspecting the random effects (see also Bolker, 2009 - supp 1)
qqnorm(unlist(ranef(model.int_sr)), 
       main = "Q-Q plot, random effects")
qqline(unlist(ranef(model.int)), col="red")

# fitted vs residuals
scatter.smooth(fitted(model.int_sr), 
               residuals(model.int, type="pearson"),
               main="fitted vs residuals",
               xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

# not sure what this does - ignore for now
  # fitted vs observed
  # scatter.smooth(fitted(model.int), BACI.dt$PropCL,
  #                xlab="Fitted Values", ylab="Observed PropCL")
  # abline(0,1, col="red")

####
####
####
refdist.pb.100.interaction <- PBrefdist(largeModel = glmm_tmp, 
                                        smallModel = model.noint, 
                                        nsim = 100, seed = 1989)

compar.interaction.100 <- PBmodcomp(largeModel = glmm_tmp, 
                                    smallModel = model.noint,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100

#Testing for significance of BACI interaction term 
#using a parametric bootstrap comparison between nested models


