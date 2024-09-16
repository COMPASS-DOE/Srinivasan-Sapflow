#Using GLMM BACI Analysis method from Pardini et. al. (2018) to analyze TEMPEST sapflow data 

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

#TEMPEST EVENTS
weeks0_start <- c("2021-05-30", "2021-06-06")
weeks0_end <- c("2021-06-20", "2021-06-27")
weeks0_start<- as_date(weeks0_start)
weeks0_end<- as_date(weeks0_end)

weeks1_start <- c("2022-06-05", "2022-06-12") #June 22
weeks1_end <- c("2022-06-26", "2022-07-03")
weeks1_start<- as_date(weeks1_start)
weeks1_end<- as_date(weeks1_end)

weeks2_start <- c("2023-05-21", "2023-05-28") #June 6, 7
weeks2_end <- c("2023-06-11", "2023-06-18")
weeks2_start<- as_date(weeks2_start)
weeks2_end<- as_date(weeks2_end)

weeks3_start <- c("2024-05-26", "2024-06-02") #june 11, 12, 13
weeks3_end <- c("2024-06-16", "2024-06-23")
weeks3_start<- as_date(weeks3_start)
weeks3_end<- as_date(weeks3_end)

#Tidy up our data: 
full_data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  filter(Hour <= 13, Hour >= 10, 
         Plot != "Freshwater",
         Species == "Tulip Poplar",
         month(TIMESTAMP) %in% c(5, 6, 7)) -> summer_tulip_salt

summer_tulip_salt %>%
  filter(Year == 2021,
         Date >= weeks0_start & Date <= weeks0_end) %>%
  mutate(BA = "Before",
         Day = as.numeric(Date - as_date("2021-06-16"))) -> year_zero

summer_tulip_salt %>%
  filter(Year == 2022,
         Date >= weeks1_start & Date <= weeks1_end) %>%
  mutate(BA = ifelse(Date < "2022-06-22", "Before", "After"),
         Day = as.numeric(Date - as_date("2022-06-22"))) -> year_one

summer_tulip_salt %>%
  filter(Year == 2023,
         Date >= weeks2_start & Date <= weeks2_end) %>%
  mutate(BA = ifelse(Date < "2023-06-07", "Before", "After"),
         Day = as.numeric(Date - as_date("2023-06-07"))) -> year_two

#doesn't work because the data aren't there
# summer_tulip_salt %>%
#   filter(Year == 2024,
#          Date >= weeks3_start & Date <= weeks3_end) %>%
#   mutate(BA = ifelse(Date < "2024-06-13", "Before", "After"),
#          Day = as.numeric(Date - as_date("2024-06-13"))) -> year_three

year_zero %>%
  bind_rows(year_one) %>%
  bind_rows(year_two) -> tmp_baci

#[tmp_baci$`F` > 6e-06,]
ggplot(tmp_baci[tmp_baci$`F` < 6e-06,], aes(Day, `F`, color = ID)) +
  geom_point() + facet_grid(Plot~Year, scales = "free")

#Obtain average F for 11 am - 12 pm period: 
tmp_baci2 <- tmp_baci %>%
  mutate(Year_F = as.factor(tmp_baci$Year),
         ID_F = as.factor(tmp_baci$ID),
         BA_F = as.factor(tmp_baci$BA),
         Plot_F = as.factor(tmp_baci$Plot)) %>%
  filter(`F` < 6e-06) %>%
  group_by(Date, Plot_F, ID_F, Year_F, Day, BA_F) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = length(.))
 
ggplot(tmp_baci2, aes(Day, F_avg, color = ID_F)) +
  geom_point() + facet_grid(Plot_F~Year_F, scales = "free")

ggplot(tmp_baci2, aes(Day, F_avg, color = ID_F, shape = BA_F)) +
  geom_point() + facet_grid(Plot_F~Year_F, scales = "free")

#Ignore everything after this!! 
#Testing the following model: 
# sapflow ~ BA + site + BA*site + (1 | TreeID)



# Fit model without the interaction of random effects (Site and Year)
#Cand.set <- list()

#Cand.set[[1]] 
glmm_tmp2 <- glmer(F_avg ~ BA_F + Plot_F + BA_F*Plot_F +
                         (1|ID_F) + (1|Year_F) +
                         (1|ID_F:1|Year_F),
                  data = tmp_baci2, family = gaussian)

#Cand.set[[2]]
model.noint <- glmer(F_avg ~ BA_F + Plot_F  + BA_F*Plot_F +
                         (1|ID_F) + (1|Year_F),
                     data = tmp_baci2, family = gaussian)


AIC.res.table <- aictab(cand.set = list(Cand.set[[1]], Cand.set[[2]]), 
                        modnames = paste0("Cand.set_", c(1,2)), 
                        second.ord = TRUE)

anova(glmm_tmp2, model.noint)




refdist.pb.100.interaction <- PBrefdist(largeModel = glmm_tmp, 
                                        smallModel = model.noint, 
                                        nsim = 100, seed = 1989)

compar.interaction.100 <- PBmodcomp(largeModel = glmm_tmp, 
                                    smallModel = model.noint,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100

#Testing for significance of BACI interaction term 
#using a parametric bootstrap comparison between nested models


