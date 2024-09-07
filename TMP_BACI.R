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
tmpfull <- readRDS("Full_21_24.rds")

#Specify pre-and-post 2022 TEMPEST event (June 22nd)
#Pre2022 <- June 15th - June 21
#Post2022 <- June 23rd - 29th
weeks_start <- c("2022-06-15", "2022-06-23")
weeks_end <- c("2022-06-21", "2022-06-29")
weeks_start<- as_date(weeks_start)
weeks_end<- as_date(weeks_end)

#Tidy up our data: 
tmp_baci <- tmpfull %>%
  select(Year, Species, ID, TIMESTAMP, Plot, F) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  filter(Hour <= 12, Hour >= 11, 
         Plot != "Freshwater") %>%
  filter(Date >= weeks_start & Date <= weeks_end) %>%
  mutate(BA = ifelse(Date < "2022-06-22", "Before", "After")) %>%
  mutate(Day = as.numeric(Date - as_date("2022-06-22")))

#Obtain average F for 11 am - 12 pm period: 
tmp_baci <- tmp_baci %>%
  group_by(Date, Plot, Species, ID, Year, Day, BA) %>%
  summarise(F_avg = mean(F, na.rm = TRUE))
 
#Ignore everything after this!! 
#Testing the following model: 
# sapflow ~ BA + site + BA*site + (1 | TreeID)



# Fit model without the interaction of random effects (Site and Year)
glmm_tmp <- glmer(F_avg ~ BA + Plot + BA*Plot +
                    (1|ID),
                  data = tmp_baci, family = binomial)

model.noint <- glmer(F_avg ~ BA + Plot +
                    (1|ID),
                  data = tmp_baci, family = binomial)

refdist.pb.100.interaction <- PBrefdist(largeModel = glmm_tmp, 
                                        smallModel = model.noint, 
                                        nsim = 100, seed = 1989)

compar.interaction.100 <- PBmodcomp(largeModel = glmm_tmp, 
                                    smallModel = model.noint,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100

#Testing for significance of BACI interaction term 
#using a parametric bootstrap comparison between nested models


