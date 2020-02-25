# Script to build panel data set of sure start spending and school readiness outcomes #
# Run this first # 

#### Load packages ####
library(fingertipsR)   # For getting PHE data
library(tidyverse)     # For everything!
library(readxl)        # For reading excel files
library(cowplot)       # For pretty graphs
library(pglm)          # For generalised panel models

#### Extract outcome variable indicators ####
sr_ids <- c(90631, # School readiness
            90632) # School readiness - free school meals

outcomes <- fingertips_data(IndicatorID = sr_ids,
                            AreaTypeID = 102) 

outcomes <- outcomes %>%
  filter(AreaType == "County & UA (pre 4/19)",
         Sex == "Persons") %>%
  select(IndicatorName,
         AreaName,
         AreaCode,
         ParentName,
         Timeperiod,
         Denominator,
         Value) %>%
  mutate(Timeperiod = as.numeric(gsub("/..", "", Timeperiod))) %>%
  gather(key = "Key1",
         value = "Val",
         Denominator, Value) %>%
  unite(Key2, IndicatorName, Key1) %>%
  spread(Key2, Val) 

# Fix the long names
names(outcomes)[5:8] <- c("n_sr",
                          "school_readiness",
                          "n_sr_fsm",
                          "school_readiness_fsm")

#### Extract predictor indicators 2012 - 2017 ####

# % children in low income families (under 16s)  

preds <- fingertips_data(IndicatorID = 10101,
                         AreaTypeID = 102) 

preds <- preds %>%
  filter(AreaType == "County & UA (pre 4/19)",
         Sex == "Persons") %>%
  filter(!(IndicatorID == 92313 & Age != "16-64 yrs")) %>%
  select(IndicatorName,
         AreaName,
         AreaCode,
         ParentName,
         Timeperiod,
         Value) %>%
  mutate(Timeperiod = as.numeric(gsub("/..", "", Timeperiod))) %>%
  filter(Timeperiod >= 2012) %>%
  spread(IndicatorName, Value) %>%
  rename(child_pov_u16 = 5)

#### Merge outcome data with other PHE indicator data ####

sr <- merge(outcomes, preds, 
            by = c("AreaName", "AreaCode", "ParentName", "Timeperiod"))

# Clear up worksspace
rm(outcomes, preds)

##### Download spending data from Gov.uk website ####

# Urls for data tables for FYs 2012/13 - 2017/18 in a data frame
yrs <- 2012:2017
urls <- c("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/264676/SR54-2013Tables.xls", # 2012/13
          "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/385992/SR52-2014Tables.xlsx", # 2013/14
          "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/483692/SR48-Tables.xlsx", # 2014/15
          "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/613286/SR63_2016_Tables.xlsx", # 2015/16
          "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/669117/SR71_2017_Tables_Table12_revised.xlsx", # 2016/17
          "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/765909/LA_and_school_expenditure_2017-18_Tables.xlsx") # 2017/18
extensions <- c(rep(".xls", 2), rep(".xlsx", 4))

spend_urls <- data.frame(yrs, urls, extensions, stringsAsFactors = FALSE)

# Loop over data frame downloading files if not already downloaded
for(y in spend_urls$yrs){
  f <- paste0("la_spend_", y, "_", gsub("20", "", (y + 1)),
              spend_urls$extensions[spend_urls$yrs == y])
  if(!file.exists(f)){
    url <- spend_urls$urls[spend_urls$yrs == y]
    download.file(url = url,
                  destfile = f,
                  mode = "wb")
  }
}

# Read in spend data

spend_1213 <- read_xls("la_spend_2012_13.xls", 
                       sheet = 7, 
                       skip = 4) %>%
  select(AreaCode = 1,
         tempcode = 2,
         education = 4,
         sure_start = 6,
         lac = 7,
         other_svcs = 8,
         safeguard = 9,
         fam_suppt = 10,
         yp_svcs = 11,
         yjs = 12,
         cap_ex = 13,
         spend_total = 14) %>%
  filter(!is.na(AreaCode)) %>%
  mutate(Timeperiod = 2012)%>%
  mutate_at(vars(-AreaCode, -Timeperiod),
            as.numeric) %>%
  mutate(spend_total = spend_total - education) %>% # Strip out education spend from total (not included in later totals)
  select(-education)

# later years don't contain the standard area codes. Create a lookup
tempcode_lookup <- spend_1213 %>% select(AreaCode, tempcode)

# Fix area codes for Northumberland & Gateshead so they match the AreaCodes from Fingertips
tempcode_lookup$AreaCode[tempcode_lookup$tempcode == 929] <- "E06000057"
tempcode_lookup$AreaCode[tempcode_lookup$tempcode == 390] <- "E08000037"

spend_1314 <- read_xlsx("la_spend_2013_14.xls", 
                        sheet = 7, 
                        skip = 4) %>%
  select(tempcode = 1,
         sure_start = 3,
         lac = 4,
         other_svcs = 5,
         safeguard = 6,
         fam_suppt = 7,
         yp_svcs = 8,
         yjs = 9,
         cap_ex = 10,
         spend_total = 11) %>%
  filter(!is.na(sure_start),
         !is.na(tempcode)) %>%
  mutate(Timeperiod = 2013) %>%
  merge(tempcode_lookup, by = "tempcode") %>%
  select(-tempcode) %>%
  mutate_at(vars(-AreaCode, -Timeperiod),
            as.numeric)

spend_1415 <- read_xlsx("la_spend_2014_15.xlsx", 
                        sheet = 7, 
                        skip = 4) %>%
  select(tempcode = 1,
         sure_start = 3,
         lac = 4,
         other_svcs = 5,
         safeguard = 6,
         fam_suppt = 7,
         yp_svcs = 8,
         yjs = 9,
         cap_ex = 10,
         spend_total = 11) %>%
  filter(!is.na(sure_start),
         !is.na(tempcode)) %>%
  mutate(Timeperiod = 2014) %>%
  merge(tempcode_lookup, by = "tempcode") %>%
  select(-tempcode) %>%
  mutate_at(vars(-AreaCode, -Timeperiod),
            as.numeric)

spend_1516 <- read_xlsx("la_spend_2015_16.xlsx", 
                        sheet = 7, 
                        skip = 4) %>%
  select(tempcode = 1,
         sure_start = 3,
         lac = 4,
         other_svcs = 5,
         safeguard = 6,
         fam_suppt = 7,
         yp_svcs = 8,
         yjs = 9,
         cap_ex = 10,
         spend_total = 11) %>%
  filter(!is.na(tempcode),
         !is.na(sure_start)) %>%
  mutate(Timeperiod = 2015) %>%
  merge(tempcode_lookup, by = "tempcode") %>%
  select(-tempcode) %>%
  mutate_at(vars(-AreaCode, -Timeperiod),
            as.numeric)

spend_1617 <- read_xlsx("la_spend_2016_17.xlsx", 
                        sheet = 7, 
                        skip = 4) %>%
  select(tempcode = 1,
         sure_start = 3,
         lac = 4,
         other_svcs = 5,
         safeguard = 6,
         fam_suppt = 7,
         yp_svcs = 8,
         yjs = 9,
         cap_ex = 10,
         spend_total = 11) %>%
  filter(!is.na(tempcode),
         !is.na(sure_start)) %>%
  mutate(Timeperiod = 2016) %>%
  merge(tempcode_lookup, by = "tempcode") %>%
  select(-tempcode) %>%
  mutate_at(vars(-AreaCode, -Timeperiod),
            as.numeric)

spend_1718 <- read_xlsx("la_spend_2017_18.xlsx", 
                        sheet = 8, 
                        skip = 4) %>%
  select(tempcode = 1,
         sure_start = 3,
         lac = 4,
         other_svcs = 5,
         safeguard = 6,
         fam_suppt = 7,
         yp_svcs = 8,
         yjs = 9,
         cap_ex = 10,
         spend_total = 11) %>%
  filter(!is.na(tempcode),
         !is.na(sure_start)) %>%
  mutate(Timeperiod = 2017) %>%
  merge(tempcode_lookup, by = "tempcode") %>%
  select(-tempcode) %>%
  mutate_at(vars(-AreaCode, -Timeperiod),
            as.numeric)

# Merge spending data across years into single data frame

spend_1213 <- select(spend_1213, -tempcode)

spend <- rbind(spend_1213,
               spend_1314,
               spend_1415,
               spend_1516,
               spend_1617,
               spend_1718) 

# Clear up workspace
rm(spend_1213, spend_1314, spend_1415, spend_1516, spend_1617, spend_1718, spend_urls)

# Merge spending data with main dataset
sr <- merge(sr, spend, by = c("AreaCode", "Timeperiod"),
            all.x = TRUE)

# Calculate total non-Sure Start spending
sr <- mutate(sr, non_ss_spend = spend_total - sure_start)

# Get total population and under-18 population from Fingertips
pop_data <- fingertips_data(92309,
                            AreaTypeID = 102)

pop_data <- pop_data %>%
  filter(AreaType == "County & UA (pre 4/19)",
         Sex == "Persons") %>%
  select(AreaName,
         AreaCode,
         Timeperiod,
         pop_u18 = Count,
         pop_total = Denominator,
         percent_u18 = Value) %>%
  mutate(Timeperiod = as.numeric(gsub("/..", "", Timeperiod))) %>%
  filter(Timeperiod >= 2012) 

# Merge with panel data set
sr <- merge(sr, pop_data,
            by = c("AreaName", "AreaCode", "Timeperiod"))

# Calculate spending per child
sr <- mutate(sr,
             spend_total = spend_total / pop_u18,
             sure_start = sure_start / pop_u18,
             non_ss_spend = non_ss_spend / pop_u18,
             lac = lac / pop_u18,
             safeguard = safeguard / pop_u18,
             yjs = yjs / pop_u18,
             fam_suppt = fam_suppt / pop_u18,
             yp_svcs = yp_svcs / pop_u18,
             cap_ex = cap_ex / pop_u18)

# Get GDP deflators
if(!file.exists("gdp_deflator.xlsx")){
  url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/813275/GDP_Deflators_Qtrly_National_Accounts_June_2019_update.xlsx"
  download.file(url = url,
                destfile = "gdp_deflator.xlsx",
                method = "curl",
                mode = "wb")
}

# Read in GDP deflators
gdp_dfl <- read_excel("gdp_deflator.xlsx",
                      skip = 6) %>%
  select(Timeperiod = 7,
         gdp_dfl = 2) %>%
  mutate(Timeperiod = as.numeric(Timeperiod),
         gdp_dfl = as.numeric(gdp_dfl)) %>%
  filter(!is.na(gdp_dfl))

# Re-base so that 2012 is the base year

rebase <- function(dfl, base_year){
  base_dfl <- dfl$gdp_dfl[dfl$Timeperiod == base_year]
  dfl$gdp_dfl <- dfl$gdp_dfl / base_dfl
  return(dfl)
}

gdp_dfl <- rebase(dfl = gdp_dfl, base_year = 2012)

# Merge with main panel
sr <- merge(sr, gdp_dfl, by = "Timeperiod")

# Convert spending variables to constant 2012 £s, multiply by 1,000 for £ per child
sr <- sr %>%
  mutate(spend_total = 1000 * spend_total / gdp_dfl, 
         sure_start = 1000 * sure_start / gdp_dfl,
         non_ss_spend = 1000 * non_ss_spend / gdp_dfl,
         lac = 1000 * lac / gdp_dfl,
         safeguard = 1000 * safeguard / gdp_dfl,
         yjs = 1000 * yjs / gdp_dfl,
         fam_suppt = 1000 * fam_suppt / gdp_dfl,
         yp_svcs = 1000 * yp_svcs / gdp_dfl,
         cap_ex = 1000 * cap_ex / gdp_dfl)

#### Calculate Counts ####
sr <- mutate(sr,
             sr_count = round(school_readiness * n_sr / 100),
             sr_fsm_count = round(school_readiness_fsm * n_sr_fsm / 100),
             sr_nonfsm_count = sr_count - sr_fsm_count,
             n_sr_nonfsm = n_sr - n_fsm)

#### Write panel data set to csv ####
write.csv(sr, "school_readiness_panel.csv")