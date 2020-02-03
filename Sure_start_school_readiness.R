

#### Load packages ####
library(fingertipsR)   # For getting PHE data
library(tidyverse)     # For everything!
library(readxl)        # For reading excel files
library(cowplot)       # For pretty graphs
library(pglm)          # For generalised panel models

#### Extract outcome variable indicators ####
sr_ids <- c(90631, # Child development
            90632) # Child developmet - free school meals

outcomes <- fingertips_data(IndicatorID = sr_ids) 

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

pred_inds_p <- c(10101) # % children in low income families (under 16s)  

predsp <- fingertips_data(IndicatorID = pred_inds_p) 

predsp <- predsp %>%
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

sr <- merge(outcomes, predsp, 
            by = c("AreaName", "AreaCode", "ParentName", "Timeperiod"))

# Clear up worksspace
rm(outcomes, predsp)

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
  mutate(Timeperiod = 2012)

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
  select(-tempcode)

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
  select(-tempcode)

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
  select(-tempcode)

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
  select(-tempcode)

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
  select(-tempcode)

# Merge spending data across years into single data frame

spend_1213 <- select(spend_1213, -tempcode)

spend <- rbind(spend_1213,
               spend_1314,
               spend_1415,
               spend_1516,
               spend_1617,
               spend_1718) %>%
  mutate_at(vars(-AreaCode, -Timeperiod),
            as.numeric)

# Clear up workspace
rm(spend_1213, spend_1314, spend_1415, spend_1516, spend_1617, spend_1718, spend_urls)

# Merge spending data with main dataset
sr <- merge(sr, spend, by = c("AreaCode", "Timeperiod"),
            all.x = TRUE)

# Calculate total non-Sure Start spending
sr <- mutate(sr, non_ss_spend = spend_total - sure_start)

# Get total population and under-18 population from Fingertips
pop_data <- fingertips_data(92309)

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
             sr_fsm_count = round(school_readiness_fsm * n_sr_fsm / 100))

#### Write panel data set to csv ####
write.csv(sr, "school_readiness_panel.csv")


#### Load panel data set ####

# Load panel data set
panel <- read.csv("school_readiness_panel.csv",
                  stringsAsFactors = FALSE,
                  header = TRUE)

# Filter out City of London and Isles of Scilly and 2017
panel <- filter(panel,
                AreaName != "City of London",
                AreaName != "Isles of Scilly",
                Timeperiod != 2017)

#### Table 1: variable summary statistics ####
to_summarise <- panel %>%
  filter(Timeperiod == "2016") %>%
  select(school_readiness,
         school_readiness_fsm,
         child_pov_u16,
         sure_start,
         non_ss_spend)

summary_mean <- summarise_all(to_summarise,
                              list(mean),
                              na.rm = TRUE)

summary_sd <- summarise_all(to_summarise,
                            list(sd),
                            na.rm = TRUE)

summary_median <- summarise_all(to_summarise,
                                list(median),
                                na.rm = TRUE)

range_sum <- function(x){
  l <- round(range(x, na.rm = TRUE)[1], digits = 1)
  u <- round(range(x, na.rm = TRUE)[2], digits = 1)
  r <- paste(l, "-", u, sep = " ")
  return(r)
}

summary_range <- summarise_all(to_summarise,
                               list(range_sum))

summary_iqr <- summarise_all(to_summarise,
                             list(IQR),
                             na.rm = TRUE)

table_1 <- rbind(summary_mean,
                 summary_sd,
                 summary_median,
                 summary_iqr,
                 summary_range) %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(variable = 1,
         mean = 2,
         SD = 3,
         median = 4,
         IQR = 5,
         range = 6) %>%
  mutate(variable = c("school readiness (% of all children)",
                      "school readiness (% of FSM children)",
                      "child poverty rate (% of under-16s)",
                      "Sure Start spending (2012 £ per child)",
                      "Non-Sure Start children's services spending (2012 £ per child)")) %>%
  mutate_at(2:5, as.character) %>%
  mutate_at(2:5, as.numeric) %>%
  mutate_at(2:5, round, digits = 1)

write.csv(table_1,
          file = "table1.csv",
          row.names = FALSE)

#### Table 2: Variable summaries by year ####
outcomes <- panel %>%
  select(year = Timeperiod, 
         school_readiness, 
         school_readiness_fsm) %>%
  group_by(year) %>% 
  summarise(sr_min = min(school_readiness),
            sr_1q = summary(school_readiness)[2],
            sr_med = median(school_readiness, na.rm = TRUE),
            sr_3q = summary(school_readiness)[5],
            sr_max = max(school_readiness),
            sr_fsm_min = min(school_readiness_fsm),
            sr_fsm_1q = summary(school_readiness_fsm)[2],
            sr_fsm_med = median(school_readiness_fsm, na.rm = TRUE),
            sr_fsm_3q = summary(school_readiness_fsm)[5],
            sr_fsm_max = max(school_readiness_fsm))

#### Figure 1: Plot trends in outcome variables ####

fig1 <- ggplot(aes(x = year),
               data = outcomes) +
  geom_line(aes(y = sr_med)) +
  geom_line(aes(y = sr_fsm_med),
            linetype = "dashed") +
  geom_ribbon(aes(ymin = sr_1q,
                  ymax = sr_3q),
              alpha = 0.3) +
  geom_ribbon(aes(ymin = sr_fsm_1q,
                  ymax = sr_fsm_3q),
              alpha = 0.3) +
  ylim(25, 80) +
  theme_cowplot() +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(hjust = 0,
                                    size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9)) +
  labs(x = NULL,
       y = "%",
       title = "Figure 1: trends in school readiness",
       subtitle = "English local authorities, 2012 - 2016",
       caption = str_wrap("Trends in school readiness for all children (solid line) and children eligible for free school meals (dashed line) for 2012 - 2016. Graph shows median (lines) and interquartile range (grey area).",
                          width = 90))

ggsave(filename = "figure1.tiff",
       plot = fig1,
       width = 10,
       height = 8,
       units = "cm",
       device = "tiff",
       dpi = "print")

#### Summarise spending by year ####
spend <- panel %>%
  select(id = AreaName,
         year = Timeperiod,
         sure_start, spend_total) %>%
  group_by(year) %>%
  summarise(ss_med = median(sure_start, na.rm = TRUE),
            ss_min = min(sure_start, na.rm = TRUE),
            ss_1q = summary(sure_start)[2],
            ss_3q = summary(sure_start)[5],
            ss_max = max(sure_start, na.rm = TRUE),
            ts_med = median(spend_total, na.rm = TRUE),
            ts_min = min(spend_total, na.rm = TRUE),
            ts_1q = summary(spend_total)[2],
            ts_3q = summary(spend_total)[5],
            ts_max = max(spend_total, na.rm = TRUE)) 

#### Figure 2: Changes in sure start & children's services spending over time ####

fig2a <- ggplot(aes(x = year,
                    y = ts_med),
                data = spend) +
  geom_line() +
  geom_ribbon(aes(ymin = ts_1q,
                  ymax = ts_3q),
              alpha = 0.3) +
  theme_cowplot() +
  theme(plot.title = element_text(face = "plain",
                                  size = 12),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 9)) +
  labs(x = NULL,
       y = "£s per child",
       title = "Children's Services")

fig2b <- ggplot(aes(x = year,
                    y = ss_med),
                data = spend) +
  geom_line() +
  geom_ribbon(aes(ymin = ss_1q,
                  ymax = ss_3q),
              alpha = 0.3) +
  theme_cowplot() +
  theme(plot.title = element_text(face = "plain",
                                  size = 11),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 9)) +
  labs(x = NULL,
       y = "£s per child",
       title = "Sure Start")

fig2_title <- ggplot() +
  labs(title = "Figure 2: Spending on Children's Services and Sure Start",
       subtitle = "English Local Authorities, 2012 - 2016") +
  theme_cowplot() +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10))

fig2_body <- plot_grid(fig2a, fig2b,
                       labels = "auto")

fig2_caption <- ggplot() +
  labs(caption = str_wrap("Trends in per child spending on all children's services and Sure Start, 2012-2016. Spending is £000s per child aged 0-17 years in constant 2012 pounds. Graph shows median (black line) and interquartile range (grey area).",
                          width = 60)) +
  theme_cowplot() + 
  theme(plot.caption = element_text(hjust = 0,
                                    size = 9))

fig2 <- plot_grid(fig2_title,
                  fig2_body,
                  fig2_caption,
                  rel_heights = c(0.2, 1, 0.25),
                  ncol = 1)

ggsave(filename = "figure2.tiff",
       plot = fig2,
       width = 11,
       height = 10,
       units = "cm",
       device = "tiff",
       dpi = "print")

#### Regression models ####

# Fixed effects poisson model for school readiness - all children
mf_sr_all <- pglm(sr_count ~ offset(log(n_sr)) + 
                    child_pov_u16 +
                    log(sure_start+1) +
                    log(non_ss_spend+1) +
                    Timeperiod,
                  data = panel,
                  index = c("AreaName", "Timeperiod"),
                  model = "within",
                  effect = "individual",
                  family = poisson)

# Fixed effects poisson model for school readiness - FSM children
mf_sr_fsm <- pglm(sr_fsm_count ~ offset(log(n_sr_fsm)) +
                    child_pov_u16 +
                    log(sure_start+1) +
                    log(non_ss_spend+1) +
                    Timeperiod,
                  data = panel,
                  index = c("AreaName", "Timeperiod"),
                  model = "within",
                  effect = "individual",
                  family = poisson)

#### Collating regression coefficients ####
coef_names <- rownames(summary(mf_sr_all)$estimate)

sr_all_coef <- as.data.frame(summary(mf_sr_all)$estimate) %>%
  mutate(outcome = "school readiness",
         coef_name = coef_names,
         population = "all children")

sr_fsm_coef <- as.data.frame(summary(mf_sr_fsm)$estimate) %>%
  mutate(outcome = "school readiness",
         coef_name = coef_names,
         population  = "FSM children")

coef_table <- rbind(sr_all_coef,
                    sr_fsm_coef)

names(coef_table)[1:4] <- c("coefficient", "se", "t", "p")

coef_table <- mutate(coef_table,
                     ci_lower = coefficient - 1.96 * se,
                     ci_upper = coefficient + 1.96 * se,
                     sig = ci_lower  > 0 | ci_upper < 0,
                     `95% CI` = paste0(signif(coefficient, digits = 2),
                                       " (",
                                       signif(ci_lower, digits = 2), 
                                       " to ", 
                                       signif(ci_upper, digits = 2), 
                                       ")"))

coef_table$coef_name[coef_table$coef_name == "log(sure_start + 1)"] <- "Sure Start spend (log)"
coef_table$coef_name[coef_table$coef_name == "log(non_ss_spend + 1)"] <- "non-Sure Start spend (log)"
coef_table$coef_name[coef_table$coef_name == "child_pov_u16"] <- "Child poverty rate"

#### Figure 3: Dot and whisker plot for regression coefficients ####

fig3_title = ggplot() +
  labs(title = "Figure 3: Coefficient Summaries",
       subtitle = "Estimates and 90% confidence intervals") +
  theme_cowplot() +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10))

fig3_body <- ggplot(aes(x = factor(population,
                                   levels = c("FSM children",
                                              "all children")),
                        y = coefficient,
                        colour = !sig,
                        fill = !sig),
                    data = filter(coef_table,
                                  !grepl("Time", coef_name))) +
  geom_point() +
  geom_linerange(aes(ymin = ci_lower,
                     ymax = ci_upper)) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  facet_wrap(facets = vars(coef_name),
             scales = "free_x",
             as.table = TRUE) +
  expand_limits(y = -0.03) +
  coord_flip() +
  scale_fill_grey(guide = FALSE) +
  scale_colour_grey(guide = FALSE) +
  theme_cowplot() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 9),
        panel.border = element_rect(colour = "black",
                                    size = 1.5)) +
  labs(x = NULL)

fig3_caption <- ggplot() +
  labs(caption = str_wrap("Regression coefficients (points) and 95% confidence intervals (lines) for Sure Start spending, non-Sure Start spending, and child poverty rates. Coefficients are adjusted for local authority and year fixed effects. Sure Start and non-Sure Start spending variables are log-transformed. Dashed lines indicate no effect. Coefficients plotted in grey were not statistically significantly different from 0 at the 0.05 level. FSM = free school meals.",
                          width = 100)) +
  theme_cowplot() +
  theme(plot.caption = element_text(size = 9,
                                    hjust = 0))

fig3 <- plot_grid(fig3_title,
                  fig3_body,
                  fig3_caption,
                  ncol = 1,
                  rel_heights = c(0.25, 1, 0.3))

ggsave(filename = "figure3.tiff",
       plot = fig3,
       width = 14,
       height = 10,
       units = "cm",
       device = "tiff",
       dpi = "print")

#### Table 2: regression coefficients ####

table_2 <- coef_table %>%
  select(coef_name, outcome, population, `95% CI`) %>%
  unite(model, outcome, population) %>%
  spread(key = model, value = `95% CI`) %>%
  select(Coefficient = coef_name,
         `School Readiness` = `school readiness_all children`,
         `School Readiness (FSM)` = `school readiness_FSM children`) %>%
  filter(!grepl("Timeperiod", Coefficient)) %>%
  arrange(desc(Coefficient))

table_2$Coefficient <- c("Sure Start spending (log 2012 £s per child)",
                         "non-Sure Start spending (log 2012 £s per child)",
                         "Child poverty rate (% of children under 16)")

write.csv(table_2,
          file = "table2.csv",
          row.names = FALSE)

#### Calculate elasticities for 10% change in Sure Start spending ####

(1.1^coef(mf_sr_all)[3] - 1) * 100
(1.1^coef(mf_sr_fsm)[3] - 1) * 100

