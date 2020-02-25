# This script checks that the original analysis holds when 
# Sure Start spending is taken from s251 returns 

#### Load packages ####
library(tidyverse)     # For everything!
library(readxl)        # For reading excel files
library(cowplot)       # For pretty graphs
library(pglm)          # For generalised panel models

#### Load main panel dataset ####

# Check that panel data set exists
if(!file.exists("school_readiness_panel.csv")){
  source("Sure_start_school_readiness_build_panel.R")
}

# Load panel data set
panel <- read.csv("school_readiness_panel.csv",
                  stringsAsFactors = FALSE,
                  header = TRUE)

#### Download s251 data from pldr.org ####

# Not able to download in script. 
# Data available at:
# https://pldr.org/dataset/2zgpe/local-authority-finance-childrens-and-young-peoples-services-s251-sure-start-childrens-centres-and-early-years-services-fin_07_51

s251 <- read.csv("sure_start_s251.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)

# Fix area codes for Northumberland and Gateshead
s251$UTLA18CD[s251$UTLA18NM == "Gateshead"] <- unique(panel$AreaCode[panel$AreaName == "Gateshead"])
s251$UTLA18CD[s251$UTLA18NM == "Northumberland"] <- unique(panel$AreaCode[panel$AreaName == "Northumberland"])


# Merge with panel dataset
panel_s251 <- merge(panel, s251,
                    by.x = c("Timeperiod", "AreaCode"),
                    by.y = c("Year", "UTLA18CD"),
                    all.x = TRUE,
                    all.y = FALSE)
                    
# Calculate inflation-adjusted Sure Start spend
panel_s251 <- panel_s251 %>%
  mutate(sure_start_s251 = TotalExpenditure_PerChild / gdp_dfl)

#### Plot original sure start spending data against sure start (s251) data ####
qplot(x = sure_start,
      y = sure_start_s251,
      data = panel_s251) +
  theme_cowplot()

# Looks like the two datasets are all but identical. 
# Repeat regression analysis just in case.

#### Fit regression models ####

# Fixed effects poisson model for school readiness - all children
mf_sr_all <- pglm(sr_count ~ offset(log(n_sr)) + 
                    child_pov_u16 +
                    log(sure_start_s251+1) +
                    log(non_ss_spend+1) +
                    Timeperiod,
                  data = panel_s251,
                  index = c("AreaName", "Timeperiod"),
                  model = "within",
                  effect = "individual",
                  family = poisson)

# Fixed effects poisson model for school readiness - FSM children
mf_sr_fsm <- pglm(sr_fsm_count ~ offset(log(n_sr_fsm)) +
                    child_pov_u16 +
                    log(sure_start_s251+1) +
                    log(non_ss_spend+1) +
                    Timeperiod,
                  data = panel_s251,
                  index = c("AreaName", "Timeperiod"),
                  model = "within",
                  effect = "individual",
                  family = poisson)

# Fixed effects poisson model for school readiness - non-FSM children
mf_sr_nonfsm <- pglm(sr_nonfsm_count ~ offset(log(n_sr_nonfsm)) +
                       child_pov_u16 +
                       log(sure_start_s251+1) +
                       log(non_ss_spend+1) +
                       Timeperiod,
                     data = panel_s251,
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

sr_nonfsm_coef <- as.data.frame(summary(mf_sr_nonfsm)$estimate) %>%
  mutate(outcome = "school readiness",
         coef_name = coef_names,
         population  = "non-FSM children")

coef_table_s251 <- rbind(sr_all_coef,
                         sr_fsm_coef,
                         sr_nonfsm_coef)

names(coef_table_s251)[1:4] <- c("coefficient", "se", "t", "p")

coef_table_s251 <- mutate(coef_table,
                          ci_lower = coefficient - 1.96 * se,
                          ci_upper = coefficient + 1.96 * se,
                          sig = ci_lower  > 0 | ci_upper < 0,
                          `95% CI` = paste0(signif(coefficient, digits = 2),
                                            " (",
                                            signif(ci_lower, digits = 2), 
                                            " to ", 
                                            signif(ci_upper, digits = 2), 
                                            ")"))


#### Figure 3: Dot and whisker plot for regression coefficients ####

fig3_title = ggplot() +
  labs(title = "Figure 3: Coefficient Summaries",
       subtitle = "Estimates and 90% confidence intervals") +
  theme_cowplot() +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10))

fig3_body <- ggplot(aes(x = factor(population,
                                   levels = c("non-FSM children",
                                              "FSM children",
                                              "all children")),
                        y = coefficient,
                        colour = !sig,
                        fill = !sig),
                    data = filter(coef_table_s251,
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

fig3_s251 <- plot_grid(fig3_title,
                  fig3_body,
                  fig3_caption,
                  ncol = 1,
                  rel_heights = c(0.25, 1, 0.3))

ggsave(filename = "figure3_s251.tiff",
       plot = fig3,
       width = 14,
       height = 10,
       units = "cm",
       device = "tiff",
       dpi = "print")

#### Table 2: regression coefficients ####

table_2 <- coef_table_s251 %>%
  select(coef_name, outcome, population, `95% CI`) %>%
  unite(model, outcome, population) %>%
  spread(key = model, value = `95% CI`) %>%
  select(Coefficient = coef_name,
         `School Readiness` = `school readiness_all children`,
         `School Readiness (FSM)` = `school readiness_FSM children`,
         `School Readiness (non-FSM)` = `school readiness_non-FSM children`) %>%
  filter(!grepl("Timeperiod", Coefficient)) %>%
  arrange(desc(Coefficient))

table_2$Coefficient <- c("Sure Start spending (log 2012 £s per child)",
                         "non-Sure Start spending (log 2012 £s per child)",
                         "Child poverty rate (% of children under 16)")

# Results are the same
