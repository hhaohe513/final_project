library(dplyr)
library(survey)
getwd()
setwd("C:/Users/hehao/Downloads/506_final/final_project/data")
# Load data and do some manipulation
demo = haven::read_xpt("DEMO_J.XPT") %>% 
  select(id = SEQN, status = RIDSTATR, weight = WTMEC2YR, age = RIDAGEYR,
         gender = RIAGENDR, psu = SDMVPSU, strata = SDMVSTRA) %>%
  filter(status == 2)
bp = haven::read_xpt("BPX_J.XPT") %>%
  select(id = SEQN, BPXSY1, BPXSY2, BPXSY3, BPXDI1, BPXDI2, BPXDI3) %>%
  filter(BPXSY1 >= 0 & BPXSY2 >=0 & BPXSY3 >= 0) %>%
  filter(BPXDI1 >= 0 & BPXDI2 >=0 & BPXDI3 >= 0) %>%
  mutate(BPXSY = (BPXSY1 + BPXSY2 + BPXSY3)/3) %>% 
  mutate(BPXDI = (BPXDI1 + BPXDI2 + BPXDI3)/3)
sleep = haven::read_xpt("SLQ_J.XPT") %>%
  select(id = SEQN, wkday_hour = SLD012, wkend_hour = SLD013,
         told_doctor = SLQ050, over_sleepy = SLQ120) %>%
  filter(wkday_hour < 15 & wkend_hour < 15 & told_doctor < 7 & 
           over_sleepy < 5) %>%
  mutate(is_disorder = ifelse(wkday_hour < 6 |
                                wkend_hour < 7 |
                                told_doctor == 1 |
                                over_sleepy > 3,1,0))
merged = demo %>%
  inner_join(sleep, by = "id") %>%
  inner_join(bp, by = "id") %>%
  select(id, age, gender, weight, psu, strata, is_disorder, BPXSY, BPXDI)

# Use svydesign for point estimate and confidence interval
svy = svydesign(ids = ~psu, weights = ~weight, strata = ~strata,
                data = merged, nest = TRUE)
mean_se = as.data.frame(svyby(~BPXSY + BPXDI,
                              by = ~is_disorder, svy,  svymean))
## Generate a nice table for presentation
nice_table = data.frame("Variables" = c("BPXSY","BPXDI"),
                        "mean_disorder" = c(mean_se[2,2],mean_se[2,3]),
                        "mean_normal" = c(mean_se[1,2],mean_se[1,3]),
                        "se_disorder" = c(mean_se[2,4],mean_se[2,5]),
                        "se_normal" = c(mean_se[2,4],mean_se[2,5])) %>%
  mutate(mean_diff = mean_disorder - mean_normal) %>%
  mutate(lwr_diff = mean_diff - sqrt(se_disorder^2 + se_normal^2)*qnorm(0.975),
         upr_diff = mean_diff + 
           sqrt(se_disorder^2 + se_normal^2)*qnorm(0.975)) %>%
  select(Variables, mean_disorder, mean_normal, mean_diff, lwr_diff, upr_diff)

# use svyglm for regression analysis