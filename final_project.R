library(dplyr)
getwd()
setwd("C:/Users/hehao/Downloads/506_final")
demo = haven::read_xpt("DEMO_J.XPT") %>% 
  select(id = SEQN, status = RIDSTATR, weight = WTMEC2YR) %>%
  filter(status == 2)
bp = haven::read_xpt("BPX_J.XPT") %>%
  select(id = SEQN, BPXSY1, BPXSY2, BPXSY3, BPXDI1, BPXDI2, BPXDI3) %>%
  filter(BPXSY1 >= 0 & BPXSY2 >=0 & BPXSY3 >= 0) %>%
  filter(BPXDI1 >= 0 & BPXDI2 >=0 & BPXDI3 >= 0) %>%
  mutate(BPXSY = (BPXSY1 + BPXSY2 + BPXSY3)/3)
sleep = haven::read_xpt("SLQ_J.XPT") %>%
  select(id = SEQN, wkday_hour = SLD012, wkend_hour = SLD013,
         told_doctor = SLQ050, over_sleepy = SLQ120)

