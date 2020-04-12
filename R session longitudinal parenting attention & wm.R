# Longitudinal dataset from FLP: parenting, attention & WM


# packages ----------------------------------------------------------------

install.packages("prettyR")
install.packages("naniar")
library(naniar)
library(prettyR)
library(tidyverse)
install.packages("xlsx")
library(xlsx)
library(psych)
library(PerformanceAnalytics)


# data file ---------------------------------------------------------------

ECBQ <- read.csv("ECBQ.csv")
income_6month <- read.csv("income_6month.csv")
income_15month <- read.csv("income_15month.csv")
income_24month <- read.csv("income_24month.csv")
income_35month <- read.csv("income_35month.csv")
parent_6month <- read.csv("Parent_6month.csv")
parent_15month <- read.csv("Parent_15month.csv")
parent_24month <- read.csv("Parent_24month.csv")
parent_35month <- read.csv("Parent_35month.csv")
verbal <- read_csv("verbal.csv")
verbal <- read.csv("verbalcount.csv")
study1 <- read.csv("study1.csv")


# verbal working memory ---------------------------------------------------

da34602.0026 <- as_tibble(da34602.0026)
verbal <- select(da34602.0026, S_ID, TAGE, BWSSCORE)
verbal <- mutate (verbal, age_in_month = str_sub(verbal$TAGE, 2, 3))
verbal <- select(verbal, S_ID, BWSSCORE, age_in_month)

write.csv(verbal, file = "verbal.csv", row.names = FALSE)

wordspan <- verbal %>% 
  select (S_ID, wordspan = BWSSCORE)

dplyr::count(wordspan, wordspan)


# Attentional focusing & shifting -----------------------------------------

da34602.0037 <- as_tibble(da34602.0037)

ECBQ <- da34602.0037 %>% 
  select(S_ID, TAGE, ECBQIMPUL:ECBQSAD)

write.csv(ECBQ, file = "ECBQ.csv", row.names = FALSE)

attention <- ECBQ %>% 
  select(S_ID, attfocus24 = ECBQATTNF, attshift24 = ECBQATTNS)


# Parenting ---------------------------------------------------------------


parent <- da34602.0076 %>% 
  select(S_ID, TAGE, WHO, PCX_SENN:PCX_ANIM, NPCX_SENN, NPCX_INTR, NPCX_DETA, NPCX_PREG, NPCX_NREG, NPCX_ANIM, NPCX_STIM)

parent <- mutate (parent, SENN = str_sub(parent$PCX_SENN, 2, 2),
                  INTR = str_sub(parent$PCX_INTR, 2, 2),
                  DETA = str_sub(parent$PCX_DETA, 2, 2),
                  STIM = str_sub(parent$PCX_STIM, 2, 2),
                  PREG = str_sub(parent$PCX_PREG, 2, 2),
                  NREG = str_sub(parent$PCX_NREG, 2, 2),
                  ANIM = str_sub(parent$PCX_ANIM, 2, 2),
                  N_SENN = str_sub(parent$NPCX_SENN, 2, 2),
                  N_INTR = str_sub(parent$NPCX_INTR, 2, 2),
                  N_DETA = str_sub(parent$NPCX_DETA, 2, 2),
                  N_STIM = str_sub(parent$NPCX_STIM, 2, 2),
                  N_PREG = str_sub(parent$NPCX_PREG, 2, 2),
                  N_NREG = str_sub(parent$NPCX_NREG, 2, 2),
                  N_ANIM = str_sub(parent$NPCX_ANIM, 2, 2))

parent <- mutate(parent, NWHO = str_sub(parent$WHO, 2, 2))

parent <- parent %>% 
  select (S_ID, TAGE, NWHO, SENN:N_ANIM)

parent <- parent %>% 
  filter(NWHO == 1)

parent <- parent %>% 
  select (- NWHO)

Parent_6month <- parent %>% 
  filter (TAGE == 6)

Parent_15month <- parent %>% 
  filter (TAGE == 15)

Parent_24month <- parent %>% 
  filter (TAGE == 24)

Parent_35month <- parent %>% 
  filter (TAGE == 35)

write.csv(Parent_6month, file = "Parent_6month.csv", row.names = FALSE)
write.csv(Parent_15month, file = "Parent_15month.csv", row.names = FALSE)
write.csv(Parent_24month, file = "Parent_24month.csv", row.names = FALSE)
write.csv(Parent_35month, file = "Parent_35month.csv", row.names = FALSE)

#### parenting 

## parenting 6 month

describe(income_6month, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE,omit=FALSE)

parent_6month <- parent_6month %>% 
  mutate(INTR_R = 5 - INTR, DETA_R = 5 - DETA, NREG_R = 5 - NREG) %>% 
  mutate(parent6m = (SENN + INTR_R + DETA_R + STIM + PREG + NREG_R + ANIM)/7)

parent_6month <- parent_6month %>% 
  select(S_ID, parent6m)

## parenting 15 month

parent_15month <- parent_15month %>% 
  mutate(INTR_R = 5 - INTR, DETA_R = 5 - DETA, NREG_R = 5 - NREG) %>% 
  mutate(parent15m = (SENN + INTR_R + DETA_R + STIM + PREG + NREG_R + ANIM)/7)

parent_15month <- parent_15month %>% 
  select(S_ID, parent15m)

## parenting 24 month

parent_24month <- parent_24month %>% 
  mutate(INTR_R = 5 - INTR, DETA_R = 5 - DETA, NREG_R = 5 - NREG) %>% 
  mutate(parent24m = (SENN + INTR_R + DETA_R + STIM + PREG + NREG_R + ANIM)/7)

parent_24month <- parent_24month %>% 
  select(S_ID, parent24m)

## parenting 35 month

parent_35month <- parent_35month %>% 
  mutate(INTR_R = 5 - INTR, DETA_R = 5 - DETA, NREG_R = 5 - NREG) %>% 
  mutate(parent35m = (SENN + INTR_R + DETA_R + STIM + PREG + NREG_R + ANIM)/7)

parent_35month <- parent_35month %>% 
  select(S_ID, parent35m)

### merge data
parenting <- left_join(parent_6month, parent_15month, by = "S_ID")
parenting <- left_join(parenting, parent_24month, by = "S_ID")
parenting <- left_join(parenting, parent_35month, by = "S_ID")


# Family income -----------------------------------------------------------

da34602.0062 <- as_tibble(da34602.0062)
income <- as_tibble(income)
income <- da34602.0062 %>% 
  select (S_ID, TAGE, INRATIOCOR)

income <- replace_with_na(income, replace = list (INRATIOCOR = 9993.0000000))

income_6month <- income %>% 
  filter(TAGE == 6)

income_15month <- income %>% 
  filter(TAGE == 15)

income_24month <- income %>% 
  filter(TAGE == 24)

income_35month <- income %>% 
  filter(TAGE ==35)

write.csv(income_6month, file = "income_6month.csv", row.names = FALSE)
write.csv(income_15month, file = "income_15month.csv", row.names = FALSE)
write.csv(income_24month, file = "income_24month.csv", row.names = FALSE)
write.csv(income_35month, file = "income_35month.csv", row.names = FALSE)

#### income at 6 month
income_6month <- income_6month %>% 
  select(S_ID, ratio_6m = INRATIOCOR)

income_6month <- income_6month %>% 
  dplyr::na_if(9993)

#### income at 15 month
income_15month <- income_15month %>% 
  select(S_ID, ratio_15m = INRATIOCOR)

income_15month <- income_15month %>% 
  dplyr::na_if(9993)

#### income at 24 month
income_24month <- income_24month %>% 
  select(S_ID, ratio_24m = INRATIOCOR)

income_24month <- income_24month %>% 
  dplyr::na_if(9993)

#### income at 35 month
income_35month <- income_35month %>% 
  select(S_ID, ratio_35m = INRATIOCOR)

income_35month <- income_35month %>% 
  dplyr::na_if(9993)

#### merge data

income <- left_join(income_6month, income_15month, by = "S_ID")
income <- left_join(income,        income_24month, by = "S_ID")
income <- left_join(income,        income_35month, by = "S_ID")

#### EXPORT
write.csv(income, file = "income.csv", row.names = FALSE)
