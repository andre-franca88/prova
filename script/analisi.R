library(tidyverse)
library(here)
library(lubridate)
library(skimr)
library(expss)
library(tidyverse)
library(here)
library(lubridate)
library(skimr)
library(gtsummary)



#importiamo dataset

# data import and cleaning ------------------------------------------------


dati <- read_csv(here("data_raw",
                      "CorrectTimingOfHeadI_DATA_2022-03-23_1244.csv"))



##manipolo db

data_clean <- dati %>% 
  mutate(
    eta=interval(
      data_di_nascita, data_trauma) %/% years(1),
    tc_cat=if_else(
      esito_tc___2==1|   ## con questa dicotomica individuo pazienti con 
      esito_tc___3==1|   ## qualsiasi tipo di emorragia ( 1) vs no (0)
      esito_tc___4==1|
      esito_tc___6==1|
      esito_tc___7==1 &
      esito_tc___1!=1 &
      esito_tc___8!=1,
      
      1, 0
    ),
    sesso=as.factor(sesso)
  ) %>% 
  mutate(
    across(c(
      dinamica_del_trauma:motivo_terapia_anticoagula___4
    ), as.factor)

)

# DATASET PAZIENTI UNICI PER ANAGRAFICA/ ANAMNESI 
# filtro per la variabile "sesso" compilata ( non ha missing veri)

not_all_na <- function(x) any(!is.na(x)) ## f per eliminare righe dove all_NA

pazienti_unici <- data_clean %>% ## credo db di pazienti unici, applico f
  filter(!is.na(sesso)) %>%      ## per trattenere var dove not_all_NA
  select(where(not_all_na))
  
### DATASET PAZIENTI per le schede ripetibili sulle TC

tc_successive <- data_clean %>% 
  filter(is.na(sesso) & redcap_repeat_instance==1) %>% 
  select(where(not_all_na))

## converto il dataset contenente le info SOLO sulle schede ripetibili TC
## da formato long a formato wide


tc_wide<-pivot_wider(tc_successive, 
               id_cols = study_id,
               names_from = redcap_repeat_instance,
               values_from = c(nuovi_sintomi_paziente___1:tc_cat))

## faccio un merge tra il dataset contentente tutte le TC e il dataset 
## contentente le info anagrafica / anamnesi

db_join <- left_join(pazienti_unici, tc_wide, by="study_id")

tc_successive %>% 
  filter  (esito_tc___1==1) %>% 
  filter  ( esito_tc___2==1|
           esito_tc___3==1| esito_tc___4==1|
           esito_tc___5==1|esito_tc___6==1|
           esito_tc___7==1)

prova <- pazienti_unici %>% filter(tc_positiva_reingresso==1 | tc_positiva_reingresso==0 ) %>% select(study_id)
prova2 <- pazienti_unici %>% filter(eseguita_tc_al_reingresso==1) %>% select(study_id)                     

data_clean %>% group_by(redcap_repeat_instance,tc_cat )
  filter(redcap_repeat_instance==2) %>% 
  filter(esito_tc___8==1) %>% 
  filter(esito_tc___2==1| esito_tc___3==1| esito_tc___4==1| esito_tc___5==1|
           esito_tc___6==1| esito_tc___7==1| esito_tc___1==1)



seconda_tc <- data_clean %>% 
  filter(is.na(sesso) & redcap_repeat_instance==2) %>% 
  select(where(not_all_na))
