data_clean %>% 
  filter(!is.na(eta)) %>% 
  select(eta) %>% 
  skim()


data_clean %>% 
  arrange(desc(eta)) %>% 
  select(study_id, eta) %>% 
  view()


data_clean %>% 
  filter(!is.na(eta)) %>%
  filter(eta<35 | eta>105) %>% 
  select(study_id, eta) %>% 
  view()
write_csv(here("analisi", "eta_errata.csv"))