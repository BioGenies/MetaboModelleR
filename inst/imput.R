library(readr)
library(dplyr)
zbior <- read_csv("inst/app/data/missing/Statystyka_pigs_EDTA_missing.csv")
zbior %>% 
  select(-c(Compound, ...1)) %>% 
  as.matrix() %>% 
  impute::impute.knn() -> temp
temp$data %>% 
  as.data.frame() %>% 
  mutate('Compound' = zbior$Compound, .before = 'S1_EDTA_T0') -> temp
