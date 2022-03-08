dat <- readxl::read_excel("Statystyka pigs EDTA.xlsx", sheet = 1) %>% 
  pivot_longer(cols = -`Compound`) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(group_label = ifelse(grepl(pattern = "_T0", x = name), "control", "case"))


group_label <- unique(dat[["group_label"]])
cmp <- unique(dat[["Compound"]])

shp_test <- lapply(group_label, function(ith_group) {
  lapply(compound, function(ith_compound) {
    print(ith_compound)
    dat %>% 
      filter(group_label == ith_group, `Compound Method` == ith_compound) %>%
      pull(value) %>%
      shapiro.test() %>%
      getElement("p.value") %>%
      data.frame(group_label = ith_group, 
                 Compound = ith_compound, 
                 pval = .)
  }) %>% bind_rows()
}) %>%
  bind_rows() %>%
  group_by(group_label) %>% 
  mutate(adjusted_pval = p.adjust(pval, method = "BH"))
})



dat %>% 
  filter(`Compound` == ith_compound) %>%
  t.test(value ~ group_label, 
         data = .,
         paired = is_paired) %>% 
  getElement("p.value")


vt <- lapply(cmp, function(ith_compound) {
  try({
    dat %>% 
      filter(`Compound` == ith_compound) %>%
      t.test(value ~ group_label, 
             data = .,
             paired = is_paired) %>% {
               data.frame(t(.[["estimate"]]), pval = .[["p.value"]])
             } %>% 
      cbind(
        data.frame(test = "T-test",
                   Compound = ith_compound
        ), 
        .)
  }, silent = TRUE)
}) 


df <- bind_rows(vt[-length(vt)])
library(ggplot2)
library(tidyr)

filter(df, pval < 0.05) %>% 
  select(-test, -pval) %>% 
  pivot_longer(cols = -Compound) %>% 
  mutate(name = gsub(pattern = "mean.in.group.", replacement = "", name)) %>% 
  ggplot(aes(x = value, y = Compound, color = name)) +
  geom_point()




