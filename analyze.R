library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

theme_set(theme_minimal())

raw_dat <- read_excel("Example.xlsx")

dat <- pivot_longer(data = raw_dat, cols = -Compound) %>% 
  mutate(group_label = substr(name, 0, 2))

group_label <- unique(dat[["group_label"]])

shp_pval <- lapply(unique(dat[["Compound"]]), function(ith_cmp) {
  lapply(group_label, function(ith_group) {
    filter(dat, Compound == ith_cmp, group_label == ith_group) %>% 
      pull(value) %>% 
      shapiro.test() %>% 
      getElement("p.value") %>% 
      data.frame(Compound = ith_cmp, group_label = ith_group, pval = .)
  }) %>% 
    bind_rows()
}) %>% 
  bind_rows() %>% 
  mutate(apval = p.adjust(pval, method = "BH"))

dist_plot <- lapply(unique(dat[["Compound"]]), function(ith_cmp) {
  part_dat <- filter(dat, Compound == ith_cmp)
  
  hist <- ggplot(part_dat, aes(x = value, fill = group_label)) + 
    geom_histogram() +
    xlab("") +
    ggtitle("Histogram")
  
  qqplot <- ggplot(part_dat, aes(sample = value, color = group_label)) + 
    stat_qq() + 
    stat_qq_line() +
    ggtitle("Quantile-quantile chart")
  
  boxplot <- ggplot(part_dat, aes(x="", y = value, fill = group_label)) +
    geom_boxplot() +
    ggtitle("Boxplot")
  
  (hist + boxplot + qqplot)* facet_wrap(~ group_label, ncol = 1)* theme(legend.position = "bottom") + plot_annotation(paste0("Compound ", ith_cmp))
})


diff_pval_t <- sapply(unique(dat[["Compound"]]), function(ith_cmp) {
  filter(dat, Compound == ith_cmp) %>% 
    t.test(value ~ group_label, data = .) %>% 
    getElement("p.value")
}, USE.NAMES = FALSE) %>% 
  data.frame(Compound = unique(dat[["Compound"]]), pval = .) %>% 
  mutate(apval = p.adjust(pval, method = "BH"))


diff_pval_wilcox <- sapply(unique(dat[["Compound"]]), function(ith_cmp) {
  filter(dat, Compound == ith_cmp) %>% 
    wilcox.test(value ~ group_label, data = .) %>% 
    getElement("p.value")
}, USE.NAMES = FALSE) %>% 
  data.frame(Compound = unique(dat[["Compound"]]), pval = .) %>% 
  mutate(apval = p.adjust(pval, method = "BH"))




