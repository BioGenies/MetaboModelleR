
library(readxl)

lose_some_dat <- function(dat, pctg) {
  Compound <- dat[, 1]
  dat <- dat[, -1]
  n <- nrow(dat)
  p <- ncol(dat)
  NAloc <- rep(FALSE, n * p)
  NAloc[sample.int(n * p, floor(n * p * pctg))] <- TRUE
  dat[matrix(NAloc, nrow = n, ncol = p)] <- NA
  
  cbind(Compound, dat)
  }

insert_and_save <- function(complete_file, missing_file, pctg = 0.1) {
  dat <- read_excel(complete_file)
  
  dat <- lose_some_dat(dat = dat, 
                       pctg = pctg)
  
  write.csv(dat, paste0("./missing/", missing_file), )
}


set.seed(17)

insert_and_save("Example_paired.xlsx", 
                "Example_paired_missing.csv")

insert_and_save("Example.xlsx", 
                "Example_missing.csv")

insert_and_save("Statystyka_pigs_EDTA.xlsx", 
                "Statystyka_pigs_EDTA_missing.csv")
