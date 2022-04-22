
library(readxl)
library(xlsx)

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
  
  write.xlsx(dat, paste0("./missing/", 
                         missing_file), 
             sheetName = "Sheet1", 
             col.names = TRUE, 
             row.names = FALSE, 
             append = FALSE)
}


set.seed(17)

insert_and_save("Example_paired.xlsx", 
                "Example_paired_missing.xlsx")

insert_and_save("Example.xlsx", 
                "Example_missing.xlsx")

insert_and_save("Statystyka_pigs_EDTA.xlsx", 
                "Statystyka_pigs_EDTA_missing.xlsx")
