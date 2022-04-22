#' Data Imputation
#' 
#' @importFrom impute impute.knn
#' @import dplyr
#' @importFrom stats median
#'
#' @description This function completes the metabolomic data using four 
#' approaches of imputation.
#'
#' @param dat metabolomic data. Should contain `Compound` column.
#' @param method imputation method. See details for more information.
#'
#' @return completed data frame. 
#'
#' @export


complete_data <- function(dat, 
                          method = "kNN") {
  Compound <- dat[["Compound"]]
  dat_missing <- dat[, -(colnames(dat) == "Compound")]
  
  result <- switch (method,
                    kNN = {
                      as.data.frame(impute.knn(as.matrix(dat_missing))[["data"]])
                    },
                    zeros = {
                      dat_missing[is.na(dat_missing)] <- 0
                      dat_missing
                    },
                    median = {
                      suppressWarnings({
                        dat_missing %>%
                          t() %>% 
                          as_tibble() %>% 
                          mutate_all(function(x) {
                            ifelse(is.na(x), median(x, na.rm = TRUE), x)
                          }) %>% 
                          t() %>% 
                          as_tibble()
                      })
                    },
                    `1/2 minimum` = {
                      suppressWarnings({
                        dat_missing %>%
                          t() %>% 
                          as_tibble() %>% 
                          mutate_all(function(x){
                            ifelse(is.na(x), 0.5*min(x, na.rm = T), x)
                          }) %>% 
                          t() %>% 
                          as_tibble()
                      })
                    }
  )
  result <- cbind(Compound, result)
  colnames(result) <- colnames(dat)
  result %>% 
    as_tibble() %>% 
    mutate(across(!Compound, as.numeric))
}
