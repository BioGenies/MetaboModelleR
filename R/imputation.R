#' Data Imputation
#' 
#' @importFrom impute impute.knn
#' @import dplyr
#' @importFrom stats median
#' @importFrom shiny showNotification
#'
#' @description This function completes the metabolomics data using four 
#' approaches of imputation.
#'
#' @param dat metabolomic data. Should contain `Compound` column.
#' @param method imputation method. One of "kNN", "zeros", "median", 
#' "1/2 minimum". 
#'
#' @return completed data frame. 
#'
#' @export


complete_data <- function(dat, 
                          method = "kNN") {
  
  match.arg(method, c("kNN", "zeros", "median", "1/2 minimum"))
  
  Compound <- dat[["Compound"]]
  dat_missing <- dat[, -(colnames(dat) == "Compound")]
  
  result <- switch (method,
                    kNN = {
                      tryCatch({
                        as.data.frame(impute.knn(as.matrix(dat_missing))[["data"]])
                      }, error = function(e){
                        if (interactive()) {
                          showNotification(paste0(e, " Median used instead."),
                                           duration = 5,
                                           closeButton = TRUE,
                                           type = "message",
                                           id = "imputation")
                        }
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
                      })
                    },
                    zeros = {
                      if (interactive()) {
                        removeNotification(id = "imputation")
                      }
                      dat_missing[is.na(dat_missing)] <- 0
                      dat_missing
                    },
                    median = {
                      if (interactive()) {
                        removeNotification(id = "imputation")
                      }
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
                      if (interactive()) {
                        removeNotification(id = "imputation")
                      }
                      suppressWarnings({
                        dat_missing %>%
                          t() %>% 
                          as_tibble() %>% 
                          mutate_all(function(x){
                            ifelse(is.na(x), 0.5 * min(x, na.rm = T), x)
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
