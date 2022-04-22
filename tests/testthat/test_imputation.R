

test_that("imputation works", {
  set.seed(44)
  
  values <- t(sapply(1:length(letters), function(i) runif(4, 10 * i, 10 * i + 1)))
  colnames(values) <- c("S1", "S2", "S3", "S4")
  dat <- data.frame(Compound = letters,
                    values)
  
  Compound <- dat[, 1]
  dat <- dat[, -1]
  n <- nrow(dat)
  p <- ncol(dat)
  NAloc <- rep(FALSE, n * p)
  NAloc[sample.int(n * p, floor(n * p * 0.2), replace = TRUE)] <- TRUE
  dat[matrix(NAloc, nrow = n, ncol = p)] <- NA
  dat <- cbind(Compound, dat)
  completed <- complete_data(dat)
  
  expect_equal(completed[is.na(dat)], 
               c(55.9809805835442, 101.691172189952, 130.589396816154, 
                 163.868675350807, 210.53721088756, 205.586164665117, 
                 54.6531978353991, 54.6531978353991, 70.3905300455434, 
                 121.708499263739, 185.040441122915, 209.535116452336, 
                 132.958709437953, 146.751210878196, 200.520385263188, 
                 212.713980396718, 180.437296375516, 204.955247189177))
  
  completed <- complete_data(dat, method = "median")
  
  expect_equal(completed[is.na(dat)], 
               c(60.4300603473093, 100.479024216766, 130.071252652211, 
                 160.484304685378, 220.502243444556, 250.497239628341, 
                 40.4352769344114, 60.4300603473093, 70.240528159542, 
                 110.522281060228, 190.521846498363, 240.406229408225, 
                 140.566197311739, 150.416079465533, 190.521846498363, 
                 210.430570167489, 180.7496663325, 250.497239628341))
})