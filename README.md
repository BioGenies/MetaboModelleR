## Install easyR

Execute following commands in the R console:

``` r
install.packages("devtools")
devtools::install_github("michbur/easyR", upgrade = "always", dependencies = TRUE)
```

## Run easyR

If you have successfully installed the package, run the GUI using:

``` r
easyR::easyR_gui()
```