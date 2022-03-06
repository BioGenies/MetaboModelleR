## Install easyR

Execute following commands in the R console:

``` r
install.packages("devtools")
devtools::install_github("michbur/easyR", upgrade = "always", dependencies = TRUE)
```

See the example below: 

<img src="https://raw.githubusercontent.com/michbur/easyR/main/inst/additional-figures/r-console.png" alt="rconsole" style="height: 200px;"/>


## Run easyR

If you have successfully installed the package, run the GUI using:

``` r
easyR::easyR_gui()
```

Remember that the column with compound names in your data should be named **Compound**.