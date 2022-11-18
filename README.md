### MetaboModelleR web server

TODO

### Running MetaboModelleR locally

Execute following commands in the R console:

``` r
install.packages("devtools")
devtools::install_github("BioGenies/MetaboModelleR", upgrade = "always", dependencies = TRUE)
```

See the example below: 

<img src="https://raw.githubusercontent.com/michbur/easyR/main/inst/additional-figures/r-console.png" alt="rconsole" style="height: 200px;"/>

If you have successfully installed the package, run the GUI in your web browser using:

``` r
MetaboModelleR::run_MetaboModelleR()
```

Remember that the column with compound names in your data should be named **Compound**.

### Citation (coming soon)

Grzesiak K., Ciborowski M., Burdukiewicz M. (2022). MetaboModeller: efficient processing of GC-MS and LC-MS data. XXXYYY, doi.
