Extension for changepoint Package
=================================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN
Version](http://www.r-pkg.org/badges/version/changepointExt)](https://cran.r-project.org/package=changepointExt)

``` r
set.seed(314)
x1 <- c(rnorm( 50, mean = 100, sd = 50),
        rnorm(100, mean = 200, sd = 50),
        rnorm( 50, mean = 300, sd = 50))
x2 <- c(rnorm(100, mean = 100, sd = 50),
        rnorm(100, mean = 200, sd = 50))
y <- x1 + x2

library(changepoint)
library(changepointExt)

cpt_y <- cpt.meanvar(y, method = "PELT")
autoplot(cpt_y)
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r

cpt_x1 <- cpt.meanvar(x1, method = "PELT")
autoplot(cpt_x1)
```

![](README_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
cpt_x2 <- cpt.meanvar(x2, method = "PELT")
autoplot(cpt_x2)
```

![](README_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r

combi <- combine_cpts("x1" = cpt_x1, "x2" = cpt_x2)
autoplot(cpt_y) + autolayer(combi)
```

![](README_files/figure-markdown_github/unnamed-chunk-1-4.png)
