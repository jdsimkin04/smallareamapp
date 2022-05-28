# *smallareamApp*

<!-- badges: start -->

<!-- badges: end -->

This package runs a Shiny app that helps you estimate disease risk across small geographical areas using Bayesian spatial modeling. And yes, you can use your own data.

It's built with the aim to support cancer surveillance and disease mapping but works with any area-based disease event data.

Bayesian computation is done with integrated nested Laplace approximation (INLA). We use the amazing package created by the R-INLA team. Check out their website and install their package from the [R-INLA website](https://www.r-inla.org/).

<img src="https://github.com/jdsimkin04/smallareamapp/blob/main/app_demo.gif" width="50%"/>

## Updates

Updates to come! 
* Checking for INLA model failures
* Modified PIT calculations
* New posterior predictive checks including observed vs. fitted counts, PIT historgram and Pearson residuals.

## Installation

First things first.. you'll need the INLA package by R-INLA. I recommend running either of these lines on your machine for the testing or stable versions. FYI R-INLA is hosted on [R-INLA website](https://www.r-inla.org/) and not on CRAN or github.

``` r
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
```

**smallareamApp** is not available on [CRAN](https://cran.r-project.org/). You can install it now through Github via [my Github repo](https://github.com/jdsimkin04/smallareamapp) with:

``` r
devtools::install_github("jdsimkin04/smallareamapp")
```

**IMPORTANT!** There are quite a few dependencies for this... so it may take long.

## Example

The app has one function... yes it's that simple. **runmApp()** will launch the shiny application and the rest is done through the app.

``` r
library(smallareamapp)
runmApp()
```

### Prepping data for upload

**smallareamApp** analyzes *your* data. There's one catch though. The data has to be set up exactly right for the app to accept it. I know it's not ideal... but I'm working on that! For the meantime, there's a template table on the app that shows you what it is expecting. It includes area name, cancer type, sex, observed counts, age-adjusted expected counts, standardized incidence ratios, etc. I use the [epitool](shttps://cran.r-project.org/web/packages/epitools/epitools.pdfpackage) for indirect age-standardization.

# Package Walkthrough

I've put together a package installation and [walkthrough guide here](https://github.com/jdsimkin04/smallareamapp/blob/main/smallareamApp%20guide.pdf).
