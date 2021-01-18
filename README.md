
# small area mApp

<!-- badges: start -->
<!-- badges: end -->

This Shiny app helps you estimate disease risk across small geographical areas using Bayesian spatial modeling. And yes, you can you use your own data. 

It's built with the intention to support cancer surveillance and disease mapping but works with any area-based disease event data.

Computation is done with integrated nested Laplace approximation (INLA). We use the amazing package created by the R-INLA team. Check out their website and install their package from the [R-INLA website](https://www.r-inla.org/). 

## Installation

You can install the Github version of smallareamapp from [my Github repo](https://github.com/jdsimkin04/smallareamapp) with:

``` r
devtools::install_github("jdsimkin04/smallareamapp")
```

IMPORTANT! The download will take LONG if you haven't installed R-INLA before. It's a beast of package (but it's worth it!).

## Example

The app has one function... yes it's that simple. runmApp() will launch the shiny application and the rest is done through the app. 

``` r
library(smallareamapp)
runmApp()
## basic example code
```

