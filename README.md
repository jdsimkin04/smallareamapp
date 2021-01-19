# small area mApp

<!-- badges: start -->

<!-- badges: end -->

This package runs a Shiny app that helps you estimate disease risk across small geographical areas using Bayesian spatial modeling. And yes, you can you use your own data.

It's built with the intention to support cancer surveillance and disease mapping but works with any area-based disease event data.

Computation is done with integrated nested Laplace approximation (INLA). We use the amazing package created by the R-INLA team. Check out their website and install their package from the [R-INLA website](https://www.r-inla.org/).

## Installation

**smallareamApp** is not available on [CRAN](https://cran.r-project.org/). You can install it now through Github via [my Github repo](https://github.com/jdsimkin04/smallareamapp) with:

``` r
devtools::install_github("jdsimkin04/smallareamapp")
```

**IMPORTANT!** The download will take LONG if you haven't installed R-INLA before. It's a beast of package (but it's worth it!).

## Example

The app has one function... yes it's that simple. **runmApp()** will launch the shiny application and the rest is done through the app.

``` r
library(smallareamapp)
runmApp()
## basic example code
```

And this is what that looks like!

<img src="https://github.com/jdsimkin04/smallareamapp/blob/main/app_demo.gif" width="75%"/>

### Prepping data for upload

**smallareamApp** analyzes *your* data. There's one catch though. The data has to be set up exactly right for the app to accept it. I know it's not ideal... but we're working on that! For the meantime, there's a template table on the app that shows you what it is expecting.


More to come... Stay tuned!
