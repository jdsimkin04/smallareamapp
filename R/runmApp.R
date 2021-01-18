#' Launch the small area mApp
#'
#' Launch the small area mApp Shiny application locally and use Bayesian spatial models to spatially analyze health data
#'
#' @export
#'
#' @examples
runmApp <- function() {
  system.file('smallareamapp', 'dp_prepv2.R', package='smallareamapp')
  shiny::runApp(system.file('smallareamapp', package='smallareamapp'))
}
