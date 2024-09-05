#' Launch a Shiny Application that houses all of the algorithms seen in this package
#' @description
#' This application serves to provide a way to find and analyze frequency bands in Univariate, Multivariate, and Functional data, in a graphic-based, user-friendly manner.
#'
#' @return Running this function will launch this shiny application in a new window.
#' @export
#' @importFrom shinyjs useShinyjs
#' @examples
#' launchApp()
#' @details
#' There is a vignette entitled "ShinyAppUsage" that is included with this package, which goes into much greater detail about how to use and understand the application.

launchApp <- function(){
  shinyjs::useShinyjs()

  shinyApp(ui = ui, server = server)
}
