
#' @export
#' @importFrom shinyjs useShinyjs
launchApp <- function(){
  shinyjs::useShinyjs()

  shinyApp(ui = ui, server = server)
}
