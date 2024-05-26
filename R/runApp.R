
#' @export
#' @importFrom shinyjs useShinyjs
runApp <- function(){
  shinyjs::useShinyjs()

  shinyApp(ui = ui, server = server)
}
