
#' Run the KBO Cheerleader application
#'
#' @inheritDotParams shiny::shinyApp -ui -server
#'
#' @export
#'
run_app <- function(...) {

  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...)
}
