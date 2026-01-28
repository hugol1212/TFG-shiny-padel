#' Run shiny application
#'
#' Runs the shiny app, keeping listening in a given port (by default 3838).
#' The default host address is 0.0.0.0. It may be changed to 127.0.0.1 to be
#' run as localhost.
#'
#' @param .port Port number on which the application listens. Default is 3838.
#' @param .host Host address where the application runs. Default is 127.0.0.1.
#' @param .dev Logical flag indicating whether to enable development mode. When
#' `TRUE`, the app runs in development mode using `shiny::devmode()`, enabling
#' additional logging, hot reloading, and suppressing bslib contrast warnings.
#' Useful for iterative development and debugging.
#'
#' @return a shiny.appobj object, run the app and keep listening in a port
#'
#' @export
#'
#' @examplesIf interactive()
#' app_run()
app_run <- function(.port = 3838, .host = "127.0.0.1", .dev = FALSE) {
  if (.dev) {
    shiny::devmode(TRUE, verbose = TRUE)
    options(bslib.color_contrast_warnings = FALSE)
  }
  shiny::shinyApp(
    app_ui(),
    app_server,
    options = list(port = .port, host = .host)
  )
}
