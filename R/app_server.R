#' Server-side app code
#'
#' Internal function, called within app_run()
#'
#' @param input A Shiny input object.
#' @param output A Shiny output object.
#'
#' @return A server object, used as the server argument in a call to
#' shiny::shinyApp()
#' @export
#'
#' @seealso [app_ui()]
#'
#' @examplesIf interactive()
#' shiny::shinyApp(app_ui(), app_server)
app_server <- function(input, output, session) {
  i18n <- getOption("template.translator")
  i18n$set_translation_language(
    transl_language = getOption("template.default.lang")
  )

  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(
      language = input$selected_language,
      session = session
    )
    i18n$set_translation_language(transl_language = input$selected_language)
  })

  # Uncomment to include a theme selector
  # bs_themer()
  output$p <- renderPlot({
    ggplot(penguins) +
      geom_histogram(aes(!!input$var), bins = input$bins) +
      theme_bw(base_size = 20)
  })
}
