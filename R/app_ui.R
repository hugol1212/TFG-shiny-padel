#' App user interface
#'
#' #' Internal function, called within app_run()
#'
#'
#' @return A bslib_page object, used as the UI argument in a call to
#' shiny::shinyApp()
#' @export
#'
#' @seealso [app_server()]
#'
#' @examplesIf interactive()
#' shiny::shinyApp(app_ui(), app_server)
app_ui <- function() {
  i18n <- getOption("template.translator")
  i18n$set_translation_language(getOption("template.default.lang"))

  # To handle warnings from page_navbar
  withCallingHandlers(
    page_navbar(
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "www/css/styles.css"
        )
      ),
      window_title = "dash",
      title = i18n$t("My dashboard"),
      theme = mytheme(),
      navbar_options = navbar_options(
        theme = "dark",
        position = "static-top",
        collapsible = TRUE,
        underline = TRUE
      ),
      sidebar = sidebar(
        title = i18n$t("Selections"),
        position = "left",
        shinyWidgets::pickerInput(
          inputId = "somevalue",
          label = "A label",
          choices = c("a", "b")
        ),
        shiny.i18n::usei18n(i18n)
      ),
      nav_panel(
        title = i18n$t("Overview"),
        layout_sidebar(
          sidebar = sidebar(
            position = "right",
            myvbs()[1],
            myvbs()[2],
            myvbs()[3]
          ),
          layout_columns(
            fill = FALSE,
            col_widths = c(6, 6),
            card("map"),
            card("plot")
          )
        ),
      ),
      nav_panel(
        title = i18n$t("Section 1"),
        i18n$t("Content of second section"),
        layout_columns(myvbs()[1], myvbs()[2], myvbs()[3])
      ),
      nav_spacer(),
      # Language button
      nav_item(
        shinyWidgets::radioGroupButtons(
          inputId = "selected_language",
          size = "sm",
          choices = setNames(
            tolower(i18n$get_languages()),
            toupper(i18n$get_languages())
          ),
          checkIcon = list(
            yes = bs_icon("check-lg")
          )
        )
      ),
      # Dark mode button
      nav_item(
        input_dark_mode(
          id = "dark_mode",
          mode = "light"
        )
      )
    ),
    warning = function(w) {
      # Handle undesired dark button warning
      if (grepl("Navigation containers expect", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
