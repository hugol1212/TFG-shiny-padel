.onLoad <- function(libname, pkgname) {
  resources <- system.file("app/www", package = "template.shiny.app")
  addResourcePath("www", resources)

  op <- options()
  op.template <- list(
    # nolint
    template.translator = suppressWarnings(
      shiny.i18n::Translator$new(
        translation_csvs_path = system.file(
          "app",
          "translations",
          package = "template.shiny.app"
        )
      )
    ),
    template.default.lang = "en"
  )
  toset <- !(names(op.template) %in% names(op))
  if (any(toset)) options(op.template[toset])
}
