## Include functions for:
## - Generate reactives from inputs
## - Generate outputs from inputs
## - Generate inputs, e.g., from config

#' Create a custom Bootstrap theme
#'
#' This function generates a Bootstrap theme using a custom color palette.
#'
#' @return A Bootstrap theme object with customized colors and fonts.
#' @export
#'
#' @examplesIf interactive()
#' mytheme()
mytheme <- function() {
  bslib::bs_theme(
    bg = "#FFF",
    fg = "#001F66",
    # "navbar-fg" = "#FFF",
    # "navbar-bg" = "#001F66",
    primary = "#0d6efd",
    secondary = "#6c757d",
    success = "#198754",
    info = "#0dcaf0",
    warning = "#ffc107",
    danger = "#dc3545",
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("JetBrains Mono")
  )
}

#' Generate a list of value boxes with icons
#'
#' This function creates a list of three value boxes with customizable values,
#' icons, position, fill, and size.
#'
#' @param values Numeric vector of length 3 specifying the values to display in
#' each value box. Defaults to `1:3`.
#' @param icons Character vector of length 3 specifying Bootstrap icons for each
#' value box. Defaults to `c("graph-up", "thermometer-sun", "handbag")`.
#' @param pos Character string specifying the position of the showcase icon.
#' Defaults to `"top right"`.
#' @param .fill Logical indicating whether the value boxes should be filled.
#' Defaults to `FALSE`.
#' @param .size Character string specifying the size of the icons. Defaults to
#' `"0.5em"`.
#'
#' @return A list of three `value_box` elements, each with a title, value,
#' theme, and icon.
#' @export
#'
#' @examples
#' myvbs()[1]
#' myvbs(values = c(10, 20, 30), icons = c("star", "bell", "check-circle"))
myvbs <- function(
  values = 1:3,
  icons = c("graph-up", "thermometer-sun", "handbag"),
  pos = "top right",
  .fill = FALSE,
  .size = "0.5em"
) {
  list(
    value_box(
      title = "Correlation",
      value = values[1],
      theme = "warning",
      showcase = bsicons::bs_icon(icons[1], size = .size),
      showcase_layout = pos,
      fill = .fill
    ),
    value_box(
      title = "KPI_2",
      value = values[2],
      theme = "danger",
      showcase = bsicons::bs_icon(icons[2], size = .size),
      showcase_layout = pos,
      fill = .fill
    ),
    value_box(
      title = "KPI_3",
      value = values[3],
      theme = "success",
      showcase = bsicons::bs_icon(icons[3], size = .size),
      showcase_layout = pos,
      fill = .fill
    )
  )
}
