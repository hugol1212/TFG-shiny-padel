## Include functions for:
## - Data manipulation and transformation
## - Performing statistical and mathematical calculations

# -----------------------------
# 0) 
# -----------------------------
padel_file <- function() {
  "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"
}

padel_list_sheets <- function(path = padel_file(), include_hidden = FALSE) {
  sheets <- readxl::excel_sheets(path)
  
  # De momento no filtramos "hidden" porque readxl no distingue hidden sheets.
  # Dejamos el argumento para compatibilidad con el resto del código.
  if (!include_hidden) {
    return(sheets)
  }
  sheets
}


padel_read_sheet <- function(sheet) {
  readxl::read_excel(padel_file(), sheet = sheet)
}

padel_read_sheet_raw <- function(sheet) {
  readxl::read_excel(padel_file(), sheet = sheet, col_names = FALSE)
}

padel_norm_name <- function(x) {
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x
}

padel_find_col <- function(nms, patterns) {
  nms0 <- padel_norm_name(nms)
  for (p in patterns) {
    idx <- which(grepl(p, nms0, ignore.case = TRUE))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  NA_character_
}

padel_as_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("\\.", "", x)    # separador miles
  x <- gsub(",", ".", x)     # coma decimal
  suppressWarnings(as.numeric(x))
}

padel_parse_excel_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  nx <- suppressWarnings(as.numeric(x))
  if (sum(!is.na(nx)) > 0) {
    return(as.Date(nx, origin = "1899-12-30"))
  }
  suppressWarnings(as.Date(x))
}

padel_detect_header_and_clean <- function(x, min_non_na = 4) {
  n <- nrow(x)
  if (n == 0) return(x)
  
  score <- sapply(seq_len(min(n, 30)), function(i) {
    row <- x[i, ]
    vals <- unlist(row, use.names = FALSE)
    nn <- sum(!is.na(vals) & vals != "")
    txt <- sum(grepl("[A-Za-zÁÉÍÓÚÜÑáéíóúüñ]", as.character(vals), perl = TRUE), na.rm = TRUE)
    if (nn < min_non_na) return(0)
    nn + 0.5 * txt
  })
  
  header_row <- which.max(score)
  if (length(header_row) == 0 || score[header_row] == 0) {
    return(x)
  }
  
  header <- as.character(unlist(x[header_row, ], use.names = FALSE))
  empty <- is.na(header) | header == ""
  header[empty] <- paste0("V", which(empty))
  
  dat <- x[-seq_len(header_row), , drop = FALSE]
  names(dat) <- header
  
  # quitar filas completamente vacías
  dat <- dat[rowSums(!is.na(dat) & dat != "") > 0, , drop = FALSE]
  dat
}

# -----------------------------
# 1) CLASIFICACIÓN INDIVIDUAL (GENÉRICA para año IV/V/etc.)
# -----------------------------
padel_read_clasificacion_indiv <- function(sheet) {
  xraw <- padel_read_sheet_raw(sheet)
  dat  <- padel_detect_header_and_clean(xraw)
  
  nms <- names(dat)
  
  # Columna posición / jugador (para filtrar filas útiles)
  pos_col     <- padel_find_col(nms, c("posici", "^pos$"))
  jugador_col <- padel_find_col(nms, c("^jugador$", "jugad"))
  
  if (!is.na(pos_col)) {
    dat <- dat[!is.na(dat[[pos_col]]) & dat[[pos_col]] != "", , drop = FALSE]
  }
  if (!is.na(jugador_col)) {
    dat <- dat[!is.na(dat[[jugador_col]]) & dat[[jugador_col]] != "", , drop = FALSE]
  }
  
  dat
}

padel_standings_indiv <- function(sheet) {
  df <- padel_read_clasificacion_indiv(sheet)
  nms <- names(df)
  
  # Por cómo está tu Excel, normalmente:
  # - la 1ª col = grupo (TOP8..., PLAYOFFS...)
  # - la 2ª col = posición (1º, 2º...)
  # - luego jugador, id, etc.
  # Intentamos detectar, si no, caemos a posiciones típicas.
  grupo_col   <- nms[1]
  pos_col     <- padel_find_col(nms, c("posici"))
  jugador_col <- padel_find_col(nms, c("^jugador$", "jugad"))
  id_col      <- padel_find_col(nms, c("^id$", " id$"))
  ant_col     <- padel_find_col(nms, c("anter"))
  liga_col    <- padel_find_col(nms, c("en liga", "liga"))
  mano_col    <- padel_find_col(nms, c("^mano$", "diestro", "zurdo"))
  pista_col   <- padel_find_col(nms, c("^pista$", "drive", "rev"))
  
  # fallback por posición si no encuentra
  if (is.na(pos_col) && length(nms) >= 2) pos_col <- nms[2]
  if (is.na(jugador_col) && length(nms) >= 4) jugador_col <- nms[4]
  if (is.na(id_col) && length(nms) >= 5) id_col <- nms[5]
  
  out <- df
  
  # filtrar filas donde la posición tiene º (1º,2º,...)
  if (!is.na(pos_col)) {
    out <- out[grepl("º", as.character(out[[pos_col]])), , drop = FALSE]
  }
  
  # construir tabla estándar
  safe_pick <- function(colname) if (!is.na(colname) && colname %in% names(out)) out[[colname]] else NA
  
  res <- tibble::tibble(
    grupo    = safe_pick(grupo_col),
    posicion = safe_pick(pos_col),
    jugador  = safe_pick(jugador_col),
    id       = safe_pick(id_col),
    anterior = safe_pick(ant_col),
    en_liga  = safe_pick(liga_col),
    mano     = safe_pick(mano_col),
    pista    = safe_pick(pista_col)
  )
  
  # rellenar grupo hacia abajo
  res <- tidyr::fill(res, grupo, .direction = "down")
  
  res
}

# -----------------------------
# 2) CLASIFICACIÓN PAREJAS (GENÉRICA)
# -----------------------------
padel_parejas <- function(sheet = "Clas. Parejas") {
  xraw <- padel_read_sheet_raw(sheet)
  dat  <- padel_detect_header_and_clean(xraw)
  
  nms <- names(dat)
  
  col_pareja  <- padel_find_col(nms, c("^pareja$", "pareja"))
  col_puntos  <- padel_find_col(nms, c("^puntos$", "puntos"))
  col_ganados <- padel_find_col(nms, c("partidos gan", "ganados"))
  col_fecha   <- padel_find_col(nms, c("^fecha$", "fecha"))
  col_jornada <- padel_find_col(nms, c("jornada"))
  col_efect   <- padel_find_col(nms, c("efectiv", "efectividad"))
  col_rep     <- padel_find_col(nms, c("repetido"))
  
  out <- dat
  
  if (!is.na(col_pareja))  names(out)[names(out) == col_pareja]  <- "pareja"
  if (!is.na(col_puntos))  names(out)[names(out) == col_puntos]  <- "puntos"
  if (!is.na(col_ganados)) names(out)[names(out) == col_ganados] <- "partidos_ganados"
  if (!is.na(col_fecha))   names(out)[names(out) == col_fecha]   <- "fecha"
  if (!is.na(col_jornada)) names(out)[names(out) == col_jornada] <- "jornada"
  if (!is.na(col_efect))   names(out)[names(out) == col_efect]   <- "efectividad"
  if (!is.na(col_rep))     names(out)[names(out) == col_rep]     <- "repetido"
  
  # tipos
  if ("puntos" %in% names(out)) out$puntos <- padel_as_num(out$puntos)
  if ("partidos_ganados" %in% names(out)) out$partidos_ganados <- padel_as_num(out$partidos_ganados)
  if ("jornada" %in% names(out)) out$jornada <- padel_as_num(out$jornada)
  
  if ("fecha" %in% names(out)) {
    out$fecha <- padel_parse_excel_date(out$fecha)
  }
  
  if ("efectividad" %in% names(out)) {
    ef <- as.character(out$efectividad)
    ef <- gsub("%", "", ef)
    out$efectividad <- padel_as_num(ef)
  }
  
  # filtrar filas basura
  if ("pareja" %in% names(out)) {
    out <- out[!is.na(out$pareja) & nzchar(trimws(out$pareja)), , drop = FALSE]
  }
  
  out
}

# -----------------------------
# 3) Lectura genérica para TODAS las hojas
# -----------------------------
padel_sheet_type <- function(sheet) {
  if (grepl("^Clasificación indiv\\.", sheet)) return("indiv")
  if (grepl("^Clas\\.?\\s*Parejas", sheet)) return("parejas")
  "other"
}

padel_read_any <- function(sheet) {
  type <- padel_sheet_type(sheet)
  
  if (type == "indiv") {
    return(padel_standings_indiv(sheet))
  }
  if (type == "parejas") {
    return(padel_parejas(sheet))
  }
  
  # genérico
  xraw <- padel_read_sheet_raw(sheet)
  dat  <- padel_detect_header_and_clean(xraw)
  
  # quitar columnas completamente vacías
  keep <- colSums(!is.na(dat) & dat != "") > 0
  dat <- dat[, keep, drop = FALSE]
  
  dat
}

# -----------------------------
# 4) Choices agrupados para el picker (sin openxlsx)
# -----------------------------
padel_sheet_choices_grouped <- function(include_hidden = TRUE) {
  sheets <- padel_list_sheets()
  
  group <- function(x) {
    if (grepl("^Clasificación indiv\\.", x)) return("Clasificación individual")
    if (grepl("^Clas\\.?\\s*Parejas", x)) return("Clasificación parejas")
    if (grepl("^TOTALES", x)) return("Totales")
    if (grepl("^Tesorería", x)) return("Tesorería")
    if (grepl("^Calendario", x)) return("Calendario")
    if (grepl("^(J\\d+|J\\d+-)", x, ignore.case = TRUE)) return("Jornadas")
    "Otras"
  }
  
  groups <- vapply(sheets, group, character(1))
  split(sheets, groups)
}


# -----------------------------
# 5) Filtro de hojas visibles en el selector
# -----------------------------
padel_visible_sheets <- function(mode = c("basic", "all")) {
  mode <- match.arg(mode)
  
  sheets <- padel_list_sheets()
  
  if (mode == "all") {
    return(sheets)
  }
  
  # MODO "basic": solo lo que quieras ver en la app
  # Ajusta estos patrones a tu gusto.
  keep <- grepl("^Calendario", sheets) |
    grepl("^Clasificación indiv\\.", sheets) |
    grepl("^Clas\\.?\\s*Parejas", sheets)
  
  sheets[keep]
}

padel_sheet_choices_grouped_filtered <- function(mode = c("basic", "all")) {
  mode <- match.arg(mode)
  sheets <- padel_visible_sheets(mode)
  
  group <- function(x) {
    if (grepl("^Clasificación indiv\\.", x)) return("Clasificación individual")
    if (grepl("^Clas\\.?\\s*Parejas", x)) return("Clasificación parejas")
    if (grepl("^Calendario", x)) return("Calendario")
    "Otras"
  }
  
  groups <- vapply(sheets, group, character(1))
  split(sheets, groups)
}

# ------------------------------------------------------------
# Selector inteligente: mostrar SOLO hojas con datos
# (sin cargar todas las hojas completas)
# ------------------------------------------------------------

# Cache interno para no releer lo mismo muchas veces
.padel_cache <- new.env(parent = emptyenv())
.padel_cache$nonempty <- NULL
.padel_cache$file_mtime <- NULL

padel_excel_path <- function() {
  padel_file()
}

padel_sheet_has_data <- function(sheet, path = padel_excel_path(), n_max = 10) {
  # Leemos pocas filas y SIN nombres de columnas para evitar el spam de "New names"
  df <- suppressMessages(
    tryCatch(
      readxl::read_excel(path, sheet = sheet, n_max = n_max, col_names = FALSE),
      error = function(e) NULL
    )
  )
  if (is.null(df)) return(FALSE)
  if (nrow(df) == 0 || ncol(df) == 0) return(FALSE)
  
  # Considera "con datos" si existe al menos una celda no vacía / no NA
  mat <- as.matrix(df)
  any(!is.na(mat) & trimws(as.character(mat)) != "")
}


padel_list_nonempty_sheets <- function(path = padel_excel_path(), include_hidden = FALSE) {
  # Cache: se recalcula solo si el fichero cambia
  mtime <- tryCatch(file.info(path)$mtime, error = function(e) NA)
  
  if (!is.null(.padel_cache$nonempty) &&
      !is.null(.padel_cache$file_mtime) &&
      !is.na(mtime) &&
      identical(.padel_cache$file_mtime, mtime)) {
    return(.padel_cache$nonempty)
  }
  
  sheets <- padel_list_sheets(path = path, include_hidden = include_hidden)
  
  nonempty <- sheets[vapply(sheets, padel_sheet_has_data, logical(1), path = path)]
  
  .padel_cache$nonempty <- nonempty
  .padel_cache$file_mtime <- mtime
  
  nonempty
}

# Si quieres además filtrar "familias" (Calendario / Clasif indiv / Parejas), úsalo aquí:
padel_visible_sheets <- function(mode = c("basic", "all"),
                                 path = padel_excel_path(),
                                 include_hidden = FALSE) {
  mode <- match.arg(mode)
  
  sheets <- padel_list_nonempty_sheets(path = path, include_hidden = include_hidden)
  
  if (mode == "all") return(sheets)
  
  # MODO basic: SOLO las familias que quieres exponer en la app
  keep <- grepl("^Calendario", sheets) |
    grepl("^Clasificación indiv\\.", sheets) |
    grepl("^Clas\\.?\\s*Parejas", sheets)
  
  sheets[keep]
}

padel_sheet_choices_grouped_filtered <- function(mode = c("basic", "all"),
                                                 path = padel_excel_path(),
                                                 include_hidden = FALSE) {
  mode <- match.arg(mode)
  sheets <- padel_visible_sheets(mode = mode, path = path, include_hidden = include_hidden)
  
  group <- function(x) {
    if (grepl("^Calendario", x)) return("Calendario")
    if (grepl("^Clasificación indiv\\.", x)) return("Clasificación individual")
    if (grepl("^Clas\\.?\\s*Parejas", x)) return("Clasificación parejas")
    "Otras"
  }
  
  groups <- vapply(sheets, group, character(1))
  split(sheets, groups)
}

####################################################################################3
#' App user interface
#'
#' Internal function, called within app_run()
#'
#' @return A bslib_page object
#' @export
app_ui <- function() {
  i18n <- getOption("template.translator")
  i18n$set_translation_language(getOption("template.default.lang"))
  
  withCallingHandlers(
    bslib::page_navbar(
      shiny::tags$head(
        shiny::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "www/css/styles.css"
        )
      ),
      window_title = "dash",
      title = i18n$t("My dashboard"),
      theme = mytheme(),
      navbar_options = bslib::navbar_options(
        theme = "dark",
        position = "static-top",
        collapsible = TRUE,
        underline = TRUE
      ),
      
      sidebar = bslib::sidebar(
        title = i18n$t("Selections"),
        position = "left",
        width = 700,
        
        shinyWidgets::pickerInput(
          inputId = "sheet",
          label = "Hoja del Excel",
          choices = padel_sheet_choices_grouped_filtered(mode = "basic", include_hidden = FALSE),
          selected = "Clasificación indiv. año V",
          options = list(`live-search` = TRUE)
        ),
        
        # ✅ Mensaje de estado debajo del selector
        shiny::textOutput("sheet_status"),
        
        shiny::uiOutput("filters_ui"),
        DT::DTOutput("tabla_excel"),
        
        shiny.i18n::usei18n(i18n)
      ),
      
      bslib::nav_panel(
        title = i18n$t("Overview"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            position = "right",
            shiny::uiOutput("kpis")
          ),
          bslib::layout_columns(
            fill = FALSE,
            col_widths = c(6, 6),
            shiny::uiOutput("left_card"),
            shiny::uiOutput("right_card")
          )
        )
      ),
      
      bslib::nav_panel(
        title = i18n$t("Section 1"),
        i18n$t("Content of second section")
      ),
      
      bslib::nav_spacer(),
      
      bslib::nav_item(
        shinyWidgets::radioGroupButtons(
          inputId = "selected_language",
          size = "sm",
          choices = setNames(
            tolower(i18n$get_languages()),
            toupper(i18n$get_languages())
          ),
          checkIcon = list(yes = bsicons::bs_icon("check-lg"))
        )
      ),
      
      bslib::nav_item(
        bslib::input_dark_mode(
          id = "dark_mode",
          mode = "light"
        )
      )
    ),
    warning = function(w) {
      if (grepl("Navigation containers expect", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
###################################3
#' Server-side app code
#'
#' Internal function, called within app_run()
#'
#' @param input A Shiny input object.
#' @param output A Shiny output object.
#' @param session A Shiny session object.
#'
#' @return A server object
#' @export
app_server <- function(input, output, session) {
  
  i18n <- getOption("template.translator")
  i18n$set_translation_language(
    transl_language = getOption("template.default.lang")
  )
  
  shiny::observeEvent(input$selected_language, {
    shiny.i18n::update_lang(
      language = input$selected_language,
      session = session
    )
    i18n$set_translation_language(transl_language = input$selected_language)
  })
  
  # --------- Detectores de hoja (POR TIPO, no por nombre exacto) ----------
  is_indiv <- shiny::reactive({
    shiny::req(input$sheet)
    padel_sheet_type(input$sheet) == "indiv"
  })
  
  is_parejas <- shiny::reactive({
    shiny::req(input$sheet)
    padel_sheet_type(input$sheet) == "parejas"
  })
  
  # --------- Datos base (siempre con padel_read_any) ----------
  stand_data <- shiny::reactive({
    shiny::req(input$sheet)
    if (!is_indiv()) return(NULL)
    padel_read_any(input$sheet)  # devuelve tibble con: grupo, posicion, jugador, etc.
  })
  
  parejas_data <- shiny::reactive({
    shiny::req(input$sheet)
    if (!is_parejas()) return(NULL)
    padel_read_any(input$sheet)  # devuelve tibble con: pareja, puntos, jornada, fecha...
  })
  
  # -----------------------------
  # ✅ Estado de la hoja seleccionada (mensaje debajo del selector)
  # -----------------------------
  sheet_data <- shiny::eventReactive(input$sheet, {
    shiny::req(input$sheet)
    suppressMessages(readxl::read_excel(padel_file(), sheet = input$sheet))
  }, ignoreInit = TRUE)
  
  output$sheet_status <- shiny::renderText({
    shiny::req(input$sheet)
    
    df <- if (is_indiv()) {
      stand_data()
    } else if (is_parejas()) {
      parejas_data()
    } else {
      sheet_data()
    }
    
    if (is.null(df)) return("")
    
    # hoja "vacía" si no tiene filas o si todo es NA/"" (en toda la tabla)
    if (nrow(df) == 0) return("Esta hoja todavía no tiene datos.")
    mat <- as.matrix(df)
    if (!any(!is.na(mat) & trimws(as.character(mat)) != "")) {
      return("Esta hoja todavía no tiene datos.")
    }
    
    "Datos cargados correctamente."
  })
  
  # --------- UI de filtros dinámicos ----------
  output$filters_ui <- shiny::renderUI({
    
    if (is_indiv()) {
      stand <- stand_data()
      if (is.null(stand) || !"grupo" %in% names(stand)) return(NULL)
      
      grupos <- unique(stand$grupo)
      grupos <- grupos[!is.na(grupos) & nzchar(grupos)]
      
      return(
        shinyWidgets::pickerInput(
          inputId = "grupo",
          label = "Filtrar por grupo",
          choices = c("Todos" = "__ALL__", grupos),
          selected = "__ALL__",
          options = list(`live-search` = TRUE)
        )
      )
    }
    
    if (is_parejas()) {
      df <- parejas_data()
      if (is.null(df) || !"pareja" %in% names(df)) return(NULL)
      
      pares <- unique(df$pareja)
      pares <- pares[!is.na(pares) & nzchar(trimws(pares))]
      
      return(
        shinyWidgets::pickerInput(
          inputId = "pareja_sel",
          label = "Filtrar por pareja",
          choices = c("Todas" = "__ALL__", pares),
          selected = "__ALL__",
          options = list(`live-search` = TRUE)
        )
      )
    }
    
    NULL
  })
  
  # --------- Filtrados ----------
  stand_filtered <- shiny::reactive({
    stand <- stand_data()
    if (is.null(stand)) return(NULL)
    
    if (!is.null(input$grupo) && input$grupo != "__ALL__" && "grupo" %in% names(stand)) {
      stand <- stand[stand$grupo == input$grupo, , drop = FALSE]
    }
    stand
  })
  
  parejas_filtered <- shiny::reactive({
    df <- parejas_data()
    if (is.null(df)) return(NULL)
    
    if (!is.null(input$pareja_sel) && input$pareja_sel != "__ALL__" && "pareja" %in% names(df)) {
      df <- df[df$pareja == input$pareja_sel, , drop = FALSE]
    }
    df
  })
  
  # --------- KPIs dinámicos ----------
  output$kpis <- shiny::renderUI({
    
    if (is_indiv()) {
      stand <- stand_filtered()
      if (is.null(stand)) return(myvbs(values = c("-", "-", "-")))
      
      k1 <- nrow(stand)  # jugadores
      k2 <- if ("grupo" %in% names(stand)) length(unique(stand$grupo)) else NA
      
      # en_liga puede venir como "Sí", "SI", "sí", NA...
      k3 <- if ("en_liga" %in% names(stand)) {
        sum(tolower(trimws(as.character(stand$en_liga))) == "sí", na.rm = TRUE)
      } else {
        NA
      }
      
      return(shiny::tagList(
        myvbs(
          values = c(k1, k2, k3),
          icons = c("people", "collection", "check-circle"),
          .fill = TRUE
        )
      ))
    }
    
    if (is_parejas()) {
      df <- parejas_filtered()
      if (is.null(df)) return(myvbs(values = c("-", "-", "-")))
      if (!all(c("pareja", "puntos") %in% names(df))) return(myvbs(values = c("-", "-", "-")))
      
      pts <- df$puntos
      if (!is.numeric(pts)) pts <- suppressWarnings(as.numeric(pts))
      
      agg <- stats::aggregate(
        pts ~ pareja,
        data = data.frame(pareja = df$pareja, pts = pts),
        FUN = function(x) sum(x, na.rm = TRUE)
      )
      names(agg) <- c("pareja", "puntos")
      
      k1 <- nrow(agg)
      k2 <- if (nrow(agg) > 0) max(agg$puntos, na.rm = TRUE) else NA
      k3 <- if (nrow(agg) > 0) mean(agg$puntos, na.rm = TRUE) else NA
      
      return(shiny::tagList(
        myvbs(
          values = c(k1, round(k2, 1), round(k3, 1)),
          icons = c("people", "trophy", "graph-up"),
          .fill = TRUE
        )
      ))
    }
    
    myvbs(values = c("-", "-", "-"))
  })
  
  # --------- Tabla (para TODAS las hojas) ----------
  output$tabla_excel <- DT::renderDT({
    shiny::req(input$sheet)
    
    df <- if (is_indiv()) {
      shiny::req(stand_filtered())
      stand_filtered()
    } else if (is_parejas()) {
      shiny::req(parejas_filtered())
      parejas_filtered()
    } else {
      padel_read_any(input$sheet)
    }
    
    DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15))
  })
  
  # --------- Cards dinámicos (Overview) ----------
  output$left_card <- shiny::renderUI({
    if (is_indiv()) {
      return(bslib::card(
        full_screen = TRUE,
        bslib::card_header("Jugadores por grupo"),
        shiny::plotOutput("grupo_plot", height = "350px")
      ))
    }
    
    if (is_parejas()) {
      return(bslib::card(
        full_screen = TRUE,
        bslib::card_header("Top 10 parejas por puntos (acumulado)"),
        shiny::plotOutput("parejas_top_plot", height = "350px")
      ))
    }
    
    bslib::card(
      bslib::card_header("Vista previa"),
      "Selecciona una hoja de clasificación (individual o parejas) para ver KPIs y gráficos."
    )
  })
  
  output$right_card <- shiny::renderUI({
    if (is_indiv()) {
      return(bslib::card(
        full_screen = TRUE,
        bslib::card_header("Top 10 (clasificación)"),
        shiny::plotOutput("top10_plot", height = "350px")
      ))
    }
    
    if (is_parejas()) {
      return(bslib::card(
        full_screen = TRUE,
        bslib::card_header("Puntos por jornada (sumatorio)"),
        shiny::plotOutput("parejas_jornada_plot", height = "350px")
      ))
    }
    
    bslib::card(
      bslib::card_header("Info"),
      "Esta hoja se muestra en modo tabla. Iremos añadiendo visualizaciones por tipo (jornadas, tesorería, calendario...)."
    )
  })
  
  # --------- Plots INDIVIDUAL ----------
  output$grupo_plot <- shiny::renderPlot({
    stand <- stand_data()
    shiny::req(stand)
    shiny::req("grupo" %in% names(stand))
    
    counts <- sort(table(stand$grupo), decreasing = TRUE)
    graphics::par(mar = c(8, 4, 3, 1))
    graphics::barplot(
      counts,
      las = 2,
      ylab = "Nº jugadores",
      main = "Distribución por grupo"
    )
  })
  
  output$top10_plot <- shiny::renderPlot({
    stand <- stand_filtered()
    shiny::req(stand)
    shiny::req(all(c("posicion", "jugador") %in% names(stand)))
    
    stand$pos_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(stand$posicion))))
    stand <- stand[!is.na(stand$pos_num), , drop = FALSE]
    
    top10 <- stand[order(stand$pos_num), , drop = FALSE]
    top10 <- top10[top10$pos_num <= 10, , drop = FALSE]
    
    top10$score <- 11 - top10$pos_num
    
    graphics::par(mar = c(5, 12, 3, 2))
    graphics::barplot(
      height = rev(top10$score),
      names.arg = rev(top10$jugador),
      horiz = TRUE,
      las = 1,
      xlab = "Ranking (más alto = mejor)"
    )
  })
  
  # --------- Plots PAREJAS ----------
  output$parejas_top_plot <- shiny::renderPlot({
    df <- parejas_filtered()
    shiny::req(df)
    shiny::req(all(c("pareja", "puntos") %in% names(df)))
    
    pts <- df$puntos
    if (!is.numeric(pts)) pts <- suppressWarnings(as.numeric(pts))
    
    agg <- stats::aggregate(
      pts ~ pareja,
      data = data.frame(pareja = df$pareja, pts = pts),
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    names(agg) <- c("pareja", "puntos")
    
    agg <- agg[order(agg$puntos, decreasing = TRUE), , drop = FALSE]
    top <- utils::head(agg, 10)
    
    graphics::par(mar = c(5, 12, 3, 2))
    graphics::barplot(
      height = rev(top$puntos),
      names.arg = rev(top$pareja),
      horiz = TRUE,
      las = 1,
      xlab = "Puntos acumulados"
    )
  })
  
  output$parejas_jornada_plot <- shiny::renderPlot({
    df <- parejas_filtered()
    shiny::req(df)
    shiny::req(all(c("jornada", "puntos") %in% names(df)))
    
    j <- suppressWarnings(as.numeric(df$jornada))
    pts <- df$puntos
    if (!is.numeric(pts)) pts <- suppressWarnings(as.numeric(pts))
    
    agg <- stats::aggregate(
      pts ~ j,
      data = data.frame(j = j, pts = pts),
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    names(agg) <- c("jornada", "puntos")
    agg <- agg[order(agg$jornada), , drop = FALSE]
    
    graphics::plot(
      agg$jornada, agg$puntos,
      type = "b",
      xlab = "Jornada",
      ylab = "Puntos (sumatorio)",
      main = "Puntos por jornada"
    )
  })
}


############################################################################3
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
      title = "Jugadores",
      value = values[1],
      theme = "warning",
      showcase = bsicons::bs_icon(icons[1], size = .size),
      showcase_layout = pos,
      fill = .fill
    ),
    value_box(
      title = "Grupos",
      value = values[2],
      theme = "danger",
      showcase = bsicons::bs_icon(icons[2], size = .size),
      showcase_layout = pos,
      fill = .fill
    ),
    value_box(
      title = "En liga",
      value = values[3],
      theme = "success",
      showcase = bsicons::bs_icon(icons[3], size = .size),
      showcase_layout = pos,
      fill = .fill
    )
  )
}
