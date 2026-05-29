library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)

# ---- limpiar nombres de columnas (sin janitor) ----
clean_names_simple <- function(nm) {
  nm <- tolower(nm)
  nm <- gsub("[^a-z0-9]+", "_", nm)
  nm <- gsub("^_|_$", "", nm)
  nm
}

# ---- detectar fila cabecera ----
detect_header_row <- function(raw_df, min_non_empty = 4) {
  non_empty <- apply(raw_df, 1, function(r) sum(!is.na(r) & r != ""))
  idx <- which(non_empty >= min_non_empty)[1]
  if (is.na(idx)) 1 else idx
}

# ---- extraer season desde el nombre de la hoja ----
extract_season <- function(sheet) {
  s <- str_squish(sheet)
  m <- str_match(s, "Calendario\\s+([0-9]{4})([0-9]{4})")
  
  if (!is.na(m[1, 2])) {
    return(paste0(m[1, 2], "-", m[1, 3]))
  }
  
  s
}

# ---- parseo robusto de fechas ----
parse_date_robust <- function(x) {
  # 1) si es número Excel (serial), lo convertimos
  xn <- suppressWarnings(as.numeric(x))
  out_num <- as.Date(xn, origin = "1899-12-30")
  out_num[is.na(xn) | xn < 30000] <- NA  # evita basura
  
  # 2) si es texto tipo dd/mm/yyyy o dd-mm-yyyy
  xs <- as.character(x)
  xs <- stringr::str_squish(xs)
  
  # normalizar separadores a "/"
  xs2 <- gsub("-", "/", xs)
  
  # primero intentamos dmy (España)
  out_chr <- suppressWarnings(lubridate::dmy(xs2))
  
  # si falla, probamos ymd por si viniera así ya
  out_chr2 <- suppressWarnings(lubridate::ymd(xs2))
  
  # combinar: prioriza num si existe; si no, dmy; si no, ymd
  dplyr::coalesce(out_num, out_chr, out_chr2)
}


# ---- leer una hoja calendario robusta ----
read_calendar_sheet <- function(sheet) {
  raw <- read_excel(excel_path, sheet = sheet, col_names = FALSE, n_max = 120)
  header_row <- detect_header_row(raw, min_non_empty = 4)
  
  df <- read_excel(excel_path, sheet = sheet, skip = header_row - 1)
  
  names(df) <- clean_names_simple(names(df))
  df <- df %>% select(-matches("^\\.{3}\\d+$"))
  
  
  # normalizar el nombre del año si viene como a_o por la Ñ
  if ("a_o" %in% names(df) && !"ano" %in% names(df)) {
    df <- df %>% rename(ano = a_o)
  }
  
  # quitar columnas ...1 etc si existieran
  df <- df %>% select(-matches("^\\.\\.\\."))
  
  # IMPORTANTÍSIMO: convertir todo a character para poder hacer bind_rows sin conflictos
  df <- df %>% mutate(across(everything(), ~ as.character(.x)))
  
  df$source_sheet <- sheet
  df$season <- extract_season(sheet)
  
  df
}

# ------------------------------------------------------------------
# 1) detectar hojas Calendario automáticamente
# ------------------------------------------------------------------
sheets <- excel_sheets(excel_path)
calendar_sheets <- sheets[grepl("^Calendario", sheets, ignore.case = TRUE)]

if (length(calendar_sheets) == 0) stop("No se encontraron hojas que empiecen por 'Calendario'.")

message("Hojas detectadas: ", paste(calendar_sheets, collapse = " | "))

# ------------------------------------------------------------------
# 2) leer + unir
# ------------------------------------------------------------------
calendar_raw <- bind_rows(lapply(calendar_sheets, read_calendar_sheet))

# ------------------------------------------------------------------
# 3) detectar columna fecha
# ------------------------------------------------------------------
date_col <- names(calendar_raw)[str_detect(names(calendar_raw), "fecha")][1]
if (is.na(date_col)) stop("No se encontró columna que contenga 'fecha'.")

# ------------------------------------------------------------------
# 4) limpieza canónica final
# ------------------------------------------------------------------
calendar_clean <- calendar_raw %>%
  mutate(across(where(is.character), ~ str_squish(.x))) %>%
  mutate(
    fecha_raw = .data[[date_col]],
    fecha = parse_date_robust(.data[[date_col]]),
    trimestre = if ("trimestre" %in% names(.)) {
      str_replace_all(trimestre, "[^A-Za-záéíóúÁÉÍÓÚ ]", "") |> str_squish()
    } else NA_character_,
    jornada = if ("jornada" %in% names(.)) as.integer(as.numeric(jornada)) else NA_integer_,
    ano = if ("ano" %in% names(.)) {
      a <- suppressWarnings(as.integer(as.numeric(ano)))
      ifelse(is.na(a), year(fecha), a)
    } else year(fecha),
    numero_de_asistentes = if ("numero_de_asistentes" %in% names(.)) {
      as.integer(as.numeric(numero_de_asistentes))
    } else NA_integer_
  ) %>%
  filter(if_any(everything(), ~ !is.na(.x) & .x != "")) %>%
  filter(!(is.na(fecha) & is.na(fecha_raw) & is.na(jornada))) %>%   # 👈 AÑADIR ESTA
  arrange(fecha, jornada) %>%
  relocate(any_of(c(
    "season", "ano", "trimestre", "jornada", "fecha", "dia", "sede",
    "numero_de_asistentes",
    "pareja_bloqueada_alta", "pareja_bloqueada_baja",
    "jornada_de_ascenso_descenso", "notas",
    "source_sheet", "fecha_raw"
  )), .before = everything())
# ---- limpiar columnas basura típicas de Excel ----

# 1) quitar columnas con nombres tipo "...1", "...2"
calendar_clean <- calendar_clean %>%
  dplyr::select(-dplyr::matches("^\\.{3}\\d+$"))

# 2) quitar columnas con nombres numéricos ("2", "11", "13", etc.)
calendar_clean <- calendar_clean %>%
  dplyr::select(-dplyr::matches("^\\d+$"))

# 3) quitar columnas tipo x13, x14...
calendar_clean <- calendar_clean %>%
  dplyr::select(-dplyr::matches("^x\\d+$"))

# 4) quitar columnas casi vacías (>=95% NA o "")
is_mostly_empty <- function(x, thr = 0.95) {
  x <- as.character(x)
  empties <- is.na(x) | trimws(x) == ""
  mean(empties) >= thr
}

drop_cols <- names(calendar_clean)[vapply(calendar_clean, is_mostly_empty, logical(1))]
# OJO: no borres columnas clave aunque estuvieran vacías en alguna temporada
keep_cols <- c("season", "source_sheet", "fecha", "fecha_raw", "jornada", "ano", "trimestre")
drop_cols <- setdiff(drop_cols, keep_cols)

calendar_clean <- calendar_clean %>% dplyr::select(-dplyr::any_of(drop_cols))



saveRDS(calendar_clean, "data/clean/calendar.rds")

message("OK -> guardado data/clean/calendar.rds")
message("Hojas integradas: ", length(calendar_sheets))
message("Filas: ", nrow(calendar_clean))
message("Fechas NA: ", sum(is.na(calendar_clean$fecha)))
