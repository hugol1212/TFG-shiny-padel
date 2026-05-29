library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)

# ---------- helpers ----------
clean_names_simple <- function(nm) {
  nm <- tolower(nm)
  nm <- gsub("[^a-z0-9]+", "_", nm)
  nm <- gsub("^_|_$", "", nm)
  nm
}

detect_header_row <- function(raw_df, min_non_empty = 4) {
  non_empty <- apply(raw_df, 1, function(r) sum(!is.na(r) & r != ""))
  idx <- which(non_empty >= min_non_empty)[1]
  if (is.na(idx)) 1 else idx
}

extract_season <- function(sheet) {
  s <- str_squish(sheet)
  m <- str_match(s, "Tesorer[ií]a\\s+([0-9]{2})([0-9]{2})")
  if (!is.na(m[1, 2])) {
    y1 <- paste0("20", m[1, 2])
    y2 <- paste0("20", m[1, 3])
    return(paste0(y1, "-", y2))
  }
  s
}

# PARSEO ROBUSTO: evita que números pequeños se conviertan en fechas de 1900
parse_excel_date <- function(x, min_serial = 30000) {
  xn <- suppressWarnings(as.numeric(x))
  xn[is.na(xn) | xn < min_serial] <- NA_real_
  as.Date(xn, origin = "1899-12-30")
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# ---------- leer hoja tesorería ----------
read_finance_sheet <- function(sheet) {
  raw <- read_excel(excel_path, sheet = sheet, col_names = FALSE, n_max = 60, .name_repair = "unique_quiet")
  header_row <- detect_header_row(raw, min_non_empty = 4)
  
  df <- read_excel(excel_path, sheet = sheet, skip = header_row - 1, .name_repair = "unique_quiet")
  names(df) <- clean_names_simple(names(df))
  
  # quitar columnas ...1 ...2 etc (columnas vacías sin nombre)
  df <- df %>% select(-matches("^\\.{3}\\d+$"))
  
  # convertir todo a character para unir sin problemas
  df <- df %>% mutate(across(everything(), ~ as.character(.x)))
  
  df$source_sheet <- sheet
  df$season <- extract_season(sheet)
  df
}

# ---------- detectar hojas tesorería ----------
sheets <- excel_sheets(excel_path)
finance_sheets <- sheets[grepl("^Tesorer", sheets, ignore.case = TRUE)]
if (length(finance_sheets) == 0) stop("No se encontraron hojas que empiecen por 'Tesorería'.")

message("Hojas detectadas: ", paste(finance_sheets, collapse = " | "))

finance_raw <- bind_rows(lapply(finance_sheets, read_finance_sheet))

# ---------- localizar columnas clave por nombre ----------
pick_col <- function(nms, patterns) {
  nms0 <- tolower(nms)
  for (p in patterns) {
    hit <- which(str_detect(nms0, p))[1]
    if (!is.na(hit)) return(nms[hit])
  }
  NA_character_
}

nms <- names(finance_raw)

col_fecha   <- pick_col(nms, c("^fecha$"))
col_jornada <- pick_col(nms, c("^jornada$"))
col_trim    <- pick_col(nms, c("^trimestre$"))
col_bote    <- pick_col(nms, c("^bote$"))
col_bolas   <- pick_col(nms, c("gasto.*bolas", "bolas"))
col_mont    <- pick_col(nms, c("^montados$"))
col_otros   <- pick_col(nms, c("otros.*gastos", "^otros$"))
col_det     <- pick_col(nms, c("^detalles?$"))
col_prem    <- pick_col(nms, c("^premios", "premio"))

if (is.na(col_fecha) || is.na(col_jornada) || is.na(col_trim)) {
  stop("No pude encontrar columnas básicas (fecha/jornada/trimestre). Revisa nombres en la hoja.")
}

# ---------- construir tabla canónica ----------
finance_clean <- finance_raw %>%
  mutate(across(where(is.character), ~ str_squish(.x))) %>%
  transmute(
    season = season,
    source_sheet = source_sheet,
    
    fecha_raw = .data[[col_fecha]],
    fecha = parse_excel_date(.data[[col_fecha]]),
    
    jornada_raw = .data[[col_jornada]],
    jornada = as.integer(as_num(.data[[col_jornada]])),
    
    trimestre_raw = if (!is.na(col_trim)) .data[[col_trim]] else NA_character_,
    trimestre = if (!is.na(col_trim)) {
      str_replace_all(.data[[col_trim]], "[^A-Za-záéíóúÁÉÍÓÚ ]", "") |> str_squish()
    } else NA_character_,
    
    bote = if (!is.na(col_bote)) as_num(.data[[col_bote]]) else NA_real_,
    gasto_bolas = if (!is.na(col_bolas)) as_num(.data[[col_bolas]]) else NA_real_,
    montados = if (!is.na(col_mont)) as_num(.data[[col_mont]]) else NA_real_,
    otros_gastos = if (!is.na(col_otros)) as_num(.data[[col_otros]]) else NA_real_,
    
    detalles = if (!is.na(col_det)) .data[[col_det]] else NA_character_,
    premios = if (!is.na(col_prem)) .data[[col_prem]] else NA_character_
  ) %>%
  # eliminar filas vacías evidentes
  filter(!(is.na(fecha) & is.na(jornada) & (is.na(trimestre) | trimestre == ""))) %>%
  # quedarnos SOLO con movimientos por jornada (para dashboard inicial)
  filter(!is.na(fecha) & !is.na(jornada)) %>%
  arrange(fecha, jornada)

saveRDS(finance_clean, "data/clean/finance.rds")

message("OK -> guardado data/clean/finance.rds")
message("Filas: ", nrow(finance_clean))
message("Fechas NA: ", sum(is.na(finance_clean$fecha)))
message("Jornadas NA: ", sum(is.na(finance_clean$jornada)))
message("Fechas en 1900: ", sum(lubridate::year(finance_clean$fecha) == 1900, na.rm = TRUE))
