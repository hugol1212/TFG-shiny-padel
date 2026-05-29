library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"

# 1) Detectar hojas de jornadas AÑO IV
sheets <- excel_sheets(excel_path)
j4 <- sheets[str_detect(sheets, "^J\\d+.*AÑO IV")]

cat("Hojas detectadas AÑO IV:", length(j4), "\n")
cat(paste(j4, collapse = " | "), "\n\n")

# 2) Función: extraer partidos (15 filas típicamente) desde una hoja
extract_jornada_sheet <- function(sheet) {
  
  raw <- read_excel(excel_path, sheet = sheet, col_names = FALSE)
  
  # matriz de texto
  m <- as.matrix(raw)
  m <- apply(m, c(1,2), function(x) if (is.na(x)) "" else as.character(x))
  
  # localizar fila "Partido"
  part_row <- which(apply(m, 1, function(r) any(str_detect(r, regex("^part", ignore_case = TRUE)))))[1]
  if (is.na(part_row)) {
    warning("No se encontró fila 'Partido' en hoja: ", sheet)
    return(tibble())
  }
  
  # columnas donde hay "Partido"
  part_cols <- which(str_detect(m[part_row, ], regex("^part", ignore_case = TRUE)))
  if (length(part_cols) == 0) {
    warning("No se encontraron columnas 'Partido' en hoja: ", sheet)
    return(tibble())
  }
  
  # jornada desde el nombre: J10-...
  jornada_num <- suppressWarnings(as.integer(str_match(sheet, "^J(\\d+)")[,2]))
  if (is.na(jornada_num)) jornada_num <- NA_integer_
  
  season <- "2024-2025 (año iv)"
  
  extract_block <- function(col) {
    
    partido_label <- str_trim(m[part_row, col]) # "Partido 1"
    bloque_num    <- suppressWarnings(as.integer(str_extract(partido_label, "\\d+")))
    
    # tres enfrentamientos (1..3)
    rows_pairs <- part_row + 1 + (0:2) * 2
    rows_pts   <- part_row + 2 + (0:2) * 2
    
    tibble(
      season = season,
      jornada = jornada_num,
      source_sheet = sheet,
      bloque = partido_label,
      bloque_num = bloque_num,
      enfrentamiento = 1:3,
      pareja_a = str_trim(m[rows_pairs, col]),
      puntos_a = suppressWarnings(as.numeric(m[rows_pts, col])),
      pareja_b = str_trim(m[rows_pairs, col + 2]),
      puntos_b = suppressWarnings(as.numeric(m[rows_pts, col + 2]))
    )
  }
  
  out <- bind_rows(lapply(part_cols, extract_block)) %>%
    filter(!(pareja_a == "" & pareja_b == "")) %>%
    mutate(
      winner = case_when(
        is.na(puntos_a) | is.na(puntos_b) ~ NA_character_,
        puntos_a > puntos_b ~ "A",
        puntos_b > puntos_a ~ "B",
        TRUE ~ "Empate"
      )
    ) %>%
    arrange(bloque_num, enfrentamiento)
  
  out
}

# 3) Ejecutar para todas las hojas
matches_year4 <- bind_rows(lapply(j4, extract_jornada_sheet)) %>%
  arrange(jornada, bloque_num, enfrentamiento)

# 4) Guardar
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
saveRDS(matches_year4, "data/clean/matches_year4.rds")

cat("OK -> guardado data/clean/matches_year4.rds\n")
cat("Filas:", nrow(matches_year4), "\n")
cat("Hojas procesadas:", length(j4), "\n")
cat("Jornadas únicas:", length(unique(matches_year4$jornada)), "\n")
