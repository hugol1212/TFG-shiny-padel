library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"

sheets <- excel_sheets(excel_path)

# hojas que parecen "jornadas"
j_sheets <- sheets[str_detect(sheets, "^J\\d+")]

cat("Hojas de jornada detectadas:", length(j_sheets), "\n")

# ---- helpers ----
season_from_sheet <- function(sheet_name) {
  s <- str_to_lower(sheet_name)
  
  # AÑO IV / V / 3 / 2 / 1
  if (str_detect(s, "año\\s*iv|año\\s*4")) return("2024-2025")
  if (str_detect(s, "año\\s*v|año\\s*5"))  return("2025-2026")
  if (str_detect(s, "año\\s*3|\\ba3\\b"))  return("2023-2024")
  if (str_detect(s, "año\\s*2|\\ba2\\b"))  return("2022-2023")
  if (str_detect(s, "año\\s*1|\\ba1\\b"))  return("2021-2022")
  
  # si no se detecta, NA
  NA_character_
}

extract_jornada_sheet <- function(sheet) {
  
  raw <- read_excel(excel_path, sheet = sheet, col_names = FALSE)
  
  m <- as.matrix(raw)
  m <- apply(m, c(1,2), function(x) if (is.na(x)) "" else as.character(x))
  
  part_row <- which(apply(m, 1, function(r) any(str_detect(r, regex("^part", ignore_case = TRUE)))))[1]
  if (is.na(part_row)) return(tibble())
  
  part_cols <- which(str_detect(m[part_row, ], regex("^part", ignore_case = TRUE)))
  if (length(part_cols) == 0) return(tibble())
  
  jornada_num <- suppressWarnings(as.integer(str_match(sheet, "^J(\\d+)")[,2]))
  if (is.na(jornada_num)) jornada_num <- NA_integer_
  
  season <- season_from_sheet(sheet)
  
  extract_block <- function(col) {
    partido_label <- str_trim(m[part_row, col])
    bloque_num    <- suppressWarnings(as.integer(str_extract(partido_label, "\\d+")))
    
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
  
  bind_rows(lapply(part_cols, extract_block)) %>%
    filter(!(pareja_a == "" & pareja_b == "")) %>%
    mutate(
      winner = case_when(
        is.na(puntos_a) | is.na(puntos_b) ~ NA_character_,
        puntos_a > puntos_b ~ "A",
        puntos_b > puntos_a ~ "B",
        TRUE ~ "Empate"
      )
    )
}

# ejecutar para todas
matches_all <- bind_rows(lapply(j_sheets, extract_jornada_sheet)) %>%
  filter(!is.na(season)) %>%      # si no detecta año, lo descartamos por seguridad
  arrange(season, jornada, bloque_num, enfrentamiento)

dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
saveRDS(matches_all, "data/clean/matches_all.rds")

cat("OK -> guardado data/clean/matches_all.rds\n")
cat("Filas:", nrow(matches_all), "\n")
cat("Seasons:\n")
print(table(matches_all$season, useNA = "ifany"))
