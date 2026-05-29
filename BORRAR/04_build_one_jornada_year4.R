library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"
sheet <- "J10-VERANO-AÑO IV"

raw <- read_excel(excel_path, sheet = sheet, col_names = FALSE)

# matriz de texto
m <- as.matrix(raw)
m <- apply(m, c(1,2), function(x) if (is.na(x)) "" else as.character(x))

# localizar fila "Partido"
part_row <- which(apply(m, 1, function(r) any(str_detect(r, regex("^part", ignore_case = TRUE)))))[1]

# columnas donde hay "Partido"
part_cols <- which(str_detect(m[part_row, ], regex("^part", ignore_case = TRUE)))

# extraer numero de jornada desde el nombre de la hoja (ej: J10-...)
jornada_num <- as.integer(str_match(sheet, "^J(\\d+)")[,2])

# season fija (AÑO IV = 2024-2025)
season <- "2024-2025 (año iv)"

# k = 0,1,2 (tres enfrentamientos)
extract_block <- function(col) {
  
  partido_label <- str_trim(m[part_row, col]) # "Partido 1"
  partido_num   <- suppressWarnings(as.integer(str_extract(partido_label, "\\d+")))
  
  rows_pairs <- part_row + 1 + (0:2) * 2
  rows_pts   <- part_row + 2 + (0:2) * 2
  
  tibble(
    season = season,
    jornada = jornada_num,
    source_sheet = sheet,
    bloque = partido_label,
    bloque_num = partido_num,
    enfrentamiento = 1:3,
    pareja_a = str_trim(m[rows_pairs, col]),
    puntos_a = suppressWarnings(as.numeric(m[rows_pts, col])),
    pareja_b = str_trim(m[rows_pairs, col + 2]),
    puntos_b = suppressWarnings(as.numeric(m[rows_pts, col + 2]))
  )
}

matches_one <- bind_rows(lapply(part_cols, extract_block)) %>%
  # quitamos filas vacías (por si alguna jornada tiene menos partidos)
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

print(matches_one)
cat("\nFilas extraídas:", nrow(matches_one), "\n")
