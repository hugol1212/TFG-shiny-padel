# R/etl/05_build_satellites_all.R
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(stringi)
})

padel_norm_cell <- function(x){
  x %>%
    as.character() %>%
    str_replace_all("\u00a0", " ") %>%  # NBSP -> space
    str_squish()
}

padel_sheet_meta <- function(sheet){
  jornada <- suppressWarnings(as.integer(str_match(sheet, "^J(\\d+)")[,2]))
  trimestre <- str_match(sheet, "^J\\d+-(.*?)-AÑO")[,2] %>% toupper()
  if (is.na(trimestre)) trimestre <- NA_character_
  list(jornada = jornada, trimestre = trimestre)
}

# Detecta "Partido X" (incluye "Partido 4-Extra")
is_partido_label <- function(x){
  x0 <- tolower(padel_norm_cell(x))
  str_detect(x0, "^partido\\s*\\d")  # Partido 1, Partido 2, Partido 3, Partido 4-Extra...
}

# Detecta fila "vs"
is_vs <- function(x){
  x0 <- tolower(padel_norm_cell(x))
  x0 == "vs"
}

# Intenta convertir a número (puntos)
as_num <- function(x){
  suppressWarnings(as.numeric(str_replace_all(padel_norm_cell(x), ",", ".")))
}

# Extrae un bloque de satélite de 3 columnas a partir de ancla "PISTA X"
# NOTA: en satélite el patrón es (por enfrentamiento):
#   fila A (texto)
#   fila "vs"
#   fila B (texto)
#   fila puntos (col2 y col3 num)
extract_satellite_block <- function(df, start_row, start_col, pista_label){
  
  # Ventana razonable hacia abajo (ajusta si alguna hoja es más larga)
  end_row <- min(nrow(df), start_row + 80)
  
  blk <- df[start_row:end_row, start_col:(start_col+2), drop = FALSE]
  colnames(blk) <- c("c1", "c2", "c3")
  
  # localizar filas donde empieza cada "Partido"
  idx_part <- which(map_lgl(blk$c1, is_partido_label))
  if (length(idx_part) == 0) return(tibble())
  
  map_dfr(seq_along(idx_part), function(k){
    
    i1 <- idx_part[k]
    i2 <- if (k < length(idx_part)) idx_part[k+1] - 1 else nrow(blk)
    
    seg <- blk[i1:i2, , drop = FALSE]
    partido_label <- padel_norm_cell(seg$c1[1])
    
    # quitamos la fila del título "Partido X"
    seg2 <- seg[-1, , drop = FALSE]
    if (nrow(seg2) < 4) return(tibble())
    
    # buscamos filas de puntos: donde c2 y c3 sean numéricos
    pts_rows <- which(!is.na(as_num(seg2$c2)) & !is.na(as_num(seg2$c3)))
    if (length(pts_rows) == 0) return(tibble())
    
    # para cada fila de puntos, reconstruimos A / vs / B mirando hacia arriba
    out <- map_dfr(seq_along(pts_rows), function(j){
      r_pts <- pts_rows[j]
      
      # necesitamos al menos 3 filas arriba: A, vs, B
      if (r_pts < 3) return(tibble())
      
      txt_a <- padel_norm_cell(seg2$c1[r_pts - 3])
      txt_vs <- padel_norm_cell(seg2$c1[r_pts - 2])
      txt_b <- padel_norm_cell(seg2$c1[r_pts - 1])
      
      # validación básica
      if (!is_vs(txt_vs)) return(tibble())
      if (txt_a == "" || txt_b == "") return(tibble())
      
      # En algunos casos la pareja puede venir como "Nombre1 Nombre2"
      # Si solo hay 1 nombre, se queda 1. Si hay más, mantenemos tal cual.
      pareja_a <- txt_a
      pareja_b <- txt_b
      
      pa <- as_num(seg2$c2[r_pts])
      pb <- as_num(seg2$c3[r_pts])
      
      tibble(
        pista = pista_label,
        partido = partido_label,
        enfrentamiento = j,         # 1..n dentro del partido
        pareja_a = pareja_a,        # aquí ya van "dos jugadores" si la celda lo trae así
        pareja_b = pareja_b,
        puntos_a = pa,
        puntos_b = pb,
        winner = case_when(
          pa > pb ~ "A",
          pb > pa ~ "B",
          TRUE ~ "EMPATE"
        )
      )
    })
    
    out
  })
}

extract_satellites_one_sheet <- function(excel_path, sheet){
  raw <- read_excel(excel_path, sheet = sheet, col_names = FALSE)
  df <- raw %>% mutate(across(everything(), padel_norm_cell))
  
  pos <- function(value){
    w <- which(df == value, arr.ind = TRUE)
    if (nrow(w) == 0) return(NULL)
    w[1, , drop = FALSE]
  }
  
  p2 <- pos("PISTA 2")
  p4 <- pos("PISTA 4")
  if (is.null(p2) && is.null(p4)) return(tibble())
  
  meta <- padel_sheet_meta(sheet)
  
  out <- bind_rows(
    if (!is.null(p2)) extract_satellite_block(df, start_row = p2[1,"row"], start_col = p2[1,"col"], pista_label = "PISTA 2") else tibble(),
    if (!is.null(p4)) extract_satellite_block(df, start_row = p4[1,"row"], start_col = p4[1,"col"], pista_label = "PISTA 4") else tibble()
  )
  
  if (nrow(out) == 0) return(tibble())
  
  out %>%
    mutate(
      source_sheet = sheet,
      jornada = meta$jornada,
      trimestre = meta$trimestre
    )
}

build_satellites_all <- function(excel_path, out_path = "data/clean/satellites_all.rds"){
  sheets <- excel_sheets(excel_path)
  j_sheets <- sheets[str_detect(sheets, "^J\\d+-")]
  
  sat_all <- map_dfr(j_sheets, ~extract_satellites_one_sheet(excel_path, .x))
  
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sat_all, out_path)
  
  message("OK -> guardado ", out_path)
  message("Filas: ", nrow(sat_all))
  if (nrow(sat_all) > 0) message("Hojas con satélite: ", sat_all %>% distinct(source_sheet) %>% nrow())
  
  invisible(sat_all)
}

# --- EJECUCIÓN ---
excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"
build_satellites_all(excel_path, "data/clean/satellites_all.rds")
