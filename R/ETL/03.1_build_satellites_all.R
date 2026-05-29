library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

padel_norm_cell <- function(x){
  x %>%
    as.character() %>%
    str_replace_all("\u00a0", " ") %>%   # NBSP -> espacio normal
    str_squish()
}

padel_sheet_meta <- function(sheet){
  jornada <- suppressWarnings(as.integer(str_match(sheet, "^J(\\d+)")[,2]))
  trimestre <- str_match(sheet, "^J\\d+-(.*?)-AÑO")[,2] %>% toupper()
  if (is.na(trimestre)) trimestre <- NA_character_
  list(jornada = jornada, trimestre = trimestre)
}

extract_satellite_block <- function(df, start_row, start_col, pista_label){
  blk <- df[start_row:(start_row+40), start_col:(start_col+2), drop = FALSE]
  
  idx_part <- which(str_detect(tolower(blk[[1]]), "^partido"))
  if (length(idx_part) == 0) return(tibble())
  
  map_dfr(seq_along(idx_part), function(k){
    i1 <- idx_part[k]
    i2 <- if (k < length(idx_part)) idx_part[k+1] - 1 else nrow(blk)
    
    seg <- blk[i1:i2, , drop = FALSE]
    partido_label <- seg[1,1] %>% as.character()
    
    seg2 <- seg[-1, , drop = FALSE]
    
    pick <- NULL
    for (rr in seq_len(nrow(seg2))) {
      a <- suppressWarnings(as.numeric(seg2[rr,2]))
      b <- suppressWarnings(as.numeric(seg2[rr,3]))
      txt <- seg2[rr,1] %>% as.character()
      
      if (!is.na(a) && !is.na(b) && !is.na(txt) && txt != "") {
        pick <- list(txt = txt, a = a, b = b)
        break
      }
    }
    
    if (is.null(pick)) return(tibble())
    
    # ---- PARSEO CORREGIDO DE PAREJAS ----
    # Formatos:
    # 1) "A vs B"
    # 2) "A1 - A2 B1 - B2"   <-- este es tu caso real
    txt <- pick$txt
    pareja_a <- str_squish(txt)
    pareja_b <- NA_character_
    
    if (str_detect(txt, "\\s+vs\\s+")) {
      parts <- str_split(txt, "\\s+vs\\s+", simplify = TRUE)
      pareja_a <- str_squish(parts[1])
      pareja_b <- str_squish(parts[2])
      
    } else if (str_detect(txt, "\\s*-\\s*")) {
      # Regex: (A1) - (A2) (B1) - (B2)
      m <- str_match(txt, "^\\s*(.+?)\\s*-\\s*(.+?)\\s+(.+?)\\s*-\\s*(.+?)\\s*$")
      
      if (!is.na(m[1,1])) {
        pareja_a <- str_squish(paste(m[1,2], m[1,3], sep = " - "))
        pareja_b <- str_squish(paste(m[1,4], m[1,5], sep = " - "))
      } else {
        # fallback: "A - B"
        parts <- str_split(txt, "\\s*-\\s*", simplify = TRUE) %>% as.character()
        parts <- parts[parts != "" & !is.na(parts)] %>% str_squish()
        
        if (length(parts) == 2) {
          pareja_a <- parts[1]
          pareja_b <- parts[2]
        } else {
          pareja_a <- str_squish(txt)
          pareja_b <- NA_character_
        }
      }
    }
    
    tibble(
      pista = pista_label,
      partido = partido_label,
      pareja_a = pareja_a,
      pareja_b = pareja_b,
      puntos_a = pick$a,
      puntos_b = pick$b,
      winner = case_when(
        pick$a > pick$b ~ "A",
        pick$b > pick$a ~ "B",
        TRUE ~ "EMPATE"
      )
    )
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
  invisible(sat_all)
}

# --- EJECUCIÓN ---
excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"
build_satellites_all(excel_path, "data/clean/satellites_all.rds")
