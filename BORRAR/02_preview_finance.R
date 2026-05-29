library(readxl)
library(dplyr)

excel_path <- "data/Copia de Four Seasons Pádel_ Resultados, tesorería, calendario, eventos.xlsx"

# Hojas de tesorería detectadas automáticamente
sheets <- excel_sheets(excel_path)
finance_sheets <- sheets[grepl("^Tesorer", sheets, ignore.case = TRUE)]
print(finance_sheets)

# Detectar fila de cabecera (igual que en calendario)
detect_header_row <- function(raw_df, min_non_empty = 4) {
  non_empty <- apply(raw_df, 1, function(r) sum(!is.na(r) & r != ""))
  idx <- which(non_empty >= min_non_empty)[1]
  if (is.na(idx)) 1 else idx
}

preview_one <- function(sheet) {
  raw <- read_excel(excel_path, sheet = sheet, col_names = FALSE, n_max = 60)
  header_row <- detect_header_row(raw, min_non_empty = 4)
  
  message("\n============================")
  message("SHEET: ", sheet)
  message("Header row guess: ", header_row)
  
  # Enseñamos 12 filas alrededor de donde creemos que empieza (máx 12 columnas)
  start <- max(1, header_row - 3)
  end   <- min(nrow(raw), header_row + 8)
  print(raw[start:end, 1:min(12, ncol(raw))])
  
  # Intento de leer ya “bien” desde la cabecera
  df <- read_excel(excel_path, sheet = sheet, skip = header_row - 1)
  
  message("\nColumnas detectadas (primeras 40):")
  print(head(names(df), 40))
  message("Total columnas: ", ncol(df))
  
  message("\nPrimeras 5 filas (primeras 12 columnas):")
  print(df[1:5, 1:min(12, ncol(df))])
}

# Previsualizamos solo las 2 más recientes (para no inundarte)
preview_one("Tesorería 2425 AÑO IV")
preview_one("Tesorería 2526 AÑO V")

