# R/etl/06_build_satellites_only.R
suppressPackageStartupMessages({
  library(dplyr)
})

in_path  <- "data/clean/matches_plus_satellites.rds"
out_path <- "data/clean/satellites_only.rds"

stopifnot(file.exists(in_path))

x <- readRDS(in_path)

sat <- x %>%
  filter(tipo == "SATELITE") %>%
  # opcional: deja solo columnas útiles
  select(
    season, jornada, source_sheet,
    pista = bloque_num,   # si quieres, luego lo renombramos a PISTA 2/4
    partido = enfrentamiento,
    pareja_a, puntos_a,
    pareja_b, puntos_b,
    winner, tipo
  )

saveRDS(sat, out_path)

cat("OK -> guardado", out_path, "\n")
cat("Filas:", nrow(sat), "\n")
cat("Seasons:", paste(sort(unique(sat$season)), collapse = " | "), "\n")
