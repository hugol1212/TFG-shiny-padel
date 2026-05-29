library(dplyr)
library(stringr)

in_matches <- "data/clean/matches_all.rds"
in_sat     <- "data/clean/satellites_all.rds"
out_file   <- "data/clean/matches_plus_satellites.rds"

stopifnot(file.exists(in_matches), file.exists(in_sat))

m <- readRDS(in_matches)
s <- readRDS(in_sat)

# --- normaliza texto (quita comillas raras y espacios) ---
clean_name <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\u00a0", " ") %>%   # NBSP -> space
    str_replace_all('^"+|"+$', "") %>%   # quita comillas al principio/fin
    str_replace_all('^\\\\+"|\\\\+"$', "") %>% # por si vienen escapadas
    str_squish()
}

# CUADRO
m2 <- m %>%
  mutate(
    tipo = "CUADRO",
    pareja_a = clean_name(pareja_a),
    pareja_b = clean_name(pareja_b)
  )

# SATÉLITE -> lo adaptamos al mismo “schema”
# (bloque/bloque_num/enfrentamiento se rellenan con info de pista/partido)
s2 <- s %>%
  mutate(
    tipo = "SATELITE",
    season = if ("season" %in% names(.)) season else NA_character_,
    bloque = "SATELITE",
    bloque_num = suppressWarnings(as.integer(str_extract(pista, "\\d+"))),
    enfrentamiento = suppressWarnings(as.integer(str_extract(partido, "\\d+"))),
    pareja_a = clean_name(pareja_a),
    pareja_b = clean_name(pareja_b)
  ) %>%
  # garantizamos columnas que existen en el “cuadro”
  mutate(
    winner = ifelse(is.na(winner), NA_character_, winner)
  ) %>%
  select(any_of(names(m2)))  # deja exactamente las columnas de m2 que pueda

# Unión
all2 <- bind_rows(m2, s2)

saveRDS(all2, out_file)

cat("OK -> guardado", out_file, "\n")
cat("Filas total:", nrow(all2), "\n")
cat("CUADRO:", sum(all2$tipo == "CUADRO", na.rm = TRUE), "\n")
cat("SATELITE:", sum(all2$tipo == "SATELITE", na.rm = TRUE), "\n")
