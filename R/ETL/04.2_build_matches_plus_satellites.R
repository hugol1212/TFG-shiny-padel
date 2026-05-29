library(dplyr)
library(stringr)

in_file  <- "data/clean/matches_plus_satellites.rds"
out_file <- "data/clean/matches_plus_satellites.rds"  # sobreescribe

x <- readRDS(in_file)

# mapear AÑO -> season igual que tu calendario/tesorería
guess_season_from_sheet <- function(sheet) {
  s <- toupper(sheet)
  dplyr::case_when(
    str_detect(s, "AÑO\\s*IV") ~ "2024-2025",
    str_detect(s, "AÑO\\s*V")  ~ "2025-2026",
    # si en el futuro haces AÑO III/II/I:
    str_detect(s, "AÑO\\s*III") ~ "2023-2024",
    str_detect(s, "AÑO\\s*II")  ~ "2022-2023",
    str_detect(s, "AÑO\\s*I")   ~ "2021-2022",
    TRUE ~ NA_character_
  )
}

x2 <- x %>%
  mutate(
    season = if_else(
      tipo == "SATELITE" & (is.na(season) | season == ""),
      guess_season_from_sheet(source_sheet),
      season
    )
  )

saveRDS(x2, out_file)

cat("OK -> season SATELITE relleno\n")
cat("NA season total:", sum(is.na(x2$season)), "\n")
cat("NA season en SATELITE:", sum(is.na(x2$season) & x2$tipo=="SATELITE"), "\n")
cat("Seasons:", paste(sort(unique(x2$season)), collapse=" | "), "\n")
