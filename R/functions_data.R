# R/functions_data.R

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(tibble)
library(purrr)

# ----------------------------
# Diccionario: alias -> nombre corto (CANÓNICO)
# ----------------------------
player_alias <- c(
  "Sanjur"  = "David S",
  "Lisbel" = "Lisbel V",
  "Adri" = "Adri L",
  "Tito M" = "Rubén D",
  "Golden" = "Rubén D",
  "DJ" = "David J",
  "Alfon"= "Alfon R",
  "Ace" = "Victor A",
  "Mora" = "Alejandro M",
  "Anés" = "Rubén A",
  "Astu" = "Rubén D",
  "Tito" = "Antonio M",
  "Loren" = "Rubén R",
  "Canario" = "Jonatan A",
  "Torbe" = "Francisco J",
  "Cami"= "Camilo G",
  "Alarcón" = "Javier A",
  "Cate"= "Alegandro C",
  "Fonsy"= "Alberto F",
  "Funci" = "David L",
  "Quero" = "Óscar Q"
)

# ----------------------------
# Normalización de nombres
# ----------------------------
normalize_player_name <- function(x) {
  x <- as.character(x)
  
  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, "\\.+", "")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  
  key <- stringr::str_to_lower(x)
  alias_key <- stringr::str_to_lower(stringr::str_squish(names(player_alias)))
  idx <- match(key, alias_key)
  hit <- !is.na(idx)
  x[hit] <- as.character(player_alias[idx[hit]])
  
  x <- stringr::str_replace(
    x,
    "^([A-Za-zÁÉÍÓÚÜÑáéíóúüñ]+)\\s+([A-Za-zÁÉÍÓÚÜÑáéíóúüñ]).*$",
    "\\1 \\2"
  )
  
  x
}

# ----------------------------
# Fix leaked initials
# ----------------------------
fix_leaked_initials_row <- function(pareja_a, pareja_b) {
  
  norm_sep <- function(x) {
    x <- as.character(x)
    x <- stringr::str_squish(x)
    x <- stringr::str_replace_all(x, "[–—−]", "-")
    x <- stringr::str_replace_all(x, "\\s*-\\s*", " - ")
    x
  }
  
  pareja_a <- norm_sep(pareja_a)
  pareja_b <- norm_sep(pareja_b)
  
  if (is.na(pareja_a) || is.na(pareja_b) || pareja_a == "" || pareja_b == "") {
    return(tibble::tibble(pareja_a = pareja_a, pareja_b = pareja_b))
  }
  
  a_parts <- stringr::str_split_fixed(pareja_a, " - ", 2)
  b_parts <- stringr::str_split_fixed(pareja_b, " - ", 2)
  
  a1 <- stringr::str_squish(a_parts[, 1])
  a2 <- stringr::str_squish(a_parts[, 2])
  b1 <- stringr::str_squish(b_parts[, 1])
  b2 <- stringr::str_squish(b_parts[, 2])
  
  m <- stringr::str_match(b1, "^([A-Za-z])\\s+(.+)$")
  
  if (!is.na(m[1, 1])) {
    ini <- m[1, 2]
    rest_b1 <- m[1, 3]
    
    a2_words <- stringr::str_count(a2, "\\S+")
    a2_has_trailing_initial <- stringr::str_detect(a2, "\\b[A-Za-z]$")
    
    if (a2_words == 1 && !a2_has_trailing_initial) {
      a2 <- paste(a2, ini)
      b1 <- rest_b1
    }
  }
  
  tibble::tibble(
    pareja_a = stringr::str_squish(paste(a1, a2, sep = " - ")),
    pareja_b = stringr::str_squish(paste(b1, b2, sep = " - "))
  )
}

# ----------------------------
# Tabla jugador individual (cuadro)
# ----------------------------
make_player_stats <- function(matches_df) {
  
  pairs_long <- matches_df %>%
    dplyr::mutate(
      pareja_a = stringr::str_squish(pareja_a),
      pareja_b = stringr::str_squish(pareja_b)
    ) %>%
    dplyr::filter(
      !is.na(puntos_a), !is.na(puntos_b),
      pareja_a != "", pareja_b != ""
    ) %>%
    dplyr::transmute(
      season, jornada,
      pareja_a, pareja_b,
      puntos_a = as.numeric(puntos_a),
      puntos_b = as.numeric(puntos_b)
    ) %>%
    tidyr::pivot_longer(
      cols = c(pareja_a, pareja_b),
      names_to = "lado",
      values_to = "pareja"
    ) %>%
    dplyr::mutate(
      games_for     = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
      games_against = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
      win = games_for > games_against
    ) %>%
    dplyr::select(season, jornada, pareja, games_for, games_against, win)
  
  pair_jornada <- pairs_long %>%
    dplyr::group_by(season, jornada, pareja) %>%
    dplyr::summarise(
      partidos_jugados = dplyr::n(),
      partidos_ganados = sum(win),
      partidos_perdidos = partidos_jugados - partidos_ganados,
      games_for = sum(games_for, na.rm = TRUE),
      games_against = sum(games_against, na.rm = TRUE),
      diff = games_for - games_against,
      .groups = "drop"
    )
  
  bonus_from_rank <- function(r) {
    dplyr::case_when(
      r == 1 ~ 3.0,
      r == 2 ~ 2.5,
      r == 3 ~ 2.0,
      r == 4 ~ 1.5,
      r == 5 ~ 1.0,
      r == 6 ~ 0.5,
      TRUE ~ 0.0
    )
  }
  
  pair_jornada <- pair_jornada %>%
    dplyr::group_by(season, jornada) %>%
    dplyr::mutate(
      rank_group = dplyr::dense_rank(dplyr::desc(diff)),
      bonus_average = bonus_from_rank(rank_group),
      puntuacion = partidos_ganados + bonus_average
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-rank_group)
  
  players_long <- pair_jornada %>%
    tidyr::separate(pareja, into = c("j1", "j2"), sep = "-", fill = "right") %>%
    dplyr::mutate(
      j1 = normalize_player_name(stringr::str_squish(j1)),
      j2 = normalize_player_name(stringr::str_squish(j2))
    ) %>%
    tidyr::pivot_longer(
      cols = c(j1, j2),
      names_to = "pos",
      values_to = "jugador"
    ) %>%
    dplyr::filter(!is.na(jugador), jugador != "")
  
  players_long %>%
    dplyr::group_by(season, jugador) %>%
    dplyr::summarise(
      partidos_jugados = sum(partidos_jugados),
      partidos_ganados = sum(partidos_ganados),
      partidos_perdidos = sum(partidos_perdidos),
      win_rate = partidos_ganados / partidos_jugados,
      games_for = sum(games_for),
      games_against = sum(games_against),
      diff = sum(diff),
      bonus_average = sum(bonus_average),
      puntuacion = sum(puntuacion),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(puntuacion), dplyr::desc(win_rate), dplyr::desc(diff))
}

# ----------------------------
# Tabla jugador satélites
# ----------------------------
make_player_stats_satellites <- function(sat_df) {
  
  sat_df <- sat_df %>%
    dplyr::mutate(
      pareja_a = stringr::str_squish(as.character(pareja_a)),
      pareja_b = stringr::str_squish(as.character(pareja_b)),
      pareja_a = stringr::str_replace_all(pareja_a, "[–—−]", "-"),
      pareja_b = stringr::str_replace_all(pareja_b, "[–—−]", "-"),
      pareja_a = stringr::str_replace_all(pareja_a, "\\s*-\\s*", " - "),
      pareja_b = stringr::str_replace_all(pareja_b, "\\s*-\\s*", " - ")
    ) %>%
    dplyr::filter(
      !is.na(puntos_a), !is.na(puntos_b),
      !is.na(pareja_a), !is.na(pareja_b),
      pareja_a != "", pareja_b != ""
    ) %>%
    dplyr::mutate(
      match_id = dplyr::row_number(),
      puntos_a = as.numeric(puntos_a),
      puntos_b = as.numeric(puntos_b)
    )
  
  sat_df <- sat_df %>%
    dplyr::mutate(
      completo = pmax(puntos_a, puntos_b, na.rm = TRUE) >= 6
    )
  
  pairs_long <- sat_df %>%
    dplyr::transmute(
      match_id, season, jornada, completo,
      pareja_a, pareja_b,
      puntos_a, puntos_b
    ) %>%
    tidyr::pivot_longer(
      cols = c(pareja_a, pareja_b),
      names_to = "lado",
      values_to = "pareja"
    ) %>%
    dplyr::mutate(
      games_for     = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
      games_against = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
      diff          = games_for - games_against,
      win           = games_for > games_against
    ) %>%
    dplyr::select(match_id, season, jornada, pareja, completo, games_for, games_against, diff, win)
  
  pair_jornada <- pairs_long %>%
    dplyr::group_by(match_id, season, jornada, pareja, completo) %>%
    dplyr::summarise(
      partidos_jugados = 1L,
      partidos_ganados = as.integer(any(win, na.rm = TRUE)),
      partidos_perdidos = 1L - partidos_ganados,
      games_for = sum(games_for, na.rm = TRUE),
      games_against = sum(games_against, na.rm = TRUE),
      diff = games_for - games_against,
      .groups = "drop"
    )
  
  players_long <- pair_jornada %>%
    dplyr::mutate(
      pareja = stringr::str_squish(as.character(pareja)),
      pareja = stringr::str_replace_all(pareja, "[–—−]", "-"),
      pareja = stringr::str_replace_all(pareja, "\\s*-\\s*", " - ")
    ) %>%
    tidyr::separate(pareja, into = c("j1", "j2"), sep = " - ", fill = "right") %>%
    dplyr::mutate(
      j1 = normalize_player_name(stringr::str_squish(j1)),
      j2 = normalize_player_name(stringr::str_squish(j2))
    ) %>%
    tidyr::pivot_longer(
      cols = c(j1, j2),
      names_to = "pos",
      values_to = "jugador"
    ) %>%
    dplyr::filter(!is.na(jugador), jugador != "")
  
  players_long <- players_long %>%
    dplyr::arrange(season, jornada, jugador, match_id) %>%
    dplyr::group_by(season, jornada, jugador) %>%
    dplyr::mutate(
      n_partido_jugador = dplyr::row_number(),
      is_extra = n_partido_jugador > 3
    ) %>%
    dplyr::ungroup()
  
  players_long <- players_long %>%
    dplyr::mutate(
      pts_diff_gt2 = dplyr::if_else(partidos_ganados == 1L & diff > 2, 0.5, 0.0),
      pts_ganar = dplyr::case_when(
        partidos_ganados == 0L ~ 0.0,
        partidos_ganados == 1L & !is_extra ~ 1.0,
        partidos_ganados == 1L &  is_extra &  completo ~ 1.0,
        partidos_ganados == 1L &  is_extra & !completo ~ 0.5,
        TRUE ~ 0.0
      ),
      pts_partido = pts_ganar + pts_diff_gt2
    )
  
  asistencia <- players_long %>%
    dplyr::distinct(season, jornada, jugador) %>%
    dplyr::group_by(season, jugador) %>%
    dplyr::summarise(asistencia_pts = dplyr::n() * 0.5, .groups = "drop")
  
  players_long %>%
    dplyr::group_by(season, jugador) %>%
    dplyr::summarise(
      partidos_jugados  = sum(partidos_jugados, na.rm = TRUE),
      partidos_ganados  = sum(partidos_ganados, na.rm = TRUE),
      partidos_perdidos = sum(partidos_perdidos, na.rm = TRUE),
      win_rate          = ifelse(partidos_jugados > 0, partidos_ganados / partidos_jugados, NA_real_),
      diff              = sum(diff, na.rm = TRUE),
      puntos_partidos   = sum(pts_partido, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(asistencia, by = c("season", "jugador")) %>%
    dplyr::mutate(
      asistencia_pts = dplyr::coalesce(asistencia_pts, 0),
      puntuacion_satelite = puntos_partidos + asistencia_pts
    ) %>%
    dplyr::arrange(dplyr::desc(puntuacion_satelite), dplyr::desc(win_rate), dplyr::desc(diff))
}

# ----------------------------
# Total jugadores (cuadro + satélite)
# ----------------------------
make_player_stats_total <- function(players_cuadro, players_satelite) {
  
  players_cuadro   <- as.data.frame(players_cuadro)
  players_satelite <- as.data.frame(players_satelite)
  
  get_num <- function(df, col) {
    if (!col %in% names(df)) return(rep(0, nrow(df)))
    x <- df[[col]]
    if (!is.numeric(x)) x <- as.numeric(x)
    dplyr::coalesce(x, 0)
  }
  
  cuadro <- players_cuadro %>%
    dplyr::rename_with(~ paste0("cuadro_", .x), -c(season, jugador))
  
  sat <- players_satelite %>%
    dplyr::rename_with(~ paste0("sat_", .x), -c(season, jugador))
  
  out <- dplyr::full_join(cuadro, sat, by = c("season", "jugador"))
  
  out <- out %>%
    dplyr::mutate(
      total_partidos_jugados  = get_num(out, "cuadro_partidos_jugados")  + get_num(out, "sat_partidos_jugados"),
      total_partidos_ganados  = get_num(out, "cuadro_partidos_ganados")  + get_num(out, "sat_partidos_ganados"),
      total_partidos_perdidos = get_num(out, "cuadro_partidos_perdidos") + get_num(out, "sat_partidos_perdidos"),
      total_games_for     = get_num(out, "cuadro_games_for")     + get_num(out, "sat_games_for"),
      total_games_against = get_num(out, "cuadro_games_against") + get_num(out, "sat_games_against"),
      total_diff          = get_num(out, "cuadro_diff")          + get_num(out, "sat_diff"),
      total_win_rate = dplyr::if_else(
        total_partidos_jugados > 0,
        total_partidos_ganados / total_partidos_jugados,
        NA_real_
      ),
      total_puntuacion = get_num(out, "cuadro_puntuacion") + get_num(out, "sat_puntuacion_satelite")
    ) %>%
    dplyr::arrange(dplyr::desc(total_puntuacion), dplyr::desc(total_win_rate), dplyr::desc(total_diff))
  
  out
}

rename_total_cols_es <- function(df) {
  map <- c(
    "season"  = "temporada",
    "jugador" = "jugador",
    "cuadro_partidos_jugados"  = "cuadro_partidos_jugados",
    "cuadro_partidos_ganados"  = "cuadro_partidos_ganados",
    "cuadro_partidos_perdidos" = "cuadro_partidos_perdidos",
    "cuadro_win_rate"          = "cuadro_porcentaje_victorias",
    "cuadro_games_for"         = "cuadro_juegos_a_favor",
    "cuadro_games_against"     = "cuadro_juegos_en_contra",
    "cuadro_diff"              = "cuadro_diferencia",
    "cuadro_bonus_average"     = "cuadro_bonus",
    "cuadro_puntuacion"        = "cuadro_puntuacion",
    "sat_partidos_jugados"     = "satelite_partidos_jugados",
    "sat_partidos_ganados"     = "satelite_partidos_ganados",
    "sat_partidos_perdidos"    = "satelite_partidos_perdidos",
    "sat_win_rate"             = "satelite_porcentaje_victorias",
    "sat_diff"                 = "satelite_diferencia",
    "sat_puntos_partidos"      = "satelite_puntos_partidos",
    "sat_asistencia_pts"       = "satelite_puntos_asistencia",
    "sat_puntuacion_satelite"  = "satelite_puntuacion",
    "total_partidos_jugados"   = "partidos_jugados_total",
    "total_partidos_ganados"   = "partidos_ganados_total",
    "total_partidos_perdidos"  = "partidos_perdidos_total",
    "total_win_rate"           = "porcentaje_victorias_total",
    "total_games_for"          = "juegos_a_favor_total",
    "total_games_against"      = "juegos_en_contra_total",
    "total_diff"               = "diferencia_total",
    "total_puntuacion"         = "puntuacion_total"
  )
  
  hits <- intersect(names(map), names(df))
  names(df)[match(hits, names(df))] <- unname(map[hits])
  df
}

# ----------------------------
# TOTAL por jornada (cuadro + satélite)
# ----------------------------
make_player_stats_by_jornada_total <- function(matches_df, sat_df) {
  
  # CUADRO por jornada (reutilizando tu lógica)
  cuadro_j <- {
    pairs_long <- matches_df %>%
      dplyr::mutate(
        pareja_a = stringr::str_squish(pareja_a),
        pareja_b = stringr::str_squish(pareja_b)
      ) %>%
      dplyr::filter(!is.na(puntos_a), !is.na(puntos_b),
                    pareja_a != "", pareja_b != "") %>%
      dplyr::transmute(
        season, jornada,
        pareja_a, pareja_b,
        puntos_a = as.numeric(puntos_a),
        puntos_b = as.numeric(puntos_b)
      ) %>%
      tidyr::pivot_longer(
        cols = c(pareja_a, pareja_b),
        names_to = "lado",
        values_to = "pareja"
      ) %>%
      dplyr::mutate(
        games_for     = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
        games_against = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
        win = games_for > games_against
      ) %>%
      dplyr::select(season, jornada, pareja, games_for, games_against, win)
    
    pair_jornada <- pairs_long %>%
      dplyr::group_by(season, jornada, pareja) %>%
      dplyr::summarise(
        partidos_ganados = sum(win),
        games_for = sum(games_for, na.rm = TRUE),
        games_against = sum(games_against, na.rm = TRUE),
        diff = games_for - games_against,
        .groups = "drop"
      )
    
    bonus_from_rank <- function(r) {
      dplyr::case_when(
        r == 1 ~ 3.0,
        r == 2 ~ 2.5,
        r == 3 ~ 2.0,
        r == 4 ~ 1.5,
        r == 5 ~ 1.0,
        r == 6 ~ 0.5,
        TRUE ~ 0.0
      )
    }
    
    pair_jornada <- pair_jornada %>%
      dplyr::group_by(season, jornada) %>%
      dplyr::mutate(
        rank_group = dplyr::dense_rank(dplyr::desc(diff)),
        bonus_average = bonus_from_rank(rank_group),
        puntuacion = partidos_ganados + bonus_average
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-rank_group)
    
    pair_jornada %>%
      tidyr::separate(pareja, into = c("j1", "j2"), sep = "-", fill = "right") %>%
      dplyr::mutate(
        j1 = normalize_player_name(stringr::str_squish(j1)),
        j2 = normalize_player_name(stringr::str_squish(j2))
      ) %>%
      tidyr::pivot_longer(cols = c(j1, j2), names_to = "pos", values_to = "jugador") %>%
      dplyr::filter(!is.na(jugador), jugador != "") %>%
      dplyr::group_by(season, jornada, jugador) %>%
      dplyr::summarise(puntos_cuadro = sum(puntuacion, na.rm = TRUE), .groups = "drop")
  }
  
  # SATELITE por jornada
  sat_clean <- sat_df %>%
    dplyr::mutate(
      pareja_a = stringr::str_squish(as.character(pareja_a)),
      pareja_b = stringr::str_squish(as.character(pareja_b)),
      pareja_a = stringr::str_replace_all(pareja_a, "[–—−]", "-"),
      pareja_b = stringr::str_replace_all(pareja_b, "[–—−]", "-"),
      pareja_a = stringr::str_replace_all(pareja_a, "\\s*-\\s*", " - "),
      pareja_b = stringr::str_replace_all(pareja_b, "\\s*-\\s*", " - ")
    ) %>%
    dplyr::filter(
      !is.na(puntos_a), !is.na(puntos_b),
      !is.na(pareja_a), !is.na(pareja_b),
      pareja_a != "", pareja_b != ""
    ) %>%
    dplyr::mutate(
      match_id = dplyr::row_number(),
      puntos_a = as.numeric(puntos_a),
      puntos_b = as.numeric(puntos_b),
      completo = pmax(puntos_a, puntos_b, na.rm = TRUE) >= 6
    )
  
  pairs_long <- sat_clean %>%
    dplyr::transmute(
      match_id, season, jornada, completo,
      pareja_a, pareja_b, puntos_a, puntos_b
    ) %>%
    tidyr::pivot_longer(cols = c(pareja_a, pareja_b), names_to = "lado", values_to = "pareja") %>%
    dplyr::mutate(
      games_for     = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
      games_against = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
      diff          = games_for - games_against,
      win           = games_for > games_against
    ) %>%
    dplyr::select(match_id, season, jornada, pareja, completo, diff, win)
  
  pair_jornada <- pairs_long %>%
    dplyr::group_by(match_id, season, jornada, pareja, completo) %>%
    dplyr::summarise(
      partidos_ganados  = as.integer(any(win, na.rm = TRUE)),
      diff              = sum(diff, na.rm = TRUE),
      .groups = "drop"
    )
  
  players_long <- pair_jornada %>%
    dplyr::mutate(
      pareja = stringr::str_squish(as.character(pareja)),
      pareja = stringr::str_replace_all(pareja, "[–—−]", "-"),
      pareja = stringr::str_replace_all(pareja, "\\s*-\\s*", " - ")
    ) %>%
    tidyr::separate(pareja, into = c("j1", "j2"), sep = " - ", fill = "right") %>%
    dplyr::mutate(
      j1 = normalize_player_name(stringr::str_squish(j1)),
      j2 = normalize_player_name(stringr::str_squish(j2))
    ) %>%
    tidyr::pivot_longer(cols = c(j1, j2), names_to = "pos", values_to = "jugador") %>%
    dplyr::filter(!is.na(jugador), jugador != "") %>%
    dplyr::arrange(season, jornada, jugador, match_id) %>%
    dplyr::group_by(season, jornada, jugador) %>%
    dplyr::mutate(
      n_partido_jugador = dplyr::row_number(),
      is_extra = n_partido_jugador > 3
    ) %>%
    dplyr::ungroup()
  
  players_long <- players_long %>%
    dplyr::mutate(
      pts_diff_gt2 = dplyr::if_else(partidos_ganados == 1L & diff > 2, 0.5, 0.0),
      pts_ganar = dplyr::case_when(
        partidos_ganados == 0L ~ 0.0,
        partidos_ganados == 1L & !is_extra ~ 1.0,
        partidos_ganados == 1L &  is_extra &  completo ~ 1.0,
        partidos_ganados == 1L &  is_extra & !completo ~ 0.5,
        TRUE ~ 0.0
      ),
      pts_partido = pts_ganar + pts_diff_gt2
    )
  
  asistencia_j <- players_long %>%
    dplyr::distinct(season, jornada, jugador) %>%
    dplyr::mutate(asistencia_pts = 0.5)
  
  sat_j <- players_long %>%
    dplyr::group_by(season, jornada, jugador) %>%
    dplyr::summarise(puntos_partidos = sum(pts_partido, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(asistencia_j, by = c("season", "jornada", "jugador")) %>%
    dplyr::mutate(
      asistencia_pts = dplyr::coalesce(asistencia_pts, 0),
      puntos_sat = puntos_partidos + asistencia_pts
    ) %>%
    dplyr::select(season, jornada, jugador, puntos_sat)
  
  dplyr::full_join(cuadro_j, sat_j, by = c("season", "jornada", "jugador")) %>%
    dplyr::mutate(
      puntos_cuadro = dplyr::coalesce(puntos_cuadro, 0),
      puntos_sat    = dplyr::coalesce(puntos_sat, 0),
      puntuacion_total = puntos_cuadro + puntos_sat
    ) %>%
    dplyr::select(season, jornada, jugador, puntuacion_total) %>%
    dplyr::arrange(season, jornada, dplyr::desc(puntuacion_total))
}

# ----------------------------
# Datos para Bar Chart Race (acumulado)
# Devuelve puntos_acum + rank por jornada + color automático por jugador
# ----------------------------
make_rank_race_data <- function(df_by_jornada_total, top_n = 10) {
  
  df <- df_by_jornada_total %>%
    dplyr::mutate(
      jornada = as.integer(jornada),
      puntos_jornada = suppressWarnings(as.numeric(puntuacion_total)),
      jugador = as.character(jugador)
    ) %>%
    dplyr::filter(!is.na(jornada), !is.na(jugador), jugador != "")
  
  if (nrow(df) == 0) return(tibble::tibble())
  
  jornadas_all <- sort(unique(df$jornada))
  
  df_full <- df %>%
    dplyr::select(season, jornada, jugador, puntos_jornada) %>%
    tidyr::complete(
      season,
      jornada = jornadas_all,
      jugador,
      fill = list(puntos_jornada = 0)
    ) %>%
    dplyr::arrange(season, jugador, jornada) %>%
    dplyr::group_by(season, jugador) %>%
    dplyr::mutate(puntos_acum = cumsum(dplyr::coalesce(puntos_jornada, 0))) %>%
    dplyr::ungroup()
  
  top_n <- as.integer(top_n)
  
  ranked <- df_full %>%
    dplyr::group_by(season, jornada) %>%
    dplyr::arrange(dplyr::desc(puntos_acum), jugador, .by_group = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(rank <= top_n)
  
  # Colores automáticos (consistentes por jugador)
  players <- sort(unique(ranked$jugador))
  pal <- grDevices::hcl.colors(length(players), palette = "Dark 3")
  color_map <- tibble::tibble(jugador = players, color = pal)
  
  ranked <- ranked %>%
    dplyr::left_join(color_map, by = "jugador") %>%
    dplyr::mutate(
      y_rank = rank,
      puntos_txt = format(round(puntos_acum, 1), decimal.mark = ",", nsmall = 1),
      label_out = paste0(jugador, "   ", puntos_txt, " pts")
    )
  
  ranked
}

# ----------------------------
# Mayor remontada/caída TOTAL (respetando el tiempo)
# ----------------------------
compute_biggest_swings_total <- function(df_by_jornada_total) {
  
  df <- df_by_jornada_total %>%
    dplyr::mutate(
      jornada = as.integer(jornada),
      puntos_jornada = suppressWarnings(as.numeric(puntuacion_total)),
      jugador = as.character(jugador)
    ) %>%
    dplyr::filter(!is.na(jornada), !is.na(jugador), jugador != "")
  
  if (nrow(df) == 0) return(list(comeback_total = NULL, drop_total = NULL))
  
  jornadas_all <- sort(unique(df$jornada))
  
  full <- df %>%
    dplyr::select(season, jornada, jugador, puntos_jornada) %>%
    tidyr::complete(
      season,
      jornada = jornadas_all,
      jugador,
      fill = list(puntos_jornada = 0)
    ) %>%
    dplyr::arrange(season, jugador, jornada) %>%
    dplyr::group_by(season, jugador) %>%
    dplyr::mutate(puntos_acum = cumsum(dplyr::coalesce(puntos_jornada, 0))) %>%
    dplyr::ungroup()
  
  ranks <- full %>%
    dplyr::group_by(season, jornada) %>%
    dplyr::arrange(dplyr::desc(puntos_acum), jugador, .by_group = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  swings <- ranks %>%
    dplyr::arrange(season, jugador, jornada) %>%
    dplyr::group_by(season, jugador) %>%
    dplyr::group_modify(~{
      x <- .x
      
      worst_rank_so_far <- -Inf
      worst_j_so_far <- NA_integer_
      best_impr <- -Inf
      impr_from_rank <- NA_integer_
      impr_from_j <- NA_integer_
      impr_to_rank <- NA_integer_
      impr_to_j <- NA_integer_
      
      best_rank_so_far <- Inf
      best_j_so_far <- NA_integer_
      best_drop <- -Inf
      drop_from_rank <- NA_integer_
      drop_from_j <- NA_integer_
      drop_to_rank <- NA_integer_
      drop_to_j <- NA_integer_
      
      for (i in seq_len(nrow(x))) {
        r <- x$rank[i]
        j <- x$jornada[i]
        
        if (r > worst_rank_so_far) {
          worst_rank_so_far <- r
          worst_j_so_far <- j
        }
        impr <- worst_rank_so_far - r
        if (impr > best_impr) {
          best_impr <- impr
          impr_from_rank <- worst_rank_so_far
          impr_from_j <- worst_j_so_far
          impr_to_rank <- r
          impr_to_j <- j
        }
        
        if (r < best_rank_so_far) {
          best_rank_so_far <- r
          best_j_so_far <- j
        }
        drp <- r - best_rank_so_far
        if (drp > best_drop) {
          best_drop <- drp
          drop_from_rank <- best_rank_so_far
          drop_from_j <- best_j_so_far
          drop_to_rank <- r
          drop_to_j <- j
        }
      }
      
      tibble::tibble(
        comeback_delta = as.integer(best_impr),
        comeback_from_rank = impr_from_rank,
        comeback_from_jornada = impr_from_j,
        comeback_to_rank = impr_to_rank,
        comeback_to_jornada = impr_to_j,
        
        drop_delta = as.integer(best_drop),
        drop_from_rank = drop_from_rank,
        drop_from_jornada = drop_from_j,
        drop_to_rank = drop_to_rank,
        drop_to_jornada = drop_to_j
      )
    }) %>%
    dplyr::ungroup()
  
  best_comeback <- swings %>%
    dplyr::arrange(dplyr::desc(comeback_delta), season, jugador) %>%
    dplyr::slice_head(n = 1)
  
  worst_drop <- swings %>%
    dplyr::arrange(dplyr::desc(drop_delta), season, jugador) %>%
    dplyr::slice_head(n = 1)
  
  list(comeback_total = best_comeback, drop_total = worst_drop)
}

##########parejas
# ==========================================================
# PAREJAS (CUADRO): stats por pareja
# ==========================================================
make_pair_stats_cuadro <- function(matches_df) {
  
  df <- matches_df %>%
    dplyr::mutate(
      pareja_a = normalize_pair(pareja_a),   # en cuadro venía "A-B" pero aquí lo normalizamos
      pareja_b = normalize_pair(pareja_b),
      puntos_a = as.numeric(puntos_a),
      puntos_b = as.numeric(puntos_b)
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b),
                  !is.na(pareja_a), !is.na(pareja_b),
                  pareja_a != "", pareja_b != "")
  
  pairs_long <- df %>%
    dplyr::transmute(
      season, jornada,
      pareja_a, pareja_b, puntos_a, puntos_b
    ) %>%
    tidyr::pivot_longer(cols = c(pareja_a, pareja_b),
                        names_to = "lado", values_to = "pareja") %>%
    dplyr::mutate(
      games_for     = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
      games_against = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
      win = games_for > games_against
    ) %>%
    dplyr::select(season, pareja, games_for, games_against, win)
  
  pairs_long %>%
    dplyr::group_by(season, pareja) %>%
    dplyr::summarise(
      partidos_jugados  = dplyr::n(),
      partidos_ganados  = sum(win, na.rm = TRUE),
      partidos_perdidos = partidos_jugados - partidos_ganados,
      win_rate          = ifelse(partidos_jugados > 0, partidos_ganados / partidos_jugados, NA_real_),
      games_for         = sum(games_for, na.rm = TRUE),
      games_against     = sum(games_against, na.rm = TRUE),
      diff              = games_for - games_against,
      puntos            = partidos_ganados, # puedes cambiar si quieres reglas de puntos para parejas
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(puntos), dplyr::desc(win_rate), dplyr::desc(diff))
}

# ==========================================================
# PAREJAS (SAT): stats por pareja
# ==========================================================
make_pair_stats_satellites <- function(sat_df) {
  
  df <- sat_df %>%
    dplyr::mutate(
      pareja_a = normalize_pair(pareja_a),  # en sat ya venía "A - B"
      pareja_b = normalize_pair(pareja_b),
      puntos_a = as.numeric(puntos_a),
      puntos_b = as.numeric(puntos_b)
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b),
                  !is.na(pareja_a), !is.na(pareja_b),
                  pareja_a != "", pareja_b != "")
  
  pairs_long <- df %>%
    dplyr::transmute(season, pareja_a, pareja_b, puntos_a, puntos_b) %>%
    tidyr::pivot_longer(cols = c(pareja_a, pareja_b),
                        names_to = "lado", values_to = "pareja") %>%
    dplyr::mutate(
      games_for     = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
      games_against = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
      win = games_for > games_against
    ) %>%
    dplyr::select(season, pareja, games_for, games_against, win)
  
  pairs_long %>%
    dplyr::group_by(season, pareja) %>%
    dplyr::summarise(
      partidos_jugados  = dplyr::n(),
      partidos_ganados  = sum(win, na.rm = TRUE),
      partidos_perdidos = partidos_jugados - partidos_ganados,
      win_rate          = ifelse(partidos_jugados > 0, partidos_ganados / partidos_jugados, NA_real_),
      games_for         = sum(games_for, na.rm = TRUE),
      games_against     = sum(games_against, na.rm = TRUE),
      diff              = games_for - games_against,
      puntos            = partidos_ganados, # idem
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(puntos), dplyr::desc(win_rate), dplyr::desc(diff))
}

#########################################################
# ==========================================================
# SIMULADOR PARTIDOS: helpers + features + predicción
# ==========================================================

# --- Normaliza separadores de pareja y ordena jugadores (A - B == B - A)
normalize_pair <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, "[–—−]", "-")
  x <- stringr::str_replace_all(x, "\\s*-\\s*", " - ")
  
  parts <- stringr::str_split_fixed(x, " - ", 2)
  j1 <- normalize_player_name(stringr::str_squish(parts[, 1]))
  j2 <- normalize_player_name(stringr::str_squish(parts[, 2]))
  
  k1 <- stringi::stri_trans_general(j1, "Latin-ASCII") |> toupper() |> stringr::str_squish()
  k2 <- stringi::stri_trans_general(j2, "Latin-ASCII") |> toupper() |> stringr::str_squish()
  swap <- k2 < k1
  
  j1_out <- ifelse(swap, j2, j1)
  j2_out <- ifelse(swap, j1, j2)
  
  stringr::str_squish(paste(j1_out, j2_out, sep = " - "))
}

# --- Key normalizada para joins robustos
key_norm <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  stringr::str_squish(x)
}

# --- Min-max seguro
minmax01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

# ----------------------------------------------------------
# Features individuales globales (a partir de players_total_f())
# Devuelve: jugador, win_rate, diff_per_match, points_per_match, exp_scaled
# ----------------------------------------------------------
compute_player_features_global <- function(players_total_df) {
  df <- players_total_df %>%
    dplyr::mutate(
      jugador = as.character(jugador),
      jugador_key = key_norm(jugador),
      total_win_rate = suppressWarnings(as.numeric(total_win_rate)),
      total_diff = suppressWarnings(as.numeric(total_diff)),
      total_puntuacion = suppressWarnings(as.numeric(total_puntuacion)),
      total_partidos_jugados = suppressWarnings(as.numeric(total_partidos_jugados))
    ) %>%
    dplyr::filter(!is.na(jugador_key), jugador_key != "")
  
  df <- df %>%
    dplyr::mutate(
      diff_per_match = dplyr::if_else(total_partidos_jugados > 0, total_diff / total_partidos_jugados, 0),
      points_per_match = dplyr::if_else(total_partidos_jugados > 0, total_puntuacion / total_partidos_jugados, 0),
      exp_scaled = minmax01(total_partidos_jugados)
    )
  
  df %>%
    dplyr::select(jugador, jugador_key, total_win_rate, diff_per_match, points_per_match, exp_scaled)
}

# ----------------------------------------------------------
# Features individuales recientes (últimas k jornadas)
# A partir de players_by_jornada_f(): season, jornada, jugador, puntuacion_total
# Devuelve: jugador_key, recent_points_mean, recent_points_trend
# ----------------------------------------------------------
compute_player_features_recent <- function(players_by_jornada_total_df, k_last = 3) {
  df <- players_by_jornada_total_df %>%
    dplyr::mutate(
      jugador = as.character(jugador),
      jugador_key = key_norm(jugador),
      jornada = as.integer(jornada),
      pts = suppressWarnings(as.numeric(puntuacion_total))
    ) %>%
    dplyr::filter(!is.na(jornada), !is.na(jugador_key), jugador_key != "")
  
  if (nrow(df) == 0) {
    return(tibble::tibble(
      jugador_key = character(),
      recent_points_mean = numeric(),
      recent_points_trend = numeric()
    ))
  }
  
  jmax <- max(df$jornada, na.rm = TRUE)
  jmin <- max(1, jmax - as.integer(k_last) + 1)
  
  w <- df %>% dplyr::filter(jornada >= jmin, jornada <= jmax)
  
  # media + tendencia simple (última - primera)
  out <- w %>%
    dplyr::group_by(jugador_key) %>%
    dplyr::summarise(
      recent_points_mean = mean(pts, na.rm = TRUE),
      recent_points_trend = dplyr::last(pts) - dplyr::first(pts),
      .groups = "drop"
    )
  
  out
}

# ----------------------------------------------------------
# Sinergia de pareja (win_rate y diff medio cuando juegan juntos)
# Usa matches_f() y satellites_f() ya filtrados por season/trimestre/jornada si quieres
# Devuelve: pareja_norm, pair_matches, pair_win_rate, pair_diff_mean
# ----------------------------------------------------------
# ----------------------------------------------------------
# Features recientes por ÚLTIMOS N PARTIDOS (cuadro + satélite)
# Devuelve: jugador_key, recent_win_rate, recent_diff_mean, recent_matches
# ----------------------------------------------------------
compute_player_features_recent_matches <- function(matches_df, sat_df, n_last = 6) {
  
  # --- CUADRO -> partidos jugador ---
  cu <- matches_df %>%
    dplyr::mutate(
      pareja_a = as.character(pareja_a),
      pareja_b = as.character(pareja_b),
      puntos_a = suppressWarnings(as.numeric(puntos_a)),
      puntos_b = suppressWarnings(as.numeric(puntos_b))
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pareja_a != "", pareja_b != "") %>%
    dplyr::mutate(
      match_id = dplyr::row_number(),
      # en cuadro vienen sin espacios: "A-B"
      pareja_a = stringr::str_squish(pareja_a),
      pareja_b = stringr::str_squish(pareja_b)
    ) %>%
    tidyr::pivot_longer(cols = c(pareja_a, pareja_b), names_to = "lado", values_to = "pareja") %>%
    dplyr::mutate(
      gf = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
      ga = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
      win = gf > ga,
      diff = gf - ga
    ) %>%
    dplyr::select(match_id, season, jornada, pareja, win, diff)
  
  cu_players <- cu %>%
    tidyr::separate(pareja, into = c("j1","j2"), sep = "-", fill = "right") %>%
    dplyr::mutate(
      j1 = normalize_player_name(stringr::str_squish(j1)),
      j2 = normalize_player_name(stringr::str_squish(j2))
    ) %>%
    tidyr::pivot_longer(cols = c(j1,j2), names_to = "pos", values_to = "jugador") %>%
    dplyr::filter(!is.na(jugador), jugador != "") %>%
    dplyr::mutate(jugador_key = key_norm(jugador)) %>%
    dplyr::select(season, jornada, match_id, jugador_key, win, diff)
  
  # --- SAT -> partidos jugador ---
  sa <- sat_df %>%
    dplyr::mutate(
      pareja_a = as.character(pareja_a),
      pareja_b = as.character(pareja_b),
      puntos_a = suppressWarnings(as.numeric(puntos_a)),
      puntos_b = suppressWarnings(as.numeric(puntos_b))
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pareja_a != "", pareja_b != "") %>%
    dplyr::mutate(
      match_id = dplyr::row_number(),
      # en sat vienen con espacios: "A - B"
      pareja_a = stringr::str_replace_all(stringr::str_squish(pareja_a), "\\s*-\\s*", " - "),
      pareja_b = stringr::str_replace_all(stringr::str_squish(pareja_b), "\\s*-\\s*", " - ")
    ) %>%
    tidyr::pivot_longer(cols = c(pareja_a, pareja_b), names_to = "lado", values_to = "pareja") %>%
    dplyr::mutate(
      gf = dplyr::if_else(lado == "pareja_a", puntos_a, puntos_b),
      ga = dplyr::if_else(lado == "pareja_a", puntos_b, puntos_a),
      win = gf > ga,
      diff = gf - ga
    ) %>%
    dplyr::select(match_id, season, jornada, pareja, win, diff)
  
  sa_players <- sa %>%
    tidyr::separate(pareja, into = c("j1","j2"), sep = " - ", fill = "right") %>%
    dplyr::mutate(
      j1 = normalize_player_name(stringr::str_squish(j1)),
      j2 = normalize_player_name(stringr::str_squish(j2))
    ) %>%
    tidyr::pivot_longer(cols = c(j1,j2), names_to = "pos", values_to = "jugador") %>%
    dplyr::filter(!is.na(jugador), jugador != "") %>%
    dplyr::mutate(jugador_key = key_norm(jugador)) %>%
    dplyr::select(season, jornada, match_id, jugador_key, win, diff)
  
  allm <- dplyr::bind_rows(cu_players, sa_players)
  
  if (nrow(allm) == 0) {
    return(tibble::tibble(
      jugador_key = character(),
      recent_win_rate = numeric(),
      recent_diff_mean = numeric(),
      recent_matches = integer()
    ))
  }
  
  # orden temporal aproximado: season + jornada + match_id
  allm <- allm %>% dplyr::arrange(season, jornada, match_id)
  
  # últimos N partidos por jugador
  n_last <- as.integer(n_last)
  
  out <- allm %>%
    dplyr::group_by(jugador_key) %>%
    dplyr::group_modify(~{
      x <- .x
      x <- utils::tail(x, n_last)
      tibble::tibble(
        recent_matches  = nrow(x),
        recent_win_rate = mean(x$win,  na.rm = TRUE),
        recent_diff_mean= mean(x$diff, na.rm = TRUE)
      )
    }) %>%
    dplyr::ungroup()
  
  out
}

# ----------------------------------------------------------
# Scoring final: rating de jugador + bonus sinergia pareja -> prob victoria
predict_match_winner <- function(
    players_total_df,
    matches_df,
    sat_df,
    A1, A2, B1, B2,
    n_last = 6
) {
  # --- features globales ---
  g <- compute_player_features_global(players_total_df)
  
  # --- features recientes por últimos N PARTIDOS ---
  n_last <- as.integer(n_last)
  r <- compute_player_features_recent_matches(matches_df, sat_df, n_last = n_last)
  
  # join global + reciente (si no hay reciente -> 0/NA controlado)
  feats <- g %>%
    dplyr::left_join(r, by = "jugador_key") %>%
    dplyr::mutate(
      recent_matches  = dplyr::coalesce(recent_matches, 0L),
      recent_win_rate = dplyr::coalesce(recent_win_rate, 0.5),
      recent_diff_mean= dplyr::coalesce(recent_diff_mean, 0)
    )
  
  # normalizaciones
  feats <- feats %>%
    dplyr::mutate(
      win01   = dplyr::coalesce(total_win_rate, 0.5),
      diff01  = minmax01(diff_per_match),
      ppm01   = minmax01(points_per_match),
      rwin01  = dplyr::coalesce(recent_win_rate, 0.5),
      rdiff01 = minmax01(recent_diff_mean),
      exp01   = dplyr::coalesce(exp_scaled, 0.5)
    )
  
  # rating individual (mezcla histórico + forma reciente)
  feats <- feats %>%
    dplyr::mutate(
      rating = 0.35 * win01 +
        0.20 * diff01 +
        0.20 * ppm01 +
        0.15 * rwin01 +
        0.05 * rdiff01 +
        0.05 * exp01
    )
  
  # helper rating por jugador
  get_rating <- function(name) {
    k <- key_norm(name)
    row <- feats %>% dplyr::filter(jugador_key == k) %>% dplyr::slice_head(n = 1)
    if (nrow(row) == 0) return(list(r = 0.5, row = NULL))
    list(r = row$rating[1], row = row)
  }
  
  a1 <- get_rating(A1); a2 <- get_rating(A2); b1 <- get_rating(B1); b2 <- get_rating(B2)
  
  baseA <- a1$r + a2$r
  baseB <- b1$r + b2$r
  
  # --- sinergia pareja ---
  syn <- compute_pair_synergy(matches_df, sat_df)
  pairA <- normalize_pair(paste(A1, A2, sep = " - "))
  pairB <- normalize_pair(paste(B1, B2, sep = " - "))
  
  # ==========================================================
  # H2H bonus (Pareja A vs Pareja B)
  # ==========================================================
  h2h <- compute_h2h_pair_vs_pair(matches_df, sat_df, pairA, pairB)
  
  # Bonus por H2H:
  # - se centra en 0.5 (neutral)
  # - aumenta con nº enfrentamientos (confianza)
  # - usa también diff medio para separar "ganar sufriendo" vs "ganar fácil"
  h2h_bonus <- 0
  if (!is.na(h2h$h2h_win_rate_A) && h2h$h2h_matches > 0) {
    
    conf_h2h <- pmin(1, sqrt(h2h$h2h_matches) / 4)  # 4 enfrentamientos -> conf ~1
    wr_term  <- (h2h$h2h_win_rate_A - 0.5)          # [-0.5, +0.5]
    
    # diff mean lo pasamos a escala suave con tanh (evita valores locos)
    diff_term <- tanh(h2h$h2h_diff_mean_A / 3)      # aprox [-1,1]
    
    # peso global del H2H 
    h2h_bonus <- conf_h2h * (0.2 * wr_term + 0.10 * diff_term)
  }
  
  synA <- syn %>% dplyr::filter(pareja_norm == pairA) %>% dplyr::slice_head(n = 1)
  synB <- syn %>% dplyr::filter(pareja_norm == pairB) %>% dplyr::slice_head(n = 1)
  
  bonus_syn <- function(syn_row) {
    if (is.null(syn_row) || nrow(syn_row) == 0) return(0)
    m  <- syn_row$pair_matches[1]
    wr <- syn_row$pair_win_rate[1]
    conf <- pmin(1, sqrt(m) / 5)
    conf * (wr - 0.5) * 0.40
  }
  
  bonusA <- bonus_syn(synA)
  bonusB <- bonus_syn(synB)
  
  ratingA <- baseA + bonusA + h2h_bonus
  ratingB <- baseB + bonusB - h2h_bonus
  
  # probabilidad logística
  temp <- 3.0
  pA <- 1 / (1 + exp(-(ratingA - ratingB) * temp))
  pB <- 1 - pA
  
  # detalle jugadores (con campos recientes correctos)
  detail_players <- dplyr::bind_rows(
    if (!is.null(a1$row)) a1$row %>% dplyr::mutate(role = "A1") else NULL,
    if (!is.null(a2$row)) a2$row %>% dplyr::mutate(role = "A2") else NULL,
    if (!is.null(b1$row)) b1$row %>% dplyr::mutate(role = "B1") else NULL,
    if (!is.null(b2$row)) b2$row %>% dplyr::mutate(role = "B2") else NULL
  ) %>%
    dplyr::select(
      role, jugador,
      total_win_rate, diff_per_match, points_per_match,
      recent_matches, recent_win_rate, recent_diff_mean,
      exp_scaled, rating
    )
  
  detail_pairs <- tibble::tibble(
    pareja = c(pairA, pairB),
    side = c("A", "B"),
    base_rating = c(baseA, baseB),
    synergy_bonus = c(bonusA, bonusB),
    final_rating = c(ratingA, ratingB),
    synergy_matches = c(ifelse(nrow(synA)==0, 0, synA$pair_matches[1]),
                        ifelse(nrow(synB)==0, 0, synB$pair_matches[1])),
    synergy_win_rate = c(ifelse(nrow(synA)==0, NA_real_, synA$pair_win_rate[1]),
                         ifelse(nrow(synB)==0, NA_real_, synB$pair_win_rate[1]))
  )
  
  list(
    pairA = pairA, pairB = pairB,
    ratingA = ratingA, ratingB = ratingB,
    pA = pA, pB = pB,
    detail_players = detail_players,
    detail_pairs = detail_pairs
  )
}

# ----------------------------------------------------------
# H2H: Pareja A vs Pareja B (cuadro + satélite)
# Devuelve stats desde perspectiva de A
# ----------------------------------------------------------
compute_h2h_pair_vs_pair <- function(matches_df, sat_df, pairA, pairB) {
  
  pairA <- normalize_pair(pairA)
  pairB <- normalize_pair(pairB)
  
  # ---- CUADRO ----
  cu <- matches_df %>%
    dplyr::mutate(
      pa = normalize_pair(pareja_a),
      pb = normalize_pair(pareja_b),
      puntos_a = suppressWarnings(as.numeric(puntos_a)),
      puntos_b = suppressWarnings(as.numeric(puntos_b))
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pa != "", pb != "") %>%
    dplyr::transmute(pa, pb, puntos_a, puntos_b)
  
  cu_h2h <- cu %>%
    dplyr::filter((pa == pairA & pb == pairB) | (pa == pairB & pb == pairA)) %>%
    dplyr::mutate(
      gf_A = dplyr::if_else(pa == pairA, puntos_a, puntos_b),
      ga_A = dplyr::if_else(pa == pairA, puntos_b, puntos_a),
      win_A = gf_A > ga_A,
      diff_A = gf_A - ga_A
    ) %>%
    dplyr::select(win_A, diff_A)
  
  # ---- SAT ----
  sa <- sat_df %>%
    dplyr::mutate(
      pa = normalize_pair(pareja_a),
      pb = normalize_pair(pareja_b),
      puntos_a = suppressWarnings(as.numeric(puntos_a)),
      puntos_b = suppressWarnings(as.numeric(puntos_b))
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pa != "", pb != "") %>%
    dplyr::transmute(pa, pb, puntos_a, puntos_b)
  
  sa_h2h <- sa %>%
    dplyr::filter((pa == pairA & pb == pairB) | (pa == pairB & pb == pairA)) %>%
    dplyr::mutate(
      gf_A = dplyr::if_else(pa == pairA, puntos_a, puntos_b),
      ga_A = dplyr::if_else(pa == pairA, puntos_b, puntos_a),
      win_A = gf_A > ga_A,
      diff_A = gf_A - ga_A
    ) %>%
    dplyr::select(win_A, diff_A)
  
  h2h <- dplyr::bind_rows(cu_h2h, sa_h2h)
  
  if (nrow(h2h) == 0) {
    return(list(
      h2h_matches = 0L,
      h2h_win_rate_A = NA_real_,
      h2h_diff_mean_A = NA_real_
    ))
  }
  
  list(
    h2h_matches = nrow(h2h),
    h2h_win_rate_A = mean(h2h$win_A, na.rm = TRUE),
    h2h_diff_mean_A = mean(h2h$diff_A, na.rm = TRUE)
  )
}


# ----------------------------------------------------------
# Sinergia de pareja (cuadro + sat)
# Devuelve: pareja_norm, pair_matches, pair_win_rate, pair_diff_mean
# ----------------------------------------------------------
compute_pair_synergy <- function(matches_df, sat_df) {
  
  # CUADRO
  cu <- matches_df %>%
    dplyr::mutate(
      pa = normalize_pair(pareja_a),
      pb = normalize_pair(pareja_b),
      puntos_a = suppressWarnings(as.numeric(puntos_a)),
      puntos_b = suppressWarnings(as.numeric(puntos_b))
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pa != "", pb != "") %>%
    dplyr::transmute(
      pareja = pa, gf = puntos_a, ga = puntos_b
    ) %>%
    dplyr::bind_rows(
      matches_df %>%
        dplyr::mutate(
          pa = normalize_pair(pareja_a),
          pb = normalize_pair(pareja_b),
          puntos_a = suppressWarnings(as.numeric(puntos_a)),
          puntos_b = suppressWarnings(as.numeric(puntos_b))
        ) %>%
        dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pa != "", pb != "") %>%
        dplyr::transmute(
          pareja = pb, gf = puntos_b, ga = puntos_a
        )
    )
  
  # SAT
  sa <- sat_df %>%
    dplyr::mutate(
      pa = normalize_pair(pareja_a),
      pb = normalize_pair(pareja_b),
      puntos_a = suppressWarnings(as.numeric(puntos_a)),
      puntos_b = suppressWarnings(as.numeric(puntos_b))
    ) %>%
    dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pa != "", pb != "") %>%
    dplyr::transmute(
      pareja = pa, gf = puntos_a, ga = puntos_b
    ) %>%
    dplyr::bind_rows(
      sat_df %>%
        dplyr::mutate(
          pa = normalize_pair(pareja_a),
          pb = normalize_pair(pareja_b),
          puntos_a = suppressWarnings(as.numeric(puntos_a)),
          puntos_b = suppressWarnings(as.numeric(puntos_b))
        ) %>%
        dplyr::filter(!is.na(puntos_a), !is.na(puntos_b), pa != "", pb != "") %>%
        dplyr::transmute(
          pareja = pb, gf = puntos_b, ga = puntos_a
        )
    )
  
  allp <- dplyr::bind_rows(cu, sa)
  
  if (nrow(allp) == 0) {
    return(tibble::tibble(
      pareja_norm = character(),
      pair_matches = integer(),
      pair_win_rate = numeric(),
      pair_diff_mean = numeric()
    ))
  }
  
  allp %>%
    dplyr::mutate(
      win = gf > ga,
      diff = gf - ga,
      pareja_norm = pareja
    ) %>%
    dplyr::group_by(pareja_norm) %>%
    dplyr::summarise(
      pair_matches = dplyr::n(),
      pair_win_rate = mean(win, na.rm = TRUE),
      pair_diff_mean = mean(diff, na.rm = TRUE),
      .groups = "drop"
    )
}