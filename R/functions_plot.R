# R/functions_plot.R

library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(tibble)
library(plotly)

# -----------------------------
# Top N jugadores (Ranking)
# -----------------------------
plot_top_players_total <- function(df, n = 10) {
  df <- df %>%
    mutate(
      jugador = as.character(jugador),
      total_puntuacion = as.numeric(total_puntuacion)
    ) %>%
    filter(!is.na(total_puntuacion), jugador != "")
  
  top <- df %>%
    arrange(desc(total_puntuacion)) %>%
    slice_head(n = n) %>%
    mutate(
      jugador = factor(jugador, levels = rev(jugador)),
      label_pts = format(round(total_puntuacion, 1),
                         big.mark = ".", decimal.mark = ",", nsmall = 1
      )
    )
  
  ggplot(top, aes(x = jugador, y = total_puntuacion)) +
    geom_col(fill = "#2E7D32", width = 0.7) +
    geom_text(aes(label = label_pts), hjust = -0.1, size = 4, fontface = "bold") +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11),
      plot.margin = grid::unit(c(10, 50, 10, 10), "pt")
    )
}

# -----------------------------
# Evolución puntos por jornada (NO acumulado)
# -----------------------------
plot_player_evolution <- function(df_by_jornada, player_name) {
  
  key_norm <- function(x) {
    x <- as.character(x)
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- toupper(x)
    stringr::str_squish(x)
  }
  
  df <- df_by_jornada %>%
    mutate(
      jugador = as.character(jugador),
      jugador_key = key_norm(jugador),
      jornada = as.integer(jornada),
      puntos_jornada = as.numeric(puntuacion_total)
    ) %>%
    filter(!is.na(jornada), !is.na(jugador), jugador != "")
  
  if (nrow(df) == 0 || is.null(player_name) || player_name == "") {
    return(ggplot() + theme_void() + labs(title = "No hay datos para mostrar"))
  }
  
  pdat <- df %>%
    filter(jugador_key == key_norm(player_name)) %>%
    arrange(jornada)
  
  if (nrow(pdat) == 0) {
    return(ggplot() + theme_void() + labs(title = "Sin datos con este filtro"))
  }
  
  player_label <- pdat$jugador[1]
  
  pdat <- pdat %>%
    mutate(label_pts = format(round(puntos_jornada, 1),
                              big.mark = ".", decimal.mark = ",", nsmall = 1
    ))
  
  ggplot(pdat, aes(x = jornada, y = puntos_jornada)) +
    geom_line(linewidth = 1.2, color = "#2E7D32") +
    geom_point(size = 2.6, color = "#2E7D32") +
    geom_text(aes(label = label_pts), vjust = -1.0, size = 4, fontface = "bold") +
    scale_x_continuous(breaks = sort(unique(pdat$jornada))) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
    labs(
      title = player_label,
      x = "Jornada",
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

# -----------------------------
# Donut: Cuadro vs Satélite
# -----------------------------
plot_player_split_donut <- function(players_total_df, player_name) {
  
  key_norm <- function(x) {
    x <- as.character(x)
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- toupper(x)
    stringr::str_squish(x)
  }
  
  get_num_col <- function(df, candidates) {
    nm <- intersect(candidates, names(df))
    if (length(nm) == 0) return(rep(0, nrow(df)))
    x <- df[[nm[1]]]
    x <- suppressWarnings(as.numeric(x))
    dplyr::coalesce(x, 0)
  }
  
  fmt_num <- function(x) {
    format(round(x, 1), big.mark = ".", decimal.mark = ",", nsmall = 1)
  }
  
  df <- players_total_df %>%
    mutate(
      jugador = as.character(jugador),
      jugador_key = key_norm(jugador)
    )
  
  row <- df %>%
    filter(jugador_key == key_norm(player_name)) %>%
    slice_head(n = 1)
  
  if (nrow(row) == 0) {
    return(ggplot() + theme_void() + labs(title = "Origen de puntos"))
  }
  
  a <- get_num_col(row, c("cuadro_puntuacion"))[1]
  b <- get_num_col(row, c("sat_puntuacion_satelite"))[1]
  
  tot <- get_num_col(row, c("total_puntuacion"))[1]
  if ((a + b) == 0 && tot > 0) b <- tot
  
  if ((a + b) == 0) {
    return(ggplot() + theme_void() + labs(title = "Origen de puntos"))
  }
  
  plot_df <- tibble(
    tipo = c("Cuadro", "Satélite"),
    puntos = c(a, b)
  ) %>%
    mutate(
      pct = puntos / sum(puntos),
      label = ifelse(puntos <= 0, "", paste0(fmt_num(puntos), " (", round(pct * 100, 0), "%)"))
    )
  
  ggplot(plot_df, aes(x = 2, y = puntos, fill = tipo)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 4,
              fontface = "bold"
    ) +
    theme_void(base_size = 12) +
    labs(title = "Origen de puntos", fill = NULL) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(face = "bold")
    )
}

# -----------------------------
# Scatter liga: win_rate vs diff (resalta jugador)
# -----------------------------
plot_league_scatter_highlight <- function(players_total_df, player_name) {
  
  key_norm <- function(x) {
    x <- as.character(x)
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- toupper(x)
    stringr::str_squish(x)
  }
  
  df <- players_total_df %>%
    mutate(
      jugador = as.character(jugador),
      jugador_key = key_norm(jugador),
      total_win_rate = suppressWarnings(as.numeric(total_win_rate)),
      total_diff = suppressWarnings(as.numeric(total_diff)),
      total_partidos_jugados = suppressWarnings(as.numeric(total_partidos_jugados))
    ) %>%
    filter(!is.na(total_win_rate), !is.na(total_diff), jugador != "")
  
  if (nrow(df) == 0) return(ggplot() + theme_void() + labs(title = "Sin datos"))
  
  df <- df %>% mutate(is_sel = jugador_key == key_norm(player_name))
  
  ggplot(df, aes(x = total_win_rate, y = total_diff)) +
    geom_point(aes(size = total_partidos_jugados),
               alpha = 0.22, color = "#81C784"
    ) +
    geom_point(data = df %>% filter(is_sel),
               size = 5, color = "#2E7D32"
    ) +
    scale_x_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
    labs(
      x = "% victorias",
      y = "Diferencia total (juegos)"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank()) +
    guides(size = "none")
}

# -----------------------------
# Bar Chart Race (Plotly) - PREMIUM
# df debe traer: jornada, jugador, puntos_acum, rank, y_rank, color, label_out
# -----------------------------
plot_rank_race_plotly <- function(rank_race_df, top_n = 10) {
  
  if (nrow(rank_race_df) == 0) {
    return(plotly::plot_ly() %>% plotly::layout(title = "Sin datos"))
  }
  
  top_n <- as.integer(top_n)
  
  # ---- Paleta automática (estable) por jugador ----
  players <- sort(unique(rank_race_df$jugador))
  ncol <- length(players)
  pal <- grDevices::hcl.colors(n = max(ncol, 3), palette = "Dark 3")
  names(pal) <- players
  
  # ---- Aire para texto fuera ----
  xmax <- max(rank_race_df$puntos_acum, na.rm = TRUE)
  if (!is.finite(xmax)) xmax <- 0
  xmax_pad <- xmax * 1.15 + 0.01
  
  plotly::plot_ly(
    data = rank_race_df,
    x = ~puntos_acum,
    y = ~y_rank,
    frame = ~jornada,
    ids = ~jugador,              # ✅ mantiene identidad entre frames
    type = "bar",
    orientation = "h",
    
    # ✅ colores automáticos por jugador
    color = ~jugador,
    colors = pal,
    
    # ✅ texto fuera (nombre + puntos) sin cortes
    text = ~label_out,
    textposition = "outside",
    cliponaxis = FALSE,
    constraintext = "none",
    
    customdata = ~jugador,
    hovertemplate = paste(
      "<b>Posición:</b> %{y}<br>",
      "<b>Jugador:</b> %{customdata}<br>",
      "<b>Jornada:</b> %{frame}<br>",
      "<b>Puntos acumulados:</b> %{x:.1f}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#f8faf9",
      font = list(family = "Arial", size = 14),
      
      xaxis = list(
        title = "Puntos acumulados",
        range = c(0, xmax_pad),
        showgrid = FALSE,
        zeroline = FALSE,
        fixedrange = TRUE
      ),
      
      yaxis = list(
        title = "",
        autorange = "reversed",          # 1 arriba
        tickmode = "array",
        tickvals = 1:top_n,
        ticktext = sprintf("%02d", 1:top_n),
        range = c(top_n + 0.5, 0.5),
        fixedrange = TRUE
      ),
      
      margin = list(l = 60, r = 220, t = 10, b = 40),
      showlegend = FALSE
    ) %>%
    # ✅ animación más fluida (menos tirones)
    plotly::animation_opts(
      frame = 1600,
      transition = 1600,
      easing = "linear",
      redraw = FALSE
    ) %>%
    plotly::animation_slider(
      currentvalue = list(prefix = "Jornada: ", font = list(size = 16))
    ) %>%
    plotly::animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
}

##########################################3
# -----------------------------
# Probabilidad victoria (barra simple)
# -----------------------------
plot_win_probability <- function(pA, labelA = "Pareja A", labelB = "Pareja B") {
  df <- tibble::tibble(
    pareja = c(labelA, labelB),
    prob = c(pA, 1 - pA)
  )
  
  ggplot2::ggplot(df, ggplot2::aes(x = pareja, y = prob)) +
    ggplot2::geom_col(fill = "#2E7D32", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(prob * 100, 1), "%")),
                       vjust = -0.3, fontface = "bold", size = 5) +
    ggplot2::scale_y_continuous(limits = c(0, 1.05), labels = function(x) paste0(round(x*100,0), "%")) +
    ggplot2::labs(x = NULL, y = "Probabilidad de victoria") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(face = "bold"))
}



# ==========================================================
# MONTE CARLO juego-a-juego (set a 6, sin tie-break)
# - Simula juegos con probabilidad p_game
# - Convierte p_match -> p_game por bisección usando DP
# ==========================================================

# Prob de que A gane un set a 6 si gana cada juego con prob p
p_set6_from_pgame <- function(p_game, max_games = 6) {
  dp <- matrix(0, nrow = max_games + 1, ncol = max_games + 1)
  dp[1, 1] <- 1
  
  for (a in 0:max_games) {
    for (b in 0:max_games) {
      prob <- dp[a + 1, b + 1]
      if (prob == 0) next
      if (a == max_games || b == max_games) next
      
      dp[a + 2, b + 1] <- dp[a + 2, b + 1] + prob * p_game
      dp[a + 1, b + 2] <- dp[a + 1, b + 2] + prob * (1 - p_game)
    }
  }
  
  # Estados finales donde A llega a 6 y B está en 0..5
  sum(dp[max_games + 1, 1:max_games])
}

# Convierte P(partido) -> p_game (aprox numérica)
p_game_from_pmatch <- function(p_match, max_games = 6, tol = 1e-4) {
  p_match <- max(min(as.numeric(p_match), 0.999), 0.001)
  
  lo <- 0.01; hi <- 0.99
  for (i in 1:45) {
    mid <- (lo + hi) / 2
    pmid <- p_set6_from_pgame(mid, max_games = max_games)
    if (abs(pmid - p_match) < tol) return(mid)
    if (pmid < p_match) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}

# Simula un set (a 6 juegos)
simulate_set_once <- function(p_game, max_games = 6) {
  a <- 0L; b <- 0L
  while (a < max_games && b < max_games) {
    if (runif(1) < p_game) a <- a + 1L else b <- b + 1L
  }
  c(a = a, b = b)
}

# Simulación Monte Carlo de marcadores
simulate_scorelines_game_by_game <- function(p_match_A, n_sims = 10000, max_games = 6) {
  p_match_A <- max(min(as.numeric(p_match_A), 0.999), 0.001)
  
  # Si A no es favorito, simulamos desde el favorito y luego invertimos marcador
  flip <- p_match_A < 0.5
  pW <- if (flip) 1 - p_match_A else p_match_A
  
  p_game <- p_game_from_pmatch(pW, max_games = max_games)
  
  scores <- character(n_sims)
  for (i in seq_len(n_sims)) {
    s <- simulate_set_once(p_game, max_games = max_games)
    if (!flip) {
      scores[i] <- paste0(s["a"], "-", s["b"])
    } else {
      scores[i] <- paste0(s["b"], "-", s["a"])
    }
  }
  
  df <- dplyr::count(tibble::tibble(score = scores), score, name = "n") %>%
    dplyr::mutate(prob = n / sum(n)) %>%
    dplyr::arrange(dplyr::desc(prob))
  
  list(p_game = p_game, table = df)
}

# ----------------------------------------------------------
# Plot top marcadores (barra)
# ----------------------------------------------------------
plot_top_scorelines <- function(score_df, top_k = 6) {
  df <- score_df %>%
    dplyr::slice_head(n = top_k) %>%
    dplyr::mutate(score = factor(score, levels = rev(score)))
  
  ggplot2::ggplot(df, ggplot2::aes(x = score, y = prob)) +
    ggplot2::geom_col(fill = "#2E7D32", width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(prob * 100, 1), "%")),
      hjust = -0.1, fontface = "bold", size = 4
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(x = NULL, y = "Probabilidad (Monte Carlo)") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
