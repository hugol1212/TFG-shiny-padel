# R/app_server.R

library(shiny)
library(DT)
library(plotly)

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(tibble)
library(purrr)
library(ggplot2)

source("R/functions_data.R")
source("R/functions_plot.R")

app_server <- function(input, output, session) {
  
  data <- load_data()
  
  seasons_all <- sort(unique(c(
    data$calendar$season,
    data$finance$season,
    data$matches$season,
    data$satellites$season
  )))
  
  seasons_all <- seasons_all[seasons_all != "2021-2022"]
  
  shiny::updateSelectInput(
    session,
    inputId = "season",
    choices = seasons_all,
    selected = seasons_all[length(seasons_all)]
  )
  
  detect_trimestre <- function(x) {
    x_norm <- x |>
      as.character() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      toupper()
    
    dplyr::case_when(
      stringr::str_detect(x_norm, "INVIERNO")  ~ "INVIERNO",
      stringr::str_detect(x_norm, "OTONO")     ~ "OTOÑO",
      stringr::str_detect(x_norm, "PRIMAVERA") ~ "PRIMAVERA",
      stringr::str_detect(x_norm, "VERANO")    ~ "VERANO",
      TRUE ~ "OTROS"
    )
  }
  
  matches_base <- shiny::reactive({
    data$matches %>%
      dplyr::mutate(trimestre = detect_trimestre(source_sheet))
  })
  
  satellites_base <- shiny::reactive({
    df <- data$satellites %>%
      dplyr::mutate(
        pareja_a = as.character(pareja_a),
        pareja_b = as.character(pareja_b)
      )
    
    fixed <- purrr::map2_dfr(df$pareja_a, df$pareja_b, fix_leaked_initials_row)
    
    df %>%
      dplyr::mutate(
        pareja_a = fixed$pareja_a,
        pareja_b = fixed$pareja_b,
        trimestre = detect_trimestre(source_sheet)
      )
  })
  
  # ==========================================================
  # DATASETS GLOBAL (SIN FILTROS) -> SOLO SIMULADOR
  # (NO usan season / trimestre / jornada)
  # ==========================================================
  matches_sim <- shiny::reactive({
    matches_base()
  })
  
  satellites_sim <- shiny::reactive({
    satellites_base()
  })
  
  players_total_sim <- shiny::reactive({
    make_player_stats_total(
      make_player_stats(matches_sim()),
      make_player_stats_satellites(satellites_sim())
    )
  })
  
  satellites_f <- shiny::reactive({
    sb <- satellites_base()
    
    if (!is.null(input$season)) sb <- dplyr::filter(sb, season == input$season)
    if (!is.null(input$trimestre) && input$trimestre != "__ALL__") sb <- dplyr::filter(sb, trimestre == input$trimestre)
    if (!is.null(input$jornada) && input$jornada != "__ALL__") sb <- dplyr::filter(sb, jornada == as.integer(input$jornada))
    
    sb
  })
  
  cal_f <- shiny::reactive({
    if (is.null(input$season)) return(data$calendar)
    dplyr::filter(data$calendar, season == input$season)
  })
  
  fin_f <- shiny::reactive({
    if (is.null(input$season)) return(data$finance)
    dplyr::filter(data$finance, season == input$season)
  })
  
  observeEvent(input$season, {
    mb <- matches_base()
    mb_season <- if (!is.null(input$season)) dplyr::filter(mb, season == input$season) else mb
    
    tr_choices <- sort(unique(mb_season$trimestre))
    tr_choices <- tr_choices[!is.na(tr_choices) & tr_choices != ""]
    
    shiny::updateSelectInput(
      session,
      inputId = "trimestre",
      choices = c("Todos" = "__ALL__", tr_choices),
      selected = "__ALL__"
    )
    
    j_choices <- sort(unique(mb_season$jornada))
    j_choices <- j_choices[!is.na(j_choices)]
    
    shiny::updateSelectInput(
      session,
      inputId = "jornada",
      choices = c("Todas" = "__ALL__", j_choices),
      selected = "__ALL__"
    )
  }, ignoreInit = FALSE)
  
  matches_f <- shiny::reactive({
    mb <- matches_base()
    
    if (!is.null(input$season)) mb <- dplyr::filter(mb, season == input$season)
    if (!is.null(input$trimestre) && input$trimestre != "__ALL__") mb <- dplyr::filter(mb, trimestre == input$trimestre)
    if (!is.null(input$jornada) && input$jornada != "__ALL__") mb <- dplyr::filter(mb, jornada == as.integer(input$jornada))
    
    mb
  })
  
  # ----------------------------
  # TABLAS (DEBUG)
  # ----------------------------
  output$tbl_matches <- DT::renderDT({
    DT::datatable(
      as.data.frame(matches_f()),
      options = list(pageLength = 25, scrollX = TRUE),
      class = "nowrap"
    )
  })
  
  players_f <- shiny::reactive({ make_player_stats(matches_f()) })
  
  output$tbl_players <- DT::renderDT({
    DT::datatable(
      as.data.frame(players_f()),
      options = list(pageLength = 25, scrollX = TRUE),
      class = "nowrap"
    )
  })
  
  output$tbl_calendar <- DT::renderDT({
    cal_show <- cal_f() %>%
      dplyr::select(
        season, ano, trimestre, jornada, fecha, sede,
        pareja_bloqueada_alta, pareja_bloqueada_baja,
        jornada_de_ascenso_descenso, notas, n_mero_de_asistentes
      ) %>%
      dplyr::rename(asistentes = n_mero_de_asistentes) %>%
      dplyr::mutate(fecha = format(fecha, "%d/%m/%Y"))
    
    DT::datatable(
      as.data.frame(cal_show),
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      class = "nowrap"
    )
  })
  
  output$tbl_finance <- DT::renderDT({
    fin_show <- fin_f() %>%
      dplyr::select(season, jornada, fecha, bote, gasto_bolas, montados, otros_gastos) %>%
      dplyr::mutate(fecha = format(fecha, "%d/%m/%Y"))
    
    DT::datatable(
      as.data.frame(fin_show),
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE),
      class = "nowrap"
    )
  })
  
  output$tbl_satellites <- DT::renderDT({
    DT::datatable(
      as.data.frame(satellites_f()),
      options = list(pageLength = 25, scrollX = TRUE),
      class = "nowrap"
    )
  })
  
  players_sat_f <- shiny::reactive({ make_player_stats_satellites(satellites_f()) })
  
  output$tbl_players_sat <- DT::renderDT({
    DT::datatable(
      as.data.frame(players_sat_f()),
      options = list(pageLength = 25, scrollX = TRUE),
      class = "nowrap"
    )
  })
  
  players_total_f <- shiny::reactive({ make_player_stats_total(players_f(), players_sat_f()) })
  
  output$tbl_players_total <- DT::renderDT({
    df <- rename_total_cols_es(players_total_f())
    DT::datatable(
      as.data.frame(df),
      options = list(pageLength = 25, scrollX = TRUE),
      class = "nowrap"
    )
  })
  
  # ---- Gráfico Top N (Resumen) ----
  output$plot_top_total <- shiny::renderPlot({
    n <- if (is.null(input$top_n)) 10 else as.integer(input$top_n)
    plot_top_players_total(players_total_f(), n = n)
  })
  
  # ---- KPIs Resumen ----
  output$kpi_players_unique <- shiny::renderText({
    df <- players_total_f() %>% dplyr::filter(!is.na(total_puntuacion), jugador != "")
    nrow(df)
  })
  
  output$kpi_max_points <- shiny::renderText({
    df <- players_total_f() %>% dplyr::filter(!is.na(total_puntuacion), jugador != "")
    mx <- max(as.numeric(df$total_puntuacion), na.rm = TRUE)
    format(round(mx, 1), big.mark = ".", decimal.mark = ",", nsmall = 1)
  })
  
  output$kpi_top_player <- shiny::renderText({
    df <- players_total_f() %>%
      dplyr::filter(!is.na(total_puntuacion), jugador != "") %>%
      dplyr::arrange(dplyr::desc(total_puntuacion)) %>%
      dplyr::slice_head(n = 1)
    if (nrow(df) == 0) return("-")
    df$jugador[1]
  })
  
  # Rellenar selector jugador
  shiny::observeEvent(players_total_f(), {
    choices <- players_total_f() %>% dplyr::arrange(jugador) %>% dplyr::pull(jugador)
    if (length(choices) == 0) return()
    shiny::updateSelectInput(session, "player_sel", choices = choices, selected = choices[1])
  }, ignoreInit = TRUE)
  
  # Dataset por jornada TOTAL
  players_by_jornada_f <- shiny::reactive({
    req(input$season)
    
    df_cuadro <- matches_base() %>% dplyr::filter(season == input$season)
    df_sat    <- satellites_base() %>% dplyr::filter(season == input$season)
    
    if (!is.null(input$trimestre) && input$trimestre != "__ALL__") {
      df_cuadro <- dplyr::filter(df_cuadro, trimestre == input$trimestre)
      df_sat    <- dplyr::filter(df_sat, trimestre == input$trimestre)
    }
    
    make_player_stats_by_jornada_total(df_cuadro, df_sat)
  })
  
  # Rellenar selector desde dataset por jornada
  shiny::observeEvent(players_by_jornada_f(), {
    dfj <- players_by_jornada_f()
    choices <- dfj %>% dplyr::distinct(jugador) %>% dplyr::arrange(jugador) %>% dplyr::pull(jugador)
    if (length(choices) == 0) return()
    shiny::updateSelectInput(session, "player_sel", choices = choices, selected = choices[1])
  }, ignoreInit = TRUE)
  
  # Plot evolución jugador
  output$plot_player_evolution <- shiny::renderPlot({
    req(input$player_sel)
    plot_player_evolution(players_by_jornada_f(), input$player_sel)
  })
  
  # KPIs jugador
  player_row_total_f <- shiny::reactive({
    req(input$player_sel)
    df <- players_total_f()
    
    key_norm_local <- function(x) {
      x <- as.character(x)
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
      x <- toupper(x)
      stringr::str_squish(x)
    }
    
    df <- df %>% mutate(jugador_key = key_norm_local(jugador))
    row <- df %>% filter(jugador_key == key_norm_local(input$player_sel)) %>% slice_head(n = 1)
    if (nrow(row) == 0) return(NULL)
    row
  })
  
  output$player_kpi_total_points <- shiny::renderText({
    row <- player_row_total_f()
    if (is.null(row)) return("-")
    format(round(as.numeric(row$total_puntuacion), 1), big.mark = ".", decimal.mark = ",", nsmall = 1)
  })
  
  output$player_kpi_matches <- shiny::renderText({
    req(input$player_sel)
    df <- players_total_f() %>% dplyr::filter(jugador == input$player_sel)
    if (nrow(df) == 0) return("0")
    val <- df$total_partidos_jugados[1]; if (is.na(val)) val <- 0
    format(val, big.mark = ".", decimal.mark = ",")
  })
  
  output$player_kpi_winrate <- shiny::renderText({
    row <- player_row_total_f()
    if (is.null(row)) return("-")
    paste0(format(round(as.numeric(row$total_win_rate) * 100, 1), decimal.mark = ","), "%")
  })
  
  output$player_kpi_diff <- shiny::renderText({
    row <- player_row_total_f()
    if (is.null(row)) return("-")
    as.integer(round(as.numeric(row$total_diff), 0))
  })
  
  output$player_kpi_wins <- shiny::renderText({
    row <- player_row_total_f()
    if (is.null(row)) return("0")
    val <- suppressWarnings(as.numeric(row$total_partidos_ganados[1])); if (is.na(val)) val <- 0
    format(val, big.mark = ".", decimal.mark = ",")
  })
  
  output$plot_player_split <- shiny::renderPlot({
    req(input$player_sel)
    plot_player_split_donut(players_total_f(), input$player_sel)
  })
  
  output$plot_player_scatter <- shiny::renderPlot({
    req(input$player_sel)
    plot_league_scatter_highlight(players_total_f(), input$player_sel)
  })
  
  # ==========================================================
  # EVOLUCIÓN: BAR CHART RACE (TODOS) + INSIGHTS TOTAL
  # ==========================================================
  rank_race_f <- shiny::reactive({
    dfj <- players_by_jornada_f()
    n_all <- nrow(dplyr::distinct(dfj, jugador))
    make_rank_race_data(dfj, top_n = n_all)
  })
  
  output$race_leader <- shiny::renderText({
    df <- rank_race_f()
    if (nrow(df) == 0) return("Líder: -")
    
    jmax <- max(df$jornada, na.rm = TRUE)
    lead <- df %>% dplyr::filter(jornada == jmax, rank == 1) %>% dplyr::slice_head(n = 1)
    if (nrow(lead) == 0) return("Líder: -")
    paste0("Líder: ", lead$jugador[1])
  })
  
  output$plot_rank_race <- plotly::renderPlotly({
    df <- rank_race_f()
    top_n_real <- if (nrow(df) == 0) 10 else max(df$rank, na.rm = TRUE)
    p <- plot_rank_race_plotly(df, top_n = top_n_real)
    plotly::config(p, displayModeBar = FALSE)
  })
  
  swings_total_f <- shiny::reactive({
    compute_biggest_swings_total(players_by_jornada_f())
  })
  
  output$kpi_best_comeback_total <- shiny::renderText({
    res <- swings_total_f(); cb <- res$comeback_total
    if (is.null(cb) || nrow(cb) == 0) return("-")
    paste0(cb$jugador[1], " (+", as.integer(cb$comeback_delta[1]), " puestos)")
  })
  
  output$kpi_best_comeback_total_detail <- shiny::renderText({
    res <- swings_total_f(); cb <- res$comeback_total
    if (is.null(cb) || nrow(cb) == 0) return("-")
    paste0(
      "De puesto ", cb$comeback_from_rank[1], " a ", cb$comeback_to_rank[1],
      " (J", cb$comeback_from_jornada[1], "–J", cb$comeback_to_jornada[1], ")"
    )
  })
  
  output$kpi_worst_drop_total <- shiny::renderText({
    res <- swings_total_f(); dr <- res$drop_total
    if (is.null(dr) || nrow(dr) == 0) return("-")
    paste0(dr$jugador[1], " (-", as.integer(dr$drop_delta[1]), " puestos)")
  })
  
  output$kpi_worst_drop_total_detail <- shiny::renderText({
    res <- swings_total_f(); dr <- res$drop_total
    if (is.null(dr) || nrow(dr) == 0) return("-")
    paste0(
      "De puesto ", dr$drop_from_rank[1], " a ", dr$drop_to_rank[1],
      " (J", dr$drop_from_jornada[1], "–J", dr$drop_to_jornada[1], ")"
    )
  })
  
  # ==========================================================
  # SIMULADOR DE PARTIDOS
  # ==========================================================
  
  # Toggle A+ para mostrar/ocultar "tablas técnicas"
  show_details <- shiny::reactiveVal(FALSE)
  
  shiny::observeEvent(input$toggle_details, {
    show_details(!isTRUE(show_details()))
  })
  
  output$show_details <- shiny::reactive({
    show_details()
  })
  shiny::outputOptions(output, "show_details", suspendWhenHidden = FALSE)
  
  output$toggle_details_label <- shiny::renderText({
    if (isTRUE(show_details())) "Ocultar detalles técnicos" else "Mostrar detalles técnicos"
  })
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # IMPORTANTE: el simulador usa players_total_sim() (SIN FILTROS)
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  shiny::observeEvent(players_total_sim(), {
    choices <- players_total_sim() %>% dplyr::arrange(jugador) %>% dplyr::pull(jugador)
    choices <- choices[!is.na(choices) & choices != ""]
    if (length(choices) == 0) return()
    
    shiny::updateSelectInput(session, "sim_A1", choices = choices, selected = choices[1])
    shiny::updateSelectInput(session, "sim_A2", choices = choices, selected = choices[min(2, length(choices))])
    shiny::updateSelectInput(session, "sim_B1", choices = choices, selected = choices[min(3, length(choices))])
    shiny::updateSelectInput(session, "sim_B2", choices = choices, selected = choices[min(4, length(choices))])
  }, ignoreInit = FALSE)
  
  sim_res <- shiny::eventReactive(input$sim_run, {
    req(input$sim_A1, input$sim_A2, input$sim_B1, input$sim_B2)
    
    if (key_norm(input$sim_A1) == key_norm(input$sim_A2)) {
      return(list(error = "La Pareja A no puede tener el mismo jugador dos veces."))
    }
    if (key_norm(input$sim_B1) == key_norm(input$sim_B2)) {
      return(list(error = "La Pareja B no puede tener el mismo jugador dos veces."))
    }
    
    all_keys <- c(key_norm(input$sim_A1), key_norm(input$sim_A2), key_norm(input$sim_B1), key_norm(input$sim_B2))
    if (length(unique(all_keys)) < 4) {
      return(list(error = "Un jugador no puede estar en ambas parejas a la vez."))
    }
    
    n_last_val <- if (is.null(input$sim_n_last) || is.na(input$sim_n_last)) 6L else as.integer(input$sim_n_last)
    
    out <- predict_match_winner(
      players_total_df = players_total_sim(),  # SIN FILTROS
      matches_df = matches_sim(),              # SIN FILTROS
      sat_df = satellites_sim(),               # SIN FILTROS
      A1 = input$sim_A1, A2 = input$sim_A2,
      B1 = input$sim_B1, B2 = input$sim_B2,
      n_last = n_last_val
    )
    
    out
  })
  
  output$sim_result_text <- shiny::renderText({
    res <- sim_res()
    if (is.null(res)) return("Selecciona las parejas y pulsa Simular.")
    if (!is.null(res$error)) return(res$error)
    
    ganador <- if (res$pA >= 0.5) paste0("Favorece a Pareja A") else paste0("Favorece a Pareja B")
    prob_txt <- format(round(max(res$pA, res$pB) * 100, 1), decimal.mark = ",")
    
    paste0(ganador, " (", prob_txt, "%)")
  })
  
  output$sim_prob_plot <- shiny::renderPlot({
    res <- sim_res()
    if (is.null(res) || !is.null(res$error)) return(ggplot2::ggplot() + ggplot2::theme_void())
    
    plot_win_probability(
      pA = res$pA,
      labelA = paste0("A: ", res$pairA),
      labelB = paste0("B: ", res$pairB)
    )
  })
  
  output$sim_tbl_players <- DT::renderDT({
    res <- sim_res()
    if (is.null(res) || !is.null(res$error)) {
      return(DT::datatable(data.frame()))
    }
    
    df <- res$detail_players %>%
      dplyr::mutate(
        total_win_rate = round(total_win_rate * 100, 1),
        recent_win_rate = round(recent_win_rate * 100, 1),
        rating = round(rating, 3),
        diff_por_partido = round(diff_per_match, 3),
        puntos_por_partido = round(points_per_match, 3),
        diff_reciente_media = round(recent_diff_mean, 3),
        experiencia_01 = round(exp_scaled, 3)
      ) %>%
      dplyr::select(
        role, jugador,
        total_win_rate, diff_por_partido, puntos_por_partido,
        recent_matches, recent_win_rate, diff_reciente_media,
        experiencia_01, rating
      ) %>%
      dplyr::rename(
        rol = role,
        win_rate_total_pct = total_win_rate,
        partidos_recientes = recent_matches,
        win_rate_reciente_pct = recent_win_rate
      )
    
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), class = "nowrap")
  })
  
  output$sim_tbl_pairs <- DT::renderDT({
    res <- sim_res()
    if (is.null(res) || !is.null(res$error)) {
      return(DT::datatable(data.frame()))
    }
    
    df <- res$detail_pairs %>%
      dplyr::mutate(
        synergy_win_rate = ifelse(is.na(synergy_win_rate), NA, round(synergy_win_rate * 100, 1)),
        base_rating = round(base_rating, 3),
        synergy_bonus = round(synergy_bonus, 3),
        final_rating = round(final_rating, 3)
      ) %>%
      dplyr::rename(
        lado = side,
        pareja = pareja,
        rating_base = base_rating,
        bonus_sinergia = synergy_bonus,
        rating_final = final_rating,
        partidos_juntos = synergy_matches,
        win_rate_pareja_pct = synergy_win_rate
      )
    
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE), class = "nowrap")
  })
  
  # Monte Carlo scorelines (juego a juego)
  score_mc <- shiny::reactive({
    res <- sim_res()
    if (is.null(res) || !is.null(res$error)) return(NULL)
    
    simulate_scorelines_game_by_game(
      p_match_A = res$pA,
      n_sims = 12000,
      max_games = 6
    )
  })
  
  output$sim_tbl_scores <- DT::renderDT({
    sc <- score_mc()
    if (is.null(sc)) {
      return(
        DT::datatable(
          data.frame(),
          options = list(dom = "t"),
          rownames = FALSE
        )
      )
    }
    
    df <- sc$table %>%
      dplyr::slice_head(n = 6) %>%
      dplyr::mutate(
        prob_pct = paste0(format(round(prob * 100, 2), decimal.mark = ","), "%")
      ) %>%
      dplyr::select(
        Marcador = score,
        Probabilidad = prob_pct
      )
    
    DT::datatable(
      df,
      rownames = FALSE,
      class = "compact",
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = 0),
          list(className = "dt-right", targets = 1)
        )
      )
    )
  })
}