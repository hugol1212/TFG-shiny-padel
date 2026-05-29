# R/app_ui.R

library(shiny)
library(DT)
library(plotly)

app_ui <- function() {
  shiny::navbarPage(
    title = "TFG Pádel - Dashboard",
    id = "main_tab",
    
    # ---- Styles + filtros bajo pestañas ----
    header = tagList(
      tags$head(
        tags$style(HTML("
          body { background: #ffffff; }

          .kpi-card {
            background: linear-gradient(180deg, #ffffff 0%, #f9fbfa 100%);
            border: 1px solid #e6ece8;
            border-left: 6px solid #2E7D32;
            border-radius: 16px;
            padding: 18px;
            box-shadow: 0 6px 18px rgba(0,0,0,0.06);
            height: 110px;
            display: flex;
            flex-direction: column;
            justify-content: center;
            margin-bottom: 12px;
            transition: all 0.2s ease;
          }
          .kpi-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 10px 22px rgba(0,0,0,0.08);
          }

          .kpi-label {
            font-size: 12px;
            color: #495057;
            margin-bottom: 6px;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            font-weight: 800;
          }
          .kpi-value {
            font-size: 22px;
            font-weight: 800;
            color: #212121;
            line-height: 1.15;
          }
          .kpi-sub {
            font-size: 12px;
            color: #6c757d;
            margin-top: 6px;
            font-weight: 600;
          }

          .box {
            background: #fff;
            border: 1px solid #e9ecef;
            border-radius: 16px;
            padding: 16px 18px;
            box-shadow: 0 6px 18px rgba(0,0,0,0.05);
            margin-bottom: 14px;
          }

          /* --- Filter card (selector jugador) --- */
          .filter-card {
            background: linear-gradient(180deg, rgba(46,125,50,0.10) 0%, rgba(255,255,255,1) 55%);
            border: 1px solid #dfe7e2;
            border-top: 4px solid #2E7D32;
            border-radius: 16px;
            padding: 14px 16px;
            box-shadow: 0 8px 20px rgba(0,0,0,0.06);
            margin-bottom: 12px;
          }

          .filter-title {
            font-size: 12px;
            font-weight: 900;
            color: #1B5E20;
            text-transform: uppercase;
            letter-spacing: 0.06em;
            margin-bottom: 8px;
            display: flex;
            align-items: center;
            gap: 8px;
          }

          .filter-card .form-control {
            height: 44px;
            border-radius: 12px;
            border: 1px solid #cfd8d3;
            font-weight: 700;
          }

          .filter-card .form-control:focus {
            border-color: #2E7D32;
            box-shadow: 0 0 0 0.2rem rgba(46,125,50,0.18);
          }

          /* Evolución (bar race) */
          .race-sub {
            color: #6c757d;
            font-size: 12px;
            font-weight: 600;
            margin-bottom: 6px;
          }
          .leader-box {
            font-size: 20px;
            font-weight: 900;
            color: #1B5E20;
            margin-bottom: 10px;
          }
          .race-foot {
            text-align: center;
            font-size: 12px;
            color: #6c757d;
            font-weight: 600;
            margin-top: 6px;
          }

          /* Toggle detalles técnicos */
          a#toggle_details {
            font-weight: 800;
            color: #1B5E20;
            text-decoration: none;
          }
          a#toggle_details:hover {
            text-decoration: underline;
          }

          /* Navbar */
          .navbar {
            margin-bottom: 10px;
          }

          /* =========================
             SIMULADOR
          ========================= */
          .sim-card {
            background: linear-gradient(180deg, #ffffff 0%, #fbfcfb 100%);
            border: 1px solid #e5ebe7;
            border-radius: 18px;
            padding: 18px 20px;
            box-shadow: 0 8px 22px rgba(0,0,0,0.05);
            margin-bottom: 16px;
          }

          .sim-card-compact {
            background: #ffffff;
            border: 1px solid #e7ece9;
            border-radius: 18px;
            padding: 16px 18px;
            box-shadow: 0 6px 18px rgba(0,0,0,0.05);
            margin-bottom: 16px;
          }

          .sim-title {
            font-size: 18px;
            font-weight: 800;
            color: #1f2d1f;
            margin-bottom: 6px;
          }

          .sim-subtitle {
            font-size: 13px;
            color: #6c757d;
            margin-bottom: 12px;
            font-weight: 500;
          }

          .sim-section-title {
            font-size: 15px;
            font-weight: 800;
            color: #1B5E20;
            text-transform: uppercase;
            letter-spacing: 0.04em;
            margin-bottom: 12px;
          }

          .sim-result-text {
            font-size: 20px;
            font-weight: 800;
            color: #1B5E20;
            margin-bottom: 8px;
            line-height: 1.3;
          }

          .sim-note {
            font-size: 12px;
            color: #6c757d;
            font-weight: 600;
            margin-top: 6px;
          }

          .sim-card .form-group,
          .sim-card-compact .form-group {
            margin-bottom: 14px;
          }

          .sim-card .form-control,
          .sim-card-compact .form-control {
            height: 44px;
            border-radius: 12px;
            border: 1px solid #cfd8d3;
            font-weight: 700;
          }

          .sim-card .form-control:focus,
          .sim-card-compact .form-control:focus {
            border-color: #2E7D32;
            box-shadow: 0 0 0 0.2rem rgba(46,125,50,0.15);
          }

          .sim-btn .btn {
            width: 100%;
            border-radius: 12px;
            font-weight: 800;
            padding: 10px 14px;
          }

          .sim-pairs-row {
            margin-top: 6px;
          }

          .sim-results-row {
            margin-top: 2px;
          }
                  /* Tabla marcadores simulador */
        table.dataTable.compact thead th {
          font-size: 14px;
          font-weight: 800;
          color: #1f2d1f;
          border-bottom: 2px solid #e5ebe7 !important;
        }

        table.dataTable.compact tbody td {
          font-size: 15px;
          padding: 10px 8px !important;
          border-bottom: 1px solid #f0f3f1 !important;
        }

        table.dataTable.no-footer {
          border-bottom: none !important;
        }

        .dataTables_wrapper .dataTables_paginate,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_length {
          display: none !important;
        }
        "))
      ),
      
      # Filtros bajo las pestañas (ocultos en Simulador)
      div(
        style = "padding: 0 15px 10px 15px;",
        shiny::conditionalPanel(
          condition = "input.main_tab != 'sim'",
          fluidRow(
            column(4, selectInput("season", "Temporada", choices = NULL, selected = NULL)),
            column(4, selectInput("trimestre", "Trimestre", choices = NULL, selected = "__ALL__")),
            
            conditionalPanel(
              condition = "input.main_tab != 'evolucion'",
              column(4, selectInput("jornada", "Jornada (filtra tablas y ranking)", choices = NULL, selected = "__ALL__"))
            )
          )
        )
      )
    ),
    
    # ==========================================================
    # RESUMEN
    # ==========================================================
    tabPanel(
      title = "Resumen",
      value = "resumen",
      
      fluidRow(
        column(
          3,
          div(
            class = "box",
            selectInput("top_n", "Top N", choices = c(5, 10, 15, 20), selected = 10)
          )
        ),
        column(
          3,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "PARTICIPANTES"),
            div(class = "kpi-value", textOutput("kpi_players_unique", inline = TRUE))
          )
        ),
        column(
          3,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "MÁXIMA PUNTUACIÓN"),
            div(class = "kpi-value", textOutput("kpi_max_points", inline = TRUE))
          )
        ),
        column(
          3,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "GANADOR"),
            div(class = "kpi-value", textOutput("kpi_top_player", inline = TRUE))
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "box",
            h4("Ranking por puntuación total"),
            plotOutput("plot_top_total", height = "520px")
          )
        )
      )
    ),
    
    # ==========================================================
    # EVOLUCIÓN
    # ==========================================================
    tabPanel(
      title = "Evolución",
      value = "evolucion",
      
      fluidRow(
        column(
          12,
          div(
            class = "box",
            div(
              class = "race-sub",
              "Pulsa Play para ver el ranking acumulado evolucionar por jornada (todos los jugadores)."
            ),
            div(class = "leader-box", textOutput("race_leader", inline = TRUE)),
            
            fluidRow(
              column(
                6,
                div(
                  class = "kpi-card",
                  div(class = "kpi-label", "REMONTADA TOTAL"),
                  div(class = "kpi-value", textOutput("kpi_best_comeback_total", inline = TRUE)),
                  div(class = "kpi-sub", textOutput("kpi_best_comeback_total_detail", inline = TRUE))
                )
              ),
              column(
                6,
                div(
                  class = "kpi-card",
                  div(class = "kpi-label", "CAÍDA TOTAL"),
                  div(class = "kpi-value", textOutput("kpi_worst_drop_total", inline = TRUE)),
                  div(class = "kpi-sub", textOutput("kpi_worst_drop_total_detail", inline = TRUE))
                )
              )
            ),
            
            plotlyOutput("plot_rank_race", height = "620px"),
            div(class = "race-foot", "Ranking acumulado por jornada (Cuadro + Satélite)")
          )
        )
      )
    ),
    
    # ==========================================================
    # JUGADOR
    # ==========================================================
    tabPanel(
      title = "Jugador",
      value = "jugador",
      
      fluidRow(
        column(
          4,
          fluidRow(
            column(
              6,
              div(
                class = "filter-card",
                div(class = "filter-title", "Jugador"),
                selectInput("player_sel", label = NULL, choices = NULL, selected = NULL)
              )
            ),
            column(
              6,
              div(
                class = "kpi-card",
                div(class = "kpi-label", "PARTIDOS GANADOS"),
                div(class = "kpi-value", textOutput("player_kpi_wins", inline = TRUE)),
                div(class = "kpi-sub", "Cuadro + Satélite")
              )
            )
          ),
          
          fluidRow(
            column(
              6,
              div(
                class = "kpi-card",
                div(class = "kpi-label", "PUNTOS TOTALES"),
                div(class = "kpi-value", textOutput("player_kpi_total_points", inline = TRUE))
              )
            ),
            column(
              6,
              div(
                class = "kpi-card",
                div(class = "kpi-label", "PARTIDOS JUGADOS"),
                div(class = "kpi-value", textOutput("player_kpi_matches", inline = TRUE)),
                div(class = "kpi-sub", "Cuadro + Satélite")
              )
            )
          ),
          
          fluidRow(
            column(
              6,
              div(
                class = "kpi-card",
                div(class = "kpi-label", "% VICTORIAS"),
                div(class = "kpi-value", textOutput("player_kpi_winrate", inline = TRUE))
              )
            ),
            column(
              6,
              div(
                class = "kpi-card",
                div(class = "kpi-label", "DIFERENCIA"),
                div(class = "kpi-value", textOutput("player_kpi_diff", inline = TRUE)),
                div(class = "kpi-sub", "Juegos (ganados - perdidos)")
              )
            )
          ),
          
          div(
            class = "box",
            h4("Cuadro vs Satélite"),
            plotOutput("plot_player_split", height = "200px")
          )
        ),
        
        column(
          8,
          div(
            class = "box",
            h4("Evolución: puntos por jornada"),
            plotOutput("plot_player_evolution", height = "220px")
          ),
          div(
            class = "box",
            h4("Comparativa en liga"),
            plotOutput("plot_player_scatter", height = "220px")
          )
        )
      )
    ),
    
    # ==========================================================
    # SIMULADOR
    # ==========================================================
    tabPanel(
      title = "Simulador",
      value = "sim",
      
      fluidRow(
        column(
          12,
          div(
            class = "sim-card",
            div(class = "sim-title", "Simulador de partido"),
            div(
              class = "sim-subtitle",
              "Selecciona 2 jugadores por pareja. El modelo combina rendimiento global, forma reciente y sinergia histórica."
            )
          )
        )
      ),
      
      fluidRow(
        class = "sim-pairs-row",
        column(
          6,
          div(
            class = "sim-card-compact",
            div(class = "sim-section-title", "Pareja A"),
            selectInput("sim_A1", "Jugador A1", choices = NULL),
            selectInput("sim_A2", "Jugador A2", choices = NULL)
          )
        ),
        column(
          6,
          div(
            class = "sim-card-compact",
            div(class = "sim-section-title", "Pareja B"),
            selectInput("sim_B1", "Jugador B1", choices = NULL),
            selectInput("sim_B2", "Jugador B2", choices = NULL)
          )
        )
      ),
      
      fluidRow(
        class = "sim-results-row",
        column(
          3,
          div(
            class = "sim-card-compact",
            div(class = "sim-section-title", "Configuración"),
            sliderInput(
              "sim_n_last",
              "Últimos partidos considerados",
              min = 2, max = 20, value = 6, step = 1
            ),
            div(
              class = "sim-btn",
              actionButton("sim_run", "Simular", class = "btn-success")
            ),
            div(
              class = "sim-note",
              "La forma reciente se calcula con los últimos partidos del historial."
            )
          )
        ),
        column(
          5,
          div(
            class = "sim-card",
            div(class = "sim-section-title", "Resultado"),
            div(
              class = "sim-result-text",
              textOutput("sim_result_text", inline = TRUE)
            ),
            plotOutput("sim_prob_plot", height = "240px")
          )
        ),
        column(
          4,
          div(
            class = "sim-card",
            div(class = "sim-section-title", "Marcadores más probables"),
            DTOutput("sim_tbl_scores")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "box",
            actionLink("toggle_details", textOutput("toggle_details_label", inline = TRUE))
          )
        )
      ),
      
      conditionalPanel(
        condition = "output.show_details === true",
        fluidRow(
          column(
            6,
            div(class = "box", h4("Detalle por jugador"), DTOutput("sim_tbl_players"))
          ),
          column(
            6,
            div(class = "box", h4("Detalle por pareja (sinergia)"), DTOutput("sim_tbl_pairs"))
          )
        )
      )
    ),
    
    # ==========================================================
    # DEBUG
    # ==========================================================
    #tabPanel("Calendario", value = "cal", DTOutput("tbl_calendar")),
    #tabPanel("Tesorería", value = "fin", DTOutput("tbl_finance")),
    #tabPanel("Jornadas (debug)", value = "dbg_matches", DTOutput("tbl_matches")),
    #tabPanel("Jugadores (debug)", value = "dbg_players", DTOutput("tbl_players")),
    #tabPanel("Satélites (debug)", value = "dbg_sat", DTOutput("tbl_satellites")),
    #tabPanel("Jugadores Satélites (debug)", value = "dbg_players_sat", DTOutput("tbl_players_sat")),
    #tabPanel("Jugadores TOTAL", value = "dbg_total", DTOutput("tbl_players_total"))
  )
}