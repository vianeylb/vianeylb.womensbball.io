library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)

# ── load & prep data ─────────────────────────────────────────────────────────
df_raw <- read.csv(
  "wehoop_wnba_pbp_parsed.csv",
  stringsAsFactors = FALSE,
  colClasses = c(team_id = "character", home_team_id = "character",
                 away_team_id = "character")
)

df_raw <- df_raw |>
  filter(!is.na(season), season == floor(season)) |>
  mutate(
    season         = as.integer(season),
    is_three_point = as.logical(is_three_point),
    game_id        = as.character(as.integer(game_id)),
    game_date      = as.Date(game_date),
    team_id        = as.character(as.integer(as.numeric(team_id))),
    shooting_team  = ifelse(team_id == home_team_id, home_team_abbrev, away_team_abbrev),
    shooting_team_name = ifelse(team_id == home_team_id, home_team_name, away_team_name)
  )

shots_all <- df_raw |>
  filter(play_category == "shot", season == 2025) |>
  filter(!is.na(shooting_team), shooting_team != "")

valid_teams <- shots_all |> count(shooting_team) |> filter(n >= 200) |> pull(shooting_team)
shots_all   <- shots_all |> filter(shooting_team %in% valid_teams)

team_lookup <- shots_all |>
  distinct(shooting_team, shooting_team_name) |>
  arrange(shooting_team) |>
  mutate(label = paste0(shooting_team, " — ", shooting_team_name))

game_lookup <- shots_all |>
  distinct(game_id, game_date, home_team_abbrev, away_team_abbrev) |>
  arrange(game_date) |>
  mutate(label = paste0(format(game_date, "%b %d"), "  ",
                        away_team_abbrev, " @ ", home_team_abbrev,
                        "  [", game_id, "]"))

# ── theme ─────────────────────────────────────────────────────────────────────
pal_result <- c(made = "#1D9E75", missed = "#E24B4A", blocked = "#888780")

theme_wnba <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(size = base + 1, face = "bold", margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base - 1, color = "grey50", margin = margin(b = 8)),
      axis.title       = element_text(size = base - 1, color = "grey40"),
      axis.text        = element_text(size = base - 2, color = "grey40"),
      panel.grid.major = element_line(color = "grey93", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position  = "bottom",
      legend.text      = element_text(size = base - 2),
      legend.title     = element_text(size = base - 2, face = "bold")
    )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f8f8f8; font-family:'Helvetica Neue',sans-serif; }
    .header { background:#1a1a2e; color:white; padding:16px 24px;
              margin-bottom:20px; border-radius:8px; }
    .header h2 { margin:0; font-size:22px; font-weight:700; }
    .header p  { margin:4px 0 0; font-size:13px; color:#aaa; }
    .filter-panel { background:white; border:1px solid #e8e8e8; border-radius:8px;
                    padding:16px 20px; margin-bottom:18px; }
    .filter-panel h4 { margin:0 0 12px; font-size:13px; font-weight:600;
                       text-transform:uppercase; letter-spacing:.05em; color:#555; }
    .stat-box { background:white; border:1px solid #e8e8e8; border-radius:8px;
                padding:14px 18px; text-align:center; margin-bottom:16px; }
    .stat-val { font-size:26px; font-weight:700; color:#1a1a2e; }
    .stat-lbl { font-size:11px; color:#888; margin-top:2px;
                text-transform:uppercase; letter-spacing:.04em; }
    .stat-formula { font-size:10px; color:#aaa; margin-top:4px;
                    font-style:italic; line-height:1.4; }
    .plot-card { background:white; border:1px solid #e8e8e8; border-radius:8px;
                 padding:16px; margin-bottom:16px; }
    .selectize-input { font-size:13px !important; }
  "))),
  
  # header
  div(class = "header",
      tags$h2("WNBA Play-by-Play — Shooting Dashboard"),
      tags$p("2025 season  |  Use filters below to drill into any team or game")
  ),
  
  # filters
  div(class = "filter-panel",
      tags$h4("Filters"),
      fluidRow(
        column(2,
               selectInput("sel_team", "Team A",
                           choices  = c("All teams" = "ALL",
                                        setNames(team_lookup$shooting_team, team_lookup$label)),
                           selected = "ALL")
        ),
        column(3,
               selectInput("sel_team_b", "Team B",
                           choices  = c("None" = "NONE",
                                        setNames(team_lookup$shooting_team, team_lookup$label)),
                           selected = "NONE"),
               div(style="font-size:11px;color:#999;margin-top:-6px;line-height:1.4;",
                   "For shot chart comparison only. Leave blank for single-team stats.")
        ),
        column(3,
               selectInput("sel_game", "Game",
                           choices  = c("All games" = "ALL"),
                           selected = "ALL")
        ),
        column(2,
               selectInput("sel_result", "Shot result",
                           choices  = c("All results" = "ALL", "made", "missed", "blocked"),
                           selected = "ALL")
        ),
        column(2,
               br(),
               actionButton("btn_reset", "Reset filters",
                            style = "width:100%;background:#1a1a2e;color:white;
                              border:none;border-radius:6px;padding:7px;
                              font-size:13px;cursor:pointer;")
        )
      )
  ),
  
  # summary metrics — 2 rows of 4 boxes (4×3 = 12 cols each row)
  fluidRow(
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("n_shots")),
                  div(class="stat-lbl", "Shot attempts")
    )),
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("n_made")),
                  div(class="stat-lbl", "Made")
    )),
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("fg_pct")),
                  div(class="stat-lbl", "FG%"),
                  div(class="stat-formula", "FGM ÷ FGA")
    )),
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("efg_pct")),
                  div(class="stat-lbl", "eFG%"),
                  div(class="stat-formula", "(FGM + 0.5 × 3PM) ÷ FGA")
    ))
  ),
  fluidRow(
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("pct_3pt")),
                  div(class="stat-lbl", "3PT rate"),
                  div(class="stat-formula", "3PA ÷ FGA")
    )),
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("fg3_pct")),
                  div(class="stat-lbl", "3PT%"),
                  div(class="stat-formula", "3PM ÷ 3PA")
    )),
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("n_games")),
                  div(class="stat-lbl", "Games")
    )),
    column(3, div(class="stat-box",
                  div(class="stat-val", textOutput("n_blocked")),
                  div(class="stat-lbl", "Blocked"),
                  div(class="stat-formula", "shots blocked by defense")
    ))
  ),
  
  # info note when game selected without team
  conditionalPanel(
    condition = "input.sel_game != 'ALL' && input.sel_team == 'ALL'",
    div(style = "background:#fff8e1; border-left:4px solid #f0b429; border-radius:6px;
                 padding:10px 16px; margin:12px 0; font-size:12px; color:#7a5c00;",
        tags$b("\u2139\ufe0f Note: "),
        "No team filter is active — stats above and charts below reflect ",
        tags$b("both teams combined"),
        " for this game. To see one team's stats only, also select a team from the Team filter."
    )
  ),
  
  br(),
  
  # charts row 1
  fluidRow(
    column(4, div(class="plot-card", plotOutput("plot_result",  height="260px"))),
    column(4, div(class="plot-card", plotOutput("plot_period",  height="260px"))),
    column(4, div(class="plot-card", plotOutput("plot_2v3",     height="260px")))
  ),
  
  # charts row 2
  fluidRow(
    column(6, div(class="plot-card", plotOutput("plot_distance", height="260px"))),
    column(6, div(class="plot-card", plotOutput("plot_shottype", height="260px")))
  ),
  
  # shot chart + FG%/eFG% side by side
  fluidRow(
    column(7, div(class="plot-card",
                  div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
                      tags$b("Shot chart", style="font-size:13px;"),
                      radioButtons("shot_chart_type", NULL,
                                   choices  = c("Made vs Missed" = "result",
                                                "2PT / 3PT zone"  = "zone",
                                                "Shot frequency map" = "density"),
                                   selected = "result", inline = TRUE)
                  ),
                  plotOutput("plot_shotchart", height = "420px")
    )),
    column(5, div(class="plot-card",
                  plotOutput("plot_teamfg", height="460px")
    ))
  )
)

# ── server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # update game choices when team changes
  observeEvent(input$sel_team, {
    gl <- if (input$sel_team == "ALL") {
      game_lookup
    } else {
      game_lookup |> filter(home_team_abbrev == input$sel_team |
                              away_team_abbrev == input$sel_team)
    }
    updateSelectInput(session, "sel_game",
                      choices  = c("All games" = "ALL", setNames(gl$game_id, gl$label)),
                      selected = "ALL")
  })
  
  # reset all filters
  observeEvent(input$btn_reset, {
    updateSelectInput(session, "sel_team",   selected = "ALL")
    updateSelectInput(session, "sel_team_b", selected = "NONE")
    updateSelectInput(session, "sel_game",   selected = "ALL")
    updateSelectInput(session, "sel_result", selected = "ALL")
  })
  
  # reactive filtered data
  filtered <- reactive({
    d <- shots_all
    if (input$sel_team   != "ALL") d <- d |> filter(shooting_team == input$sel_team)
    if (input$sel_game   != "ALL") d <- d |> filter(game_id       == input$sel_game)
    if (input$sel_result != "ALL") d <- d |> filter(shot_result   == input$sel_result)
    d
  })
  
  # chart subtitle
  sub_txt <- reactive({
    parts <- character(0)
    if (input$sel_team != "ALL") parts <- c(parts, input$sel_team)
    if (input$sel_game != "ALL") {
      gl <- game_lookup |> filter(game_id == input$sel_game)
      if (nrow(gl)) parts <- c(parts, gl$label[1])
    }
    if (!length(parts)) "All teams  |  All games" else paste(parts, collapse = "  |  ")
  })
  
  # ── metric outputs ──
  output$n_shots <- renderText({ comma(nrow(filtered())) })
  output$n_made  <- renderText({ comma(sum(filtered()$shot_result == "made")) })
  
  output$fg_pct <- renderText({
    d <- filtered(); if (nrow(d) == 0) return("—")
    percent(sum(d$shot_result == "made") / nrow(d), accuracy = 0.1)
  })
  
  output$efg_pct <- renderText({
    d <- filtered(); if (nrow(d) == 0) return("—")
    fgm  <- sum(d$shot_result == "made")
    fg3m <- sum(d$shot_result == "made" & d$is_three_point == TRUE)
    fga  <- nrow(d)
    percent((fgm + 0.5 * fg3m) / fga, accuracy = 0.1)
  })
  
  output$pct_3pt <- renderText({
    d <- filtered(); if (nrow(d) == 0) return("—")
    percent(sum(d$is_three_point == TRUE) / nrow(d), accuracy = 0.1)
  })
  
  output$fg3_pct <- renderText({
    d <- filtered() |> filter(is_three_point == TRUE)
    if (nrow(d) == 0) return("—")
    percent(sum(d$shot_result == "made") / nrow(d), accuracy = 0.1)
  })
  
  output$n_games <- renderText({ comma(n_distinct(filtered()$game_id)) })
  output$n_blocked <- renderText({ comma(sum(filtered()$shot_result == "blocked")) })
  
  # ── plot: shot result donut ──
  output$plot_result <- renderPlot({
    d <- filtered(); if (nrow(d) == 0) return(NULL)
    rc <- d |> count(shot_result) |> mutate(pct = n / sum(n))
    ggplot(rc, aes(x = 2, y = n, fill = shot_result)) +
      geom_col(width = 1, color = "white", linewidth = .6) +
      geom_text(aes(label = paste0(shot_result, "\n", percent(pct, accuracy = .1))),
                position = position_stack(vjust = .5),
                size = 3.2, color = "white", fontface = "bold") +
      coord_polar(theta = "y") + xlim(.5, 2.5) +
      scale_fill_manual(values = pal_result) +
      labs(title = "Shot outcomes", subtitle = sub_txt()) +
      theme_void() +
      theme(plot.title    = element_text(size = 12, face = "bold", hjust = .5),
            plot.subtitle = element_text(size = 8, color = "grey50", hjust = .5),
            legend.position = "none",
            plot.background = element_rect(fill = "white", color = NA))
  })
  
  # ── plot: shots by period ──
  output$plot_period <- renderPlot({
    d <- filtered(); if (nrow(d) == 0) return(NULL)
    pc <- d |>
      filter(!is.na(period_number)) |>
      mutate(period_label = case_when(
        period_number <= 4 ~ paste0("Q", period_number),
        TRUE               ~ paste0("OT", period_number - 4)
      )) |>
      count(period_label) |>
      mutate(period_label = factor(period_label,
                                   levels = c("Q1","Q2","Q3","Q4","OT1","OT2","OT3")),
             is_ot = grepl("OT", period_label))
    ggplot(pc, aes(x = period_label, y = n, fill = is_ot)) +
      geom_col(width = .65, show.legend = FALSE) +
      geom_text(aes(label = comma(n)), vjust = -.4, size = 3, color = "grey30") +
      scale_fill_manual(values = c("FALSE" = "#7F77DD", "TRUE" = "#D85A30")) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .18))) +
      labs(title = "Shots by period", subtitle = sub_txt(), x = NULL, y = "FGA") +
      theme_wnba()
  })
  
  # ── plot: 2PT vs 3PT ──
  output$plot_2v3 <- renderPlot({
    d <- filtered(); if (nrow(d) == 0) return(NULL)
    tt <- d |>
      mutate(zone = ifelse(is_three_point == TRUE, "3PT", "2PT")) |>
      count(zone, shot_result) |>
      group_by(zone) |> mutate(pct = n / sum(n))
    ggplot(tt, aes(x = zone, y = n, fill = shot_result)) +
      geom_col(width = .55) +
      geom_text(aes(label = percent(pct, accuracy = .1)),
                position = position_stack(vjust = .5),
                size = 3, color = "white", fontface = "bold") +
      scale_fill_manual(values = pal_result, name = NULL) +
      scale_y_continuous(labels = comma) +
      labs(title = "2PT vs 3PT", subtitle = sub_txt(), x = NULL, y = "FGA") +
      theme_wnba() + theme(legend.position = "right")
  })
  
  # ── plot: shot distance ──
  output$plot_distance <- renderPlot({
    d <- filtered() |> filter(!is.na(shot_distance_ft), shot_distance_ft <= 60)
    if (nrow(d) < 5) return(NULL)
    ggplot(d, aes(x = shot_distance_ft, fill = shot_result)) +
      geom_histogram(binwidth = 2, color = "white", linewidth = .2) +
      geom_vline(xintercept = 22, linetype = "dashed", color = "grey40", linewidth = .6) +
      annotate("text", x = 23, y = Inf, label = "3PT line",
               vjust = 1.5, hjust = 0, size = 2.8, color = "grey40") +
      scale_fill_manual(values = pal_result, name = NULL) +
      scale_x_continuous(breaks = seq(0, 60, 10)) +
      scale_y_continuous(labels = comma) +
      labs(title = "Shot distance distribution", subtitle = sub_txt(),
           x = "Distance (ft)", y = "Count") +
      theme_wnba() + theme(legend.position = "right")
  })
  
  # ── plot: top shot types ──
  output$plot_shottype <- renderPlot({
    d <- filtered(); if (nrow(d) == 0) return(NULL)
    top <- d |>
      filter(!is.na(shot_type), shot_type != "") |>
      count(shot_type, sort = TRUE) |>
      slice_head(n = 10) |>
      mutate(shot_type = reorder(shot_type, n))
    ggplot(top, aes(x = n, y = shot_type)) +
      geom_col(fill = "#D85A30", width = .65) +
      geom_text(aes(label = comma(n)), hjust = -.1, size = 3, color = "grey30") +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .18))) +
      labs(title = "Top 10 shot types", subtitle = sub_txt(), x = "Count", y = NULL) +
      theme_wnba()
  })
  
  
  # ── helper: draw half-court lines ──────────────────────────────────────────────────────────────────
  draw_court <- function() {
    bx <- 41.75
    list(
      annotate("rect", xmin=0, xmax=46.75, ymin=-25, ymax=25, fill=NA, color="grey30", linewidth=.6),
      annotate("rect", xmin=28, xmax=46.75, ymin=-8,  ymax=8,  fill="#f5f0e8", color="grey40", linewidth=.5),
      annotate("path", x=28+6*cos(seq(pi/2,3*pi/2,length=80)), y=6*sin(seq(pi/2,3*pi/2,length=80)), color="grey40", linewidth=.5),
      annotate("point", x=bx, y=0, size=3, color="grey20"),
      annotate("segment", x=46.75, xend=46.75, y=-3, yend=3, color="grey20", linewidth=1.2),
      annotate("path", x=bx+4*cos(seq(pi,0,length=60)), y=4*sin(seq(pi,0,length=60)), color="grey40", linewidth=.5),
      annotate("path", x=bx+23.75*cos(seq(pi*0.54,pi,length=120)), y=23.75*sin(seq(pi*0.54,pi,length=120)), color="grey40", linewidth=.6),
      annotate("segment", x=43.75, xend=43.75, y= 22, yend= 25, color="grey40", linewidth=.6),
      annotate("segment", x=43.75, xend=43.75, y=-22, yend=-25, color="grey40", linewidth=.6),
      annotate("segment", x=0, xend=0, y=-25, yend=25, color="grey40", linewidth=.5, linetype="dashed")
    )
  }
  
  # ── helper: build one shot chart panel ──
  make_shot_panel <- function(d, title_suffix, type, cl, btheme) {
    d <- d |> mutate(
      hx = ifelse(coordinate_x >= 0,  coordinate_x, -coordinate_x),
      hy = ifelse(coordinate_x >= 0,  coordinate_y, -coordinate_y)
    )
    if (type == "result") {
      ggplot(d, aes(x=hx, y=hy, color=shot_result)) +
        cl + geom_point(alpha=.55, size=1.4) +
        scale_color_manual(values=pal_result, name="Result") +
        coord_fixed(xlim=c(-1,48), ylim=c(-26,26)) +
        labs(title=paste0("Made vs Missed — ", title_suffix)) + btheme +
        guides(color=guide_legend(override.aes=list(size=3,alpha=1)))
      
    } else if (type == "zone") {
      d <- d |> mutate(zone=ifelse(is_three_point==TRUE,"3PT","2PT"))
      ggplot(d, aes(x=hx, y=hy, color=zone)) +
        cl + geom_point(alpha=.45, size=1.4) +
        scale_color_manual(values=c("2PT"="#378ADD","3PT"="#D85A30"), name="Zone") +
        coord_fixed(xlim=c(-1,48), ylim=c(-26,26)) +
        labs(title=paste0("2PT vs 3PT — ", title_suffix)) + btheme +
        guides(color=guide_legend(override.aes=list(size=3,alpha=1)))
      
    } else {
      # bin2d: count shots per 2x2 ft zone — works correctly at any sample size
      ggplot(d, aes(x=hx, y=hy)) +
        geom_bin2d(binwidth=c(2,2)) +
        cl +
        scale_fill_gradientn(
          colors = c("#f5f0e8","#ffffcc","#fed976","#fd8d3c","#e31a1c","#800026"),
          name   = "Shot
count"
        ) +
        coord_fixed(xlim=c(-1,48), ylim=c(-26,26)) +
        labs(title=paste0("Shot frequency — ", title_suffix),
             subtitle="Each cell = 2x2 ft zone, color = shot count") +
        btheme
    }
  }
  
  # ── plot: shot chart ──
  output$plot_shotchart <- renderPlot({
    cl <- draw_court()
    btheme <- theme_void() +
      theme(plot.title=element_text(size=11,face="bold",margin=margin(b=4)),
            plot.subtitle=element_text(size=8,color="grey50",margin=margin(b=6)),
            legend.position="bottom", legend.text=element_text(size=8),
            legend.title=element_text(size=8,face="bold"),
            plot.background=element_rect(fill="white",color=NA),
            panel.background=element_rect(fill="#fafaf8",color=NA))
    
    type <- input$shot_chart_type
    
    # comparison mode: Team B selected
    if (input$sel_team_b != "NONE") {
      team_a <- if (input$sel_team == "ALL") "All teams" else input$sel_team
      team_b <- input$sel_team_b
      
      da <- shots_all
      if (input$sel_team != "ALL") da <- da |> filter(shooting_team == input$sel_team)
      if (input$sel_game   != "ALL") da <- da |> filter(game_id     == input$sel_game)
      if (input$sel_result != "ALL") da <- da |> filter(shot_result == input$sel_result)
      
      db <- shots_all |> filter(shooting_team == team_b)
      if (input$sel_game   != "ALL") db <- db |> filter(game_id     == input$sel_game)
      if (input$sel_result != "ALL") db <- db |> filter(shot_result == input$sel_result)
      
      if (nrow(da) == 0 && nrow(db) == 0) return(NULL)
      
      pa <- if (nrow(da) > 0) make_shot_panel(da, team_a, type, cl, btheme) else NULL
      pb <- if (nrow(db) > 0) make_shot_panel(db, team_b, type, cl, btheme) else NULL
      
      if (!is.null(pa) && !is.null(pb)) {
        gridExtra::grid.arrange(pa, pb, ncol=2)
      } else {
        pa %||% pb
      }
      
      # single mode
    } else {
      d <- filtered()
      if (nrow(d) == 0) return(NULL)
      make_shot_panel(d, sub_txt(), type, cl, btheme)
    }
  })
  
  # ── plot: FG% & eFG% — per-game line when team selected, cross-team bar otherwise ──
  output$plot_teamfg <- renderPlot({
    
    # SINGLE TEAM: line chart of FG% & eFG% across games
    if (input$sel_team != "ALL") {
      d <- shots_all |> filter(shooting_team == input$sel_team)
      if (input$sel_result != "ALL") d <- d |> filter(shot_result == input$sel_result)
      if (nrow(d) == 0) return(NULL)
      
      gd <- d |>
        group_by(game_id) |>
        summarise(
          fga       = n(),
          fgm       = sum(shot_result == "made"),
          fg3m      = sum(shot_result == "made" & is_three_point == TRUE),
          game_date = first(game_date),
          .groups   = "drop"
        ) |>
        filter(fga >= 5) |>
        left_join(game_lookup |> select(game_id, home_team_abbrev, away_team_abbrev),
                  by = "game_id") |>
        mutate(
          opponent = ifelse(home_team_abbrev == input$sel_team,
                            away_team_abbrev, home_team_abbrev),
          game_lbl = paste0(format(as.Date(game_date), "%b %d"), " vs ", opponent),
          fg_pct   = fgm / fga,
          efg_pct  = (fgm + 0.5 * fg3m) / fga,
          game_lbl = reorder(game_lbl, as.Date(game_date))
        ) |>
        pivot_longer(cols = c(fg_pct, efg_pct),
                     names_to = "metric", values_to = "value") |>
        mutate(metric = ifelse(metric == "fg_pct", "FG%", "eFG%"))
      
      season_avg <- d |>
        summarise(
          "FG%"  = sum(shot_result == "made") / n(),
          "eFG%" = (sum(shot_result == "made") +
                      0.5 * sum(shot_result == "made" & is_three_point == TRUE)) / n()
        ) |>
        pivot_longer(everything(), names_to = "metric", values_to = "avg")
      
      ggplot(gd, aes(x = game_lbl, y = value, color = metric, group = metric)) +
        geom_hline(data = season_avg,
                   aes(yintercept = avg, color = metric),
                   linetype = "dashed", linewidth = .7, alpha = .6) +
        geom_line(linewidth = .9, alpha = .8) +
        geom_point(size = 2.5) +
        scale_color_manual(values = c("FG%" = "#378ADD", "eFG%" = "#1D9E75"), name = NULL) +
        scale_y_continuous(labels = percent, limits = c(0, NA)) +
        labs(
          title    = paste0(input$sel_team, " — FG% & eFG% per game (2025 season)"),
          subtitle = "Dashed lines = season averages  |  FG% = FGM÷FGA  |  eFG% = (FGM + 0.5×3PM)÷FGA",
          x = NULL, y = NULL
        ) +
        theme_wnba() +
        theme(axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
              legend.position = "top")
      
      # ALL TEAMS: cross-team comparison bar chart
    } else {
      d <- shots_all
      if (input$sel_game   != "ALL") d <- d |> filter(game_id     == input$sel_game)
      if (input$sel_result != "ALL") d <- d |> filter(shot_result == input$sel_result)
      
      tfg <- d |>
        group_by(shooting_team) |>
        summarise(
          fga  = n(),
          fgm  = sum(shot_result == "made"),
          fg3m = sum(shot_result == "made" & is_three_point == TRUE),
          .groups = "drop"
        ) |>
        filter(fga >= 50) |>
        mutate(
          fg_pct  = fgm / fga,
          efg_pct = (fgm + 0.5 * fg3m) / fga
        ) |>
        pivot_longer(cols = c(fg_pct, efg_pct),
                     names_to = "metric", values_to = "value") |>
        mutate(
          metric        = ifelse(metric == "fg_pct", "FG%", "eFG%"),
          shooting_team = reorder(shooting_team, value, mean)
        )
      
      if (nrow(tfg) == 0) return(NULL)
      
      league_avg <- tfg |>
        group_by(metric) |>
        summarise(avg = mean(value), .groups = "drop")
      
      ggplot(tfg, aes(x = value, y = shooting_team, fill = metric)) +
        geom_col(position = position_dodge(width = .7), width = .6) +
        geom_vline(data = league_avg,
                   aes(xintercept = avg, color = metric),
                   linetype = "dashed", linewidth = .7, show.legend = FALSE) +
        geom_text(aes(label = percent(value, accuracy = .1)),
                  position = position_dodge(width = .7),
                  hjust = -.1, size = 2.8, color = "grey30") +
        scale_fill_manual(values  = c("FG%" = "#378ADD", "eFG%" = "#1D9E75"), name = NULL) +
        scale_color_manual(values = c("FG%" = "#2255aa", "eFG%" = "#0d6b50")) +
        scale_x_continuous(labels = percent, expand = expansion(mult = c(0, .14))) +
        labs(
          title    = "FG% vs eFG% by team",
          subtitle = "FG% = FGM÷FGA   |   eFG% = (FGM + 0.5×3PM)÷FGA   |   Dashed = league avg",
          x = NULL, y = NULL
        ) +
        theme_wnba() +
        theme(legend.position = "top")
    }
  })
}

shinyApp(ui, server)