# ============================================================
# NCAA MEN'S LACROSSE — SEASON PLAY-BY-PLAY SCRAPER
#
# Reads manually saved HTML files from stats.ncaa.org and
# produces two season-level CSVs:
#
#   {TEAM}_{SEASON_ID}_Season_PBP.csv
#   {TEAM}_{SEASON_ID}_Season_Possessions.csv
#
# HOW TO USE
#
# 1. Open your team's schedule page on stats.ncaa.org,
#    wait for the table to load, and save the page as
#    "Webpage, Complete" (File > Save Page As in Chrome).
#
# 2. For each game, open:
#      https://stats.ncaa.org/contests/{game_id}/play_by_play
#    Wait for all four quarter tables to load, then save
#    the page as "{game_id}.html" into PBP_HTML_FOLDER.
#
# 3. Fill in the constants below and source this file.
# ============================================================


# ============================================================
# PACKAGES
# ============================================================

packages <- c(
  "rvest",
  "xml2",
  "dplyr",
  "stringr",
  "purrr",
  "tidyr",
  "lubridate"
)

new_packages <-
  packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)


# ============================================================
# CONSTANTS — edit these for each team / season
# ============================================================

PBP_HTML_FOLDER <-
  "~/Downloads/Python, R, and CSV/SAIL:Sports Analytics/NCAALax/OSU2526"

SCHEDULE_HTML_FILE <-
  file.path(PBP_HTML_FOLDER, "OSU2526NCAAStatistics.html")

TEAM_NAME <- "Ohio St."

TEAM_NCAA_CODE <- "OHIOST"

TEAM_SEASON_ID <- 612251

REQUEST_PAUSE_SECONDS <- 0

TEAM_FILE_NAME <-
  TEAM_NAME |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  str_remove("^_|_$")


# ============================================================
# HTML READER
# ============================================================

read_game_html <- function(game_id) {
  
  path <-
    file.path(
      PBP_HTML_FOLDER,
      paste0(game_id, ".html")
    )
  
  if (!file.exists(path)) {
    message("  HTML file not found: ", path)
    return(NULL)
  }
  
  cat("  Reading:", path, "\n")
  read_html(path)
}


# ============================================================
# B. SCHEDULE SCRAPER
# ============================================================

scrape_schedule <- function(html_file) {
  
  cat("Reading schedule from:\n ", html_file, "\n\n")
  
  page <- read_html(html_file)
  
  rows <-
    map_dfr(
      html_elements(page, "tr"),
      function(row) {
        
        link <-
          row |>
          html_element("a[href*='/contests/'][href*='box_score']") |>
          html_attr("href")
        
        if (is.na(link)) return(NULL)
        
        game_id <- str_extract(link, "(?<=/contests/)[0-9]+")
        
        cells <- html_elements(row, "td")
        
        game_date <-
          if (length(cells) >= 1) {
            html_text2(cells[[1]]) |> str_squish()
          } else {
            NA_character_
          }
        
        opponent <-
          if (length(cells) >= 2) {
            html_text2(cells[[2]]) |>
              str_squish() |>
              str_remove("^(@\\s*|vs\\.?\\s*|#[0-9]+\\s*)+") |>  # strip @, vs., rankings
              str_remove("\\s*@.*$") |>                            # strip trailing @ location
              str_remove("\\s*\\(.*\\)$") |>                       # strip trailing (W/L)
              str_remove("\\s+[0-9]{4}\\b.*$") |>                  # strip year + conf champ tag
              str_squish()
          } else {
            NA_character_
          }
        
        tibble(game_id = game_id, date = game_date, opponent = opponent)
      }
    )
  
  if (nrow(rows) == 0 || !"game_id" %in% names(rows)) {
    warning("No play-by-play links found in schedule HTML.")
    return(tibble(game_id = character(), date = character(), opponent = character()))
  }
  
  rows |>
    filter(!is.na(.data$game_id)) |>
    distinct(.data$game_id, .keep_all = TRUE)
}


# ============================================================
# C. SINGLE-GAME PARSER
# ============================================================

parse_game <- function(game_id, opponent, date) {
  
  tryCatch({
    
    cat("Parsing game", game_id, "|", TEAM_NAME, "vs", opponent, "|", date, "\n")
    
    page <- read_game_html(game_id)
    if (is.null(page)) return(NULL)
    
    
    # ----------------------------------------------------------
    # STEP 1 — locate tables
    # ----------------------------------------------------------
    
    tables <- html_elements(page, "table")
    cat("  Tables found:", length(tables), "\n")
    
    pbp_tables <- tables[
      map_lgl(tables, function(tbl) {
        h <- html_elements(tbl, "th") |> html_text2() |> str_squish()
        all(c("Time", "Score") %in% h) && length(h) == 4
      })
    ]
    
    cat("  PBP tables:", length(pbp_tables), "\n\n")
    
    if (length(pbp_tables) < 4) {
      message("  Skipping: only ", length(pbp_tables), " PBP tables found")
      return(NULL)
    }
    
    
    # ----------------------------------------------------------
    # STEP 2 — detect team names from the first table header
    # ----------------------------------------------------------
    
    header_names <-
      html_elements(pbp_tables[[1]], "th") |>
      html_text2() |>
      str_squish()
    
    team_cols <- header_names[!header_names %in% c("Time", "Score", "")]
    
    away_team <- team_cols[1]
    home_team <- team_cols[2]
    
    if (str_to_lower(away_team) == str_to_lower(TEAM_NAME)) {
      target_team   <- away_team
      opponent_team <- home_team
    } else if (str_to_lower(home_team) == str_to_lower(TEAM_NAME)) {
      target_team   <- home_team
      opponent_team <- away_team
    } else {
      stop("TEAM_NAME not found in headers: ", away_team, " vs ", home_team)
    }
    
    away_col <- str_to_lower(str_replace_all(away_team, "\\s+", "_"))
    home_col <- str_to_lower(str_replace_all(home_team, "\\s+", "_"))
    
    cat("  Teams:", away_team, "vs", home_team, "\n")
    
    
    # ----------------------------------------------------------
    # Helper — return the opposing team
    # ----------------------------------------------------------
    
    get_opponent <- function(team) {
      if (is.na(team))        return(NA_character_)
      if (team == away_team)  return(home_team)
      if (team == home_team)  return(away_team)
      NA_character_
    }
    
    
    # ----------------------------------------------------------
    # STEP 3 — extract period labels from card headers
    # ----------------------------------------------------------
    
    get_quarter_name <- function(tbl, n) {
      
      card <-
        xml_find_first(
          tbl,
          "ancestor::div[
            contains(concat(' ', normalize-space(@class), ' '), ' card ')
          ][1]"
        )
      
      header <-
        xml_find_first(
          card,
          ".//div[
            contains(concat(' ', normalize-space(@class), ' '), ' card-header ')
          ]"
        )
      
      q <- html_text2(header) |> str_squish()
      
      if (is.na(q) || q == "") {
        q <- c(
          "1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter",
          "1st OT", "2nd OT", "3rd OT"
        )[n]
      }
      
      q
    }
    
    quarter_names <-
      map2_chr(pbp_tables, seq_along(pbp_tables), get_quarter_name)
    
    cat("  Quarters:", paste(quarter_names, collapse = " | "), "\n\n")
    
    
    # ----------------------------------------------------------
    # STEP 4 — parse each period into a data frame
    # ----------------------------------------------------------
    
    pbp_list <-
      map2(
        pbp_tables,
        quarter_names,
        function(tbl, quarter_name) {
          
          df <- html_table(tbl, header = TRUE, convert = FALSE)
          names(df) <- c("time", away_col, "score", home_col)
          df <- mutate(df, across(everything(), ~str_squish(as.character(.x))))
          
          df <- filter(df, is.na(time) | !str_detect(time, regex("^End of", ignore_case = TRUE)))
          
          footer      <- xml_find_first(tbl, ".//tfoot")
          footer_text <- html_text2(footer) |> str_squish()
          
          is_regulation <- quarter_name %in%
            c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter")
          
          is_overtime <- str_detect(quarter_name, regex("OT$", ignore_case = TRUE))
          
          last_time <- tail(df$time[df$time != "" & !is.na(df$time)], 1)
          
          last_away <- tail(df[[away_col]][df[[away_col]] != "" & !is.na(df[[away_col]])], 1)
          last_home <- tail(df[[home_col]][df[[home_col]] != "" & !is.na(df[[home_col]])], 1)
          if (length(last_away) == 0) last_away <- ""
          if (length(last_home) == 0) last_home <- ""
          last_event_text <- paste(last_away, last_home)
          
          footer_period_end <-
            !is.na(footer_text) &&
            str_detect(
              footer_text,
              regex(
                "End of (1st|2nd|3rd|4th) Quarter|End of (1st|2nd|3rd) OT",
                ignore_case = TRUE
              )
            )
          
          final_was_goal <-
            str_detect(last_event_text, regex("^GOAL", ignore_case = TRUE))
          
          add_period_end <-
            is_regulation ||
            (is_overtime && !final_was_goal &&
               (footer_period_end || identical(last_time, "00:00")))
          
          if (add_period_end) {
            
            last_score <-
              df$score[
                which(!is.na(df$score) & df$score != "")[
                  max(which(!is.na(df$score) & df$score != ""))
                ]
              ]
            
            end_row <- tibble(
              time             = "00:00",
              !!away_col       := "",
              score            = last_score,
              !!home_col       := "",
              quarter_end      = TRUE,
              quarter_end_text =
                if (is_overtime) paste0("End of ", quarter_name)
              else footer_text
            )
            
          } else {
            
            end_row <- tibble(
              time             = character(),
              !!away_col       := character(),
              score            = character(),
              !!home_col       := character(),
              quarter_end      = logical(),
              quarter_end_text = character()
            )
          }
          
          df <-
            df |>
            mutate(quarter_end = FALSE, quarter_end_text = NA_character_) |>
            bind_rows(end_row) |>
            mutate(quarter = quarter_name)
          
          df
        }
      )
    
    
    # ----------------------------------------------------------
    # STEP 5 — combine periods and build event text / team
    # ----------------------------------------------------------
    
    pbp <-
      bind_rows(pbp_list) |>
      mutate(event_id = row_number())
    
    pbp <-
      pbp |>
      mutate(
        event_text = case_when(
          quarter_end == TRUE ~
            quarter_end_text,
          .data[[away_col]] != "" & .data[[home_col]] == "" ~
            .data[[away_col]],
          .data[[home_col]] != "" & .data[[away_col]] == "" ~
            .data[[home_col]],
          .data[[away_col]] != "" & .data[[home_col]] != "" ~
            paste(.data[[away_col]], .data[[home_col]]),
          TRUE ~ NA_character_
        ),
        team = case_when(
          quarter_end == TRUE ~
            NA_character_,
          .data[[away_col]] != "" & .data[[home_col]] == "" ~
            away_team,
          .data[[home_col]] != "" & .data[[away_col]] == "" ~
            home_team,
          TRUE ~ NA_character_
        )
      )
    
    
    # ----------------------------------------------------------
    # STEP 6 — classify events
    # ----------------------------------------------------------
    
    pbp <-
      pbp |>
      mutate(
        event_type = case_when(
          quarter_end == TRUE ~
            "End of Quarter",
          str_detect(event_text, regex("^GOAL",                          ignore_case = TRUE)) ~ "Goal",
          str_detect(event_text, regex("^Shot",                          ignore_case = TRUE)) ~ "Shot",
          str_detect(event_text, regex("^Faceoff",                       ignore_case = TRUE)) ~ "Faceoff",
          str_detect(event_text, regex("^Ground ball",                   ignore_case = TRUE)) ~ "Ground Ball",
          str_detect(event_text, regex("^Turnover",                      ignore_case = TRUE)) ~ "Turnover",
          str_detect(event_text, regex("clock violation|stall warning",  ignore_case = TRUE)) ~ "Turnover",
          str_detect(event_text, regex("^Penalty",                       ignore_case = TRUE)) ~ "Penalty",
          str_detect(event_text, regex("^Clear attempt",                 ignore_case = TRUE)) ~ "Clear",
          str_detect(event_text, regex("^Timeout",                       ignore_case = TRUE)) ~ "Timeout",
          TRUE ~ "Other"
        ),
        # On Cage = saved, blocked, or hit post; Off Target = wide, high, or other miss
        shot_type = case_when(
          event_type != "Shot" ~ NA_character_,
          str_detect(event_text, regex("SAVE|BLOCKED|HIT POST", ignore_case = TRUE)) ~ "On Cage",
          str_detect(event_text, regex("WIDE|HIGH",             ignore_case = TRUE)) ~ "Off Target",
          TRUE ~ "Off Target"
        )
      )
    
    
    # ----------------------------------------------------------
    # STEP 7 — faceoff winner
    # ----------------------------------------------------------
    
    identify_faceoff_winner <- function(event_text, target_team, opponent_team, ncaa_code) {
      
      winner_text <-
        str_match(event_text, regex("won by\\s+([A-Za-z0-9]+)", ignore_case = TRUE))[, 2]
      
      if (is.na(winner_text)) return(NA_character_)
      
      winner_code <- winner_text |> str_to_upper() |> str_remove_all("[^A-Z0-9]")
      ncaa_code   <- ncaa_code   |> str_to_upper() |> str_remove_all("[^A-Z0-9]")
      
      if (winner_code == ncaa_code) return(target_team)
      return(opponent_team)
    }
    
    pbp <-
      pbp |>
      mutate(
        faceoff_winner = map_chr(
          event_text,
          ~identify_faceoff_winner(.x, target_team, opponent_team, TEAM_NCAA_CODE)
        )
      )
    
    
    # ----------------------------------------------------------
    # STEP 8 — time conversions and elapsed seconds
    # ----------------------------------------------------------
    
    pbp <-
      pbp |>
      mutate(
        minutes        = as.numeric(str_extract(time, "^[0-9]+")),
        seconds        = as.numeric(str_extract(time, "[0-9]+$")),
        time_remaining = minutes * 60 + seconds,
        quarter_number = case_when(
          quarter == "1st Quarter" ~ 1,
          quarter == "2nd Quarter" ~ 2,
          quarter == "3rd Quarter" ~ 3,
          quarter == "4th Quarter" ~ 4,
          quarter == "1st OT"      ~ 5,
          quarter == "2nd OT"      ~ 6,
          quarter == "3rd OT"      ~ 7,
          TRUE ~ NA_real_
        ),
        elapsed_game_seconds = case_when(
          quarter_number <= 4 ~
            (quarter_number - 1) * 900 + (900 - time_remaining),
          quarter_number >= 5 ~
            3600 + (quarter_number - 5) * 240 + (240 - time_remaining),
          TRUE ~ NA_real_
        )
      )
    
    
    # ----------------------------------------------------------
    # STEP 9 — fix untimed events
    #   Untimed Turnover rows:
    #     - next event is a Goal  -> last listed time (carry forward)
    #     - anything else         -> next listed time in the period
    #   All other untimed events -> last listed time (carry forward)
    # ----------------------------------------------------------
    
    # Look-ahead values for every row, within each period
    pbp <-
      pbp |>
      group_by(quarter) |>
      mutate(
        next_event = lead(event_type),
        .untimed   = is.na(time) | time == "",
        next_listed_time      = if_else(.untimed, NA_character_, time),
        next_listed_elapsed   = if_else(.untimed, NA_real_, elapsed_game_seconds),
        next_listed_remaining = if_else(.untimed, NA_real_, time_remaining)
      ) |>
      fill(next_listed_time, next_listed_elapsed, next_listed_remaining,
           .direction = "up") |>
      ungroup() |>
      select(-.untimed)
    
    last_time_by_quarter      <- NA_character_
    last_elapsed_by_quarter   <- NA_real_
    last_remaining_by_quarter <- NA_real_
    
    pbp$time_imputed         <- FALSE
    pbp$imputed_method       <- NA_character_
    pbp$time_carried_forward <- NA_character_   # what the old logic would have used
    
    for (i in seq_len(nrow(pbp))) {
      
      q <- pbp$quarter[i]
      
      if (i == 1 || pbp$quarter[i - 1] != q) {
        last_time_by_quarter      <- NA_character_
        last_elapsed_by_quarter   <- NA_real_
        last_remaining_by_quarter <- NA_real_
      }
      
      was_untimed <- is.na(pbp$time[i]) || pbp$time[i] == ""
      
      if (was_untimed) {
        
        pbp$time_carried_forward[i] <- last_time_by_quarter
        
        use_next_listed <-
          pbp$event_type[i] == "Turnover" &&
          !is.na(pbp$next_event[i]) &&
          pbp$next_event[i] != "Goal" &&
          !is.na(pbp$next_listed_elapsed[i])
        
        if (use_next_listed) {
          pbp$time[i]                 <- pbp$next_listed_time[i]
          pbp$time_remaining[i]       <- pbp$next_listed_remaining[i]
          pbp$elapsed_game_seconds[i] <- pbp$next_listed_elapsed[i]
          pbp$imputed_method[i]       <- "next_listed_time"
        } else {
          pbp$time[i]                 <- last_time_by_quarter
          pbp$time_remaining[i]       <- last_remaining_by_quarter
          pbp$elapsed_game_seconds[i] <- last_elapsed_by_quarter
          pbp$imputed_method[i]       <- "carry_forward"
        }
        
        pbp$time_imputed[i] <- TRUE
      }
      
      # Only genuinely timed rows update the running "last listed" values,
      # so an imputed future time never leaks into later carry-forwards
      if (!was_untimed) {
        last_time_by_quarter      <- pbp$time[i]
        last_elapsed_by_quarter   <- pbp$elapsed_game_seconds[i]
        last_remaining_by_quarter <- pbp$time_remaining[i]
      }
    }
    
    pbp <- select(pbp, -next_event, -next_listed_time,
                  -next_listed_elapsed, -next_listed_remaining)
    
    
    # ----------------------------------------------------------
    # STEP 10 — possession tracking
    # ----------------------------------------------------------
    
    current_possession_id <- NA_integer_
    current_team          <- NA_character_
    next_possession_id    <- 1L
    loose_ball            <- FALSE   # TRUE after an off-target shot; cleared each iteration
    loose_ball_time     <- NA_character_
    loose_ball_elapsed  <- NA_real_
    
    possession_start_time     <- list()
    possession_start_elapsed  <- list()
    possession_start_reason   <- list()
    possession_team_list      <- list()
    possession_end_time       <- list()
    possession_end_elapsed    <- list()
    possession_end_reason     <- list()
    event_possession_id       <- rep(NA_integer_, nrow(pbp))
    
    
    open_possession <- function(id, team, time, elapsed, reason) {
      possession_start_time[[as.character(id)]]    <<- time
      possession_start_elapsed[[as.character(id)]] <<- elapsed
      possession_start_reason[[as.character(id)]]  <<- reason
      possession_team_list[[as.character(id)]]     <<- team
    }
    
    close_possession <- function(id, time, elapsed, reason) {
      possession_end_time[[as.character(id)]]    <<- time
      possession_end_elapsed[[as.character(id)]] <<- elapsed
      possession_end_reason[[as.character(id)]]  <<- reason
    }
    
    new_possession <- function(team, time, elapsed, reason) {
      id <- next_possession_id
      next_possession_id    <<- next_possession_id + 1L
      current_possession_id <<- id
      current_team          <<- team
      open_possession(id, team, time, elapsed, reason)
      id
    }
    
    
    for (i in seq_len(nrow(pbp))) {
      
      event         <- pbp$event_type[i]
      team          <- pbp$team[i]
      event_time    <- pbp$time[i]
      event_elapsed <- pbp$elapsed_game_seconds[i]
      
      
      # ---- Resolve loose ball after off-target shot ----
      # An off-target shot leaves the ball loose. The first event from the
      # opposing team confirms they recovered it — flip possession at that
      # event's timestamp. Reset the flag every iteration regardless.
      if (loose_ball &&
          !is.na(team) &&
          !is.na(current_team) &&
          team != current_team) {
        # Use the SHOT's timestamp, not the current event's — possession was
        # lost at the moment of the miss, not when the opponent first touched it
        close_possession(current_possession_id, loose_ball_time, loose_ball_elapsed, "Miss")
        new_possession(team, loose_ball_time, loose_ball_elapsed, "Miss")
      }
      loose_ball         <- FALSE
      loose_ball_time    <- NA_character_
      loose_ball_elapsed <- NA_real_
      
      
      # ---- Resolve deferred save ----
      # A save leaves the ball with the goalie's team. The next opposing-team
      # event confirms possession changed. If that event is a Penalty, close
      # at the penalty time (not the save time) and let the Penalty block open
      # the new man-up possession.
      if (
        i > 1 &&
        pbp$event_type[i - 1] == "Shot" &&
        str_detect(pbp$event_text[i - 1], regex("SAVE", ignore_case = TRUE)) &&
        !is.na(current_possession_id) &&
        !is.na(current_team) &&
        !is.na(team) &&
        team != current_team
      ) {
        save_time    <- pbp$time[i - 1]
        save_elapsed <- pbp$elapsed_game_seconds[i - 1]
        if (event != "Penalty") {
          close_possession(current_possession_id, save_time, save_elapsed, "Save")
          new_possession(team, save_time, save_elapsed, "Save")
        } else {
          # Penalty immediately follows save — close at penalty time so the
          # Penalty block below opens the correct man-up possession
          close_possession(current_possession_id, event_time, event_elapsed, "Save")
          current_possession_id <- NA_integer_
          current_team          <- NA_character_
        }
      }
      
      
      # ---- Faceoff ----
      # A faceoff always resets possession from scratch. Close any open
      # possession (handles faceoff violations where a second faceoff
      # immediately follows the first).
      if (event == "Faceoff" && !is.na(pbp$faceoff_winner[i])) {
        if (!is.na(current_possession_id)) {
          close_possession(current_possession_id, event_time, event_elapsed, "Faceoff")
        }
        new_possession(pbp$faceoff_winner[i], event_time, event_elapsed, "Faceoff")
        event_possession_id[i] <- current_possession_id
        next
      }
      
      
      # ---- Turnover ----
      # Includes explicit turnovers and clock/stall violations (classified
      # as Turnover in Step 6). Flips possession to the opposing team.
      if (event == "Turnover") {
        if (is.na(current_possession_id)) {
          new_possession(team, event_time, event_elapsed, "Turnover")
        }
        event_possession_id[i] <- current_possession_id
        close_possession(current_possession_id, event_time, event_elapsed, "Turnover")
        new_possession(get_opponent(team), event_time, event_elapsed, "Turnover")
        next
      }
      
      
      # ---- Goal ----
      # Scores for the shooting team; clears possession state entirely
      # (next possession opens on the subsequent faceoff).
      if (event == "Goal") {
        if (is.na(current_possession_id)) {
          new_possession(team, event_time, event_elapsed, "Goal")
        }
        event_possession_id[i] <- current_possession_id
        close_possession(current_possession_id, event_time, event_elapsed, "Goal")
        current_possession_id <- NA_integer_
        current_team          <- NA_character_
        next
      }
      
      
      # ---- Shot ----
      # Tags the shot to the current possession. For saved shots, possession
      # transfer is deferred to the next event (handled by the deferred-save
      # block above). For off-target shots, sets loose_ball = TRUE so the
      # NEXT opposing-team event triggers the possession flip.
      if (event == "Shot") {
        if (is.na(current_possession_id)) {
          new_possession(team, event_time, event_elapsed, "Shot")
        } else if (!is.na(current_team) && !is.na(team) && team != current_team) {
          close_possession(current_possession_id, event_time, event_elapsed, "Turnover")
          new_possession(team, event_time, event_elapsed, "Shot")
        }
        event_possession_id[i] <- current_possession_id
        if (!is.na(pbp$shot_type[i]) && pbp$shot_type[i] == "Off Target") {
          loose_ball         <- TRUE
          loose_ball_time    <- event_time     # stamp the miss, not the recovery
          loose_ball_elapsed <- event_elapsed
        }
        next
      }
      
      
      # ---- Ground Ball ----
      # If the picking-up team differs from the current possession team,
      # flip possession. This handles recoveries after off-target shots
      # when a ground ball event is logged.
      if (event == "Ground Ball") {
        if (is.na(current_possession_id)) {
          new_possession(team, event_time, event_elapsed, "Ground Ball")
        } else if (!is.na(current_team) && current_team != team) {
          close_possession(current_possession_id, event_time, event_elapsed, "Ground Ball")
          new_possession(team, event_time, event_elapsed, "Ground Ball")
        }
        event_possession_id[i] <- current_possession_id
        next
      }
      
      
      # ---- Penalty ----
      # team = the penalized team. Three cases:
      #   1. No open possession (e.g. right after a save) → non-penalized starts man-up
      #   2. Penalized team had possession (offensive foul) → flip to opponent
      #   3. Defensive penalty → close and reopen for same team as new man-up possession
      if (event == "Penalty" && !is.na(team)) {
        non_penalized <- get_opponent(team)
        if (is.na(current_possession_id)) {
          new_possession(non_penalized, event_time, event_elapsed, "Penalty")
        } else if (current_team == team) {
          close_possession(current_possession_id, event_time, event_elapsed, "Penalty")
          new_possession(non_penalized, event_time, event_elapsed, "Penalty")
        } else {
          close_possession(current_possession_id, event_time, event_elapsed, "Penalty")
          new_possession(current_team, event_time, event_elapsed, "Penalty")
        }
        event_possession_id[i] <- current_possession_id
        next
      }
      
      
      # ---- End of period ----
      if (event == "End of Quarter") {
        if (!is.na(current_possession_id)) {
          event_possession_id[i] <- current_possession_id
          close_possession(current_possession_id, "00:00", event_elapsed, "Period End")
        }
        current_possession_id <- NA_integer_
        current_team          <- NA_character_
        next
      }
      
      
      # ---- All other events ----
      if (!is.na(current_possession_id)) {
        event_possession_id[i] <- current_possession_id
      }
    }
    
    pbp$possession_id <- event_possession_id
    
    # ----------------------------------------------------------
    # STEP 10B — ball-half tracking (follows faceoffs and clears)
    # ----------------------------------------------------------
    
    ball_half       <- rep(NA_character_, nrow(pbp))   # half during the event
    ball_half_after <- rep(NA_character_, nrow(pbp))   # half after the event resolves
    current_half    <- NA_character_
    half_conflicts  <- 0L
    
    for (i in seq_len(nrow(pbp))) {
      
      # New period: location unknown until the opening faceoff
      if (i == 1 || pbp$quarter[i] != pbp$quarter[i - 1]) {
        current_half <- NA_character_
      }
      
      event <- pbp$event_type[i]
      team  <- pbp$team[i]
      
      if (event == "Faceoff" && !is.na(pbp$faceoff_winner[i])) {
        # Winner goes on offense immediately -> ball is in the loser's half
        current_half <- get_opponent(pbp$faceoff_winner[i])
        
      } else if (event == "Clear" && !is.na(team)) {
        # A clear starts from the clearing team's own half.
        # If tracking disagrees, count it and re-sync.
        if (!is.na(current_half) && current_half != team) {
          half_conflicts <- half_conflicts + 1L
        }
        current_half <- team
      }
      
      ball_half[i] <- current_half
      
      # Apply the clear result
      if (event == "Clear" && !is.na(team) &&
          str_detect(pbp$event_text[i], regex("\\bgood\\b", ignore_case = TRUE))) {
        current_half <- get_opponent(team)
      }
      
      # Sanity check: a shooter shouldn't be shooting from their own half
      if (event %in% c("Shot", "Goal") && !is.na(team) &&
          !is.na(current_half) && current_half == team) {
        half_conflicts <- half_conflicts + 1L
      }
      
      ball_half_after[i] <- current_half
    }
    
    pbp$ball_half       <- ball_half
    pbp$ball_half_after <- ball_half_after
    
    if (half_conflicts > 0) {
      message("  Ball-half conflicts: ", half_conflicts, " (check clears in this game)")
    }
    
    # ----------------------------------------------------------
    # STEP 10C — clear timing and time spent in each half
    # ----------------------------------------------------------
    
    pbp <-
      pbp |>
      mutate(
        # ---- clear timestamps (populated only on Clear rows) ----
        clear_time       = if_else(event_type == "Clear", time, NA_character_),
        clear_elapsed    = if_else(event_type == "Clear", elapsed_game_seconds, NA_real_),
        clear_successful = case_when(
          event_type != "Clear" ~ NA,
          TRUE ~ str_detect(event_text, regex("\\bgood\\b", ignore_case = TRUE))
        ),
        
        # ---- interval between this event and the previous one ----
        # The ball sat in the previous event's resulting half for that interval.
        # Skip intervals that cross a period boundary or follow a goal
        # (the ball is dead until the next faceoff).
        prev_elapsed = lag(elapsed_game_seconds),
        prev_quarter = lag(quarter),
        prev_event   = lag(event_type),
        interval_ball_half = lag(ball_half_after),
        interval_seconds = case_when(
          is.na(prev_quarter) | quarter != prev_quarter ~ NA_real_,
          prev_event == "Goal"                           ~ NA_real_,
          TRUE ~ elapsed_game_seconds - prev_elapsed
        ),
        interval_ball_half = if_else(is.na(interval_seconds), NA_character_, interval_ball_half)
      ) |>
      # ---- seconds since the most recent clear (resets each period) ----
    group_by(quarter) |>
      mutate(last_clear_elapsed = clear_elapsed) |>
      fill(last_clear_elapsed, .direction = "down") |>
      mutate(seconds_since_last_clear = elapsed_game_seconds - last_clear_elapsed) |>
      ungroup()
    
    # ----------------------------------------------------------
    # STEP 11 — build possession table
    # ----------------------------------------------------------
    
    possession_ids <- sort(unique(na.omit(pbp$possession_id)))
    
    extract_list <- function(lst, ids) {
      map(ids, ~lst[[as.character(.x)]])
    }
    
    possessions <- tibble(possession_id = possession_ids) |>
      mutate(
        team          = map_chr(possession_id, ~possession_team_list[[as.character(.x)]]),
        start_time    = map_chr(possession_id, ~possession_start_time[[as.character(.x)]]),
        start_elapsed = map_dbl(possession_id, ~{
          x <- possession_start_elapsed[[as.character(.x)]]
          if (length(x) == 0) NA_real_ else x
        }),
        start_reason  = map_chr(possession_id, ~{
          x <- possession_start_reason[[as.character(.x)]]
          if (length(x) == 0) NA_character_ else x
        }),
        end_time      = map_chr(possession_id, ~{
          x <- possession_end_time[[as.character(.x)]]
          if (length(x) == 0) NA_character_ else x
        }),
        end_elapsed   = map_dbl(possession_id, ~{
          x <- possession_end_elapsed[[as.character(.x)]]
          if (length(x) == 0) NA_real_ else x
        }),
        end_reason    = map_chr(possession_id, ~{
          x <- possession_end_reason[[as.character(.x)]]
          if (length(x) == 0) NA_character_ else x
        })
      )
    
    
    # ----------------------------------------------------------
    # STEP 12 — event counts per possession
    # ----------------------------------------------------------
    
    possession_summary <-
      pbp |>
      filter(!is.na(possession_id)) |>
      group_by(possession_id) |>
      summarise(
        quarter          = first(quarter),
        start_half       = first(ball_half),
        end_half         = last(ball_half_after),
        events           = n(),
        # shots = all shot attempts including goals
        shots            = sum(event_type == "Shot" | event_type == "Goal",            na.rm = TRUE),
        # shots_on_cage = saves + blocked + hit post + goals
        shots_on_cage    = sum((event_type == "Shot" & shot_type == "On Cage") |
                                 event_type == "Goal",                                 na.rm = TRUE),
        # shots_off_target = wide + high
        shots_off_target = sum(event_type == "Shot" & shot_type == "Off Target",       na.rm = TRUE),
        goals            = sum(event_type == "Goal",                                   na.rm = TRUE),
        turnovers        = sum(event_type == "Turnover",                               na.rm = TRUE),
        ground_balls     = sum(event_type == "Ground Ball",                            na.rm = TRUE),
        clears           = sum(event_type == "Clear",                                  na.rm = TRUE),
        penalties        = sum(event_type == "Penalty",                                na.rm = TRUE),
        timeouts         = sum(event_type == "Timeout",                                na.rm = TRUE),
        end_of_quarter   = any(event_type == "End of Quarter",                         na.rm = TRUE),
        has_imputed_time = any(time_imputed, na.rm = TRUE),
        .groups = "drop"
      )
    
    possessions <- left_join(possessions, possession_summary, by = "possession_id")
    
    
    # ----------------------------------------------------------
    # STEP 13 — fix end-of-quarter possessions
    # ----------------------------------------------------------
    
    possessions <-
      possessions |>
      mutate(
        quarter_number = case_when(
          quarter == "1st Quarter" ~ 1L,
          quarter == "2nd Quarter" ~ 2L,
          quarter == "3rd Quarter" ~ 3L,
          quarter == "4th Quarter" ~ 4L,
          quarter == "1st OT"      ~ 5L,
          quarter == "2nd OT"      ~ 6L,
          quarter == "3rd OT"      ~ 7L,
          TRUE ~ NA_integer_
        ),
        end_time = if_else(
          end_of_quarter & (is.na(end_time) | end_time == ""),
          "00:00",
          end_time
        ),
        end_elapsed = if_else(
          end_of_quarter & is.na(end_elapsed),
          case_when(
            quarter_number <= 4 ~ as.double(quarter_number * 900L),
            TRUE ~ as.double(3600 + (quarter_number - 4) * 240L)
          ),
          end_elapsed
        ),
        end_reason = if_else(
          end_of_quarter & (is.na(end_reason) | end_reason == ""),
          "Quarter End",
          end_reason
        )
      ) |>
      select(-quarter_number)
    
    empty_eoq_possessions <-
      possessions |>
      filter(end_of_quarter, events == 1, end_elapsed == start_elapsed) |>
      pull(possession_id)
    
    if (length(empty_eoq_possessions) > 0) {
      
      for (r in which(pbp$possession_id %in% empty_eoq_possessions)) {
        
        # The possession the last real event (e.g. the untimed turnover) belongs to
        prev_pid <- tail(na.omit(pbp$possession_id[seq_len(r - 1)]), 1)
        
        if (length(prev_pid) == 1) {
          pbp$possession_id[r] <- prev_pid
          possessions$end_of_quarter[possessions$possession_id == prev_pid] <- TRUE
        } else {
          pbp$possession_id[r] <- NA_integer_
        }
      }
      
      possessions <- filter(possessions, !possession_id %in% empty_eoq_possessions)
    }
    
    # ----------------------------------------------------------
    # STEP 14 — penalty timeline and man-up / man-down flags
    # ----------------------------------------------------------
    
    penalties_raw <-
      pbp |>
      filter(event_type == "Penalty", !is.na(team)) |>
      mutate(
        penalty_duration_seconds = case_when(
          str_detect(event_text, regex("1:00|1 min|personal",   ignore_case = TRUE)) ~ 60L,
          str_detect(event_text, regex("0:30|30 sec|technical", ignore_case = TRUE)) ~ 30L,
          TRUE ~ 30L
        ),
        penalty_start_elapsed = elapsed_game_seconds,
        penalty_end_elapsed   = elapsed_game_seconds + penalty_duration_seconds
      ) |>
      select(
        penalized_team           = team,
        penalty_start_elapsed,
        penalty_end_elapsed,
        penalty_duration_seconds,
        event_text
      )
    
    goals_raw <-
      pbp |>
      filter(event_type == "Goal") |>
      select(scoring_team = team, goal_elapsed = elapsed_game_seconds)
    
    if (nrow(penalties_raw) > 0) {
      
      penalties_raw <-
        penalties_raw |>
        mutate(row_idx = row_number()) |>
        pmap_dfr(function(
    penalized_team, penalty_start_elapsed, penalty_end_elapsed,
    penalty_duration_seconds, event_text, row_idx
        ) {
          earliest <-
            goals_raw |>
            filter(
              scoring_team != penalized_team,
              goal_elapsed  > penalty_start_elapsed,
              goal_elapsed  <= penalty_end_elapsed
            ) |>
            pull(goal_elapsed) |>
            (\(x) if (length(x) == 0) Inf else min(x, na.rm = TRUE))()
          
          tibble(
            penalized_team           = penalized_team,
            penalty_start_elapsed    = penalty_start_elapsed,
            penalty_end_elapsed      = if (is.finite(earliest)) earliest else penalty_end_elapsed,
            penalty_duration_seconds = penalty_duration_seconds,
            event_text               = event_text
          )
        })
    }
    
    compute_man_advantage <- function(poss_team, poss_start, poss_end) {
      if (is.na(poss_team) || is.na(poss_start) || is.na(poss_end)) {
        return(list(man_up = FALSE, man_down = FALSE))
      }
      overlapping <-
        penalties_raw |>
        filter(penalty_start_elapsed < poss_end, penalty_end_elapsed > poss_start)
      list(
        man_up   = any(overlapping$penalized_team != poss_team, na.rm = TRUE),
        man_down = any(overlapping$penalized_team == poss_team, na.rm = TRUE)
      )
    }
    
    man_flags <-
      possessions |>
      select(possession_id, team, start_elapsed, end_elapsed) |>
      mutate(flags = pmap(list(team, start_elapsed, end_elapsed), compute_man_advantage)) |>
      unnest_wider(flags)
    
    possessions <-
      left_join(possessions, select(man_flags, possession_id, man_up, man_down),
                by = "possession_id")
    
    
    # ----------------------------------------------------------
    # STEP 15 — duration, outcome, clear marker
    # ----------------------------------------------------------
    
    possessions <-
      possessions |>
      mutate(
        duration_seconds = end_elapsed - start_elapsed,
        duration_minutes = duration_seconds / 60,
        start_zone = case_when(
          is.na(start_half)  ~ NA_character_,
          start_half == team ~ "Defensive Half",
          TRUE               ~ "Offensive Half"
        ),
        end_zone = case_when(
          is.na(end_half)  ~ NA_character_,
          end_half == team ~ "Defensive Half",
          TRUE             ~ "Offensive Half"
        ),
        outcome = case_when(
          goals     > 0 ~ "Goal",
          turnovers > 0 ~ "Turnover",
          shots     > 0 ~ "Shot",
          TRUE          ~ "Other"
        )
      ) |>
      arrange(possession_id) |>
      group_by(quarter) |>
      mutate(prev_possession_outcome = lag(outcome)) |>   
      ungroup()
    
    clear_info <-
      pbp |>
      filter(event_type == "Clear", !is.na(possession_id)) |>
      group_by(possession_id) |>
      summarise(
        clear_occurred = TRUE,
        clear_time     = first(time),
        clear_elapsed  = first(elapsed_game_seconds),
        .groups = "drop"
      )
    
    possessions <-
      possessions |>
      left_join(clear_info, by = "possession_id") |>
      mutate(clear_occurred = replace_na(clear_occurred, FALSE))
    
    # ----------------------------------------------------------
    # STEP 15B — time in offensive / defensive half per possession
    # ----------------------------------------------------------
    
    half_intervals <-
      pbp |>
      filter(!is.na(interval_ball_half), interval_seconds > 0) |>
      transmute(
        int_start = prev_elapsed,
        int_end   = elapsed_game_seconds,
        int_half  = interval_ball_half
      )
    
    possession_half_time <-
      possessions |>
      select(possession_id, team, start_elapsed, end_elapsed) |>
      cross_join(half_intervals) |>
      mutate(
        overlap = pmax(0, pmin(end_elapsed, int_end) - pmax(start_elapsed, int_start))
      ) |>
      group_by(possession_id) |>
      summarise(
        seconds_in_offensive_half = sum(overlap[int_half != team], na.rm = TRUE),
        seconds_in_defensive_half = sum(overlap[int_half == team], na.rm = TRUE),
        .groups = "drop"
      )
    
    possessions <-
      possessions |>
      left_join(possession_half_time, by = "possession_id") |>
      mutate(
        seconds_in_unknown_half = pmax(
          0,
          duration_seconds - seconds_in_offensive_half - seconds_in_defensive_half
        )
      )
    
    
    # ----------------------------------------------------------
    # STEP 16 — final datasets + game metadata
    # ----------------------------------------------------------
    
    pbp_final <-
      pbp |>
      select(
        event_id, quarter, time, time_imputed, imputed_method, time_carried_forward, elapsed_game_seconds,
        team, event_type, shot_type, event_text, score, possession_id, ball_half,
        clear_time, clear_elapsed, clear_successful, seconds_since_last_clear,
        interval_seconds, interval_ball_half
      ) |>
      mutate(game_id = game_id, opponent = opponent, date = date) |>
      relocate(game_id, opponent, date)
    
    possessions_final <-
      possessions |>
      select(
        possession_id, team, quarter,
        start_time, end_time,
        duration_seconds, duration_minutes, has_imputed_time,
        start_reason, end_reason, outcome, prev_possession_outcome,
        start_half, end_half, start_zone, end_zone, events, 
        shots, shots_on_cage, shots_off_target,
        goals, turnovers, ground_balls, penalties, timeouts,
        clears, clear_occurred, clear_time, clear_elapsed,
        seconds_in_offensive_half, seconds_in_defensive_half, seconds_in_unknown_half,
        man_up, man_down,
        end_of_quarter
      ) |>
      mutate(game_id = game_id, opponent = opponent, date = date) |>
      relocate(game_id, opponent, date)
    
    list(pbp_final = pbp_final, possessions_final = possessions_final)
    
  }, error = function(e) {
    message("  Skipping game ", game_id, ": ", e$message)
    return(NULL)
  })
}


# ============================================================
# D. RUN SCHEDULE + PARSE ALL GAMES
# ============================================================

schedule <- scrape_schedule(SCHEDULE_HTML_FILE)

cat("Games found:", nrow(schedule), "\n\n")
print(schedule)
cat("\n")

season_results <-
  pmap(
    schedule,
    function(game_id, date, opponent) {
      result <- parse_game(game_id, opponent, date)
      Sys.sleep(REQUEST_PAUSE_SECONDS)
      result
    }
  )

season_results <- season_results[!map_lgl(season_results, is.null)]

cat(
  "\nSuccessfully parsed", length(season_results),
  "of", nrow(schedule), "games.\n"
)


# ============================================================
# E. COMBINE INTO SEASON-LEVEL DATASETS
# ============================================================

season_pbp <-
  map_dfr(season_results, "pbp_final") |>
  mutate(season_event_id = paste0(game_id, "_", event_id))

season_possessions <-
  map_dfr(season_results, "possessions_final") |>
  mutate(season_possession_id = paste0(game_id, "_", possession_id))


# ============================================================
# F. SAVE
# ============================================================

write.csv(
  season_pbp,
  file.path(PBP_HTML_FOLDER,
            paste0(TEAM_FILE_NAME, "_", TEAM_SEASON_ID, "_Season_PBP.csv")),
  row.names = FALSE
)

write.csv(
  season_possessions,
  file.path(PBP_HTML_FOLDER,
            paste0(TEAM_FILE_NAME, "_", TEAM_SEASON_ID, "_Season_Possessions.csv")),
  row.names = FALSE
)

cat("\nSeason files saved to:", PBP_HTML_FOLDER, "\n")
cat("Total events:     ", nrow(season_pbp), "\n")
cat("Total possessions:", nrow(season_possessions), "\n")