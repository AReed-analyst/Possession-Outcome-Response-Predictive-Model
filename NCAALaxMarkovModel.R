# =============================================================================
# NCAA D-I Men's Lacrosse - Markov chain 
# Uses the TRUE score margin, TRUE Home/Away and TRUE regulation scores that the
# updated scraper (NCAA_Lacrosse_Scraper_with_Score_Margin.R) now writes.
# -----------------------------------------------------------------------------
# Self-contained: combines the 77 NEW *_Season_Possessions.csv files, builds the
# features, fits the model ladder, runs the possession-level Brier test, then
# simulates whole games and scores win probabilities against real regulation scores.
#
# Overtime is excluded everywhere (features, training, testing, simulation, results).
#
# Predicts the OUTCOME of possession i+1 (Goal / Turnover / Shot / Other) from what
# is known when that possession starts:
#   - how possession i ended + whether the same team keeps the ball + quarter  (Markov part)
#   - offense team and defense team                                            (team strength)
#   - possessing team's score margin (bucketed, interacted with quarter)       (game state)
#   - real home advantage (+1 offense is home, -1 away)
#
# Models
#   B0  Baseline: unconditional outcome frequencies
#   V1  Paper-style first-order chain: prev outcome x same-team x quarter
#   V2  V1 + offense/defense team effects
#   V3  V2 + score margin (x quarter) + home advantage
#   A1  Ablation: V1 + score margin + home, NO team effects
# Requires: tidyverse pieces + nnet (ships with R).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(purrr)
  library(stringr); library(lubridate); library(nnet)
})

# ------------------------------- CONFIG --------------------------------------
DATA_DIR     <- "~/Downloads/Python, R, and CSV/SAIL:Sports Analytics/NCAALax/All"      # folder with all 77 NEW possession CSVs
FILE_PATTERN <- "_Season_Possessions\\.csv$"
OUT_DIR      <- "output_v3"

TRAIN_START <- as.Date("2026-01-31"); TRAIN_END <- as.Date("2026-04-04")
TEST_START  <- as.Date("2026-04-05"); TEST_END  <- as.Date("2026-04-25")

DROP_GAP_TRANSITIONS <- TRUE   # skip pairs where possession_id jumps (see note in README text)
DECAY_GRID  <- c(5, 15, 40, 100)   # ridge strengths tried by grouped CV on TRAIN
CV_FOLDS    <- 4
N_SIM       <- 2000
N_BOOT      <- 2000
QUARTER_SEC <- 900
SEED        <- 2026

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(SEED)

OUT_LEVELS <- c("Goal", "Turnover", "Shot", "Other")
Q_LEVELS   <- c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter")
CTX_LEVELS <- c(paste(OUT_LEVELS, 0), paste(OUT_LEVELS, 1), "Start")   # index k = same0, k+4 = same1, 9 = faceoff start
bucket_of  <- function(m) case_when(m <= -3 ~ 1L, m <= -1 ~ 2L, m == 0 ~ 3L, m <= 2 ~ 4L, TRUE ~ 5L)
# buckets: 1 = down 3+, 2 = down 1-2, 3 = tied, 4 = up 1-2, 5 = up 3+   (possessing team's view)

REQUIRED_COLS <- c("game_id", "possession_id", "team", "team_side", "home_team", "away_team",
                   "quarter", "date", "outcome", "goals", "duration_seconds", "margin_start",
                   "home_score_reg", "away_score_reg", "score_matches_goal_events")

# ============================ 1. COMBINE THE 77 FILES ==========================
files <- list.files(DATA_DIR, pattern = FILE_PATTERN, full.names = TRUE)
stopifnot("No possession files found - check DATA_DIR" = length(files) > 0)
message("Found ", length(files), " possession files.")

read_one <- function(f) {
  d <- read_csv(f, col_types = cols(.default = col_character()), na = c("", "NA"), progress = FALSE)
  miss <- setdiff(REQUIRED_COLS, names(d))
  if (length(miss)) stop("File ", basename(f), " lacks: ", paste(miss, collapse = ", "),
                         "\n  -> re-run the UPDATED scraper for this team.", call. = FALSE)
  d$source_file <- basename(f); d
}
raw <- map_dfr(files, read_one)

# every game sits in both teams' files -> keep ONE copy per game_id
game_src <- raw %>% distinct(game_id, source_file) %>% arrange(game_id, source_file) %>%
  distinct(game_id, .keep_all = TRUE)
dup_check <- raw %>% count(game_id, source_file, name = "n") %>% group_by(game_id) %>%
  filter(n() > 1) %>% summarise(same_n = n_distinct(n) == 1, .groups = "drop")
message("Games stored in two team files: ", nrow(dup_check),
        " | identical possession counts in both copies: ", sum(dup_check$same_n))
poss <- raw %>% semi_join(game_src, by = c("game_id", "source_file")) %>%
  mutate(date = mdy(str_extract(date, "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}")),
         possession_id = as.integer(possession_id),
         duration_seconds = as.numeric(duration_seconds),
         goals = replace_na(as.integer(goals), 0L),
         margin_start = as.integer(margin_start),
         home_score_reg = as.integer(home_score_reg), away_score_reg = as.integer(away_score_reg),
         score_ok = as.logical(score_matches_goal_events)) %>%
  filter(!is.na(date), !is.na(outcome), !is.na(team)) %>%
  arrange(game_id, possession_id)
message("Unique games: ", n_distinct(poss$game_id), " | possessions: ", nrow(poss))

write_csv(poss, file.path(OUT_DIR, "all_teams_possessions_combined_v3.csv"))

# EXCLUDE OVERTIME everywhere
n_ot <- sum(!poss$quarter %in% Q_LEVELS)
poss <- filter(poss, quarter %in% Q_LEVELS)
message("Overtime possessions excluded: ", n_ot, " (everything below is regulation only).")
stopifnot(all(poss$team_side %in% c("Home", "Away")))

# ============================ 2. FEATURES ======================================
# --- validate the scraper's margin against an independent goal count -----------
chk <- poss %>% group_by(game_id) %>% mutate(tot_before = cumsum(goals) - goals) %>%
  group_by(game_id, team) %>% mutate(own_before = cumsum(goals) - goals) %>% ungroup() %>%
  mutate(m_goals = own_before - (tot_before - own_before))
message("margin_start == margin implied by possession goal counts: ",
        round(100 * mean(chk$margin_start == chk$m_goals, na.rm = TRUE), 2), "% of possessions")
message("Possessions with NA margin_start (dropped): ", sum(is.na(poss$margin_start)))

poss <- poss %>% filter(!is.na(margin_start)) %>%
  mutate(def_team = if_else(team == home_team, away_team, home_team),
         home_off = if_else(team_side == "Home", 1, -1)) %>%
  group_by(game_id) %>%
  mutate(prev_outcome = lag(outcome), prev_team = lag(team),
         prev_q = lag(match(quarter, Q_LEVELS)),
         gap_before = replace_na(possession_id - lag(possession_id) != 1L, FALSE)) %>%
  ungroup() %>%
  mutate(quarter_num = match(quarter, Q_LEVELS),
         outcome4 = factor(if_else(outcome %in% OUT_LEVELS, outcome, "Other"), levels = OUT_LEVELS),
         prev4 = if_else(prev_outcome %in% OUT_LEVELS, prev_outcome, "Other"))

# one row per game (for the game-level test)
games <- poss %>% group_by(game_id) %>%
  summarise(date = first(date), home_team = first(home_team), away_team = first(away_team),
            home_goals = first(home_score_reg), away_goals = first(away_score_reg),
            score_ok = first(score_ok), .groups = "drop")

# one row per TARGET possession: context = how the previous possession ended
rows <- poss %>%
  mutate(is_start = is.na(prev_outcome) | quarter_num != prev_q,           # faceoff at start of game/quarter
         same_team = as.integer(!is_start & prev_team == team),
         ctx = if_else(is_start, "Start", paste(prev4, same_team))) %>%
  { if (DROP_GAP_TRANSITIONS) filter(., is_start | !gap_before) else . } %>%
  mutate(ctx = factor(ctx, levels = CTX_LEVELS), ctx_q = paste(ctx, quarter_num),
         bucket = bucket_of(margin_start), bkt_q = paste(bucket, quarter_num)) %>%
  filter(!is.na(ctx))

train <- filter(rows, date >= TRAIN_START, date <= TRAIN_END)
test  <- filter(rows, date >= TEST_START,  date <= TEST_END)
sample_sizes <- tibble(split = c("Train", "Test"),
                       window = c(paste(TRAIN_START, "to", TRAIN_END), paste(TEST_START, "to", TEST_END)),
                       games = c(n_distinct(train$game_id), n_distinct(test$game_id)),
                       possessions = c(nrow(train), nrow(test)))
cat("\n==== SAMPLE SIZES (regulation only) ====\n"); print(sample_sizes)
write_csv(sample_sizes, file.path(OUT_DIR, "sample_sizes.csv"))
stopifnot(nrow(train) > 0, nrow(test) > 0)

team_levels <- sort(unique(c(train$team, train$def_team)))
cat("Distinct team names in training:", length(team_levels), "\n")

cat("\nMargin distribution at possession start (possessing team's view), TRAIN:\n")
print(round(prop.table(table(factor(train$bucket, 1:5,
                                    c("Down 3+", "Down 1-2", "Tied", "Up 1-2", "Up 3+")))), 3))

# ============================ 3. MODELS ========================================
dummies <- function(x, levels, prefix) {
  m <- matrix(0, length(x), length(levels), dimnames = list(NULL, paste0(prefix, levels)))
  i <- match(x, levels); ok <- !is.na(i); m[cbind(which(ok), i[ok])] <- 1; m
}
CTXQ_LEVELS <- as.vector(outer(CTX_LEVELS, 1:4, paste))
BKTQ_LEVELS <- as.vector(outer(1:5, 1:4, paste))
build_X <- function(d, teams = TRUE, state = TRUE) {
  X <- dummies(d$ctx_q, CTXQ_LEVELS, "cq_")
  if (teams) X <- cbind(X, dummies(d$team, team_levels, "off_"), dummies(d$def_team, team_levels, "def_"))
  if (state) X <- cbind(X, dummies(paste(d$bucket, d$quarter_num), BKTQ_LEVELS, "bq_"), home = d$home_off)
  X
}
fit_ridge <- function(X, y, decay) {
  nnet(X, class.ind(factor(y, levels = OUT_LEVELS)), size = 0, skip = TRUE, softmax = TRUE,
       decay = decay, maxit = 300, MaxNWts = 1e7, trace = FALSE)   # multinomial logit + ridge
}
predict_ridge <- function(fit, X) { P <- predict(fit, X, type = "raw"); colnames(P) <- OUT_LEVELS; P }
brier4_rows <- function(P, y) rowSums((P - sapply(OUT_LEVELS, function(o) as.numeric(y == o)))^2)
brierg_rows <- function(P, y) (P[, "Goal"] - as.numeric(y == "Goal"))^2

cv_decay <- function(d) {
  games_u <- unique(d$game_id)
  fold <- setNames(sample(rep(seq_len(CV_FOLDS), length.out = length(games_u))), games_u)
  f <- fold[as.character(d$game_id)]; X <- build_X(d, TRUE, TRUE)
  map_dfr(DECAY_GRID, function(dc) {
    message("  CV decay = ", dc)
    sc <- map_dbl(seq_len(CV_FOLDS), function(j) {
      m <- fit_ridge(X[f != j, , drop = FALSE], d$outcome4[f != j], dc)
      mean(brier4_rows(predict_ridge(m, X[f == j, , drop = FALSE]), d$outcome4[f == j]))
    })
    tibble(decay = dc, cv_brier4 = mean(sc))
  })
}
message("Tuning ridge penalty on TRAIN games (slow step)...")
cv <- cv_decay(train); print(cv); write_csv(cv, file.path(OUT_DIR, "decay_cv.csv"))
DECAY <- cv$decay[which.min(cv$cv_brier4)]; cat("Selected decay:", DECAY, "\n")

marg <- as.numeric(prop.table(table(factor(train$outcome4, OUT_LEVELS))))
v1_tab <- unclass(table(train$ctx_q, factor(train$outcome4, OUT_LEVELS)))
predict_v1 <- function(d) {
  P <- matrix(rep(marg, each = nrow(d)), nrow(d), dimnames = list(NULL, OUT_LEVELS))
  i <- match(d$ctx_q, rownames(v1_tab)); ok <- !is.na(i)
  P[ok, ] <- v1_tab[i[ok], , drop = FALSE] / rowSums(v1_tab[i[ok], , drop = FALSE]); P
}

message("Fitting final models...")
fit_v2 <- fit_ridge(build_X(train, TRUE,  FALSE), train$outcome4, DECAY)
fit_v3 <- fit_ridge(build_X(train, TRUE,  TRUE),  train$outcome4, DECAY)
fit_a1 <- fit_ridge(build_X(train, FALSE, TRUE),  train$outcome4, DECAY)

preds <- list(
  `B0 Baseline (marginal)`         = matrix(rep(marg, each = nrow(test)), nrow(test), dimnames = list(NULL, OUT_LEVELS)),
  `V1 First-order (paper-style)`   = predict_v1(test),
  `V2 + team offense/defense`      = predict_ridge(fit_v2, build_X(test, TRUE,  FALSE)),
  `V3 + score margin + home`       = predict_ridge(fit_v3, build_X(test, TRUE,  TRUE)),
  `A1 margin + home (no teams)`    = predict_ridge(fit_a1, build_X(test, FALSE, TRUE)))
stopifnot(all(map_lgl(preds, ~ all(abs(rowSums(.x) - 1) < 1e-6))))

# ============================ 4. POSSESSION-LEVEL BRIER ========================
y <- test$outcome4
per_row <- list(brier4 = map(preds, brier4_rows, y = y), brier_goal = map(preds, brierg_rows, y = y))
boot_tbl <- function(metric) {
  M <- do.call(cbind, per_row[[metric]]); g <- factor(test$game_id)
  S <- rowsum(M, g); n <- as.numeric(table(g)); G <- nrow(S); est <- colMeans(M)
  bt <- replicate(N_BOOT, { i <- sample.int(G, G, TRUE); colSums(S[i, , drop = FALSE]) / sum(n[i]) })
  ref <- bt["V1 First-order (paper-style)", ]; ref2 <- bt["V2 + team offense/defense", ]
  tibble(metric = metric, model = colnames(M), brier = est,
         lo = apply(bt, 1, quantile, .025), hi = apply(bt, 1, quantile, .975),
         skill_vs_baseline = 1 - est / est["B0 Baseline (marginal)"],
         diff_vs_V1 = est - est["V1 First-order (paper-style)"],
         d1_lo = apply(sweep(bt, 2, ref), 1, quantile, .025), d1_hi = apply(sweep(bt, 2, ref), 1, quantile, .975),
         diff_vs_V2 = est - est["V2 + team offense/defense"],
         d2_lo = apply(sweep(bt, 2, ref2), 1, quantile, .025), d2_hi = apply(sweep(bt, 2, ref2), 1, quantile, .975))
}
res_poss <- map_dfr(c("brier4", "brier_goal"), boot_tbl)
cat("\n==== POSSESSION-LEVEL TEST BRIER (lower = better) ====\n")
print(res_poss %>% mutate(across(where(is.numeric), ~ round(.x, 4))), n = Inf, width = Inf)
cat("diff_vs_V2 on the V3 row isolates what score margin + home add ON TOP of team strength.\n")
write_csv(res_poss, file.path(OUT_DIR, "brier_possession_level.csv"))

# ---- ratings and home effect from V3 ----
neutral <- function(off, def, home = 0) tibble(team = off, def_team = def, ctx_q = "Turnover 0 2",
                                               bucket = 3L, quarter_num = 2L, home_off = home)
nt <- length(team_levels)
r_off <- predict_ridge(fit_v3, build_X(neutral(team_levels, rep("__none__", nt)), TRUE, TRUE))[, "Goal"]
r_def <- predict_ridge(fit_v3, build_X(neutral(rep("__none__", nt), team_levels), TRUE, TRUE))[, "Goal"]
r_avg <- predict_ridge(fit_v3, build_X(neutral("__none__", "__none__"), TRUE, TRUE))[, "Goal"]
ratings <- tibble(team = team_levels, p_goal_scored = r_off, p_goal_allowed = r_def,
                  net = r_off - r_def, league_avg = r_avg) %>% arrange(desc(net))
write_csv(ratings, file.path(OUT_DIR, "team_ratings_V3.csv"))
cat("\nTop 5 / bottom 5 teams (net goal probability per possession):\n")
print(bind_rows(head(ratings, 5), tail(ratings, 5)) %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

p_home <- predict_ridge(fit_v3, build_X(neutral("__none__", "__none__", 1), TRUE, TRUE))[, "Goal"]
p_away <- predict_ridge(fit_v3, build_X(neutral("__none__", "__none__", -1), TRUE, TRUE))[, "Goal"]
cat("\nHome advantage (avg teams, tied, Q2): P(goal) on a home possession =", round(p_home, 3),
    "| on an away possession =", round(p_away, 3), "\n")

# score-margin effect (avg teams, home neutral): P(goal) by margin bucket and quarter
mg <- expand.grid(bucket = 1:5, quarter_num = 1:4) %>%
  mutate(team = "__none__", def_team = "__none__", ctx_q = paste("Turnover 0", quarter_num), home_off = 0)
mg$p_goal <- predict_ridge(fit_v3, build_X(mg, TRUE, TRUE))[, "Goal"]
mg_tab <- mg %>% mutate(margin = factor(bucket, 1:5, c("Down 3+", "Down 1-2", "Tied", "Up 1-2", "Up 3+")),
                        quarter = paste0("Q", quarter_num)) %>%
  select(margin, quarter, p_goal) %>% pivot_wider(names_from = quarter, values_from = p_goal)
cat("\nP(goal) by score margin of the possessing team (average teams):\n")
print(mg_tab %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
write_csv(mg_tab, file.path(OUT_DIR, "margin_effect_pgoal.csv"))

# ============================ 5. FULL-GAME SIMULATION ==========================
# Orientation 1 = HOME team on offense, 2 = AWAY team on offense.
dur_pool <- map(OUT_LEVELS, ~ train$duration_seconds[train$outcome4 == .x & !is.na(train$duration_seconds)])
p_same <- map_dbl(1:4, function(k) mean(train$same_team[train$ctx != "Start" & train$prev4 == OUT_LEVELS[k]]))

simulate_game <- function(H, A, n_sim) {
  grid <- expand.grid(orient = 1:2, ctx = 1:9, q = 1:4, b = 1:5)         # idx = o + 2(c-1) + 18(q-1) + 72(b-1)
  g <- tibble(team = if_else(grid$orient == 1, H, A), def_team = if_else(grid$orient == 1, A, H),
              ctx_q = paste(CTX_LEVELS[grid$ctx], grid$q), bucket = grid$b, quarter_num = grid$q,
              home_off = if_else(grid$orient == 1, 1, -1))
  Pc <- t(apply(predict_ridge(fit_v3, build_X(g, TRUE, TRUE)), 1, cumsum))
  clock <- numeric(n_sim); q <- rep(1L, n_sim); marg_h <- integer(n_sim)    # home goals - away goals
  team <- sample(1:2, n_sim, TRUE); ctx <- rep(9L, n_sim)
  gH <- gA <- nposs <- integer(n_sim); active <- rep(TRUE, n_sim)
  for (step in 1:400) {
    a <- which(active); if (!length(a)) break
    m_off <- ifelse(team[a] == 1, marg_h[a], -marg_h[a])
    idx <- team[a] + 2 * (ctx[a] - 1) + 18 * (q[a] - 1) + 72 * (bucket_of(m_off) - 1)
    u <- runif(length(a))
    o <- 1L + (u > Pc[idx, 1]) + (u > Pc[idx, 2]) + (u > Pc[idx, 3])
    d <- numeric(length(a))
    for (k in 1:4) { w <- which(o == k); if (length(w)) d[w] <- sample(dur_pool[[k]], length(w), TRUE) }
    end <- clock[a] + d; qend <- QUARTER_SEC * q[a]; cross <- end >= qend
    o[cross] <- 4L                                                          # clock expired -> no scoring
    goal <- o == 1L
    gH[a] <- gH[a] + (goal & team[a] == 1); gA[a] <- gA[a] + (goal & team[a] == 2)
    marg_h[a] <- marg_h[a] + ifelse(goal, ifelse(team[a] == 1, 1L, -1L), 0L)
    nposs[a] <- nposs[a] + 1L
    same <- runif(length(a)) < p_same[o]
    ctx[a] <- ifelse(cross, 9L, o + 4L * same)
    team[a] <- ifelse(cross, sample(1:2, length(a), TRUE), ifelse(same, team[a], 3L - team[a]))
    clock[a] <- ifelse(cross, qend, end); q[a] <- q[a] + cross; active[a] <- q[a] <= 4L
  }
  pH <- mean(marg_h > 0); pA <- mean(marg_h < 0)
  # P(home wins | game decided in regulation); regulation ties go to OT and are excluded from scoring
  list(p_home = pH / (pH + pA), p_tie_reg = mean(marg_h == 0), gH = mean(gH), gA = mean(gA), nposs = mean(nposs))
}

test_all <- games %>% filter(date >= TEST_START, date <= TEST_END, game_id %in% test$game_id,
                             !is.na(home_goals), !is.na(away_goals))
cat("\nTest games tied after regulation (went to OT, excluded from game-level test):",
    sum(test_all$home_goals == test_all$away_goals), "of", nrow(test_all), "\n")
test_games <- filter(test_all, home_goals != away_goals)

message("Simulating ", nrow(test_games), " test games x ", N_SIM, " sims...")
sims <- pmap_dfr(test_games %>% select(game_id, home_team, away_team), function(game_id, home_team, away_team) {
  s <- simulate_game(home_team, away_team, N_SIM); tibble(game_id = game_id, !!!s)
})
gl <- test_games %>% inner_join(sims, by = "game_id") %>%
  mutate(home_won = as.numeric(home_goals > away_goals), brier = (p_home - home_won)^2,
         total_goals_actual = home_goals + away_goals, total_goals_sim = gH + gA)

game_eval <- function(d, label) tibble(
  subset = label, games = nrow(d), brier_sim = mean(d$brier), brier_coinflip = 0.25,
  skill_vs_coinflip = 1 - mean(d$brier) / 0.25, pct_winners_correct = mean((d$p_home > 0.5) == (d$home_won == 1)),
  mae_total_goals = mean(abs(d$total_goals_sim - d$total_goals_actual)),
  mean_pred_home_win = mean(d$p_home), actual_home_win_rate = mean(d$home_won),
  mean_sim_possessions = mean(d$nposs))
game_res <- bind_rows(game_eval(gl, "All test games"),
                      game_eval(filter(gl, score_ok), "Printed score matches Goal rows"))
cat("\n==== GAME-LEVEL TEST (regulation; lower Brier = better) ====\n")
print(game_res %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)
cat("Actual mean possessions per test game (both teams, regulation):",
    round(mean(count(filter(poss, date >= TEST_START, date <= TEST_END), game_id)$n), 1), "\n")
set.seed(SEED); bs <- replicate(N_BOOT, mean(sample(gl$brier, replace = TRUE)))
cat("Game-level Brier 95% bootstrap CI:", round(quantile(bs, c(.025, .975)), 3), " (coin flip = 0.25)\n")

cal <- gl %>% mutate(bin = cut(p_home, c(0, .35, .5, .65, 1), include.lowest = TRUE)) %>%
  group_by(bin) %>% summarise(games = n(), mean_pred_home_win = mean(p_home),
                              actual_home_win_rate = mean(home_won), .groups = "drop")
cat("\nCalibration of P(home team wins):\n"); print(cal %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
write_csv(gl, file.path(OUT_DIR, "game_level_predictions.csv"))
write_csv(game_res, file.path(OUT_DIR, "game_level_brier.csv"))
write_csv(cal, file.path(OUT_DIR, "game_level_calibration.csv"))
message("\nDone. Outputs in ", normalizePath(OUT_DIR))

# =============================================================================
# V2 vs V3 comparison: re-run the team ratings and the full-game simulation
# with V2 (team strength only) and compare them with V3 (team strength +
# score margin + home/away).
#
# HOW TO RUN
#   1. Run lacrosse_markov_v3_score_margin.R first, in the SAME R session.
#      This script reuses the fitted models, data and settings it leaves in
#      memory (fit_v2, fit_v3, test_games, gl, ratings, simulate settings...).
#   2. Then run this script (source it, or "Run All" in RStudio).
#
# WHAT IT OUTPUTS (console + CSVs in OUT_DIR)
#   A. Team ratings under V2, and how they differ from V3
#   B. Game-level simulation using V2, with the same metrics as the V3 run
#   C. Paired bootstrap: is V2's game-level Brier different from V3's?
#   D. Calibration of V2's win probabilities
#
# NOTE: V2 has no score-margin or home/away inputs, so the "home advantage"
#       line and the "P(goal) by score margin" table do not exist for V2.
# =============================================================================

needed <- c("fit_v2", "fit_v3", "build_X", "predict_ridge", "neutral", "team_levels",
            "test_games", "gl", "ratings", "dur_pool", "p_same", "CTX_LEVELS",
            "bucket_of", "QUARTER_SEC", "N_SIM", "N_BOOT", "SEED", "OUT_DIR")
missing_objs <- needed[!vapply(needed, exists, logical(1))]
if (length(missing_objs)) {
  stop("Run lacrosse_markov_v3_score_margin.R first in this R session. Missing: ",
       paste(missing_objs, collapse = ", "), call. = FALSE)
}

# -----------------------------------------------------------------------------
# A. TEAM RATINGS UNDER V2
#    Same neutral situation as V3 (after a turnover, Q2, average opponent).
#    V2 ignores margin and home, so those columns are simply not used.
# -----------------------------------------------------------------------------
nt <- length(team_levels)
v2_off <- predict_ridge(fit_v2, build_X(neutral(team_levels, rep("__none__", nt)), TRUE, FALSE))[, "Goal"]
v2_def <- predict_ridge(fit_v2, build_X(neutral(rep("__none__", nt), team_levels), TRUE, FALSE))[, "Goal"]
v2_avg <- predict_ridge(fit_v2, build_X(neutral("__none__", "__none__"), TRUE, FALSE))[, "Goal"]

ratings_v2 <- tibble(team = team_levels, p_goal_scored = v2_off, p_goal_allowed = v2_def,
                     net = v2_off - v2_def, league_avg = v2_avg) %>%
  arrange(desc(net))
write_csv(ratings_v2, file.path(OUT_DIR, "team_ratings_V2.csv"))

cat("\n==== A. TEAM RATINGS: V2 (team strength only) ====\n")
cat("Top 5 / bottom 5 teams under V2 (net goal probability per possession):\n")
print(bind_rows(head(ratings_v2, 5), tail(ratings_v2, 5)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))

rating_cmp <- ratings %>%
  mutate(rank_V3 = rank(-net, ties.method = "first")) %>%
  select(team, net_V3 = net, rank_V3) %>%
  inner_join(ratings_v2 %>% mutate(rank_V2 = rank(-net, ties.method = "first")) %>%
               select(team, net_V2 = net, rank_V2), by = "team") %>%
  mutate(net_change = net_V2 - net_V3, rank_change = rank_V3 - rank_V2) %>%
  arrange(rank_V3)
write_csv(rating_cmp, file.path(OUT_DIR, "team_ratings_V2_vs_V3.csv"))

cat("\nV2 vs V3 ratings:\n")
cat("  Correlation of net ratings (Pearson):          ",
    round(cor(rating_cmp$net_V2, rating_cmp$net_V3), 3), "\n")
cat("  Rank correlation (Spearman):                   ",
    round(cor(rating_cmp$rank_V2, rating_cmp$rank_V3, method = "spearman"), 3), "\n")
cat("  Mean absolute change in net rating:            ",
    round(mean(abs(rating_cmp$net_change)), 4), "\n")
cat("  Teams whose rank moved by 5+ places:           ",
    sum(abs(rating_cmp$rank_change) >= 5), "of", nrow(rating_cmp), "\n")
cat("  Same top 5?    ", setequal(head(ratings$team, 5), head(ratings_v2$team, 5)), "\n")
cat("  Same bottom 5? ", setequal(tail(ratings$team, 5), tail(ratings_v2$team, 5)), "\n")
cat("\nLargest rank movers (positive = higher under V2):\n")
print(rating_cmp %>% arrange(desc(abs(rank_change))) %>% head(5) %>%
        mutate(across(where(is.double), ~ round(.x, 3))))

# -----------------------------------------------------------------------------
# B. GAME-LEVEL SIMULATION WITH V2
#    Identical engine to V3's simulation; only the outcome model changes.
#    Under V2 the outcome probabilities do not react to the score or venue.
# -----------------------------------------------------------------------------
simulate_game_model <- function(H, A, n_sim, fit, use_state) {
  grid <- expand.grid(orient = 1:2, ctx = 1:9, q = 1:4, b = 1:5)
  g <- tibble(team = if_else(grid$orient == 1, H, A), def_team = if_else(grid$orient == 1, A, H),
              ctx_q = paste(CTX_LEVELS[grid$ctx], grid$q), bucket = grid$b, quarter_num = grid$q,
              home_off = if_else(grid$orient == 1, 1, -1))
  Pc <- t(apply(predict_ridge(fit, build_X(g, TRUE, use_state)), 1, cumsum))
  clock <- numeric(n_sim); q <- rep(1L, n_sim); marg_h <- integer(n_sim)
  team <- sample(1:2, n_sim, TRUE); ctx <- rep(9L, n_sim)
  gH <- gA <- nposs <- integer(n_sim); active <- rep(TRUE, n_sim)
  for (step in 1:400) {
    a <- which(active); if (!length(a)) break
    m_off <- ifelse(team[a] == 1, marg_h[a], -marg_h[a])
    idx <- team[a] + 2 * (ctx[a] - 1) + 18 * (q[a] - 1) + 72 * (bucket_of(m_off) - 1)
    u <- runif(length(a))
    o <- 1L + (u > Pc[idx, 1]) + (u > Pc[idx, 2]) + (u > Pc[idx, 3])
    d <- numeric(length(a))
    for (k in 1:4) { w <- which(o == k); if (length(w)) d[w] <- sample(dur_pool[[k]], length(w), TRUE) }
    end <- clock[a] + d; qend <- QUARTER_SEC * q[a]; cross <- end >= qend
    o[cross] <- 4L
    goal <- o == 1L
    gH[a] <- gH[a] + (goal & team[a] == 1); gA[a] <- gA[a] + (goal & team[a] == 2)
    marg_h[a] <- marg_h[a] + ifelse(goal, ifelse(team[a] == 1, 1L, -1L), 0L)
    nposs[a] <- nposs[a] + 1L
    same <- runif(length(a)) < p_same[o]
    ctx[a] <- ifelse(cross, 9L, o + 4L * same)
    team[a] <- ifelse(cross, sample(1:2, length(a), TRUE), ifelse(same, team[a], 3L - team[a]))
    clock[a] <- ifelse(cross, qend, end); q[a] <- q[a] + cross; active[a] <- q[a] <= 4L
  }
  pH <- mean(marg_h > 0); pA <- mean(marg_h < 0)
  list(p_home = pH / (pH + pA), p_tie_reg = mean(marg_h == 0),
       gH = mean(gH), gA = mean(gA), nposs = mean(nposs))
}

message("Simulating ", nrow(test_games), " test games x ", N_SIM, " sims with V2...")
set.seed(SEED)
sims_v2 <- pmap_dfr(test_games %>% select(game_id, home_team, away_team),
                    function(game_id, home_team, away_team) {
                      s <- simulate_game_model(home_team, away_team, N_SIM, fit_v2, use_state = FALSE)
                      tibble(game_id = game_id, !!!s)
                    })

gl_v2 <- test_games %>% inner_join(sims_v2, by = "game_id") %>%
  mutate(home_won = as.numeric(home_goals > away_goals), brier = (p_home - home_won)^2,
         total_goals_actual = home_goals + away_goals, total_goals_sim = gH + gA)
write_csv(gl_v2, file.path(OUT_DIR, "game_level_predictions_V2.csv"))

summarise_games <- function(d, label) tibble(
  model = label, games = nrow(d), brier_sim = mean(d$brier), brier_coinflip = 0.25,
  skill_vs_coinflip = 1 - mean(d$brier) / 0.25,
  pct_winners_correct = mean((d$p_home > 0.5) == (d$home_won == 1)),
  mae_total_goals = mean(abs(d$total_goals_sim - d$total_goals_actual)),
  mean_pred_home_win = mean(d$p_home), actual_home_win_rate = mean(d$home_won),
  mean_sim_possessions = mean(d$nposs))

game_cmp <- bind_rows(summarise_games(gl, "V3 (teams + margin + home)"),
                      summarise_games(gl_v2, "V2 (teams only)"))
cat("\n==== B. GAME-LEVEL TEST: V3 vs V2 (regulation; lower Brier = better) ====\n")
print(game_cmp %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)
write_csv(game_cmp, file.path(OUT_DIR, "game_level_brier_V2_vs_V3.csv"))

# -----------------------------------------------------------------------------
# C. PAIRED BOOTSTRAP: V2 minus V3 game-level Brier (same games each replicate)
# -----------------------------------------------------------------------------
paired <- gl %>% select(game_id, brier_v3 = brier, p_v3 = p_home) %>%
  inner_join(gl_v2 %>% select(game_id, brier_v2 = brier, p_v2 = p_home), by = "game_id")

set.seed(SEED)
boot <- replicate(N_BOOT, {
  i <- sample.int(nrow(paired), replace = TRUE)
  c(v3 = mean(paired$brier_v3[i]), v2 = mean(paired$brier_v2[i]),
    diff = mean(paired$brier_v2[i] - paired$brier_v3[i]))
})
ci <- function(x) round(quantile(x, c(.025, .975)), 3)

cat("\n==== C. IS THE DIFFERENCE REAL? ====\n")
cat("V3 game-level Brier:", round(mean(paired$brier_v3), 3), " 95% CI", ci(boot["v3", ]), "\n")
cat("V2 game-level Brier:", round(mean(paired$brier_v2), 3), " 95% CI", ci(boot["v2", ]), "\n")
cat("V2 minus V3:        ", round(mean(paired$brier_v2 - paired$brier_v3), 4),
    " 95% CI", ci(boot["diff", ]),
    "\n  (negative = V2 better; if the interval contains 0, no reliable difference)\n")
cat("Correlation of the two models' home-win probabilities across games:",
    round(cor(paired$p_v2, paired$p_v3), 3), "\n")
cat("Games where V2 and V3 pick a different winner:",
    sum((paired$p_v2 > 0.5) != (paired$p_v3 > 0.5)), "of", nrow(paired), "\n")

write_csv(tibble(
  quantity = c("brier_V3", "brier_V2", "V2_minus_V3"),
  estimate = c(mean(paired$brier_v3), mean(paired$brier_v2), mean(paired$brier_v2 - paired$brier_v3)),
  lo = c(quantile(boot["v3", ], .025), quantile(boot["v2", ], .025), quantile(boot["diff", ], .025)),
  hi = c(quantile(boot["v3", ], .975), quantile(boot["v2", ], .975), quantile(boot["diff", ], .975))
), file.path(OUT_DIR, "game_level_V2_vs_V3_bootstrap.csv"))

# -----------------------------------------------------------------------------
# D. CALIBRATION OF V2 (same bins as V3)
# -----------------------------------------------------------------------------
cal_bins <- c(0, .35, .5, .65, 1)
calib <- function(d, label) d %>%
  mutate(bin = cut(p_home, cal_bins, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(games = n(), mean_pred_home_win = mean(p_home),
            actual_home_win_rate = mean(home_won), .groups = "drop") %>%
  mutate(model = label, .before = 1)

cal_cmp <- bind_rows(calib(gl, "V3"), calib(gl_v2, "V2"))
cat("\n==== D. CALIBRATION OF P(home team wins): V3 vs V2 ====\n")
print(cal_cmp %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
write_csv(cal_cmp, file.path(OUT_DIR, "game_level_calibration_V2_vs_V3.csv"))

message("\nDone. V2 comparison files saved in ", normalizePath(OUT_DIR))