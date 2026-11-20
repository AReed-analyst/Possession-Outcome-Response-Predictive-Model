# =============================================================================
# NCAA D-I Men's Lacrosse - Possession-level Markov chain models
# -----------------------------------------------------------------------------
# 1. Combine every *_Season_Possessions.csv (all 77 teams) into one dataset
# 2. Build Markov states S_i = (outcome x, possessing side y, quarter t)  [32 states]
# 3. Fit three Markov models on games Jan 31 - Apr 4, 2026
# 4. Test on games Apr 5 - Apr 25, 2026 using Brier scores (+ game-level bootstrap CIs)
#
# Models (all predict the NEXT full state, so they share one 32-class target):
#   M1  First-order chain, S_i -> S_{i+1}              (your paper's 32x32 matrix, plain MLE)
#   M2  Reduced second-order chain: S_i + outcome/side of possession i-1 -> S_{i+1}
#   M3  Score-aware chain: S_i + score margin after possession i -> S_{i+1}
#   B0  Baseline (not a "model"): unconditional distribution of next state, used for
#       Brier skill scores
# M2 and M3 are shrunk toward M1 (backoff smoothing, lambda picked by grouped CV
# on the TRAINING games only), because their context spaces are sparse.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(purrr)
  library(stringr); library(lubridate)
})

# ------------------------------- CONFIG --------------------------------------
DATA_DIR     <- "~/Downloads/Python, R, and CSV/SAIL:Sports Analytics/NCAALax/All"      
FILE_PATTERN <- "_Season_Possessions\\.csv$"
OUT_DIR      <- "output"

TRAIN_START  <- as.Date("2026-01-31")
TRAIN_END    <- as.Date("2026-04-04")
TEST_START   <- as.Date("2026-04-05")
TEST_END     <- as.Date("2026-04-25")

# Optional: CSV with columns game_id, home_team (team name spelled exactly as in
# the `team` column). If NULL, Home/Away is a PLACEHOLDER (see step 2).
HOME_LOOKUP  <- NULL                    # e.g. "data/home_teams.csv"

DROP_GAP_TRANSITIONS <- TRUE   # drop S_i -> S_{i+1} pairs where possession_id skips
LAMBDA_GRID  <- c(1, 3, 5, 10, 25, 50, 100)
CV_FOLDS     <- 5
N_BOOT       <- 2000
SEED         <- 2026

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(SEED)

# ============================ 1. COMBINE ALL TEAM FILES ========================
files <- list.files(DATA_DIR, pattern = FILE_PATTERN, full.names = TRUE)
stopifnot("No possession files found - check DATA_DIR" = length(files) > 0)
message("Found ", length(files), " possession files.")

# Read everything as character first: scraper versions have different columns
# (e.g. North Carolina has 39, High Point 26), so we avoid type clashes.
read_one <- function(f) {
  d <- read_csv(f, col_types = cols(.default = col_character()),
                na = c("", "NA"), progress = FALSE)
  d$schema_cols <- ncol(d)
  d$source_file <- basename(f)
  d
}
raw <- map_dfr(files, read_one)

# Each file holds BOTH teams' possessions for a game, so a game between two
# teams in your set appears twice (e.g. UNC vs High Point). Keep ONE copy per
# game_id: the file with the richer schema, then the one with more rows.
game_src <- raw %>%
  count(game_id, source_file, schema_cols, name = "n_rows") %>%
  arrange(game_id, desc(schema_cols), desc(n_rows)) %>%
  distinct(game_id, .keep_all = TRUE) %>%
  select(game_id, source_file)

poss <- raw %>% semi_join(game_src, by = c("game_id", "source_file"))
message("Unique games: ", nrow(game_src),
        " | rows before dedupe: ", nrow(raw), " | after: ", nrow(poss))

poss <- poss %>%
  mutate(
    date            = mdy(date),
    possession_id   = as.integer(possession_id),
    duration_seconds = as.numeric(duration_seconds),
    goals           = replace_na(as.integer(goals), 0L),
    across(any_of(c("man_up", "man_down", "end_of_quarter", "clear_occurred")), as.logical)
  )

write_csv(poss, file.path(OUT_DIR, "all_teams_possessions_combined.csv"))

# ============================ 2. BUILD STATES ==================================
Q_LEVELS   <- c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter")
OUT_LEVELS <- c("Goal", "Turnover", "Shot", "Other")
# NOTE: the scraper's `outcome` has 4 classes: Goal, Turnover, Shot (non-goal shot:
# save/miss) and Other. "Other" holds penalty-ended possessions, period ends without
# a shot, etc. There is no separate "Penalty" outcome in the data, so the paper's
# {goal, turnover, missed shot, penalty} becomes {Goal, Turnover, Shot, Other}.

ALL_STATES <- with(
  expand.grid(x = OUT_LEVELS, y = c("Home", "Away"), t = paste0("Q", 1:4),
              stringsAsFactors = FALSE),
  paste(x, y, t, sep = "|"))                       # 4 x 2 x 4 = 32
OUTCOME_OF_STATE <- sub("\\|.*$", "", ALL_STATES)

poss <- poss %>%
  filter(!is.na(date), !is.na(outcome), !is.na(team)) %>%
  arrange(game_id, possession_id)

# --- Home / Away -------------------------------------------------------------
# The possession files do NOT contain which team was home. If you supply
# HOME_LOOKUP we use it; otherwise the alphabetically-first team in each game is
# labeled "Home". That labeling is arbitrary but consistent within a game, so the
# model still learns same-side vs. other-side transitions - but "Home"/"Away"
# effects are NOT interpretable until you provide real home teams.
alpha_home <- poss %>% group_by(game_id) %>% summarise(home_alpha = min(team), .groups = "drop")
poss <- left_join(poss, alpha_home, by = "game_id")
if (!is.null(HOME_LOOKUP)) {
  lk <- read_csv(HOME_LOOKUP, col_types = cols(.default = col_character())) %>%
    select(game_id, home_team)
  poss <- left_join(poss, lk, by = "game_id") %>%
    mutate(home_team = coalesce(home_team, home_alpha))
  message("Home/Away: using lookup for ", sum(!is.na(unique(lk$game_id) )), " games; alphabetical fallback elsewhere.")
} else {
  poss <- mutate(poss, home_team = home_alpha)
  warning("HOME_LOOKUP not set: Home/Away is an arbitrary (alphabetical) placeholder.", call. = FALSE)
}

# --- Per-game running features (computed on the full sequence, incl. OT) -----
poss <- poss %>%
  group_by(game_id) %>%
  mutate(total_goals_before = cumsum(goals) - goals) %>%
  group_by(game_id, team) %>%
  mutate(own_goals_before = cumsum(goals) - goals) %>%
  ungroup() %>%
  group_by(game_id) %>%
  mutate(
    margin_after = (own_goals_before + goals) -
      (total_goals_before - own_goals_before),   # possessing team's margin AFTER this possession
    gap_before   = possession_id - lag(possession_id, default = 0L) != 1L,
    prev_outcome = lag(outcome),
    prev_team    = lag(team)
  ) %>%
  ungroup() %>%
  mutate(
    quarter_num = match(quarter, Q_LEVELS),            # OT -> NA (excluded)
    outcome4    = factor(if_else(outcome %in% OUT_LEVELS, outcome, "Other"), levels = OUT_LEVELS),
    side        = if_else(team == home_team, "Home", "Away"),
    state       = if_else(is.na(quarter_num), NA_character_,
                          paste(outcome4, side, paste0("Q", quarter_num), sep = "|")),
    margin_bkt  = case_when(
      margin_after <= -3 ~ "Down3+", margin_after <= -1 ~ "Down1-2",
      margin_after == 0  ~ "Tied",   margin_after <= 2  ~ "Up1-2", TRUE ~ "Up3+"),
    prev_ctx    = if_else(is.na(prev_outcome) | gap_before, "None",
                          paste0(if_else(prev_outcome %in% OUT_LEVELS, prev_outcome, "Other"),
                                 if_else(prev_team == team, "-same", "-opp")))
  )

# --- Transition pairs: (S_i  ->  S_{i+1}) within a game ----------------------
pairs <- poss %>%
  group_by(game_id) %>%
  mutate(next_state = lead(state),
         gap_next   = lead(possession_id) - possession_id != 1L) %>%
  ungroup() %>%
  filter(!is.na(state), !is.na(next_state)) %>%
  { if (DROP_GAP_TRANSITIONS) filter(., !gap_next) else . } %>%
  mutate(
    ctx_m1 = state,                                    # M1 context
    ctx_m2 = paste(state, prev_ctx, sep = " / "),      # M2 context
    ctx_m3 = paste(state, margin_bkt, sep = " / ")     # M3 context
  )

train <- filter(pairs, date >= TRAIN_START, date <= TRAIN_END)
test  <- filter(pairs, date >= TEST_START,  date <= TEST_END)

sample_sizes <- tibble(
  split       = c("Train", "Test"),
  window      = c(paste(TRAIN_START, "to", TRAIN_END), paste(TEST_START, "to", TEST_END)),
  games       = c(n_distinct(train$game_id), n_distinct(test$game_id)),
  possessions = c(sum(poss$date >= TRAIN_START & poss$date <= TRAIN_END & !is.na(poss$state)),
                  sum(poss$date >= TEST_START  & poss$date <= TEST_END  & !is.na(poss$state))),
  transitions = c(nrow(train), nrow(test)))
cat("\n==== SAMPLE SIZES ====\n"); print(sample_sizes)
cat("OT possessions excluded:", sum(is.na(poss$quarter_num)),
    "| transitions dropped for possession_id gaps:",
    sum(poss$gap_before & poss$possession_id > 1L, na.rm = TRUE), "\n")
write_csv(sample_sizes, file.path(OUT_DIR, "sample_sizes.csv"))
stopifnot("Train or test set is empty - check dates/DATA_DIR" = nrow(train) > 0 && nrow(test) > 0)

# ============================ 3. MODEL MACHINERY ===============================
count_matrix <- function(ctx, nxt) {
  tab <- table(factor(ctx), factor(nxt, levels = ALL_STATES))
  matrix(as.numeric(tab), nrow = nrow(tab),
         dimnames = list(rownames(tab), colnames(tab)))
}

marginal_of <- function(d) as.numeric(prop.table(table(factor(d$next_state, levels = ALL_STATES))))

# lambda = 0 -> pure MLE (P_kj = n_kj / n_k). lambda > 0 -> shrink toward `backoff`.
fit_markov <- function(d, ctx_col, lambda = 0, backoff = NULL, marginal = NULL) {
  list(ctx_col = ctx_col, counts = count_matrix(d[[ctx_col]], d$next_state),
       lambda = lambda, backoff = backoff, marginal = marginal)
}
fit_m1 <- function(d) fit_markov(d, "ctx_m1", 0, NULL, marginal_of(d))

predict_markov <- function(m, newdata) {
  n <- nrow(newdata)
  base <- if (is.null(m$backoff)) matrix(rep(m$marginal, each = n), nrow = n)
  else predict_markov(m$backoff, newdata)
  idx <- match(newdata[[m$ctx_col]], rownames(m$counts))
  has <- !is.na(idx)
  if (any(has)) {
    cn <- m$counts[idx[has], , drop = FALSE]
    base[has, ] <- (cn + m$lambda * base[has, , drop = FALSE]) / (rowSums(cn) + m$lambda)
  }
  base   # unseen contexts fall back to backoff model (or marginal for M1)
}

# ---- Brier helpers ----------------------------------------------------------
onehot <- function(y) {
  m <- matrix(0, length(y), length(ALL_STATES))
  m[cbind(seq_along(y), match(y, ALL_STATES))] <- 1
  m
}
collapse_outcome <- function(P) {
  sapply(OUT_LEVELS, function(o) rowSums(P[, OUTCOME_OF_STATE == o, drop = FALSE]))
}
brier_rows <- function(P, y) {          # returns 3 per-row Brier vectors
  y4 <- sub("\\|.*$", "", y)
  P4 <- collapse_outcome(P)
  list(
    brier32   = rowSums((P - onehot(y))^2),                                   # full 32-state
    brier4    = rowSums((P4 - sapply(OUT_LEVELS, function(o) as.numeric(y4 == o)))^2),  # next outcome
    brier_goal = (P4[, "Goal"] - as.numeric(y4 == "Goal"))^2                    # P(next possession = goal)
  )
}

# ---- Tune lambda for M2 / M3 with grouped CV on TRAINING games only ----------
tune_lambda <- function(d, ctx_col) {
  games <- unique(d$game_id)
  fold  <- setNames(sample(rep(seq_len(CV_FOLDS), length.out = length(games))), games)
  f     <- fold[as.character(d$game_id)]
  scores <- map_dbl(LAMBDA_GRID, function(lam) {
    mean(map_dbl(seq_len(CV_FOLDS), function(j) {
      tr <- d[f != j, ]; va <- d[f == j, ]
      m  <- fit_markov(tr, ctx_col, lam, fit_m1(tr))
      mean(brier_rows(predict_markov(m, va), va$next_state)$brier32)
    }))
  })
  tibble(model_ctx = ctx_col, lambda = LAMBDA_GRID, cv_brier32 = scores)
}
cv_m2 <- tune_lambda(train, "ctx_m2")
cv_m3 <- tune_lambda(train, "ctx_m3")
lam2  <- cv_m2$lambda[which.min(cv_m2$cv_brier32)]
lam3  <- cv_m3$lambda[which.min(cv_m3$cv_brier32)]
cat("\n==== CV-selected lambda (training games only) ====\n",
    "M2:", lam2, " M3:", lam3, "\n")
write_csv(bind_rows(cv_m2, cv_m3), file.path(OUT_DIR, "lambda_cv.csv"))

# ---- Fit on TRAIN -----------------------------------------------------------
M1 <- fit_m1(train)
M2 <- fit_markov(train, "ctx_m2", lam2, M1)
M3 <- fit_markov(train, "ctx_m3", lam3, M1)
marg_train <- marginal_of(train)

# ---- Predict on TEST --------------------------------------------------------
preds <- list(
  `B0 Baseline (marginal)`          = matrix(rep(marg_train, each = nrow(test)), nrow = nrow(test)),
  `M1 First-order (paper)`          = predict_markov(M1, test),
  `M2 Second-order (reduced hist.)` = predict_markov(M2, test),
  `M3 Score-aware`                  = predict_markov(M3, test)
)
# sanity: every predicted row must be a probability distribution
stopifnot(all(map_lgl(preds, ~ all(abs(rowSums(.x) - 1) < 1e-8))))

per_row <- map(preds, brier_rows, y = test$next_state)

# ============================ 4. BRIER EVALUATION ==============================
boot_models <- function(metric) {
  M <- do.call(cbind, map(per_row, ~ .x[[metric]]))
  g <- factor(test$game_id)
  S <- rowsum(M, g); n <- as.numeric(table(g)); G <- nrow(S)
  est  <- colMeans(M)
  boots <- replicate(N_BOOT, { i <- sample.int(G, G, TRUE); colSums(S[i, , drop = FALSE]) / sum(n[i]) })
  ref_b <- boots["B0 Baseline (marginal)", ]
  m1_b  <- boots["M1 First-order (paper)", ]
  tibble(
    metric = metric, model = colnames(M), brier = est,
    lo = apply(boots, 1, quantile, .025), hi = apply(boots, 1, quantile, .975),
    skill_vs_baseline = 1 - est / est["B0 Baseline (marginal)"],
    diff_vs_M1 = est - est["M1 First-order (paper)"],
    diff_lo = apply(sweep(boots, 2, m1_b), 1, quantile, .025),
    diff_hi = apply(sweep(boots, 2, m1_b), 1, quantile, .975))
}
results <- map_dfr(c("brier32", "brier4", "brier_goal"), boot_models)

cat("\n==== TEST-SET BRIER SCORES (lower = better) ====\n",
    "brier32 = full 32-state next-state | brier4 = next-possession outcome |",
    "brier_goal = P(next possession is a goal)\n", sep = "")
print(results %>% mutate(across(where(is.numeric), ~ round(.x, 4))), n = Inf, width = Inf)
cat("\ndiff_vs_M1 < 0 with a CI that excludes 0 => model beats the paper's chain.\n")
write_csv(results, file.path(OUT_DIR, "brier_results.csv"))

# ============================ 5. EXPORTS =======================================
# M1 transition matrix (rows = S_i, columns = S_{i+1}); unseen rows are all-NA
P_m1 <- matrix(NA_real_, length(ALL_STATES), length(ALL_STATES),
               dimnames = list(ALL_STATES, ALL_STATES))
cm <- M1$counts
P_m1[rownames(cm), ] <- cm / rowSums(cm)
write_csv(as_tibble(P_m1, rownames = "from_state"), file.path(OUT_DIR, "M1_transition_matrix.csv"))
write_csv(tibble(state = ALL_STATES, n_train_from = as.numeric(rowSums(
  count_matrix(factor(train$state, levels = ALL_STATES), train$next_state)))),
  file.path(OUT_DIR, "M1_row_counts.csv"))
cat("\nM1 rows with < 20 training transitions:",
    sum(rowSums(cm) < 20) + (length(ALL_STATES) - nrow(cm)), "of 32\n")

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  hm <- as.data.frame(as.table(P_m1)) %>% setNames(c("from", "to", "p"))
  p <- ggplot(hm, aes(to, from, fill = p)) + geom_tile() +
    scale_fill_viridis_c(na.value = "grey90") +
    labs(title = "M1 transition probabilities (train)", x = "S[i+1]", y = "S[i]", fill = "P") +
    theme_minimal(base_size = 8) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(file.path(OUT_DIR, "M1_transition_heatmap.png"), p, width = 10, height = 8, dpi = 200)
  
  pb <- results %>% filter(metric == "brier4") %>%
    ggplot(aes(model, brier)) + geom_pointrange(aes(ymin = lo, ymax = hi)) +
    coord_flip() + labs(title = "Test Brier (next-possession outcome), 95% game-bootstrap CI",
                        x = NULL, y = "Brier score") + theme_minimal()
  ggsave(file.path(OUT_DIR, "brier_by_model.png"), pb, width = 8, height = 4, dpi = 200)
}
message("\nDone. Outputs in ", normalizePath(OUT_DIR))