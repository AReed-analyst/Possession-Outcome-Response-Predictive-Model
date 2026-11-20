# =============================================================================
#   Figure 1a/1b: model comparison (improvement over V1 with 95% bootstrap CIs)
#   Figure 2:  team offense vs defense scatter (all 77 teams), V3 and V2
#
# Reads the CSVs written by NCAALaxMarkovModel.R.
# Writes PNG (300 dpi) and PDF versions of each figure into OUT_DIR.
# =============================================================================

library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)   # install.packages("ggrepel") if needed

OUT_DIR <- "output"   # same folder the model script wrote to

INK      <- "#0b0b0b"    # text and points
INK_2    <- "#52514e"    # secondary text, axes
GRID     <- "#e4e3df"    # recessive gridlines
BLUE     <- "#2a78d6"    # top teams
ORANGE   <- "#eb6834"    # bottom teams
REST     <- "#b5b4ae"    # all other teams

theme_paper <- theme_minimal(base_size = 11) +
  theme(
    text               = element_text(colour = INK),
    axis.text          = element_text(colour = INK_2),
    axis.title         = element_text(colour = INK_2),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(colour = GRID, linewidth = 0.3),
    strip.text         = element_text(face = "bold", hjust = 0),
    legend.position    = "top",
    legend.justification = "left",
    plot.title.position  = "plot",
    plot.background    = element_rect(fill = "white", colour = NA)
  )

# -----------------------------------------------------------------------------
# FIGURE 1a / 1b: Model comparison, one chart per Brier score
#   Plotted as IMPROVEMENT over V1 (V1 Brier minus model Brier), scaled by 1,000,
#   so bars to the RIGHT are better and the axis reads in whole numbers.
# -----------------------------------------------------------------------------
res <- read_csv(file.path(OUT_DIR, "brier_possession_level.csv"), show_col_types = FALSE)

model_labels <- c(
  "B0 Baseline (marginal)"        = "B0: Baseline",
  "V1 First-order (paper-style)"  = "V1: First-order Markov (reference)",
  "A1 margin + home (no teams)"   = "A1: Margin & home, no teams",
  "V2 + team offense/defense"     = "V2: + Team strength",
  "V3 + score margin + home"      = "V3: + Team strength, margin & home"
)

SCALE <- 1000   # show differences in thousandths of a Brier point

fig1_data <- res %>%
  mutate(
    model_lab = factor(model_labels[model], levels = rev(model_labels)),  # B0 at top
    improve   = -diff_vs_V1 * SCALE,      # positive = better than V1
    imp_lo    = -d1_hi * SCALE,
    imp_hi    = -d1_lo * SCALE,
    verdict   = case_when(
      model == "V1 First-order (paper-style)" ~ "Reference (V1)",
      imp_lo > 0                              ~ "Better than V1",
      imp_hi < 0                              ~ "Worse than V1",
      TRUE                                    ~ "Not distinguishable from V1"
    ),
    verdict   = factor(verdict, levels = c("Better than V1", "Worse than V1",
                                           "Not distinguishable from V1", "Reference (V1)")),
    value_lab = if_else(verdict == "Reference (V1)", "",
                        sprintf("%+.1f", improve))
  )

verdict_cols   <- c("Better than V1" = BLUE, "Worse than V1" = ORANGE,
                    "Not distinguishable from V1" = INK_2, "Reference (V1)" = INK_2)
verdict_shapes <- c("Better than V1" = 16, "Worse than V1" = 16,
                    "Not distinguishable from V1" = 21, "Reference (V1)" = 18)

make_fig1 <- function(which_metric, x_title) {
  d <- filter(fig1_data, metric == which_metric)
  span <- max(abs(c(d$imp_lo, d$imp_hi)))
  ggplot(d, aes(x = improve, y = model_lab, colour = verdict)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = INK_2, linewidth = 0.4) +
    geom_errorbar(aes(xmin = imp_lo, xmax = imp_hi), orientation = "y", width = 0,
                  linewidth = 0.9) +
    geom_point(aes(shape = verdict), size = 3.2, fill = "white", stroke = 1) +
    geom_text(aes(x = imp_hi, label = value_lab), hjust = -0.35, size = 3.4,
              colour = INK, show.legend = FALSE) +
    annotate("text", x = -span * 1.05, y = Inf, label = "Worse than V1",
             hjust = 0, vjust = -0.8, size = 3.3, colour = INK_2, fontface = "italic") +
    annotate("text", x =  span * 1.05, y = Inf, label = "Better than V1",
             hjust = 1, vjust = -0.8, size = 3.3, colour = INK_2, fontface = "italic") +
    scale_colour_manual(values = verdict_cols, name = NULL, drop = TRUE) +
    scale_shape_manual(values = verdict_shapes, name = NULL, drop = TRUE) +
    scale_x_continuous(limits = c(-span * 1.05, span * 1.25)) +
    coord_cartesian(clip = "off") +
    labs(x = x_title, y = NULL) +
    theme_paper +
    guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          plot.margin = margin(22, 12, 6, 6))
}

fig1a <- make_fig1("brier4",
                   "Improvement in four-outcome Brier score over V1 (in thousandths)\nwith 95% game-level bootstrap interval")
fig1b <- make_fig1("brier_goal",
                   "Improvement in goal-only Brier score over V1 (in thousandths)\nwith 95% game-level bootstrap interval")

ggsave(file.path(OUT_DIR, "fig1a_model_comparison_all_outcomes.png"), fig1a, width = 7, height = 4, dpi = 300)
ggsave(file.path(OUT_DIR, "fig1a_model_comparison_all_outcomes.pdf"), fig1a, width = 7, height = 4)
ggsave(file.path(OUT_DIR, "fig1b_model_comparison_goals.png"),        fig1b, width = 7, height = 4, dpi = 300)
ggsave(file.path(OUT_DIR, "fig1b_model_comparison_goals.pdf"),        fig1b, width = 7, height = 4)
# -----------------------------------------------------------------------------
# FIGURE 2: Team offense vs defense
#   Draws the chart for V3 (team_ratings_V3.csv) and, if it exists, for V2
#   (team_ratings_V2.csv, written by lacrosse_v2_vs_v3_comparison.R).
#   Both charts use the SAME axis ranges so they can be compared directly.
# -----------------------------------------------------------------------------
N_LABEL <- 5   # label the top and bottom N teams by net rating

rating_files <- c(V3 = "team_ratings_V3.csv", V2 = "team_ratings_V2.csv")
rating_files <- rating_files[file.exists(file.path(OUT_DIR, rating_files))]
if (!"V2" %in% names(rating_files)) {
  message("team_ratings_V2.csv not found - run lacrosse_v2_vs_v3_comparison.R to get the V2 chart.")
}
all_ratings <- lapply(rating_files, function(f)
  read_csv(file.path(OUT_DIR, f), show_col_types = FALSE))

# shared axis limits across every model drawn (padding leaves room for corner captions)
rng_x <- range(unlist(lapply(all_ratings, `[[`, "p_goal_scored")))
rng_y <- range(unlist(lapply(all_ratings, `[[`, "p_goal_allowed")))
pad_x <- 0.10 * diff(rng_x); pad_y <- 0.12 * diff(rng_y)
lim_x <- c(rng_x[1] - pad_x, rng_x[2] + pad_x)
lim_y <- c(rng_y[1] - pad_y, rng_y[2] + pad_y)

quad <- tibble(
  x     = c(lim_x[2], lim_x[1], lim_x[2], lim_x[1]),
  y     = c(lim_y[1], lim_y[1], lim_y[2], lim_y[2]),
  hjust = c(1, 0, 1, 0),
  vjust = c(1, 1, 0, 0),   # top labels hang down, bottom labels sit up
  text  = c("Strong offense,\nstrong defense", "Weak offense,\nstrong defense",
            "Strong offense,\nweak defense",   "Weak offense,\nweak defense")
)

make_team_chart <- function(ratings) {
  league <- ratings$league_avg[1]
  ratings <- ratings %>%
    arrange(desc(net)) %>%
    mutate(
      rank  = row_number(),
      group = case_when(rank <= N_LABEL      ~ paste("Top", N_LABEL, "(net)"),
                        rank > n() - N_LABEL ~ paste("Bottom", N_LABEL, "(net)"),
                        TRUE                 ~ "Other teams"),
      group = factor(group, levels = c(paste("Top", N_LABEL, "(net)"),
                                       paste("Bottom", N_LABEL, "(net)"),
                                       "Other teams")),
      label = if_else(group != "Other teams", team, NA_character_)
    )
  group_cols <- setNames(c(BLUE, ORANGE, REST), levels(ratings$group))
  
  ggplot(ratings, aes(x = p_goal_scored, y = p_goal_allowed)) +
    geom_vline(xintercept = league, colour = INK_2, linewidth = 0.4, linetype = "dashed") +
    geom_hline(yintercept = league, colour = INK_2, linewidth = 0.4, linetype = "dashed") +
    geom_text(data = quad, aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
              size = 3, colour = INK_2, lineheight = 0.9, inherit.aes = FALSE) +
    geom_point(data = filter(ratings, group == "Other teams"),
               aes(colour = group), size = 2.3) +
    geom_point(data = filter(ratings, group != "Other teams"),
               aes(colour = group), size = 2.8, stroke = 0.6) +
    geom_text_repel(aes(label = label), size = 3.1, colour = INK,
                    min.segment.length = 0, segment.colour = INK_2, segment.size = 0.3,
                    box.padding = 0.4, max.overlaps = Inf, na.rm = TRUE, seed = 2026) +
    scale_colour_manual(values = group_cols, breaks = levels(ratings$group), name = NULL) +
    scale_x_continuous(limits = lim_x, expand = expansion(mult = 0.01)) +
    scale_y_reverse(limits = rev(lim_y), expand = expansion(mult = 0.01)) +  # good defense at the top
    labs(
      x = "Offense: P(goal) per possession vs an average defense",
      y = "Defense: P(goal) allowed per possession vs an average offense\n(reversed: better defense is higher)"
    ) +
    theme_paper
}

out_stems <- c(V3 = "fig3_team_offense_defense", V2 = "fig3b_team_offense_defense_V2")
for (m in names(all_ratings)) {
  p <- make_team_chart(all_ratings[[m]])
  ggsave(file.path(OUT_DIR, paste0(out_stems[[m]], ".png")), p, width = 7, height = 6, dpi = 300)
  ggsave(file.path(OUT_DIR, paste0(out_stems[[m]], ".pdf")), p, width = 7, height = 6)
}

message("Saved figures to ", normalizePath(OUT_DIR))