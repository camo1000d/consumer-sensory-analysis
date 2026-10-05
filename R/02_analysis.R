# =============================================================================
#  STEP 2 – Statistical Analysis and Visualizations
#
#  Inputs:  <study_folder>/data/product_responses.csv and final_ranking.csv
#           (produced by R/01_prepare_data.R)
#  Outputs: <study_folder>/outputs/figures/  PNG charts for non-specialist readers
#           <study_folder>/outputs/sheets/   one 1-page product sheet per product
#           <study_folder>/outputs/tables/   tabular ranking summary
#           console                          statistical test reports
#
#  No study-specific hardcoded names: everything is configured in config.R.
# =============================================================================

source("demo/config.R")     # <- ONLY LINE TO CHANGE: dataset folder (here fictional demo)

# ---- 0. Packages ------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(patchwork)
library(scales)

# multcomp and emmeans load MASS, which masks dplyr::select; restore dplyr::select
select <- dplyr::select

outputs <- file.path(study_folder, "outputs")
for (d in c("figures", "sheets", "tables")) dir.create(file.path(outputs, d), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Import and Preparation ----------------------------------------------
responses <- read_csv(file.path(study_folder, "data", "product_responses.csv"), show_col_types = FALSE)
ranking   <- read_csv(file.path(study_folder, "data", "final_ranking.csv"),     show_col_types = FALSE)

codes          <- sort(unique(responses$product))
product_labels <- unname(product_names[as.character(codes)])   # names aligned with codes
product_levels <- product_labels
K              <- length(codes)                                # number of products
n_judges       <- n_distinct(ranking$judge)

responses <- responses %>%
  mutate(
    judge         = factor(judge),
    product       = factor(product, levels = codes, labels = product_labels),
    serving_order = factor(serving_order),
    innovation    = factor(innovation, levels = unname(innovation_labels))
  )

# Color palettes: fixed color per product, consistent across all figures
pal            <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")  # colorblind friendly
product_colors <- setNames(pal[seq_len(K)], product_levels)

# Ranks: green = top ranked, red = lowest ranked
rank_colors    <- setNames(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"), 1:5)

theme_set(
  theme_minimal(base_size = 14) +
    theme(plot.title          = element_text(face = "bold", size = 16),
          plot.title.position = "plot",
          plot.subtitle       = element_text(colour = "grey30"),
          plot.caption        = element_text(colour = "grey40", hjust = 0),
          panel.grid.minor    = element_blank(),
          legend.position     = "bottom")
)

save_plot <- function(g, name, w = 10, h = 6) {
  ggsave(file.path(outputs, "figures", paste0(name, ".png")), g,
         width = w, height = h, dpi = 300, bg = "white")
}

# ==============================================================================
# 2. "MOST INNOVATIVE" RANKING -> who wins?
# ==============================================================================
ranking_long <- ranking %>%
  pivot_longer(starts_with("pos"), names_to = "rank_pos", values_to = "product") %>%
  mutate(
    rank    = parse_number(rank_pos),
    product = factor(product, levels = codes, labels = product_labels),
    score   = K + 1 - rank       # score: 5 = ranked 1st ... 1 = ranked last
  ) %>%
  arrange(product, judge)

rank_summary <- ranking_long %>%
  group_by(product) %>%
  summarise(mean_rank = mean(rank), rank_sum = sum(rank), .groups = "drop") %>%
  arrange(mean_rank)
product_order <- as.character(rank_summary$product)   # best to worst

cat("\n===== RANKING: Friedman test =====\n")
fr <- friedman.test(rank ~ product | judge, data = ranking_long)
print(fr)
cat("Kendall's W concordance coefficient (0 = no agreement, 1 = complete agreement):",
    round(unname(fr$statistic) / (n_judges * (K - 1)), 3), "\n")

cat("\n===== Pairwise comparisons (paired Wilcoxon test, Holm correction) =====\n")
print(pairwise.wilcox.test(ranking_long$rank, ranking_long$product,
                           paired = TRUE, p.adjust.method = "holm", exact = FALSE))

# --- Consumer-level bootstrap (resample JUDGES, not individual ratings) ---
set.seed(2026)
B <- 5000
M <- ranking_long %>%
  select(judge, product, rank) %>%
  pivot_wider(names_from = product, values_from = rank) %>%
  select(all_of(product_levels)) %>%
  as.matrix()

boot <- t(replicate(B, {
  i <- sample(nrow(M), replace = TRUE)
  colMeans(M[i, , drop = FALSE])
}))
colnames(boot) <- product_levels

boot_winner <- factor(product_levels[max.col(-boot, ties.method = "random")], levels = product_levels)
p_first     <- prop.table(table(boot_winner))

df_score <- tibble(
  product   = product_levels,
  score     = K + 1 - colMeans(M),
  lower_ci  = apply(K + 1 - boot, 2, quantile, probs = 0.025),
  upper_ci  = apply(K + 1 - boot, 2, quantile, probs = 0.975),
  p_first   = as.numeric(p_first[product_levels])
)
write_csv(arrange(df_score, desc(score)), file.path(outputs, "tables", "ranking_summary.csv"))
print(arrange(df_score, desc(score)))

# --- Figure 1: Mean score (primary competition result chart) ---
fig1 <- df_score %>%
  mutate(product = factor(product, levels = rev(product_order))) %>%
  ggplot(aes(product, score, colour = product)) +
  geom_hline(yintercept = (K + 1) / 2, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), linewidth = 1.3, size = 1.1) +
  scale_colour_manual(values = product_colors, guide = "none") +
  scale_y_continuous(limits = c(1, K), breaks = 1:K) +
  coord_flip() +
  labs(
    title    = paste0("Which product is rated most innovative in ", ranking_label, "?"),
    subtitle = "Mean score: 5 = ranked 1st, 1 = ranked last (bars = 95% uncertainty margin)",
    caption  = str_wrap("Dashed line = ranking midpoint. Substantially overlapping bars indicate no clear statistical difference.", 100),
    x = NULL, y = paste0("Innovation score (", ranking_label, ")")
  )
save_plot(fig1, "01_innovation_score")

# --- Figure 2: Rank distribution ---
dist_rank <- ranking_long %>%
  count(product, rank) %>%
  complete(product, rank = 1:K, fill = list(n = 0)) %>%
  group_by(product) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

fig2 <- dist_rank %>%
  mutate(product = factor(product, levels = rev(product_order)),
         rank_f  = factor(rank, levels = K:1)) %>%
  ggplot(aes(product, pct, fill = rank_f)) +
  geom_col(width = 0.75, colour = "white") +
  geom_text(aes(label = ifelse(pct >= 0.07, percent(pct, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), colour = "grey15", size = 4) +
  scale_fill_manual(values = rank_colors,
                    labels = c("1st", "2nd", "3rd", "4th", "5th")[K:1], name = "Ranked:") +
  scale_y_continuous(labels = percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  labs(
    title    = "How was each product ranked?",
    subtitle = "Share of consumers placing the product at each rank (green = highly innovative, red = least innovative)",
    x = NULL, y = NULL
  )
save_plot(fig2, "02_rank_distribution")

# --- Figure 3: Probability of ranking first ---
fig3 <- df_score %>%
  mutate(product = factor(product, levels = rev(product_order))) %>%
  ggplot(aes(product, p_first, fill = product)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(p_first, accuracy = 1)), hjust = -0.15, size = 5) +
  scale_fill_manual(values = product_colors, guide = "none") +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "How confident are we in the winner?",
    subtitle = sprintf("Share of %s simulated consumer samples in which the product comes out on top",
                       format(B, big.mark = ",")),
    x = NULL, y = NULL
  )
save_plot(fig3, "03_winner_probability")

# ==============================================================================
# 3. LIKING (scores /9): Mixed-effects model, one model per sensory attribute
#    score ~ product + serving_order + (1 | judge)
#    -> judge random effect controls for lenient/strict raters
# ==============================================================================
analyze_criterion <- function(var) {
  m  <- lmer(as.formula(paste(var, "~ product + serving_order + (1 | judge)")), data = responses)
  pg <- anova(m)["product", "Pr(>F)"]
  em <- emmeans(m, "product")
  # Standard 95% confidence intervals (one per product)
  ci <- summary(em, infer = c(TRUE, FALSE), adjust = "none") %>%
    as_tibble() %>%
    transmute(product = as.character(product), emmean, lower.CL, upper.CL)
  # Significance letters: Tukey pairwise comparisons across all products
  letters_df <- suppressMessages(cld(em, Letters = letters, adjust = "tukey", reversed = TRUE)) %>%
    as_tibble() %>%
    transmute(product = as.character(product), letters = str_trim(.group))
  ci %>%
    left_join(letters_df, by = "product") %>%
    mutate(criterion = criteria[[var]], p_global = pg)
}
res_liking <- map_dfr(names(criteria), analyze_criterion)

cat("\n===== LIKING: Global p-value for product effect, by attribute =====\n")
res_liking %>% distinct(criterion, p_global) %>% mutate(p_global = round(p_global, 4)) %>% print()

fig4 <- res_liking %>%
  mutate(product   = factor(product, levels = product_order),
         criterion = factor(criterion, levels = unname(criteria))) %>%
  ggplot(aes(product, emmean, colour = product)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), linewidth = 1, size = 0.9) +
  geom_text(aes(y = upper.CL + 0.4, label = letters), colour = "grey20", fontface = "bold", size = 5) +
  facet_wrap(~ criterion, ncol = 3) +
  scale_colour_manual(values = product_colors, guide = "none") +
  scale_y_continuous(limits = c(3, 9), breaks = 3:9) +
  labs(
    title    = "What consumers appreciated",
    subtitle = "Mean score out of 9 (1 = not at all, 9 = extremely) and 95% confidence intervals",
    caption  = "Letters: products sharing a letter are not significantly different (Tukey HSD, p < 0.05).",
    x = NULL, y = NULL
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot(fig4, "04_liking", w = 12, h = 9)

# ==============================================================================
# 4. PERCEIVED MAJOR INNOVATION
# ==============================================================================
innov_pct <- responses %>%
  count(product, innovation, .drop = FALSE) %>%
  group_by(product) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

cat("\n===== PERCEIVED INNOVATION: Chi-square test (indicative: repeated measures) =====\n")
print(chisq.test(table(responses$product, responses$innovation)))

fig5 <- innov_pct %>%
  mutate(product = factor(product, levels = rev(product_order))) %>%
  ggplot(aes(product, pct, fill = innovation)) +
  geom_col(width = 0.75, colour = "white") +
  geom_text(aes(label = ifelse(pct >= 0.08, percent(pct, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = "Set2", name = NULL, labels = function(x) str_wrap(x, 30)) +
  scale_y_continuous(labels = percent) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_flip() +
  labs(
    title    = "Where do consumers perceive innovation?",
    subtitle = "Major innovation category selected by consumers for each product",
    x = NULL, y = NULL
  )
save_plot(fig5, "05_perceived_innovation", w = 12, h = 6.5)

# ==============================================================================
# 5. INTENTIONS (5-point scales): % 'Agree' + 'Strongly agree' (Top-2 box)
# ==============================================================================
top2 <- responses %>%
  pivot_longer(all_of(names(likert_labels)), names_to = "q", values_to = "score") %>%
  group_by(q, product) %>%
  summarise(pct = mean(score >= 4, na.rm = TRUE), .groups = "drop") %>%
  mutate(question = unname(likert_labels[q]),
         product  = factor(product, levels = product_order))

# Cochran's Q test: compare binary % across products with matched consumers
cochran_q <- function(y, group_var, block_var) {
  m <- tibble(y = y, group_var = group_var, block_var = block_var) %>%
    pivot_wider(names_from = group_var, values_from = y) %>%
    dplyr::select(-block_var) %>%
    as.matrix()
  m <- m[complete.cases(m), , drop = FALSE]
  k <- ncol(m); tot <- sum(m)
  q <- (k - 1) * (k * sum(colSums(m)^2) - tot^2) / (k * tot - sum(rowSums(m)^2))
  list(Q = q, df = k - 1, p = pchisq(q, df = k - 1, lower.tail = FALSE), n = nrow(m))
}

cat("\n===== INTENTIONS: Cochran's Q test (product effect, matched judges) =====\n")
for (v in names(likert_labels)) {
  r <- cochran_q(as.integer(responses[[v]] >= 4), responses$product, responses$judge)
  cat(sprintf("%s: Q = %.2f, df = %d, p = %.2e (n = %d judges)\n", likert_labels[[v]], r$Q, r$df, r$p, r$n))
}

fig6 <- ggplot(top2, aes(product, pct, fill = product)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(pct, accuracy = 1)), vjust = -0.4, size = 4.5) +
  facet_wrap(~ question, ncol = 2, labeller = label_wrap_gen(35)) +
  scale_fill_manual(values = product_colors, guide = "none") +
  scale_y_continuous(labels = percent, limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Repurchase intent and willingness to buy",
    subtitle = "Share of consumers who 'Agree' or 'Strongly agree'",
    x = NULL, y = NULL
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot(fig6, "06_intentions", w = 10, h = 6)

# ==============================================================================
# 6. AREAS FOR IMPROVEMENT + Penalty Analysis
# ==============================================================================
improv <- responses %>%
  group_by(product) %>%
  summarise(across(all_of(names(improvements)), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-product, names_to = "var", values_to = "pct") %>%
  mutate(point = factor(unname(improvements[var]), levels = unname(improvements)))

fig7 <- improv %>%
  mutate(product = factor(product, levels = rev(product_order))) %>%
  ggplot(aes(point, product, fill = pct)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = percent(pct, accuracy = 1)), size = 4.5) +
  scale_fill_gradient(low = "white", high = "#E6550D", labels = percent, name = "% of consumers") +
  scale_x_discrete(position = "top", labels = function(x) str_wrap(x, 12)) +
  labs(
    title    = "What should be improved?",
    subtitle = "Share of consumers citing each area (up to 3 selections)",
    x = NULL, y = NULL
  ) +
  theme(legend.position = "right", panel.grid = element_blank())
save_plot(fig7, "07_areas_for_improvement", w = 12, h = 5)

# Penalty = (mean overall liking of those NOT citing issue) - (mean score of those citing it)
citation_threshold <- 0.10   # ignore issues cited by fewer than 10% of consumers
penalties <- responses %>%
  select(product, overall_liking, all_of(names(improvements))) %>%
  pivot_longer(all_of(names(improvements)), names_to = "var", values_to = "cited") %>%
  group_by(product, var) %>%
  summarise(
    pct_cited  = mean(cited),
    mean_cited = mean(overall_liking[cited == 1]),
    mean_not   = mean(overall_liking[cited == 0]),
    .groups = "drop"
  ) %>%
  mutate(
    penalty = mean_not - mean_cited,
    point   = unname(improvements[var])
  ) %>%
  filter(var != "improve_none", pct_cited >= citation_threshold, !is.na(penalty))

fig8 <- ggplot(penalties, aes(pct_cited, penalty)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(aes(colour = product), size = 4) +
  geom_text(aes(label = point), vjust = -0.9, size = 3.8) +
  facet_wrap(~ product, ncol = 3) +
  scale_colour_manual(values = product_colors, guide = "none") +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0.1, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(
    title    = "Which improvements would yield the biggest gains?",
    subtitle = "Top right = frequently cited AND causes large drop in overall liking: priority #1",
    x = "Share of consumers citing this issue",
    y = "Overall liking penalty (points out of 9)"
  )
save_plot(fig8, "08_penalties", w = 12, h = 7)

# ==============================================================================
# 7. CONSUMER PROFILE (Age, Gender) – interpret cautiously with small sub-samples
# ==============================================================================
profile_heatmap <- function(var, title_text) {
  counts <- ranking %>%
    count(across(all_of(var)), name = "n") %>%
    mutate(label_col = paste0(.data[[var]], "\n(", n, " consumers)"))
  ranking_long %>%
    group_by(across(all_of(var)), product) %>%
    summarise(score = mean(score), .groups = "drop") %>%
    left_join(counts, by = var) %>%
    mutate(product = factor(product, levels = rev(product_order))) %>%
    ggplot(aes(label_col, product, fill = score)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.1f", score)), size = 5) +
    scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641",
                         midpoint = (K + 1) / 2, limits = c(1, K), name = "Innovation\nscore") +
    labs(title = title_text, x = NULL, y = NULL) +
    theme(legend.position = "right", panel.grid = element_blank())
}
fig9  <- profile_heatmap("age",    "Innovation score by consumer age")
fig10 <- profile_heatmap("gender", "Innovation score by consumer gender")
save_plot(fig9,  "09_profile_age",    w = 10, h = 5)
save_plot(fig10, "10_profile_gender", w = 8,  h = 5)

# ==============================================================================
# 8. PRODUCT SHEETS (1 page per product)
# ==============================================================================
mean_overall <- res_liking %>% group_by(criterion) %>% summarise(mean_all = mean(emmean), .groups = "drop")

generate_product_sheet <- function(p) {
  col <- product_colors[[p]]

  g_rank <- dist_rank %>%
    filter(product == p) %>%
    ggplot(aes(factor(rank), pct, fill = factor(rank))) +
    geom_col(width = 0.7) +
    geom_text(aes(label = percent(pct, accuracy = 1)), vjust = -0.4, size = 4.5) +
    scale_fill_manual(values = rank_colors, guide = "none") +
    scale_x_discrete(labels = c("1st", "2nd", "3rd", "4th", "5th")) +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Rank distribution",
         subtitle = "Share of consumers placing it at each innovation rank", x = NULL, y = NULL)

  g_liking <- res_liking %>%
    filter(product == p) %>%
    left_join(mean_overall, by = "criterion") %>%
    mutate(criterion = factor(criterion, levels = rev(unname(criteria)))) %>%
    ggplot(aes(y = criterion)) +
    geom_segment(aes(x = mean_all, xend = emmean, yend = criterion), colour = "grey75", linewidth = 2) +
    geom_point(aes(x = mean_all), colour = "grey50", size = 4) +
    geom_point(aes(x = emmean), colour = col, size = 5.5) +
    geom_text(aes(x = emmean, label = sprintf("%.1f", emmean)), vjust = -1.3, size = 4) +
    scale_x_continuous(limits = c(1, 9), breaks = 1:9) +
    scale_y_discrete(expand = expansion(add = 0.9)) +
    labs(title = "Liking scores (out of 9)",
         subtitle = "Colored point = this product · Grey point = 5-product average", x = NULL, y = NULL)

  g_innov <- innov_pct %>%
    filter(product == p) %>%
    ggplot(aes(pct, fct_rev(innovation))) +
    geom_col(fill = col, width = 0.7) +
    geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.15, size = 4.5) +
    scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
    scale_y_discrete(labels = function(x) str_wrap(x, 28)) +
    labs(title = "Perceived innovation",
         subtitle = "Major innovation according to consumers", x = NULL, y = NULL)

  g_improv <- improv %>%
    filter(product == p) %>%
    ggplot(aes(pct, fct_rev(point))) +
    geom_col(fill = col, width = 0.7) +
    geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.15, size = 4.5) +
    scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Areas for improvement",
         subtitle = "Share of consumers citing each area", x = NULL, y = NULL)

  sheet <- (g_rank | g_liking) / (g_innov | g_improv) +
    plot_annotation(
      title    = paste("Product Sheet:", p),
      subtitle = sprintf("%s · %d consumers · mean innovation rank: %.2f out of %d",
                         study_name, n_judges, rank_summary$mean_rank[rank_summary$product == p], K),
      theme = theme(plot.title    = element_text(size = 22, face = "bold", colour = col),
                    plot.subtitle = element_text(size = 13, colour = "grey30"))
    )

  ggsave(file.path(outputs, "sheets", paste0("sheet_", str_replace_all(p, "[^A-Za-z0-9]+", "_"), ".png")),
         sheet, width = 14, height = 9, dpi = 200, bg = "white")
}
walk(product_levels, generate_product_sheet)

cat("\nAnalysis complete! Outputs saved in", outputs, "\n")
