# =============================================================================
#  STEP 0 – Simulate a MOCK FIZZ export (for demo purposes)
#
#  Output: demo/data/fizz_export_mock.xlsx
#  No real data: 5 imaginary products, 120 simulated consumers/judges.
#
#  The export mimics the STRUCTURE of a FIZZ software export: each sheet contains
#  a summary table on the left and raw data on the right (one row per response),
#  starting from an anchor column ("Sample Pos." or "Date").
#
#  The experimental design is a 5-product Williams Latin square design (10 sequences):
#  each product appears at each serving position and follows every other product
#  the exact same number of times (balances order and carryover effects).
# =============================================================================

library(tidyverse)
library(writexl)
set.seed(40)

n_judges <- 120
codes    <- 101:105
products <- paste("Product", LETTERS[1:5])
K        <- length(codes)

# ---- "True" underlying product qualities (invented) -------------------------
# Perceived innovation: B > D > A > E > C (B is the most innovative)
innov_true <- c(A = 0.0, B = 1.0, C = -1.0, D = 0.6, E = -0.5)

# Mean liking score /9 by product and sensory attribute
# (D is the most appreciated overall, B has more divisive taste)
mu <- rbind(
  #        Convenience Overall liking Appearance Aroma Texture Taste
  A = c(      6.5,          6.6,        6.8,      6.4,   6.7,   6.6),
  B = c(      7.4,          6.0,        6.9,      5.8,   6.2,   5.6),
  C = c(      5.8,          6.9,        7.5,      7.0,   7.2,   7.0),
  D = c(      7.0,          7.5,        7.2,      7.3,   7.4,   7.6),
  E = c(      6.2,          7.0,        6.5,      6.9,   6.8,   7.1)
)
colnames(mu) <- c("Convenience", "Overall liking", "Appearance", "Aroma", "Texture", "Taste")

p_reco  <- c(A = 0.55, B = 0.70, C = 0.45, D = 0.65, E = 0.50)   # expected "Agree" share
p_price <- c(A = 0.55, B = 0.30, C = 0.60, D = 0.70, E = 0.45)
p_inno  <- rbind(            # probability of each major innovation category by product
  A = c(0.40, 0.10, 0.10, 0.25, 0.15),
  B = c(0.15, 0.50, 0.25, 0.05, 0.05),
  C = c(0.55, 0.10, 0.05, 0.20, 0.10),
  D = c(0.20, 0.15, 0.40, 0.15, 0.10),
  E = c(0.25, 0.05, 0.10, 0.50, 0.10)
)
inno_lab <- c("2- Recipe", "2- Format", "2- Usage", "2- Nutrition", "2- Packaging")
agreement_levels <- c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                      "Agree", "Strongly agree")

# ---- Williams Latin square design (5 products -> 10 sequences) --------------
latin_square <- t(sapply(0:4, function(i) (i + 0:4) %% 5 + 1))  # cyclic Latin square
williams_plan <- rbind(latin_square, latin_square[, 5:1])        # + reversed square = Williams design
sequences <- williams_plan[(seq_len(n_judges) - 1) %% nrow(williams_plan) + 1, ]
judge_order <- sample(n_judges)                                 # randomize sequence assignment

# ---- Consumers / Judges -----------------------------------------------------
judges <- tibble(
  num        = 1:n_judges,
  judge      = sprintf("J%03d", num),
  bias       = rnorm(n_judges, 0, 0.8),                         # rater severity/generosity bias
  gender     = sample(c("Female", "Male", "Other"), n_judges, TRUE, c(0.66, 0.32, 0.02)),
  age        = sample(c("15-24 yo", "25-49 yo", "50-64 yo", "65+ yo"), n_judges, TRUE, c(0.70, 0.20, 0.08, 0.02)),
  socio_prof = sample(c("Students", "Professionals", "Employees", "Retirees", "Other"), n_judges, TRUE, c(0.65, 0.20, 0.08, 0.04, 0.03))
)

# ---- Raw tasting rows (one row per judge x product) -------------------------
clip_bounds <- function(x, lo, hi) pmin(hi, pmax(lo, round(x)))

tasting_sessions <- map_dfr(seq_len(n_judges), function(i) {
  tibble(num = judge_order[i], pos = 1:K, p = sequences[i, ])
}) %>%
  mutate(
    judge   = sprintf("J%03d", num),
    product = products[p],
    code    = codes[p],
    date    = format(as.Date("2026-09-21") + (num %% 10), "%Y-%m-%d")
  ) %>%
  left_join(select(judges, judge, bias), by = "judge")

# Liking ratings: one row per descriptor
liking_data <- tasting_sessions %>%
  crossing(descr = colnames(mu)) %>%
  mutate(Value = clip_bounds(mu[cbind(p, match(descr, colnames(mu)))] + bias + rnorm(n(), 0, 1.3), 1, 9)) %>%
  transmute(`Sample Pos.` = pos, `Sample` = product, `Sample Code` = code, Date = date, Judge = judge,
            `Descr.` = descr, Value) %>%
  arrange(Judge, `Sample Pos.`)

# Likert agreement questions
simulate_likert <- function(p_agree) {
  map_chr(p_agree, function(po) {
    pr <- c((1 - po) * 0.35, (1 - po) * 0.65, 0.18, po * 0.60, po * 0.40)
    pr <- pr / sum(pr)
    sample(agreement_levels, 1, prob = pr)
  })
}

base_choices <- tasting_sessions %>%
  transmute(`Sample Pos.` = pos, `Sample` = product, `Sample Code` = code, Date = date, Judge = judge)

repurchase_data <- base_choices %>%
  mutate(Choice = simulate_likert(p_reco[tasting_sessions$p]))

price_data <- base_choices %>%
  mutate(Choice = simulate_likert(p_price[tasting_sessions$p]))

innovation_data <- base_choices %>%
  mutate(Choice = map_chr(tasting_sessions$p, ~ sample(inno_lab, 1, prob = p_inno[.x, ])))

# Areas for improvement: up to 3 selections; more "Taste" selections when overall liking is low
overall_scores <- liking_data %>%
  filter(`Descr.` == "Overall liking") %>%
  select(Judge, `Sample Code`, Liking = Value)

improvement_points <- c("Appearance", "Taste", "Texture", "Price")
improvement_data <- base_choices %>%
  left_join(overall_scores, by = c("Judge", "Sample Code")) %>%
  mutate(none = runif(n()) < plogis((Liking - 6.5) * 0.9 - 0.6)) %>%
  rowwise() %>%
  mutate(choice = list(if (none) "6- None" else {
    w <- c(0.25, 0.35 + 0.1 * (7 - Liking) / 3, 0.25, 0.25 + 0.1 * (p_price[["A"]] - 0.5))
    paste0("6- ", sample(improvement_points, sample(1:3, 1), prob = pmax(w, 0.05)))
  })) %>%
  ungroup() %>%
  select(-none, -Liking) %>%
  unnest(choice) %>%
  rename(Choice = choice)

# Final ranking: rank based on true perceived innovation + noise
ranking_data <- judges %>%
  mutate(order_vec = map(num, ~ order(-(innov_true + rnorm(K, 0, 1.4))))) %>%
  select(judge, order_vec) %>%
  unnest_longer(order_vec, indices_to = "Rank") %>%
  transmute(Judge = judge, `Sample Code` = codes[order_vec], Rank) %>%
  left_join(distinct(tasting_sessions, judge, code, date), by = c("Judge" = "judge", "Sample Code" = "code")) %>%
  left_join(distinct(tasting_sessions, judge, code, pos), by = c("Judge" = "judge", "Sample Code" = "code")) %>%
  transmute(`Sample Pos.` = pos, `Sample Code` = `Sample Code`, Date = date, Judge, Rank)

# ---- Assembly: summary table on left + raw data on right --------------------
side_by_side <- function(left, right) {
  n <- max(nrow(left), nrow(right))
  pad <- function(x) x[c(seq_len(nrow(x)), rep(NA_integer_, n - nrow(x))), , drop = FALSE]
  l <- pad(as.data.frame(left, check.names = FALSE))
  r <- pad(as.data.frame(right, check.names = FALSE))
  sep <- data.frame(` ` = rep(NA, n), check.names = FALSE)
  res <- cbind(l, sep, r)
  names(res) <- c(names(l), " ", names(r))
  res
}

synth_notes <- liking_data %>%
  group_by(Sample = `Sample`, Descr. = `Descr.`) %>%
  summarise(Mean = round(mean(Value), 2), StdDev = round(sd(Value), 2), .groups = "drop")

synth_choice <- function(d) d %>% count(Choice) %>% rename(Total = n)
synth_rank   <- ranking_data %>% group_by(`Sample Code`) %>% summarise(Sum = sum(Rank), .groups = "drop")
synth_demo   <- function(v) judges %>% count(Choice = .data[[v]]) %>% rename(Total = n)
demo_raw     <- function(v) judges %>% transmute(Date = "2026-09-21", Judge = judge, !!v := .data[[v]])

export_workbook <- list(
  "Overall liking"       = side_by_side(synth_notes, liking_data),
  "Major innovation"     = side_by_side(synth_choice(innovation_data), innovation_data),
  "Consume again"        = side_by_side(synth_choice(repurchase_data), repurchase_data),
  "Price"                = side_by_side(synth_choice(price_data), price_data),
  "Area for improvement" = side_by_side(synth_choice(improvement_data), improvement_data),
  "ranking"              = side_by_side(synth_rank, ranking_data),
  "gender"               = side_by_side(synth_demo("gender"), demo_raw("gender")),
  "age"                  = side_by_side(synth_demo("age"), demo_raw("age")),
  "socio_prof"           = side_by_side(synth_demo("socio_prof"), demo_raw("socio_prof"))
)

dir.create("demo/data", showWarnings = FALSE, recursive = TRUE)
write_xlsx(export_workbook, "demo/data/fizz_export_mock.xlsx")
cat("Mock export written: demo/data/fizz_export_mock.xlsx (", n_judges, "judges,", K, "products)\n")
