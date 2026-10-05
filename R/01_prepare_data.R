# =============================================================================
#  STEP 1 – Prepare data: FIZZ export -> two clean CSV files
#
#  Input:   FIZZ export (.ods or .xlsx), placed in <study_folder>/data/
#  Outputs: <study_folder>/data/product_responses.csv (1 row = 1 judge x 1 product)
#           <study_folder>/data/final_ranking.csv     (1 row = 1 judge)
#
#  This script contains NO study-specific names: everything is in config.R.
#  For a new study or edition, only update the source() line below
#  (and the corresponding config.R file).
# =============================================================================

source("demo/config.R")     # <- ONLY LINE TO CHANGE: dataset folder (here fictional demo)

library(tidyverse)

# ---- Reading utilities ------------------------------------------------------
# A FIZZ export is a workbook where each sheet contains summary tables on the LEFT
# and raw data on the RIGHT (one row per response).
# Raw data starts at the anchor column ("Sample Pos." or "Date" for demographics).
# We locate this column and extract the subsequent columns.

normalize <- function(x) str_squish(str_replace_all(x, "[\u2019\u2018]", "'"))
# FIZZ sometimes prefixes categories with question numbers ("6- Appearance"): strip them
clean_prefix <- function(x) normalize(str_remove(x, "^\\d+\\s*-\\s+"))

read_sheet <- function(sheet, anchor, col_names) {
  if (!file.exists(export_file)) stop("File not found: ", export_file)
  is_ods <- str_detect(tolower(export_file), "\\.ods$")
  d <- if (is_ods) {
    if (!requireNamespace("readODS", quietly = TRUE))
      stop("To read .ods files: install.packages('readODS'), or save the export as .xlsx")
    readODS::read_ods(export_file, sheet = sheet, .name_repair = "minimal")
  } else {
    readxl::read_excel(export_file, sheet = sheet, .name_repair = "minimal")
  }
  i <- which(names(d) == anchor)[1]
  if (is.na(i)) stop("Anchor column '", anchor, "' not found in sheet '", sheet, "'")
  d <- as.data.frame(d)[, i:(i + length(col_names) - 1), drop = FALSE]
  names(d) <- col_names
  as_tibble(d) %>% filter(if_any(everything(), ~ !is.na(.x)))
}

choice_cols <- c("pos", "product_name", "code", "date", "judge", "choice")

# Applies a lookup dictionary (keys = FIZZ labels), validating unknown categories
apply_dict <- function(values, dict, item_name) {
  keys <- normalize(names(dict))
  unknowns <- setdiff(unique(values), keys)
  if (length(unknowns) > 0)
    stop("Unknown categories for '", item_name, "': ", paste(unknowns, collapse = " | "),
         "\n  -> please add to config.R (exact labels from FIZZ export).")
  unname(dict[match(values, keys)])
}

# ---- 1. Read sheets ---------------------------------------------------------
app <- read_sheet(sheets[["liking"]], "Sample Pos.",
                  c("pos", "product_name", "code", "date", "judge", "descr", "val")) %>%
  mutate(code = as.character(code), descr = normalize(descr), val = as.numeric(val))

cl <- read_sheet(sheets[["ranking"]], "Sample Pos.",
                 c("pos", "code", "date", "judge", "rank")) %>%
  mutate(code = as.character(code), rank = as.numeric(rank))

K <- n_distinct(app$code)
codes_data <- sort(unique(app$code))
if (!all(codes_data %in% names(product_names)))
  stop("Product codes missing from product_names (config.R): ",
       paste(setdiff(codes_data, names(product_names)), collapse = ", "))

# ---- 2. Complete judges -----------------------------------------------------
# A paired analysis requires every judge to have tasted and ranked all K products.
judges_app <- app %>% group_by(judge) %>% summarise(n = n_distinct(code), .groups = "drop") %>% filter(n == K) %>% pull(judge)
judges_cl  <- cl %>% group_by(judge) %>% summarise(ok = n() == K && setequal(rank, 1:K), .groups = "drop") %>% filter(ok) %>% pull(judge)
demo_sheets <- c("gender", "age", "socio_prof")
demographics <- map(demo_sheets, function(key) {
  read_sheet(sheets[[key]], "Date", c("date", "judge", key)) %>%
    transmute(judge, !!key := normalize(.data[[key]])) %>% distinct(judge, .keep_all = TRUE)
}) %>% reduce(full_join, by = "judge")

complete_judges <- reduce(list(judges_app, judges_cl, demographics$judge), intersect)
all_judges <- union(union(unique(app$judge), unique(cl$judge)), demographics$judge)
excluded_judges <- setdiff(all_judges, complete_judges)
cat(sprintf("Retained judges: %d | Excluded (incomplete responses or ranking): %d %s\n",
            length(complete_judges), length(excluded_judges),
            if (length(excluded_judges)) paste0("(", paste(excluded_judges, collapse = ", "), ")") else ""))

# ---- 3. Build 'judge x product' table ---------------------------------------
# 3a. Sensory liking scores (one column per attribute)
ignored_descriptors <- setdiff(unique(app$descr), normalize(names(descriptors)))
if (length(ignored_descriptors))
  message("Descriptors ignored (absent from config.R): ", paste(ignored_descriptors, collapse = ", "))

app_wide <- app %>%
  filter(judge %in% complete_judges, descr %in% normalize(names(descriptors))) %>%
  mutate(descr = unname(descriptors[match(descr, normalize(names(descriptors)))])) %>%
  pivot_wider(id_cols = c(judge, code), names_from = descr, values_from = val)

serving_order <- app %>% filter(judge %in% complete_judges) %>% distinct(judge, code, pos) %>% rename(serving_order = pos)
base <- app_wide %>% left_join(serving_order, by = c("judge", "code"))

# 3b. Likert agreement questions (repurchase intent, price acceptance...)
for (v in names(likert_labels)) {
  d <- read_sheet(sheets[[v]], "Sample Pos.", choice_cols) %>%
    mutate(code = as.character(code), choice = clean_prefix(choice))
  d[[v]] <- apply_dict(d$choice, agreement_scale, likert_labels[[v]])
  base <- base %>% left_join(distinct(d, judge, code, .data[[v]]), by = c("judge", "code"))
}

# 3c. Major innovation (single choice)
inn <- read_sheet(sheets[["innovation"]], "Sample Pos.", choice_cols) %>%
  mutate(code = as.character(code), choice = clean_prefix(choice))
inn$innovation <- apply_dict(inn$choice, innovation_labels, "major innovation")
base <- base %>% left_join(distinct(inn, judge, code, innovation), by = c("judge", "code"))

# 3d. Areas for improvement (multiple choices, up to 3) -> one 0/1 column per issue
am <- read_sheet(sheets[["improvement"]], "Sample Pos.", choice_cols) %>%
  mutate(code = as.character(code), choice = clean_prefix(choice))
am$point <- apply_dict(am$choice, improvements_fizz, "areas for improvement")
am_wide <- am %>% distinct(judge, code, point) %>% mutate(val = 1L) %>%
  pivot_wider(names_from = point, values_from = val, names_prefix = "improve_", values_fill = 0L)
base <- base %>% left_join(am_wide, by = c("judge", "code"))
for (cn in names(improvements)) {
  if (!cn %in% names(base)) base[[cn]] <- 0L
  base[[cn]] <- replace_na(base[[cn]], 0L)
}

# 3e. Final formatting
judge_num <- function(x) as.integer(str_extract(x, "\\d+"))
base <- base %>%
  mutate(judge = judge_num(judge), product = as.integer(code)) %>%
  select(judge, product, serving_order,
         any_of(names(criteria)), all_of(names(likert_labels)), innovation, all_of(names(improvements))) %>%
  arrange(judge, serving_order)

# ---- 4. Ranking + demographics ----------------------------------------------
ranking <- cl %>% filter(judge %in% complete_judges) %>%
  pivot_wider(id_cols = judge, names_from = rank, values_from = code, names_prefix = "pos") %>%
  left_join(demographics, by = "judge") %>%
  mutate(judge = judge_num(judge), across(starts_with("pos"), as.integer)) %>%
  select(judge, gender, age, socio_prof, all_of(paste0("pos", 1:K))) %>%
  arrange(judge)

# ---- 5. Validation and export -----------------------------------------------
stopifnot(!anyNA(base), !anyNA(ranking), n_distinct(base$judge) == nrow(ranking))
cat(sprintf("%d products | %d judge x product rows | %d judges\n", K, nrow(base), nrow(ranking)))
cat("Tastings by serving position (experimental design balance):\n")
print(table(base$product, base$serving_order))

dir.create(file.path(study_folder, "data"), showWarnings = FALSE, recursive = TRUE)
write_csv(base,    file.path(study_folder, "data", "product_responses.csv"))
write_csv(ranking, file.path(study_folder, "data", "final_ranking.csv"))
cat("\nFiles successfully written to", file.path(study_folder, "data"), "\n")
