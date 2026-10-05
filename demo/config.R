# =============================================================================
#  CONFIGURATION – Demo (100% fictional dataset)
#  This file contains EVERYTHING that changes from one study to another:
#  export sheet names, response labels, products, plot labels.
#  Scripts in the R/ directory do not need to be modified.
# =============================================================================

study_name         <- "Demo Study"
study_folder       <- "demo"                                           # study directory
export_file        <- file.path(study_folder, "data", "fizz_export_mock.xlsx") # FIZZ export (.xlsx or .ods)
ranking_label      <- "recipes"                                        # "most innovative in …"

# ---- Export sheet names ----------------------------------------------------
sheets <- c(liking      = "Overall liking",
            innovation  = "Major innovation",
            repurchase  = "Consume again",
            price       = "Price",
            improvement = "Area for improvement",
            ranking     = "ranking",
            gender      = "gender",
            age         = "age",
            socio_prof  = "socio_prof")

# ---- Descriptors in liking sheet: export label -> column name --------------
descriptors <- c("Convenience"    = "convenience",
                 "Overall liking" = "overall_liking",
                 "Appearance"     = "appearance",
                 "Aroma"          = "aroma",
                 "Texture"        = "texture",
                 "Taste"          = "taste")

# ---- 5-point agreement scale -> value 1 to 5 -------------------------------
# EXACT labels from export. Apostrophes and spaces are normalized.
agreement_scale <- c("Strongly disagree"             = 1,
                     "Disagree"                      = 2,
                     "Neither agree nor disagree"    = 3,
                     "Agree"                         = 4,
                     "Strongly agree"                = 5)

# ---- Major innovation: export label -> display label in reports ------------
innovation_labels <- c("Recipe"    = "Original recipe",
                       "Format"    = "New format",
                       "Usage"     = "New usage or consumption moment",
                       "Nutrition" = "Nutritional benefit",
                       "Packaging" = "Packaging")

# ---- Areas for improvement: export label -> column name --------------------
improvements_fizz <- c("Appearance" = "appearance",
                       "Taste"      = "taste",
                       "Texture"    = "texture",
                       "Price"      = "price",
                       "None"       = "none")

# ---- Display labels for plots ----------------------------------------------
criteria <- c(convenience    = "Convenience",
              overall_liking = "Overall liking",
              appearance     = "Appearance",
              aroma          = "Aroma",
              texture        = "Texture",
              taste          = "Taste")

likert_labels <- c(repurchase = "Want to consume again thanks to its innovation",
                   price      = "Willing to buy at the indicated price")

improvements <- c(improve_appearance = "Appearance",
                 improve_taste      = "Taste",
                 improve_texture    = "Texture",
                 improve_price      = "Price",
                 improve_none       = "No improvement needed")

# ---- Products: sample code -> name (fictional) -----------------------------
product_names <- c("101" = "Product A",
                   "102" = "Product B",
                   "103" = "Product C",
                   "104" = "Product D",
                   "105" = "Product E")
