# analysis_code.R
# Purpose: Basic summaries and plots for the Class 1 survey dataset
# Files expected in the same folder: class1_survey.csv
# Output: console summaries and PNG plots saved to the 'plots' folder

# ---- Setup ----
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
})

# Create output dir
if (!dir.exists("plots")) dir.create("plots")

# ---- Read data ----
# Adjust the filename if needed
fname <- "class1_survey.csv"

df <- readr::read_csv(fname, show_col_types = FALSE)

# ---- Light cleaning (safe, generic) ----
# Trim whitespace in character columns
df <- df %>% mutate(across(where(is.character), ~str_trim(.x)))

std_names <- names(df)
if ("sex_at_birth" %in% std_names) names(df)[names(df)=="sex_at_birth"] <- "sex"
if ("Sex" %in% std_names) names(df)[names(df)=="Sex"] <- "sex"
if ("gender" %in% std_names & !("sex" %in% names(df))) names(df)[names(df)=="gender"] <- "sex"

if ("Age" %in% std_names) names(df)[names(df)=="Age"] <- "age"
if ("AGE" %in% std_names) names(df)[names(df)=="AGE"] <- "age"


if ("age" %in% names(df)) {
  df <- df %>% mutate(age = suppressWarnings(as.numeric(age)))
}

# ---- Summaries ----
cat("==== Data dimensions ====\n")
print(dim(df))

cat("\n==== Missingness by column (count) ====\n")
miss_tbl <- sapply(df, function(x) sum(is.na(x)))
print(miss_tbl)

# Age summary (if available)
if ("age" %in% names(df)) {
  cat("\n==== Age summary ====\n")
  print(summary(df$age))
} else {
  cat("\n(Note) 'age' column not found. Skipping age summary.\n")
}

# Sex/Gender summary (if available)
if ("sex" %in% names(df)) {
  cat("\n==== Sex distribution ====\n")
  print(df %>% count(sex, sort = TRUE))
} else {
  cat("\n(Note) 'sex' column not found. Skipping sex distribution.\n")
}

# ---- Example: frequency tables for all categorical columns ----
# Identify categorical columns (character or factor, with limited unique values)
is_categorical <- function(x) {
  is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 10)
}
cat_cols <- names(df)[sapply(df, is_categorical)]

if (length(cat_cols) > 0) {
  cat("\n==== Frequency tables (first few categorical columns) ====\n")
  for (nm in head(cat_cols, 6)) {
    cat("\n--", nm, "--\n")
    print(df %>% count(.data[[nm]], sort = TRUE))
  }
}

# ---- Plots ----
# Age histogram
if ("age" %in% names(df)) {
  p_age <- ggplot(df, aes(x = age)) +
    geom_histogram(bins = 30) +
    labs(title = "Age distribution", x = "Age", y = "Count")
  ggsave(filename = "plots/age_hist.png", plot = p_age, width = 6, height = 4, dpi = 150)
}

# Sex bar chart
if ("sex" %in% names(df)) {
  p_sex <- ggplot(df, aes(x = sex)) +
    geom_bar() +
    labs(title = "Sex distribution", x = "Sex", y = "Count") +
    coord_flip()
  ggsave(filename = "plots/sex_bar.png", plot = p_sex, width = 6, height = 4, dpi = 150)
}

# Automatic bar plots for up to 3 other categorical columns (besides sex)
other_cats <- setdiff(cat_cols, c("sex"))
for (nm in head(other_cats, 3)) {
  p <- ggplot(df, aes(x = .data[[nm]])) +
    geom_bar() +
    labs(title = paste0(nm, " distribution"), x = nm, y = "Count") +
    coord_flip()
  out <- paste0("plots/", nm, "_bar.png")
  ggsave(filename = out, plot = p, width = 6, height = 4, dpi = 150)
}

cat("\nDone. Plots saved in the 'plots' folder.\n")

