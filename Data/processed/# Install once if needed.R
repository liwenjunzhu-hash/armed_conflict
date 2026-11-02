# Assignment 1: descriptive Table 1 exported to Word
# Uses the uploaded dataset as supplied: 3,720 country-years, 2000-2019.
# armed_conflict is used AS STORED. Verify its lag definition in your
# preparation code; do not lag it again unless required.
# This describes the full supplied sample, not an outcome-specific
# complete-case sample. Missing observations are retained.

# 1. Install packages once if needed
packages <- c("flextable", "officer")
needed <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(needed)) install.packages(needed)

# 2. Read the processed CSV from the project root. If it is elsewhere,
#    choose it interactively.
input_file <- file.path("Data", "processed", "final_data.csv")
if (!file.exists(input_file)) {
  message("Could not find Data/processed/final_data.csv from the current working directory. Choose the CSV file.")
  input_file <- file.choose()
}
if (!file.exists(input_file)) stop("Input CSV does not exist: ", input_file)
dat <- read.csv(input_file, na.strings = c("", "NA"),
                stringsAsFactors = FALSE, check.names = FALSE)

continuous <- c("gdp_1000", "pop_dens", "urban", "age_dep", "male_edu",
                "temp", "rainfall_1000", "maternal_mortality",
                "infant_mortality", "neonatal_mortality", "under5_mortality")
binary <- c("oecd", "earthquake", "drought")
required <- c("iso", "year", continuous, binary, "armed_conflict")
if (!all(required %in% names(dat))) {
  stop("Missing columns: ", paste(setdiff(required, names(dat)), collapse = ", "))
}
stopifnot(!anyNA(dat$iso), !anyNA(dat$year),
          !anyDuplicated(dat[c("iso", "year")]))
for (v in c(binary, "armed_conflict")) {
  if (!all(na.omit(dat[[v]]) %in% c(0, 1))) stop(v, " must be coded 0/1.")
}
if (anyNA(dat$armed_conflict)) stop("Exposure has missing values; define how to display them.")
if (!all(vapply(dat[continuous], is.numeric, logical(1)))) {
  stop("Continuous variables must be numeric.")
}

# 3. Labels: retain stored scales. Confirm units against the codebook
# before adding units to these labels (especially pop_dens and urban).
labels <- c(
  year = "Calendar year", gdp_1000 = "GDP (gdp_1000 scale)",
  pop_dens = "Population density (stored scale)",
  urban = "Urban measure (stored scale)", age_dep = "Age dependency ratio",
  male_edu = "Male education", temp = "Temperature",
  rainfall_1000 = "Rainfall (rainfall_1000 scale)",
  maternal_mortality = "Maternal mortality", infant_mortality = "Infant mortality",
  neonatal_mortality = "Neonatal mortality", under5_mortality = "Under-five mortality",
  oecd = "OECD membership", earthquake = "Earthquake", drought = "Drought",
  armed_conflict = "Armed conflict"
)

# 4. Summary functions. Use median [Q1, Q3] for continuous variables
# throughout, accommodating the skewed GDP and mortality distributions.
median_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("--")
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
  sprintf("%.1f [%.1f, %.1f]", q[2], q[1], q[3])
}
yes_summary <- function(x) {
  denominator <- sum(!is.na(x))
  if (!denominator) return("--")
  n <- sum(x == 1, na.rm = TRUE)
  sprintf("%d (%.1f%%)", n, 100 * n / denominator)
}
groups <- list(Overall = dat,
               No_conflict = dat[dat$armed_conflict == 0, , drop = FALSE],
               Conflict = dat[dat$armed_conflict == 1, , drop = FALSE])

rows <- list()
add_row <- function(label, values) {
  rows[[length(rows) + 1L]] <<- data.frame(
    Characteristic = label, Overall = values[1],
    No_conflict = values[2], Conflict = values[3],
    stringsAsFactors = FALSE, row.names = NULL)
}
summarize_groups <- function(fun) vapply(groups, fun, character(1))
add_row("Country-years, n", summarize_groups(function(d) as.character(nrow(d))))
add_row("Unique countries/territories, n",
        summarize_groups(function(d) as.character(length(unique(d$iso)))))
add_row("  Missing country/territory identifier, n",
        summarize_groups(function(d) as.character(sum(is.na(d$iso)))))
add_row("Calendar year, range", summarize_groups(function(d) {
  if (!nrow(d)) return("--")
  paste(range(d$year), collapse = "-" )
}))
add_row("  Missing calendar year, n",
        summarize_groups(function(d) as.character(sum(is.na(d$year)))))

# Exposure shown as a row as well, so its overall prevalence and
# missing count are explicit. Within-group prevalence is 0% or 100%.
for (v in c("armed_conflict", continuous, binary)) {
  categorical <- v %in% c(binary, "armed_conflict")
  suffix <- if (categorical) ": yes, n (%)" else ", median [Q1, Q3]"
  add_row(paste0(labels[[v]], suffix), summarize_groups(function(d) {
    if (categorical) yes_summary(d[[v]]) else median_iqr(d[[v]])
  }))
  add_row("  Missing, n", summarize_groups(function(d) {
    as.character(sum(is.na(d[[v]])))
  }))
}

table_data <- do.call(rbind, rows)
names(table_data) <- c("Characteristic", "Overall", "No conflict", "Conflict")

# 5. Export the descriptive table to Word in the current working directory.
table <- flextable::flextable(table_data)
table <- flextable::theme_booktabs(table)
table <- flextable::bold(table, part = "header")
table <- flextable::autofit(table)

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Table 1. Descriptive characteristics", style = "heading 1")
doc <- flextable::body_add_flextable(doc, table)
output_file <- "Table1_descriptive.docx"
print(doc, target = output_file)
message("Wrote ", normalizePath(output_file, mustWork = TRUE))



