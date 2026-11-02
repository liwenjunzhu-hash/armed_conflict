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

# 2. Choose your CSV when prompted (or replace with its path)
input_file <- file.choose()
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



    

# Assignment 2: maternal mortality trends, 2000-2017
# Select countries by their ENDPOINT increase, then retain all their years.
# 2017 is used because maternal mortality is missing in 2018 and 2019
# throughout the supplied dataset.


packages <- c("dplyr", "ggplot2")
needed <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(needed)) install.packages(needed)
library(dplyr)
library(ggplot2)

# Prefer the project copy so source() does not stop for a file chooser.
input_file <- file.path("Data", "processed", "final_data.csv")
if (!file.exists(input_file)) {
  message("Could not find Data/processed/final_data.csv from the current working directory. Choose the CSV file.")
  input_file <- file.choose()
}
if (!file.exists(input_file)) stop("Input CSV does not exist: ", input_file)
conflict <- read.csv(input_file)
stopifnot(all(c("iso", "year", "maternal_mortality") %in% names(conflict)),
          !anyDuplicated(conflict[c("iso", "year")]))

# Explicitly match year 2000 rather than assuming the first row is 2000.
trends <- conflict |>
  select(iso, year, maternal_mortality) |>
  filter(year >= 2000, year <= 2017) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = maternal_mortality -
           maternal_mortality[match(2000, year)]) |>
  ungroup()

increased_countries <- trends |>
  filter(year == 2017, !is.na(diff_matmor), diff_matmor > 0) |>
  select(iso, maternal_mortality, diff_matmor) |>
  mutate(mortality_2000 = maternal_mortality - diff_matmor) |>
  rename(mortality_2017 = maternal_mortality) |>
  select(iso, mortality_2000, mortality_2017, diff_matmor)
print(increased_countries)
if (!nrow(increased_countries)) stop("No countries have a valid endpoint increase.")

# Keep every year for selected countries, including intervening decreases.
plot_data <- trends |>
  semi_join(increased_countries, by = "iso")

country_names <- c(BRN = "Brunei", CAN = "Canada",
  DOM = "Dominican Republic", HTI = "Haiti", JAM = "Jamaica",
  KWT = "Kuwait", LBN = "Lebanon", LBY = "Libya",
  LCA = "Saint Lucia", MUS = "Mauritius", SYR = "Syria",
  USA = "United States", VEN = "Venezuela")
codes <- sort(unique(plot_data$iso))
legend_labels <- country_names[codes]
legend_labels[is.na(legend_labels)] <- codes[is.na(legend_labels)]
colors <- setNames(grDevices::hcl.colors(length(codes), "Dark 3"), codes)

p <- ggplot(plot_data,
            aes(x = year, y = maternal_mortality, color = iso, group = iso)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  scale_color_manual(values = colors, breaks = codes,
                     labels = unname(legend_labels)) +
  scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2017)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Maternal mortality trends, 2000-2017",
       subtitle = paste(nrow(increased_countries),
                        "countries with higher maternal mortality in 2017 than in 2000"),
       x = "Year", y = "Maternal mortality (dataset units)", color = "Country",
       caption = "Selection uses the 2017 minus 2000 difference; all intervening years are shown.\nMaternal mortality is unavailable for 2018-2019 in this dataset.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")

# If confirmed by your source/codebook, replace the y-axis label with
# "Maternal deaths per 100,000 live births".
print(p)
ggsave(file.path(dirname(input_file), "maternal_mortality_trends.pdf"),
       plot = p, width = 11, height = 6.5)
ggsave(file.path(dirname(input_file), "maternal_mortality_trends.png"),
       plot = p, width = 11, height = 6.5, dpi = 300)
