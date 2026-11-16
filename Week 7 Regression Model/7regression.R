# ============================================================
# 1. Install and load packages
# ============================================================

packages <- c("plm", "stargazer", "knitr")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(plm)


# ============================================================
# 2. Import and prepare data
# ============================================================

input_file <- path.expand(
  "~/Desktop/CHL5233 /armed_conflict/Data/processed/final_data.csv"
)

if (!file.exists(input_file)) {
  stop("Cannot find the CSV file: ", input_file)
}

finaldata <- read.csv(input_file)

# Check required columns
required_vars <- c(
  "iso", "year", "maternal_mortality", "under5_mortality",
  "infant_mortality", "neonatal_mortality", "armed_conflict",
  "gdp_1000", "oecd", "pop_dens", "urban", "age_dep",
  "male_edu", "temp", "rainfall_1000", "earthquake", "drought"
)

missing_vars <- setdiff(required_vars, names(finaldata))

if (length(missing_vars) > 0) {
  stop("Missing columns: ", paste(missing_vars, collapse = ", "))
}

# Treat country codes as categories
finaldata$iso <- factor(finaldata$iso)

# Check that each country-year occurs only once
if (anyDuplicated(finaldata[c("iso", "year")]) > 0) {
  stop("Duplicate country-year rows found.")
}

head(finaldata)


# ============================================================
# 3. Country fixed effects: lm() versus plm()
#    Original GDP specification from the slides
# ============================================================

lmmod <- lm(
  maternal_mortality ~ -1 + armed_conflict + gdp_1000 +
    oecd + pop_dens + urban + age_dep + male_edu +
    temp + rainfall_1000 + earthquake + drought + iso,
  data = finaldata
)

plmmod <- plm(
  maternal_mortality ~ armed_conflict + gdp_1000 +
    oecd + pop_dens + urban + age_dep + male_edu +
    temp + rainfall_1000 + earthquake + drought,
  index = c("iso", "year"),
  model = "within",
  effect = "individual",
  data = finaldata
)

summary(lmmod)
summary(plmmod)


# ============================================================
# 4. Country AND year fixed effects: lm() versus plm()
# ============================================================

lmmod2 <- lm(
  maternal_mortality ~ -1 + armed_conflict + gdp_1000 +
    oecd + pop_dens + urban + age_dep + male_edu +
    temp + rainfall_1000 + earthquake + drought +
    iso + as.factor(year),
  data = finaldata
)

plmmod2 <- plm(
  maternal_mortality ~ armed_conflict + gdp_1000 +
    oecd + pop_dens + urban + age_dep + male_edu +
    temp + rainfall_1000 + earthquake + drought,
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata
)

summary(lmmod2)
summary(plmmod2)


# ============================================================
# 5. Compare coefficients from the two-way models
# ============================================================

lm_coef <- coef(lmmod2)
plm_coef <- coef(plmmod2)

coef_table <- merge(
  data.frame(
    Variable = names(lm_coef),
    LM = unname(lm_coef)
  ),
  data.frame(
    Variable = names(plm_coef),
    PLM = unname(plm_coef)
  ),
  by = "Variable",
  all = TRUE
)

# Hide country and year indicator coefficients
hide_terms <- grepl(
  "^iso|^as\\.factor\\(year\\)|^\\(Intercept\\)$",
  coef_table$Variable
)

coef_table <- coef_table[!hide_terms, ]

coef_table$Difference <- coef_table$LM - coef_table$PLM

print(
  knitr::kable(
    coef_table,
    digits = 6,
    caption = "Coefficient comparison: country and year fixed effects"
  )
)


# ============================================================
# 6. In-class assignment: log-transform GDP
# ============================================================

# Observed GDP values must be positive for log()
if (any(
  !is.na(finaldata$gdp_1000) &
    (!is.finite(finaldata$gdp_1000) | finaldata$gdp_1000 <= 0)
)) {
  stop("GDP contains nonpositive or infinite values. Check the data.")
}

finaldata$log_gdp <- log(finaldata$gdp_1000)

summary(finaldata$log_gdp)


# ============================================================
# 7. Define common predictors
#    Country and year effects are handled by plm()
# ============================================================

preds <- ~ armed_conflict + log_gdp + oecd + pop_dens +
  urban + age_dep + male_edu + temp + rainfall_1000 +
  earthquake + drought


# ============================================================
# 8. Fit all four mortality models
# ============================================================

maternal_mod <- plm(
  update(preds, maternal_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)

under5_mod <- plm(
  update(preds, under5_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)

infant_mod <- plm(
  update(preds, infant_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)

neonatal_mod <- plm(
  update(preds, neonatal_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)

# Number of observations used by each model
print(
  sapply(
    list(
      Maternal = maternal_mod,
      Under_five = under5_mod,
      Infant = infant_mod,
      Neonatal = neonatal_mod
    ),
    nobs
  )
)


# ============================================================
# 9. Create an HTML table on the Desktop
# ============================================================

output_file <- path.expand("~/Desktop/mortality_models.html")

# Capture the regression table as HTML
table_html <- capture.output(
  stargazer::stargazer(
    maternal_mod, under5_mod, infant_mod, neonatal_mod,
    type = "html",
    title = "Mortality and binary armed conflict",
    dep.var.labels.include = FALSE,
    column.labels = c("Maternal", "Under-five", "Infant", "Neonatal"),
    model.numbers = FALSE,
    digits = 3,
    omit.stat = c("f", "ser"),
    add.lines = list(
      c("Country fixed effects", "Yes", "Yes", "Yes", "Yes"),
      c("Year fixed effects", "Yes", "Yes", "Yes", "Yes"),
      c("GDP specification", "Log GDP", "Log GDP", "Log GDP", "Log GDP")
    ),
    notes = "Conventional model standard errors in parentheses."
  )
)

# Save a complete HTML document
writeLines(
  c(
    "<!DOCTYPE html>",
    "<html lang='en'>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<title>Mortality regression models</title>",
    "<style>",
    "body { font-family: Georgia, serif; margin: 40px; }",
    "table { border-collapse: collapse; margin: 20px auto; }",
    "td, th { padding: 6px 12px; }",
    "</style>",
    "</head>",
    "<body>",
    table_html,
    "</body>",
    "</html>"
  ),
  con = output_file
)


# ============================================================
# 10. Confirm the file exists and open it
# ============================================================

if (!file.exists(output_file)) {
  stop("The HTML file was not created.")
}

message("HTML table saved to: ", output_file)

# Open in the default browser on your Mac
system2("open", args = shQuote(output_file))

# Reveal the file in Finder
system2("open", args = c("-R", shQuote(output_file)))