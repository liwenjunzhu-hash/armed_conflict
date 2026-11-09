install.packages("plm", repos = "https://cloud.r-project.org")
install.packages("stargazer", repos = "https://cloud.r-project.org")

#import data
final_data <- read.csv(
  "/Users/julie/Desktop/CHL5233 /armed_conflict/Data/processed/final_data.csv"
)

# Check that the data loaded
head(final_data)
names(final_data)

# Treat country codes as categories (iso)
final_data$iso <- factor(final_data$iso)

################

# Natural logarithms need to be positive values, so set restrictions
finaldata$log_gdp <- log(finaldata$gdp_1000)
positive_gdp <- is.finite(finaldata$gdp_1000) &
                finaldata$gdp_1000 > 0


# Specify the common predictors
preds <- as.formula(~ armed_conflict + log_gdp + oecd + pop_dens +
  urban + age_dep + male_edu + temp + rainfall_1000 +
  earthquake + drought)

# Fit four models with country AND year fixed effects, using plm
###1
maternal_mod <- plm(
  update(preds, maternal_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)

###2
under5_mod <- plm(
  update(preds, under5_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)
###3
infant_mod <- plm(
  update(preds, infant_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)
###4
neonatal_mod <- plm(
  update(preds, neonatal_mortality ~ .),
  data = finaldata,
  index = c("iso", "year"),
  model = "within",
  effect = "twoways"
)

output_file <- path.expand(
  "~/Desktop/CHL5233 /mortality_models.html"
)

library(stargazer)
stargazer(
  maternal_mod, under5_mod, infant_mod, neonatal_mod,
  type = "html",
  out = output_file,
  title = "Mortality and binary armed conflict",
  dep.var.labels.include = FALSE,
  column.labels = c("Maternal", "Under-five", "Infant", "Neonatal"),
  model.numbers = FALSE,
  digits = 3,
  omit.stat = c("f", "ser"),
  add.lines = list(
    c("Country fixed effects", "Yes", "Yes", "Yes", "Yes"),
    c("Year fixed effects", "Yes", "Yes", "Yes", "Yes")
  ),
  notes = "Conventional model standard errors in parentheses."
)

# 7. Open the table
browseURL(output_file)