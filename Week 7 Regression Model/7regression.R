install.packages("plm", repos = "https://cloud.r-project.org")

final_data <- read.csv(
  "/Users/julie/Desktop/CHL5233 /armed_conflict/Data/processed/final_data.csv"
)

# Check that the data loaded
head(final_data)
names(final_data)

# Treat country codes as categories
final_data$iso <- factor(final_data$iso)


lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso, final_data)

summary(lmmod)


library(plm)
plmmod <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso"), model = "within", data = finaldata)

####### lm()
lmmod2 <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso + as.factor(year), 
            data = finaldata)
summary(lmmod2)

####### plm()

plmmod2 <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
summary(lmmod2)

# Extract coefficients
lm_coef  <- coef(lmmod2)
plm_coef <- coef(plmmod2)

# Match coefficients by variable name
coef_table <- merge(
  data.frame(Variable = names(lm_coef),
             LM = unname(lm_coef)),
  data.frame(Variable = names(plm_coef),
             PLM = unname(plm_coef)),
  by = "Variable",
  all = TRUE
)

# Remove individual country intercepts for a shorter comparison
coef_table <- coef_table[!grepl("^iso", coef_table$Variable), ]

# Display the table
knitr::kable(
  coef_table,
  digits = 4,
  caption = "Comparison of model coefficients"
)