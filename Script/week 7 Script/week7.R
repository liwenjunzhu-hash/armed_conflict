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
