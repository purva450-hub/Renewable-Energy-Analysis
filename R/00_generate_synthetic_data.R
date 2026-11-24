# =============================================================================
# Generate the SYNTHETIC dataset used in this project.
# These are simulated values, NOT official or real-world energy statistics.
# Country names are generic labels (Country A ... Country J) on purpose.
# Seeded, so re-running gives the identical file.   Run: source("R/00_generate_synthetic_data.R")
# =============================================================================
set.seed(2026)

countries <- paste("Country", LETTERS[1:10])
sources   <- c("Solar", "Wind", "Hydro", "Biomass")
years     <- 2015:2024

d <- expand.grid(Country = countries, Energy_Source = sources, Year = years,
                 stringsAsFactors = FALSE)                    # 10 x 4 x 10 = 400 rows

country_scale <- setNames(exp(rnorm(10, 0, 0.7)), countries)  # size of each country's sector
base_mw  <- c(Solar = 900,  Wind = 800,  Hydro = 1500, Biomass = 400)   # 2015 capacity (MW)
growth   <- c(Solar = 0.18, Wind = 0.12, Hydro = 0.02, Biomass = 0.05)  # yearly growth
cap_fac  <- c(Solar = 0.20, Wind = 0.32, Hydro = 0.42, Biomass = 0.55)  # capacity factor

n <- nrow(d)
cap <- base_mw[d$Energy_Source] * country_scale[d$Country] *
       (1 + growth[d$Energy_Source])^(d$Year - 2015) * exp(rnorm(n, 0, 0.15))
prod <- cap * 8.76 * cap_fac[d$Energy_Source] * (1 + rnorm(n, 0, 0.05))   # GWh = MW x 8.76 x CF
cons <- prod * runif(n, 0.85, 1.05)

d$Renewable_Energy_Production_GWh <- round(unname(prod), 2)
d$Estimated_Renewable_Energy_Consumption_GWh <- round(unname(cons), 2)
d$Installed_Capacity_MW <- round(unname(cap), 2)

d$Estimated_Renewable_Energy_Consumption_GWh[sample(n, 8)] <- NA          # 8 missing values

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
write.csv(d, "data/raw/renewable_energy_synthetic_dataset.csv", row.names = FALSE, na = "")
cat("Written:", nrow(d), "rows,", ncol(d), "columns; missing consumption:",
    sum(is.na(d$Estimated_Renewable_Energy_Consumption_GWh)), "\n")
