# =============================================================================
# Renewable Energy Utilization Analytics - NIET Greater Noida (BTech CSE-DS)
# Foundation of Data Science (CCSDS0301) - PBL Final Report
# Author: Purva Arora (2501331540208)
#
# Base R only (no extra packages needed). Run from RStudio after opening
# RenewableEnergy.Rproj, so the working directory is the project root:
#   source("R/analysis.R")
#
# INPUT : one CSV file in data/raw/ (see data/raw/README.md)
# OUTPUT: data/processed/cleaned_dataset.csv
#         outputs/tables/*.csv  and  outputs/key_results.txt
#         outputs/figures/*.png (five charts)
# Every number printed or saved below is computed from the CSV; nothing is typed in.
# =============================================================================

options(stringsAsFactors = FALSE, width = 100)

# ---- 0. Settings -------------------------------------------------------------
RAW_DIR  <- "data/raw"
PROC_DIR <- "data/processed"
FIG_DIR  <- "outputs/figures"
TAB_DIR  <- "outputs/tables"

COL_COUNTRY <- "Country"
COL_SOURCE  <- "Energy_Source"
COL_YEAR    <- "Year"
COL_PROD    <- "Renewable_Energy_Production_GWh"
COL_CONS    <- "Estimated_Renewable_Energy_Consumption_GWh"
COL_CAP     <- "Installed_Capacity_MW"
REQUIRED    <- c(COL_COUNTRY, COL_SOURCE, COL_YEAR, COL_PROD, COL_CONS, COL_CAP)

for (d in c(PROC_DIR, FIG_DIR, TAB_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
log_lines <- character()
note <- function(...) { s <- paste0(...); cat(s, "\n"); log_lines <<- c(log_lines, s) }

# ---- 1. Locate and load the dataset -----------------------------------------
files <- list.files(RAW_DIR, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
if (length(files) != 1) {
  stop("Put exactly ONE .csv file in ", RAW_DIR, " (found ", length(files), "). See data/raw/README.md.")
}
note("Dataset file: ", files)
df <- read.csv(files, check.names = FALSE)

missing_cols <- setdiff(REQUIRED, names(df))
if (length(missing_cols) > 0) {
  stop("The dataset does not contain the expected columns: ", paste(missing_cols, collapse = ", "),
       "\nColumns found: ", paste(names(df), collapse = ", "),
       "\nEdit the COL_* settings at the top of this script to match your file.")
}

# ---- 2. Inspection -----------------------------------------------------------
note("\n== Inspection ==")
note("Records: ", nrow(df), " | Columns: ", ncol(df))
note("Column names: ", paste(names(df), collapse = ", "))
note("Countries: ", length(unique(df[[COL_COUNTRY]])),
     " | Years: ", min(df[[COL_YEAR]], na.rm = TRUE), "-", max(df[[COL_YEAR]], na.rm = TRUE),
     " (", length(unique(df[[COL_YEAR]])), " distinct)",
     " | Energy sources: ", paste(sort(unique(df[[COL_SOURCE]])), collapse = ", "))
str(df)

missing_per_col <- colSums(is.na(df))
note("\nMissing values per column:")
print(missing_per_col)
note("Total missing values: ", sum(missing_per_col))
n_dup <- sum(duplicated(df))
note("Exact duplicate rows: ", n_dup)
write.csv(data.frame(column = names(missing_per_col), missing = as.integer(missing_per_col)),
          file.path(TAB_DIR, "missing_values_before_cleaning.csv"), row.names = FALSE)

# ---- 3. Preprocessing: group-median imputation (Country x Energy_Source) -----
n_missing_cons <- sum(is.na(df[[COL_CONS]]))
overall_median_cons <- median(df[[COL_CONS]], na.rm = TRUE)
df[[COL_CONS]] <- ave(df[[COL_CONS]], df[[COL_COUNTRY]], df[[COL_SOURCE]],
                      FUN = function(v) { v[is.na(v)] <- median(v, na.rm = TRUE); v })
still_missing <- is.na(df[[COL_CONS]])
df[[COL_CONS]][still_missing] <- overall_median_cons       # fallback if a whole group was empty
note("\n== Cleaning ==")
note("Consumption values imputed: ", n_missing_cons,
     " (of which filled by overall-median fallback: ", sum(still_missing), ")")
note("Missing values remaining: ", sum(is.na(df)))
note("Rows retained: ", nrow(df))
write.csv(df, file.path(PROC_DIR, "cleaned_dataset.csv"), row.names = FALSE)

# ---- 4. Descriptive statistics ----------------------------------------------
num_cols <- c(COL_PROD, COL_CONS, COL_CAP)
desc <- do.call(rbind, lapply(num_cols, function(v) {
  x <- df[[v]]
  data.frame(variable = v, n = sum(!is.na(x)), mean = mean(x), median = median(x),
             sd = sd(x), min = min(x), max = max(x))
}))
desc[, -1] <- round(desc[, -1], 2)
note("\n== Descriptive statistics ==")
print(desc, row.names = FALSE)
write.csv(desc, file.path(TAB_DIR, "descriptive_statistics.csv"), row.names = FALSE)

# ---- 5. Exploratory analysis -------------------------------------------------
by_year    <- tapply(df[[COL_PROD]], df[[COL_YEAR]], sum)
by_source  <- sort(tapply(df[[COL_PROD]], df[[COL_SOURCE]], sum), decreasing = TRUE)
by_country <- sort(tapply(df[[COL_PROD]], df[[COL_COUNTRY]], sum), decreasing = TRUE)
yrs <- sort(unique(df[[COL_YEAR]]))
first_year <- yrs[1]; last_year <- yrs[length(yrs)]
growth_pct <- (by_year[as.character(last_year)] / by_year[as.character(first_year)] - 1) * 100
r_cap_prod <- cor(df[[COL_CAP]], df[[COL_PROD]])                # Pearson (default)
cor_mat <- round(cor(df[, num_cols]), 3)

note("\n== Exploratory analysis ==")
note("Total production in ", last_year, " vs ", first_year, ": ", round(growth_pct, 1), "% higher")
note("Source with highest aggregate production: ", names(by_source)[1])
note("Source ranking: ", paste(names(by_source), collapse = " > "))
note("Country with highest aggregate production: ", names(by_country)[1])
note("Correlation, installed capacity vs production (Pearson): ", round(r_cap_prod, 3))

write.csv(data.frame(Year = names(by_year), Total_Production_GWh = as.numeric(by_year)),
          file.path(TAB_DIR, "production_by_year.csv"), row.names = FALSE)
write.csv(data.frame(Energy_Source = names(by_source), Total_Production_GWh = as.numeric(by_source)),
          file.path(TAB_DIR, "production_by_source.csv"), row.names = FALSE)
write.csv(data.frame(Country = names(by_country), Total_Production_GWh = as.numeric(by_country)),
          file.path(TAB_DIR, "production_by_country.csv"), row.names = FALSE)
write.csv(cor_mat, file.path(TAB_DIR, "correlation_matrix.csv"))

# Outlier count per energy source (IQR rule, as drawn by boxplot)
out_tab <- do.call(rbind, lapply(sort(unique(df[[COL_SOURCE]])), function(s) {
  x <- df[[COL_PROD]][df[[COL_SOURCE]] == s]
  data.frame(Energy_Source = s, n = length(x), median = round(median(x), 2),
             outliers = length(boxplot.stats(x)$out))
}))
note("Outliers per source (IQR rule): ",
     paste(out_tab$Energy_Source, out_tab$outliers, sep = "=", collapse = ", "))
write.csv(out_tab, file.path(TAB_DIR, "outliers_by_source.csv"), row.names = FALSE)

# ---- 6. Figures (five charts used in the report) -----------------------------
save_png <- function(name, expr) {
  png(file.path(FIG_DIR, name), width = 1024, height = 768, res = 110)
  on.exit(dev.off())
  par(mar = c(6, 7, 3, 1), mgp = c(4.2, 1, 0))
  expr()
}

save_png("fig1_production_by_year.png", function() {
  plot(as.numeric(names(by_year)), as.numeric(by_year), type = "o", pch = 16, col = "#1f77b4",
       xlab = "Year", ylab = "Production (GWh)", main = "Total Renewable Energy Production by Year")
  grid()
})
save_png("fig2_production_by_source.png", function() {
  tk <- pretty(c(0, max(by_source) * 1.05))
  barplot(as.numeric(by_source), names.arg = names(by_source), col = "#1f77b4", yaxt = "n",
          ylim = range(tk), xlab = "Energy Source", ylab = "Production (GWh)",
          main = "Total Production by Energy Source")
  axis(2, at = tk, labels = format(tk, big.mark = ",", scientific = FALSE), las = 1)
})
save_png("fig3_production_boxplot.png", function() {
  boxplot(df[[COL_PROD]] ~ df[[COL_SOURCE]], col = "white", border = "#1f77b4",
          xlab = "Energy Source", ylab = "Production (GWh)", main = "Production Distribution by Energy Source")
})
save_png("fig4_production_histogram.png", function() {
  hist(df[[COL_PROD]], breaks = 20, col = "#1f77b4", border = "white",
       xlab = "Production (GWh)", ylab = "Frequency", main = "Distribution of Renewable Energy Production")
})
save_png("fig5_capacity_vs_production.png", function() {
  plot(df[[COL_CAP]], df[[COL_PROD]], pch = 16, col = adjustcolor("#1f77b4", 0.6),
       xlab = "Installed Capacity (MW)", ylab = "Production (GWh)", main = "Installed Capacity vs Production")
  grid()
})
note("\nFigures written to ", FIG_DIR, ": ", paste(list.files(FIG_DIR, pattern = "png$"), collapse = ", "))

# ---- 7. Save log and session info -------------------------------------------
writeLines(c(log_lines, "", capture.output(sessionInfo())), "outputs/key_results.txt")
cat("\nDone. See outputs/key_results.txt, outputs/tables/ and outputs/figures/.\n")
