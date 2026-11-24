# Place your dataset here

Put exactly ONE .csv file in this folder (the original, uncleaned dataset).
The script expects these six columns:

| Column | Meaning |
|---|---|
| Country | Country name |
| Energy_Source | Solar, Wind, Hydro or Biomass |
| Year | Calendar year |
| Renewable_Energy_Production_GWh | Production in GWh |
| Estimated_Renewable_Energy_Consumption_GWh | Estimated consumption in GWh (may contain blanks) |
| Installed_Capacity_MW | Installed capacity in MW |

If your column names differ, edit the COL_* lines at the top of R/analysis.R.
No dataset is included in this package.
