# You Can't Have It All: Replication Materials

Replication materials for the MA dissertation *"You Can't Have It All: How the
Russia-Ukraine War Shaped Renewable Energy Transitions in Developing Countries"*
(University of Warwick, PAIS, 2026).

The study examines whether the Russia-Ukraine war accelerated renewable
electricity transitions across 122 developing countries (2015 to 2024), using
FGLS panel analysis and a qualitative case study of Morocco.

## Contents

| File | Description |
|---|---|
| `analysis_full_FINAL.R` | Full replication script. Sections follow the order of the dissertation and reproduce every table and figure. |
| `panel_data_122_final_v2.csv` | The constructed panel: 122 countries, 1,190 observations, 2015 to 2024. |

## How to run

1. Open `analysis_full_FINAL.R` in R (built on R 4.5.1).
2. Set the three paths at the top of Section 0 (`data_dir`, `out_dir`, `polity_path`).
3. Uncomment and run the `install.packages` line once on a fresh machine.
4. Run top to bottom. All tables and figures are written to the output folder.

## Data sources

The panel was constructed from:

- **Ember Climate Data**, yearly electricity data (https://ember-energy.org/data/yearly-electricity-data/). The raw file is not redistributed here due to size; it is required only for the Morocco generation-mix figures in Section 12.
- **World Bank World Development Indicators**, GDP per capita and income classifications (https://data.worldbank.org).
- **Polity5** (Center for Systemic Peace, https://www.systemicpeace.org/inscrdata.html), used in one robustness check (Section 7, R8). Not redistributed here per the provider's terms; the script skips this check gracefully if the file is absent.

## Not reproduced by the script

Two figures in the dissertation were produced outside R and are not generated
by this script: the Morocco location map (QGIS) and the Chapter 2 literature
synthesis map (conceptual figure).

## Author

Student, MA International Development, University of Warwick.
