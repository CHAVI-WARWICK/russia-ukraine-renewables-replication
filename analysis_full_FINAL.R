# ============================================================================
# YOU CAN'T HAVE IT ALL:
# How the Russia-Ukraine War Shaped Renewable Energy Transitions
# in Developing Countries
#
# Full replication script
# R version 4.5.1
#
# Sample: 122 developing countries, 1,190 observations, 2015 to 2024
# Estimator: FGLS (pggls) with random effects
#
# STRUCTURE: sections follow the order of the dissertation, not the order
# in which the analysis was originally run.
#
#   0.  Setup, paths, helper functions
#   1.  Data and analysis sample (Section 4.2)
#   2.  Chapter 1: global map of post-war change
#   3.  Chapter 4.3: conceptualisation and measurement table
#   4.  Chapter 4.4: descriptive statistics (Table 1)
#   5.  Chapter 4.5: model building and diagnostics (Table 2)
#   6.  Chapter 4.6: final FGLS model and cross-validation
#   7.  Chapter 4.7: robustness checks (Table 3)
#   8.  Chapter 4.8: heterogeneity by income group (Table 4)
#   9.  Chapter 4.9: interaction analysis by carbon intensity (Table 5)
#   10. Chapter 4.10: decomposition (Table 6, Figure 1)
#   11. Chapter 4.11: structural break analysis (Figures 2 and 3)
#   12. Chapter 5: Morocco case study
#   13. Chapter 6: binding zone and navigable zone
#   14. Appendix: Tables A1, A2, A3
#
# CAPTION RULE FOR QUARTO:
#   Tables are exported as PNG with NO caption baked in. Significance notes
#   stay inside the image because they belong to the table. Quarto supplies
#   the number and the caption via ![Caption](path){#tbl-xxx}.
#   Figures keep their subtitle and source line (analytical content) but the
#   plot title is dropped. Quarto supplies the caption.
#   This is what removes the duplicate "Figure 1" problem.
# ============================================================================


# ============================================================================
# 0. SETUP
# ============================================================================

# One-off install on a fresh machine (uncomment to run)
# install.packages(c("tidyverse", "plm", "lmtest", "car", "caret",
#                    "modelsummary", "tinytable", "webshot2", "ggrepel",
#                    "patchwork", "sf", "rnaturalearth", "rnaturalearthdata",
#                    "readxl", "scales"))

library(tidyverse)
library(plm)
library(lmtest)
library(car)
library(caret)
library(modelsummary)
library(tinytable)
library(ggrepel)
library(patchwork)

# ---- Paths: the ONLY lines to change when moving machines ------------------
data_dir <- "C:/Users/AASHISH ARORA/OneDrive/Desktop/Dissertation/Data"
out_dir  <- "C:/Users/AASHISH ARORA/OneDrive/Desktop/Dissertation_Duplication/output"

# Polity5 sits outside the main data folder. Give its full path here.
polity_path <- "C:/Users/AASHISH ARORA/OneDrive/Desktop/Dissertation/final analysis/polity5.xls"


# For the GitHub replication repo, replace the three lines above with:
# data_dir    <- "data"
# out_dir     <- "output"
# polity_path <- "data/polity5.xls"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Path helpers, so no full path is ever typed again
dpath <- function(f) file.path(data_dir, f)
opath <- function(f) file.path(out_dir,  f)

# ---- Shared colour palette (consistent across every figure) ----------------
col_red   <- "#E63946"   # actual values, outliers
col_amber <- "#E8A020"   # counterfactual, trend lines, solar
col_blue  <- "#2E75B6"   # main series, wind, Morocco
col_purp  <- "#6A3D9A"   # Turkmenistan (binding zone)

# ---- Helper functions ------------------------------------------------------

# Significance stars, applied consistently everywhere
stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*",
                       ifelse(p < 0.1, "+", ""))))
}

# Pull the coefficient table from either a pggls or a plm object
ctable <- function(model) {
  if (inherits(model, "pggls")) summary(model)$CoefTable
  else summary(model)$coefficients
}

# Formatted estimate with stars. Small coefficients avoid scientific notation.
get_est <- function(model, var) {
  ct <- ctable(model)
  if (!var %in% rownames(ct)) return("")
  est <- ct[var, "Estimate"]
  p   <- ct[var, ncol(ct)]
  val <- if (abs(est) < 0.001) format(round(est, 6), scientific = FALSE) else round(est, 3)
  paste0(val, stars(p))
}

# Formatted standard error in parentheses
get_se <- function(model, var) {
  ct <- ctable(model)
  if (!var %in% rownames(ct)) return("")
  se <- ct[var, "Std. Error"]
  val <- if (abs(se) < 0.001) format(round(se, 6), scientific = FALSE) else round(se, 3)
  paste0("(", val, ")")
}

# Note printed under every regression table
sig_note <- "+ p<0.1, * p<0.05, ** p<0.01, *** p<0.001"


# ============================================================================
# 1. DATA AND ANALYSIS SAMPLE (Section 4.2)
# ============================================================================

df_raw <- read.csv(dpath("panel_data_122_final_v2.csv"))

# Ember raw file, used for the Morocco generation mix in Chapter 5
ember <- read.csv(dpath("yearly_full_release_long_format.csv"))

# Analysis sample: complete cases on the three model-critical variables.
# Ukraine (party to the conflict), Eritrea and North Korea (missing GDP)
# and Lesotho (verified reporting error) were removed when the panel was
# constructed, so they are already absent from the CSV.
df <- df_raw %>%
  filter(!is.na(renewable_pct),
         !is.na(gdp_pc),
         !is.na(co2_intensity_avg))

# Panel structure, indexed by country and year throughout
pdata <- pdata.frame(df, index = c("country", "year"))

# ---- SANITY CHECK: must print 122, 1190, 2015 to 2024 ----------------------
cat("Countries:  ", n_distinct(df$country), "\n")
cat("Observations:", nrow(df), "\n")
cat("Years:      ", min(df$year), "to", max(df$year), "\n")

# Income group labels used in Section 4.8
print(table(df$income_group))


# ============================================================================
# 2. CHAPTER 1: GLOBAL MAP OF POST-WAR CHANGE
# ============================================================================

library(sf)
library(rnaturalearth)

# Pre-war (2015 to 2021) versus post-war (2022 to 2024) mean renewable share
change_data <- df %>%
  group_by(country, iso3c) %>%
  summarise(
    pre  = mean(renewable_pct[post_war == 0], na.rm = TRUE),
    post = mean(renewable_pct[post_war == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(pre), !is.na(post)) %>%
  mutate(change = post - pre)

# Headline figure quoted in Chapters 1 and 5: 82 of 122 countries accelerated
cat("Countries that accelerated:", sum(change_data$change > 0),
    "of", nrow(change_data), "\n")

world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>% left_join(change_data, by = c("iso_a3" = "iso3c"))

# Scale squished at plus or minus 10pp: the full range is dominated by a few
# fragile states, which flattens all visible variation without the limit.
ggplot(map_data) +
  geom_sf(aes(fill = change), color = "grey80", linewidth = 0.1) +
  scale_fill_gradient2(
    low = col_red, mid = "#FFFFCC", high = col_blue,
    midpoint = 0, na.value = "grey90",
    name = "Change in\nRenewable Share (pp)",
    limits = c(-10, 10),
    oob = scales::squish
  ) +
  labs(subtitle = "Average post-war (2022 to 2024) minus pre-war (2015 to 2021), 122 developing countries",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")

ggsave(opath("map_postwar_change.png"), width = 12, height = 7, dpi = 300)

# NOTE: the Chapter 2 literature synthesis map was built separately and is
# not reproduced here. Add that code as Section 2b before freezing the repo,
# or state in the README that it is a conceptual figure, not an analysis output.


# ============================================================================
# 3. CHAPTER 4.3: CONCEPTUALISATION AND MEASUREMENT
# ============================================================================

concept_table <- data.frame(
  Concept = c("Dependent Variable", "Renewable Energy Transition", "",
              "Independent Variables", "War Shock", "Pandemic Shock",
              "Income Level", "Income x War Interaction", "",
              "Controls", "Carbon Lock-in", "Hydropower Geography"),
  Attribute = c("", "Share of clean electricity", "",
                "", "Exogenous energy shock", "Demand reduction",
                "Economic capacity", "Moderating effect of income", "",
                "", "Inherited fossil infrastructure", "Geographic endowment"),
  Variable = c("", "Renewable electricity share (%)", "",
               "", "Post-war dummy (0 = 2015 to 2021, 1 = 2022 to 2024)",
               "COVID dummy (1 = 2020 to 2021)",
               "GDP per capita (USD)", "GDP per capita x Post-war", "",
               "", "Pre-war CO2 intensity (gCO2/kWh, 2015 to 2021 average)",
               "Hydro dummy (1 = below 175 gCO2/kWh)"),
  check.names = FALSE
)

tt(concept_table) |>
  style_tt(i = c(1, 4, 10), bold = TRUE) |>
  save_tt(opath("table_conceptualisation.png"), overwrite = TRUE)


# ============================================================================
# 4. CHAPTER 4.4: DESCRIPTIVE STATISTICS (Table 1)
# ============================================================================

# Labelled copy: datasummary prints column names, so the labels live in the data
df_labelled <- df %>%
  rename(
    `Renewable Electricity Share (%)`  = renewable_pct,
    `Post-War Period (2022-2024)`      = post_war,
    `COVID Period (2020-2021)`         = covid,
    `GDP per Capita (USD)`             = gdp_pc,
    `Pre-War CO2 Intensity (gCO2/kWh)` = co2_intensity_avg,
    `Hydro Endowment`                  = hydro_dummy
  )

t1 <- datasummary(
  `Renewable Electricity Share (%)` +
    `Post-War Period (2022-2024)` +
    `COVID Period (2020-2021)` +
    `GDP per Capita (USD)` +
    `Pre-War CO2 Intensity (gCO2/kWh)` +
    `Hydro Endowment` ~
    Mean + Median + SD + Min + Max + Histogram,
  data = df_labelled
)

save_tt(t1, output = opath("table1_descriptive.png"), overwrite = TRUE)

# Median CO2 intensity: the split point used in Section 4.9. Should print 518.
cat("Median pre-war CO2 intensity:", round(median(df$co2_intensity_avg), 0), "\n")


# ============================================================================
# 5. CHAPTER 4.5: MODEL BUILDING AND DIAGNOSTICS (Table 2)
# ============================================================================

# Models are built incrementally, one variable at a time, using Random Effects.
# The R-squared reported in the dissertation is taken from these RE models,
# because the FGLS R-squared is not comparable (see Section 4.5).

# M1: the two shock dummies alone
m1 <- plm(renewable_pct ~ post_war + covid,
          data = pdata, model = "random")

# M2: add income level
m2 <- plm(renewable_pct ~ post_war + covid + gdp_pc,
          data = pdata, model = "random")

# M3: add the GDP x post-war interaction, the direct test of H2
m3 <- plm(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar,
          data = pdata, model = "random")

# M4: add pre-war carbon lock-in
m4 <- plm(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
            co2_intensity_avg,
          data = pdata, model = "random")

# M5: add the hydro endowment control (geography, not policy)
m5 <- plm(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
            co2_intensity_avg + hydro_dummy,
          data = pdata, model = "random")

# R-squared path quoted in the text: 8.6, 8.7, 8.7, 35.3, 40.6 per cent
for (i in 1:5) {
  m <- list(m1, m2, m3, m4, m5)[[i]]
  cat("M", i, " R-squared: ", round(summary(m)$r.squared[1], 3), "\n", sep = "")
}

# ---- Diagnostics -----------------------------------------------------------

# Hausman: Random Effects versus Fixed Effects. p = 0.182, so RE is appropriate.
fe_model <- plm(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                  co2_intensity_avg + hydro_dummy,
                data = pdata, model = "within")
print(phtest(fe_model, m5))

# Breusch-Pagan: heteroskedasticity. p < 0.001.
print(bptest(m5))

# Wooldridge: serial correlation. p < 0.001.
print(pwartest(fe_model))

# Both violations confirmed, so the final model is estimated by FGLS.

# ---- Table 2: model building strategy, M1 to FGLS --------------------------
# Built after the FGLS model in Section 6 below, so the FGLS column is defined.


# ============================================================================
# 6. CHAPTER 4.6: FINAL FGLS MODEL AND CROSS-VALIDATION
# ============================================================================

# pggls with model = "random" is the Feasible Generalised Least Squares
# estimator. It corrects for the heteroskedasticity and autocorrelation
# confirmed above. This is the model reported throughout the dissertation.
m_final <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                   co2_intensity_avg + hydro_dummy,
                 data = pdata, model = "random")

print(summary(m_final)$CoefTable)
cat("FGLS R-squared:", round(summary(m_final)$rsqr, 3), "\n")

# Expected: post_war +2.43 (p = 0.002), covid +1.36***, co2_intensity_avg
# -0.087***, hydro_dummy +27.26***, GDP and GDP x post-war insignificant.
# R-squared 0.831.

# ---- Table 2: model building strategy --------------------------------------

vars   <- c("(Intercept)", "post_war", "covid", "gdp_pc", "gdp_postwar",
            "co2_intensity_avg", "hydro_dummy")
labels <- c("Intercept", "Post-War Period", "COVID Period",
            "GDP per Capita", "GDP x Post-War",
            "Pre-War CO2 Intensity", "Hydro Endowment")

models <- list(m1, m2, m3, m4, m5, m_final)
mnames <- c("M1", "M2", "M3", "M4", "M5", "FGLS")

combined <- data.frame(Variable = character(), stringsAsFactors = FALSE)
for (m in mnames) combined[[m]] <- character()

for (i in seq_along(vars)) {
  coef_row <- data.frame(Variable = labels[i], stringsAsFactors = FALSE)
  se_row   <- data.frame(Variable = "",        stringsAsFactors = FALSE)
  for (j in seq_along(mnames)) {
    coef_row[[mnames[j]]] <- get_est(models[[j]], vars[i])
    se_row[[mnames[j]]]   <- get_se(models[[j]],  vars[i])
  }
  combined <- rbind(combined, coef_row, se_row)
}

# R-squared row: RE R-squared for M1 to M5, FGLS R-squared for the final column
rsq_row <- data.frame(Variable = "R-squared", stringsAsFactors = FALSE)
for (j in 1:5) rsq_row[[mnames[j]]] <- round(summary(models[[j]])$r.squared[1], 3)
rsq_row[["FGLS"]] <- round(summary(m_final)$rsqr, 3)

n_row <- data.frame(Variable = "Observations", stringsAsFactors = FALSE)
for (j in seq_along(mnames)) n_row[[mnames[j]]] <- "1,190"

combined <- rbind(combined, rsq_row, n_row)

tt(combined, notes = sig_note) |>
  save_tt(opath("table2_model_building.png"), overwrite = TRUE)

# ---- Ten-fold cross-validation ---------------------------------------------
# Estimated by OLS on the same specification. Reported in Appendix Table A3.
# set.seed sits immediately before the fold split so the result is fixed
# regardless of anything run earlier in the session.

set.seed(123)
folds <- createFolds(df$renewable_pct, k = 10, returnTrain = TRUE)

cv_r2 <- numeric(10)
for (i in 1:10) {
  train <- df[folds[[i]], ]
  test  <- df[-folds[[i]], ]

  cv_model <- lm(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                   co2_intensity_avg + hydro_dummy, data = train)

  preds  <- predict(cv_model, newdata = test)
  ss_res <- sum((test$renewable_pct - preds)^2)
  ss_tot <- sum((test$renewable_pct - mean(test$renewable_pct))^2)
  cv_r2[i] <- 1 - (ss_res / ss_tot)
}

# Expected: mean 0.831, range 0.787 to 0.886
cat("CV mean R-squared:", round(mean(cv_r2), 3), "\n")
print(round(cv_r2, 3))


# ============================================================================
# 7. CHAPTER 4.7: ROBUSTNESS CHECKS (Table 3)
# ============================================================================

# R1: exclude China, the largest economy in the sample
pdata_nc <- pdata.frame(df %>% filter(country != "China"),
                        index = c("country", "year"))
r1 <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
              co2_intensity_avg + hydro_dummy,
            data = pdata_nc, model = "random")

# R2: exclude India
pdata_ni <- pdata.frame(df %>% filter(country != "India"),
                        index = c("country", "year"))
r2 <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
              co2_intensity_avg + hydro_dummy,
            data = pdata_ni, model = "random")

# R3: drop the hydro dummy entirely
r3 <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
              co2_intensity_avg,
            data = pdata, model = "random")

# R4: drop the 21 hydro-endowed countries from the sample
pdata_nh <- pdata.frame(df %>% filter(hydro_dummy == 0),
                        index = c("country", "year"))
r4 <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
              co2_intensity_avg,
            data = pdata_nh, model = "random")

# R5: winsorize the dependent variable at the 5th and 95th percentiles
df_win <- df
low  <- quantile(df_win$renewable_pct, 0.05)
high <- quantile(df_win$renewable_pct, 0.95)
df_win$renewable_pct <- pmin(pmax(df_win$renewable_pct, low), high)
pdata_win <- pdata.frame(df_win, index = c("country", "year"))
r5 <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
              co2_intensity_avg + hydro_dummy,
            data = pdata_win, model = "random")

# R6: Driscoll-Kraay standard errors on the within estimator.
# Accounts for cross-sectional dependence. Time-invariant regressors
# (CO2 intensity, hydro dummy) drop out of the within model by construction.
dk <- coeftest(fe_model, vcov = vcovSCC(fe_model))
print(dk)

# ---- Verification print: compare against Table 3 in the dissertation -------
rob_models <- list("Original FGLS" = m_final, "Excluding China" = r1,
                   "Excluding India" = r2, "Without Hydro Dummy" = r3,
                   "Excluding Hydro Countries" = r4, "Winsorized" = r5)
for (nm in names(rob_models)) {
  ct <- summary(rob_models[[nm]])$CoefTable
  cat(sprintf("%-28s post_war = %5.2f  p = %.4f  R2 = %.3f\n", nm,
              ct["post_war", "Estimate"], ct["post_war", "Pr(>|z|)"],
              summary(rob_models[[nm]])$rsqr))
}
# Expected: 2.43 / 2.47 / 2.42 / 2.44 / 3.01 / 2.43

# ---- R7: log-transformed GDP ------------------------------------------------
df_log <- df %>%
  mutate(log_gdp = log(gdp_pc),
         log_gdp_postwar = log(gdp_pc) * post_war)
pdata_log <- pdata.frame(df_log, index = c("country", "year"))
r_log <- pggls(renewable_pct ~ post_war + covid + log_gdp + log_gdp_postwar +
                 co2_intensity_avg + hydro_dummy,
               data = pdata_log, model = "random")
print(round(summary(r_log)$CoefTable, 4))
cat("Log GDP model R-squared:", round(summary(r_log)$rsqr, 3), "\n")

# ---- R8: regime type control using Polity5 ---------------------------------
# Democracy dummy from the 2015 to 2018 average polity2 score:
# 1 = democratic (polity2 > 0), 0 = autocratic. Interacted with post-war
# to test whether regime type moderated the response to the shock.
# Expected: post-war 2.00 (insignificant), CO2 intensity -0.080***,
# R-squared 0.831, N = 882 observations (script-verified 4 Aug 2026).
# Polity5 is not redistributed in the replication repo, so this block is
# guarded. The README should point to the original source.

r_polity  <- NULL
df_polity <- NULL
if (file.exists(polity_path)) {
  library(readxl)
  polity_raw <- read_xls(polity_path)

  polity_avg <- polity_raw %>%
    filter(year >= 2015 & year <= 2018) %>%
    group_by(country) %>%
    summarise(polity_avg = mean(polity2, na.rm = TRUE), .groups = "drop") %>%
    mutate(democracy_dummy = ifelse(polity_avg > 0, 1, 0))

  # The panel CSV already carries democracy_dummy and polity_avg columns from
  # an earlier build. They are dropped before the merge so the freshly
  # computed Polity5 values are the only ones present. This prevents the
  # duplicate .x/.y column bug from the original analysis.
  # Countries whose names do not match across the two sources are dropped by
  # the merge, which is why the sample shrinks.
  df_polity <- df %>%
    select(-any_of(c("democracy_dummy", "polity_avg"))) %>%
    merge(polity_avg[, c("country", "democracy_dummy", "polity_avg")],
          by = "country", all.x = FALSE)

  df_polity$post_war_democracy <- df_polity$post_war * df_polity$democracy_dummy

  cat("Polity matched countries:", n_distinct(df_polity$iso3c), "\n")
  cat("Polity observations:     ", nrow(df_polity), "\n")
  cat("Democracy distribution:\n")
  print(table(df_polity$democracy_dummy))

  pdata_pol <- pdata.frame(df_polity, index = c("iso3c", "year"))
  r_polity <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                      co2_intensity_avg + hydro_dummy +
                      democracy_dummy + post_war_democracy,
                    data = pdata_pol, model = "random")

  print(round(summary(r_polity)$CoefTable, 4))
  cat("Polity model R-squared:", round(summary(r_polity)$rsqr, 3), "\n")
} else {
  cat("Polity5 file not found. Regime type check skipped.\n")
}

# ---- Table 3: robustness checks --------------------------------------------

rob_row <- function(label, model, n) {
  data.frame(
    Specification    = label,
    `Post-War`       = get_est(model, "post_war"),
    `GDP x Post-War` = get_est(model, "gdp_postwar"),
    `CO2 Intensity`  = get_est(model, "co2_intensity_avg"),
    `R-squared`      = as.character(round(summary(model)$rsqr, 3)),
    N                = n,
    check.names = FALSE
  )
}

rob_table <- rbind(
  rob_row("Original FGLS",             m_final, "1,190"),
  rob_row("Excluding China",           r1,      "1,180"),
  rob_row("Excluding India",           r2,      "1,180"),
  rob_row("Without Hydro Dummy",       r3,      "1,190"),
  rob_row("Excluding Hydro Countries", r4,        "990"),
  rob_row("Winsorized (5th/95th)",     r5,      "1,190")
)

# Driscoll-Kraay row, built from the coeftest object rather than a model
dk_row <- data.frame(
  Specification    = "Driscoll-Kraay SE (FE)",
  `Post-War`       = paste0(round(dk["post_war", "Estimate"], 2),
                            stars(dk["post_war", "Pr(>|t|)"])),
  `GDP x Post-War` = paste0(format(round(dk["gdp_postwar", "Estimate"], 6),
                                   scientific = FALSE),
                            stars(dk["gdp_postwar", "Pr(>|t|)"])),
  `CO2 Intensity`  = "",
  `R-squared`      = "",
  N                = "1,190",
  check.names = FALSE
)

# Separator, then the two additional specification tests
sep_row <- data.frame(
  Specification = "Additional Specification Tests",
  `Post-War` = "", `GDP x Post-War` = "", `CO2 Intensity` = "",
  `R-squared` = "", N = "", check.names = FALSE
)

log_row <- data.frame(
  Specification    = "Log GDP",
  `Post-War`       = get_est(r_log, "post_war"),
  `GDP x Post-War` = get_est(r_log, "log_gdp_postwar"),
  `CO2 Intensity`  = get_est(r_log, "co2_intensity_avg"),
  `R-squared`      = as.character(round(summary(r_log)$rsqr, 3)),
  N                = "1,190",
  check.names = FALSE
)

rob_table <- rbind(rob_table, dk_row, sep_row, log_row)

if (!is.null(r_polity)) {
  polity_row <- data.frame(
    Specification    = "Regime Type (Polity5)",
    `Post-War`       = get_est(r_polity, "post_war"),
    `GDP x Post-War` = get_est(r_polity, "gdp_postwar"),
    `CO2 Intensity`  = get_est(r_polity, "co2_intensity_avg"),
    `R-squared`      = as.character(round(summary(r_polity)$rsqr, 3)),
    N                = format(nrow(df_polity), big.mark = ","),
    check.names = FALSE
  )
  rob_table <- rbind(rob_table, polity_row)
}

tt(rob_table, notes = sig_note) |>
  style_tt(i = 8, bold = TRUE, italic = TRUE) |>   # the separator row
  save_tt(opath("table3_robustness.png"), overwrite = TRUE)


# ============================================================================
# 8. CHAPTER 4.8: HETEROGENEITY BY INCOME GROUP (Table 4)
# ============================================================================

# GDP per capita and its interaction are dropped from these models: the sample
# is already split on income, so retaining them would be collinear by design.

fit_income <- function(group) {
  d <- df %>% filter(income_group == group)
  p <- pdata.frame(d, index = c("country", "year"))
  list(
    model = pggls(renewable_pct ~ post_war + covid + co2_intensity_avg +
                    hydro_dummy, data = p, model = "random"),
    countries = n_distinct(d$country),
    n = nrow(d)
  )
}

h_low <- fit_income("Low Income")
h_lm  <- fit_income("Lower Middle Income")
h_um  <- fit_income("Upper Middle Income")

het_table <- data.frame(
  `Income Group` = c("Low Income", "Lower Middle Income",
                     "Upper Middle Income", "Full Sample"),
  Countries = c(h_low$countries, h_lm$countries, h_um$countries, 122),
  `Post-War` = c(get_est(h_low$model, "post_war"),
                 get_est(h_lm$model,  "post_war"),
                 get_est(h_um$model,  "post_war"),
                 get_est(m_final,     "post_war")),
  `CO2 Intensity` = c(get_est(h_low$model, "co2_intensity_avg"),
                      get_est(h_lm$model,  "co2_intensity_avg"),
                      get_est(h_um$model,  "co2_intensity_avg"),
                      get_est(m_final,     "co2_intensity_avg")),
  `R-squared` = c(round(summary(h_low$model)$rsqr, 3),
                  round(summary(h_lm$model)$rsqr, 3),
                  round(summary(h_um$model)$rsqr, 3),
                  round(summary(m_final)$rsqr, 3)),
  N = c(h_low$n, h_lm$n, h_um$n, nrow(df)),
  check.names = FALSE
)

print(het_table)
# Expected post-war: low +1.95, lower middle +2.54, upper middle +2.23
# Expected CO2: low -0.141, upper middle -0.066

tt(het_table, notes = sig_note) |>
  save_tt(opath("table4_heterogeneity.png"), overwrite = TRUE)


# ============================================================================
# 9. CHAPTER 4.9: INTERACTION ANALYSIS BY CARBON INTENSITY (Table 5)
# ============================================================================

# Countries split at the median pre-war CO2 intensity (518 gCO2/kWh).
# co2_high and did_co2 are pre-built in the panel CSV.
# NOTE: this is an interaction analysis, not difference-in-differences. The
# label "DiD" was removed throughout the dissertation and the variable name
# did_co2 is retained only because it is the column name in the source data.

m_interaction <- pggls(renewable_pct ~ post_war + co2_high + did_co2 +
                         covid + hydro_dummy,
                       data = pdata, model = "random")

print(summary(m_interaction)$CoefTable)
cat("Interaction model R-squared:", round(summary(m_interaction)$rsqr, 3), "\n")
# Expected: interaction +1.672*, post-war +1.277+, high CO2 -32.96***,
# COVID +1.292***, hydro +49.076***, R-squared 0.816

int_vars   <- c("(Intercept)", "post_war", "co2_high", "did_co2",
                "covid", "hydro_dummy")
int_labels <- c("Intercept", "Post-War Period", "High CO2 Countries",
                "Interaction (Post-War x High CO2)", "COVID Period",
                "Hydro Endowment")

int_rows <- data.frame(Variable = character(), Estimate = character(),
                       stringsAsFactors = FALSE)
for (i in seq_along(int_vars)) {
  int_rows <- rbind(
    int_rows,
    data.frame(Variable = int_labels[i],
               Estimate = get_est(m_interaction, int_vars[i])),
    data.frame(Variable = "",
               Estimate = get_se(m_interaction, int_vars[i]))
  )
}

int_rows <- rbind(int_rows,
                  data.frame(Variable = "R-squared",
                             Estimate = as.character(round(summary(m_interaction)$rsqr, 3))),
                  data.frame(Variable = "Observations", Estimate = "1,190"),
                  data.frame(Variable = "Countries",    Estimate = "122"))

tt(int_rows, notes = sig_note) |>
  save_tt(opath("table5_interaction.png"), overwrite = TRUE)


# ============================================================================
# 10. CHAPTER 4.10: DECOMPOSITION (Table 6, Figure 1)
# ============================================================================

# Splits total renewable share into wind and solar versus hydropower and other
decomp <- df %>%
  group_by(post_war) %>%
  summarise(total = mean(renewable_pct,   na.rm = TRUE),
            ws    = mean(wind_solar_pct,  na.rm = TRUE),
            .groups = "drop") %>%
  mutate(hydro_other = total - ws)

print(decomp)
# Expected change: wind and solar +3.46, hydro and other +0.17, total +3.62

decomp_table <- data.frame(
  Component = c("Wind and Solar", "Hydropower and Other", "Total Renewable"),
  `Pre-War (2015 to 2021)` = round(c(decomp$ws[decomp$post_war == 0],
                                     decomp$hydro_other[decomp$post_war == 0],
                                     decomp$total[decomp$post_war == 0]), 2),
  `Post-War (2022 to 2024)` = round(c(decomp$ws[decomp$post_war == 1],
                                      decomp$hydro_other[decomp$post_war == 1],
                                      decomp$total[decomp$post_war == 1]), 2),
  `Change (pp)` = round(c(diff(decomp$ws),
                          diff(decomp$hydro_other),
                          diff(decomp$total)), 2),
  check.names = FALSE
)

tt(decomp_table) |>
  save_tt(opath("table6_decomposition.png"), overwrite = TRUE)

# ---- Figure 1: change from the 2015 baseline -------------------------------
# Plotted as change rather than level, because the two series sit at very
# different levels and the growth story is otherwise invisible.

trend_data <- df %>%
  group_by(year) %>%
  summarise(wind_solar = mean(wind_solar_pct, na.rm = TRUE),
            hydro      = mean(renewable_pct - wind_solar_pct, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(ws_change    = wind_solar - wind_solar[year == 2015],
         hydro_change = hydro      - hydro[year == 2015])

ggplot(trend_data, aes(x = year)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = ws_change, color = "Wind and Solar"), linewidth = 1.2) +
  geom_point(aes(y = ws_change, color = "Wind and Solar"), size = 2.5) +
  geom_line(aes(y = hydro_change, color = "Hydro and Other"), linewidth = 1.2) +
  geom_point(aes(y = hydro_change, color = "Hydro and Other"), size = 2.5) +
  annotate("text", x = 2022.2, y = 5.5, label = "Russia-Ukraine\nWar (2022)",
           color = "grey40", size = 3, hjust = 0) +
  scale_color_manual(values = c("Wind and Solar" = col_amber,
                                "Hydro and Other" = col_blue)) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(subtitle = "Change in % of electricity generation from 2015 baseline, 122 developing countries",
       x = "Year", y = "Change from 2015 (percentage points)", color = "",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(opath("figure1_decomposition.png"), width = 10, height = 6, dpi = 300)


# ============================================================================
# 11. CHAPTER 4.11: STRUCTURAL BREAK ANALYSIS (Figures 2 and 3)
# ============================================================================

# Tests whether the post-war acceleration in wind and solar is a genuine break
# or a continuation of the pre-existing cost-driven trend (Swanson's Law).

ws_trend <- df %>%
  group_by(year) %>%
  summarise(avg_ws = mean(wind_solar_pct, na.rm = TRUE), .groups = "drop")

# Chow test computed manually from residual sums of squares, because the
# aggregated series has only ten observations.
full     <- lm(avg_ws ~ year, data = ws_trend)
rss_full <- sum(residuals(full)^2)
k <- 2                  # intercept and slope
n <- nrow(ws_trend)

chow_test <- function(break_year) {
  pre  <- lm(avg_ws ~ year, data = ws_trend %>% filter(year <  break_year))
  post <- lm(avg_ws ~ year, data = ws_trend %>% filter(year >= break_year))
  rss_split <- sum(residuals(pre)^2) + sum(residuals(post)^2)
  f <- ((rss_full - rss_split) / k) / (rss_split / (n - 2 * k))
  c(F = f, p = 1 - pf(f, k, n - 2 * k))
}

# Values quoted in the text: 2022 gives F = 1.315, p = 0.336
#                            2018 gives F = 4.237, p = 0.071
cat("Break at 2022:", round(chow_test(2022), 3), "\n")
cat("Break at 2018:", round(chow_test(2018), 3), "\n")

# ---- Figure 2: observed slopes versus a hypothetical break -----------------
# The comparison panel exists because "no break" is hard to see on its own.

p_actual <- ggplot(ws_trend, aes(x = year, y = avg_ws)) +
  geom_point(size = 3, color = col_blue) +
  geom_smooth(data = ws_trend %>% filter(year <= 2021),
              method = "lm", color = col_amber, se = FALSE, linewidth = 1.2) +
  geom_smooth(data = ws_trend %>% filter(year >= 2022),
              method = "lm", color = col_red, se = FALSE, linewidth = 1.2) +
  geom_vline(xintercept = 2021.5, linetype = "dotted", color = "grey40") +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(limits = c(0, 12), labels = function(x) paste0(x, "%")) +
  labs(title = "Observed data: no structural break",
       subtitle = "Slopes are similar. p = 0.336",
       x = "Year", y = "Wind and Solar (%)") +
  theme_minimal()

# Hypothetical series: the same data with 2022 to 2024 forced upward, purely
# illustrative, to show what a genuine break would look like.
ws_hypo <- ws_trend
ws_hypo$fake_ws <- ws_hypo$avg_ws
ws_hypo$fake_ws[ws_hypo$year == 2022] <- 8
ws_hypo$fake_ws[ws_hypo$year == 2023] <- 10
ws_hypo$fake_ws[ws_hypo$year == 2024] <- 12

p_hypo <- ggplot(ws_hypo, aes(x = year, y = fake_ws)) +
  geom_point(size = 3, color = col_blue) +
  geom_smooth(data = ws_hypo %>% filter(year <= 2021),
              method = "lm", color = col_amber, se = FALSE, linewidth = 1.2) +
  geom_smooth(data = ws_hypo %>% filter(year >= 2022),
              method = "lm", color = col_red, se = FALSE, linewidth = 1.2) +
  geom_vline(xintercept = 2021.5, linetype = "dotted", color = "grey40") +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(limits = c(0, 12), labels = function(x) paste0(x, "%")) +
  labs(title = "Hypothetical: structural break",
       subtitle = "Post-war slope much steeper",
       x = "Year", y = "Wind and Solar (%)") +
  theme_minimal()

p_actual + p_hypo

ggsave(opath("figure2_chow_comparison.png"), width = 14, height = 6, dpi = 300)

# ---- Figure 3: F statistics at every candidate break year ------------------

break_years <- 2017:2022
chow_results <- sapply(break_years, function(y) chow_test(y)["F"])
f_crit <- qf(0.95, k, n - 2 * k)

chow_data <- data.frame(Year = break_years, F_stat = as.numeric(chow_results))

ggplot(chow_data, aes(x = Year, y = F_stat)) +
  geom_hline(yintercept = f_crit, color = col_red,
             linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 1.2, color = col_blue) +
  geom_point(size = 3, color = col_blue) +
  annotate("text", x = 2017.2, y = f_crit + 0.3,
           label = "Significance threshold (p = 0.05)",
           color = col_red, size = 3, hjust = 0) +
  scale_x_continuous(breaks = 2017:2022) +
  labs(subtitle = "Chow test F statistics, 2017 to 2022. No break point crosses the significance threshold.",
       x = "Potential Break Year", y = "F Statistic",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(opath("figure3_chow_fstats.png"), width = 10, height = 6, dpi = 300)


# ============================================================================
# 12. CHAPTER 5: MOROCCO CASE STUDY
# ============================================================================

# ---- 5.1 Case selection by model residual ----------------------------------
# Each country's residual is its vertical distance from the fitted line of
# average renewable share on pre-war CO2 intensity. A small residual means the
# country behaves as the model predicts, which is what makes it a typical case.

plot_data <- df %>%
  group_by(country) %>%
  summarise(avg_renewable = mean(renewable_pct, na.rm = TRUE),
            co2_intensity = mean(co2_intensity_avg, na.rm = TRUE),
            .groups = "drop")

case_trend <- lm(avg_renewable ~ co2_intensity, data = plot_data)
plot_data$residual <- residuals(case_trend)

cat("Morocco residual:",
    round(plot_data$residual[plot_data$country == "Morocco"], 2), "\n")

# Conflict and fragile states excluded: the mechanism of interest is policy
# response, which cannot be traced in a collapsed or contested energy system.
conflict <- c("Lebanon", "Somalia, Fed. Rep.", "West Bank and Gaza",
              "Myanmar", "Afghanistan", "Syria", "Yemen, Rep.", "Haiti",
              "Mali", "South Sudan")

candidates <- plot_data %>%
  filter(!country %in% conflict, avg_renewable < 90) %>%
  mutate(abs_residual = abs(residual)) %>%
  arrange(abs_residual) %>%
  head(10)

# Attributes used to screen the ten candidates
candidate_details <- df %>%
  filter(country %in% candidates$country) %>%
  group_by(country) %>%
  summarise(import_dep = round(mean(energy_imports, na.rm = TRUE), 1),
            income = first(income_group),
            acceleration = round(mean(renewable_pct[post_war == 1], na.rm = TRUE) -
                                   mean(renewable_pct[post_war == 0], na.rm = TRUE), 2),
            hydro = first(hydro_dummy),
            .groups = "drop")

case_table <- candidates %>%
  left_join(candidate_details, by = "country") %>%
  mutate(co2_intensity = round(co2_intensity, 0),
         residual = round(residual, 2),
         exporter = ifelse(import_dep < 0, "Yes", "No"),
         hydro_label = ifelse(hydro == 1, "Yes", "No")) %>%
  arrange(desc(co2_intensity)) %>%
  select(country, co2_intensity, residual, acceleration,
         exporter, hydro_label, import_dep, income)

colnames(case_table) <- c("Country", "CO2 Intensity", "Residual",
                          "Post-War Acceleration (pp)", "Fossil Exporter",
                          "Hydro Dominant", "Import Dep. (%)", "Income Group")

print(case_table)

tt(case_table) |>
  save_tt(opath("morocco_case_candidates.png"), overwrite = TRUE)

# ---- Case selection figure -------------------------------------------------

candidates_list <- candidates$country

plot_data <- plot_data %>%
  mutate(category = case_when(country == "Morocco" ~ "Morocco",
                              country %in% candidates_list ~ "Candidate",
                              abs(residual) > 20 ~ "Outlier",
                              TRUE ~ "Other"),
         label = ifelse(country %in% candidates_list, country, ""))

ggplot(plot_data, aes(x = co2_intensity, y = avg_renewable)) +
  geom_smooth(method = "lm", color = col_amber, se = TRUE,
              alpha = 0.15, linewidth = 1) +
  geom_point(data = filter(plot_data, category == "Other"),
             color = "grey70", size = 2, alpha = 0.5) +
  geom_point(data = filter(plot_data, category == "Candidate"),
             color = col_red, size = 3) +
  geom_point(data = filter(plot_data, category == "Morocco"),
             color = col_blue, size = 4) +
  geom_text_repel(aes(label = label), size = 3, max.overlaps = 20) +
  scale_x_continuous(limits = c(0, 1350)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(subtitle = "Morocco (blue) selected from candidates (red). Grey points are the remaining countries.",
       x = "Pre-war CO2 Intensity (gCO2/kWh)",
       y = "Average Renewable % of Electricity Generation",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(opath("morocco_case_selection.png"), width = 10, height = 6, dpi = 300)

# ---- 5.2 Country profile ---------------------------------------------------

morocco <- df %>% filter(country == "Morocco") %>% arrange(year)

# Wind and solar shares come from the raw Ember file, not the panel
mor_ember <- ember %>%
  filter(Area == "Morocco", Unit == "%", Year >= 2015, Year <= 2024)

mor_val <- function(variable, yr) {
  round(mor_ember$Value[mor_ember$Variable == variable & mor_ember$Year == yr], 1)
}

profile <- data.frame(
  Indicator = c("Income Group", "GDP per Capita (average)", "CO2 Intensity",
                "Energy Import Dependence", "Renewable Share (2015)",
                "Renewable Share (2024)", "Wind Share (2024)",
                "Solar Share (2024)", "Post-War Acceleration",
                "Residual from Trend"),
  Value = c(
    first(morocco$income_group),
    paste0("$", format(round(mean(morocco$gdp_pc)), big.mark = ",")),
    paste0(round(unique(morocco$co2_intensity_avg)), " gCO2/kWh"),
    paste0(round(mean(morocco$energy_imports, na.rm = TRUE), 1), "%"),
    paste0(round(morocco$renewable_pct[morocco$year == 2015], 1), "%"),
    paste0(round(morocco$renewable_pct[morocco$year == 2024], 1), "%"),
    paste0(mor_val("Wind", 2024), "%"),
    paste0(mor_val("Solar", 2024), "%"),
    paste0("+", round(mean(morocco$renewable_pct[morocco$post_war == 1]) -
                        mean(morocco$renewable_pct[morocco$post_war == 0]), 2), "pp"),
    round(plot_data$residual[plot_data$country == "Morocco"], 2)
  ),
  check.names = FALSE
)

print(profile)
# Expected: lower middle income, $3,508, 636 gCO2/kWh, 93.8%, 16.1%, 24.4%,
# 16.7%, 5.7%, +3.28pp, 1.97

tt(profile) |>
  save_tt(opath("morocco_profile.png"), overwrite = TRUE)

# NOTE: the Morocco location map was produced in QGIS and is not reproduced
# here. Record that in the README.

# ---- 5.3 Counterfactual analysis -------------------------------------------
# The counterfactual projects Morocco's pre-war (2015 to 2021) linear trend
# forward. The gap is the distance between actual and projected.

morocco_prewar <- morocco %>% filter(post_war == 0)
mor_trend <- lm(renewable_pct ~ year, data = morocco_prewar)
morocco$counterfactual <- predict(mor_trend,
                                  newdata = data.frame(year = morocco$year))
morocco$gap <- morocco$renewable_pct - morocco$counterfactual

print(morocco %>% select(year, renewable_pct, counterfactual, gap, post_war))
# Expected: 2022 gap -2.89, 2024 gap +1.86

# Annotation coordinates computed from the data, never hardcoded
y22_actual <- morocco$renewable_pct[morocco$year == 2022]
y22_proj   <- morocco$counterfactual[morocco$year == 2022]
y24_actual <- morocco$renewable_pct[morocco$year == 2024]
y24_proj   <- morocco$counterfactual[morocco$year == 2024]

ggplot(morocco, aes(x = year)) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = counterfactual, color = "Projected (without war)"),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = counterfactual, color = "Projected (without war)"), size = 2.5) +
  geom_line(aes(y = renewable_pct, color = "Actual"), linewidth = 1.2) +
  geom_point(aes(y = renewable_pct, color = "Actual"), size = 2.5) +
  annotate("segment", x = 2022, xend = 2022, y = y22_actual, yend = y22_proj,
           color = col_red, linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
  annotate("text", x = 2022.5, y = 17,
           label = paste0(round(y22_actual - y22_proj, 2), "pp below trend"),
           color = col_red, size = 3, hjust = 0) +
  annotate("segment", x = 2024, xend = 2024, y = y24_proj, yend = y24_actual,
           color = col_blue, linewidth = 0.8,
           arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
  annotate("text", x = 2022.5, y = 25.5,
           label = paste0("+", round(y24_actual - y24_proj, 2), "pp above trend"),
           color = col_blue, size = 3, hjust = 0) +
  annotate("text", x = 2021.6, y = 26.5, label = "Russia-Ukraine\nWar (2022)",
           color = "grey40", size = 3, hjust = 0) +
  scale_color_manual(values = c("Actual" = col_red,
                                "Projected (without war)" = col_amber)) +
  scale_x_continuous(breaks = 2015:2024, limits = c(2015, 2024.5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(15, 27)) +
  labs(subtitle = "Stall in 2022, recovery in 2023, acceleration above trend in 2024",
       x = "Year", y = "Renewable % of Electricity Generation", color = "",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(opath("morocco_counterfactual.png"), width = 10, height = 6, dpi = 300)

# ---- Wind versus solar -----------------------------------------------------

morocco_ws <- mor_ember %>% filter(Variable %in% c("Wind", "Solar"))

ggplot(morocco_ws, aes(x = Year, y = Value, color = Variable)) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  annotate("text", x = 2021.6, y = 17, label = "Russia-Ukraine\nWar (2022)",
           color = "grey40", size = 3, hjust = 0) +
  scale_color_manual(values = c("Wind" = col_blue, "Solar" = col_amber)) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(subtitle = "Wind dominates Morocco's renewable transition. Solar stalled in 2022.",
       x = "Year", y = "% of Electricity Generation", color = "",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(opath("morocco_wind_solar.png"), width = 10, height = 6, dpi = 300)

# ---- Full generation mix ---------------------------------------------------
# Bioenergy is merged into "Hydro and Other": on its own it is too small to
# read as a band, and it is not part of the wind and solar story.

morocco_mix <- mor_ember %>%
  filter(Variable %in% c("Coal", "Gas", "Other Fossil", "Wind", "Solar",
                         "Hydro", "Bioenergy")) %>%
  mutate(Source = case_when(
    Variable == "Coal"         ~ "Coal",
    Variable == "Gas"          ~ "Gas",
    Variable == "Other Fossil" ~ "Other Fossil",
    Variable == "Wind"         ~ "Wind",
    Variable == "Solar"        ~ "Solar",
    Variable %in% c("Hydro", "Bioenergy") ~ "Hydro & Other"
  ),
  Source = factor(Source, levels = c("Coal", "Gas", "Other Fossil",
                                     "Hydro & Other", "Solar", "Wind")))

ggplot(morocco_mix, aes(x = Year, y = Value, fill = Source)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_vline(xintercept = 2021.5, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  annotate("text", x = 2021.6, y = 102, label = "War",
           color = "black", size = 3, hjust = 0) +
  scale_fill_manual(values = c("Coal" = "#4a4a4a", "Gas" = "#8c8c8c",
                               "Other Fossil" = "#bfbfbf",
                               "Hydro & Other" = "#5DADE2",
                               "Solar" = col_amber, "Wind" = col_blue)) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(subtitle = "Coal dominates (60 to 70%). Wind and solar growing but fossil dependence remains deep.",
       x = "Year", y = "% of Electricity Generation", fill = "",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(opath("morocco_energy_mix.png"), width = 10, height = 6, dpi = 300)

list.files(out_dir, pattern = "morocco")

# ============================================================================
# 13. CHAPTER 6: BINDING ZONE AND NAVIGABLE ZONE
# ============================================================================
# Pre-war carbon intensity against post-war acceleration. Turkmenistan marks
# the binding zone, Morocco and India the navigable zone.

accel_data <- df %>%
  group_by(country) %>%
  summarise(co2  = mean(co2_intensity_avg, na.rm = TRUE),
            pre  = mean(renewable_pct[post_war == 0], na.rm = TRUE),
            post = mean(renewable_pct[post_war == 1], na.rm = TRUE),
            .groups = "drop") %>%
  filter(!is.na(pre), !is.na(post)) %>%
  mutate(acceleration = post - pre)

# Values quoted in Chapter 6
print(accel_data %>%
        filter(country %in% c("Turkmenistan", "India", "Morocco")) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))))
# Expected: Turkmenistan 1,307 gCO2/kWh and +0.02pp; India 733 and +3.16;
#           Morocco 636 and +3.28

# Labels: the three named cases plus the extreme outliers; everyone else blank
accel_data <- accel_data %>%
  mutate(label = ifelse(acceleration > 20 | acceleration < -10 |
                          country %in% c("Morocco", "Turkmenistan", "India"),
                        country, ""),
         fontface = ifelse(country %in% c("Morocco", "Turkmenistan", "India"),
                           "bold", "plain"))

ggplot(accel_data, aes(x = co2, y = acceleration)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  geom_point(color = col_blue, size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = col_amber, se = TRUE, alpha = 0.15) +
  geom_point(data = filter(accel_data, country == "Morocco"),
             color = col_red, size = 4) +
  geom_point(data = filter(accel_data, country == "India"),
             color = "#1B7837", size = 4) +
  geom_point(data = filter(accel_data, country == "Turkmenistan"),
             color = col_purp, size = 4) +
  geom_text_repel(aes(label = label, fontface = fontface),
                  size = 3, max.overlaps = 20) +
  labs(subtitle = "Morocco and India (navigable zone), Turkmenistan (binding zone).\nUnlabelled extremes are fragile states.",
       x = "Pre-war CO2 Intensity (gCO2/kWh)",
       y = "Post-War Acceleration (pp)",
       caption = "Source: Ember Climate Data. Author's own analysis.") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(opath("co2_vs_acceleration.png"), width = 10, height = 6, dpi = 300)


# ============================================================================
# 14. APPENDIX
# ============================================================================

# ---- Table A1: additional control variables --------------------------------
# Each is added to the full FGLS specification one at a time. All three are
# insignificant and the post-war coefficient stays between 2.37 and 2.49.

a_fdi <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                 co2_intensity_avg + hydro_dummy + fdi,
               data = pdata, model = "random")

a_trade <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                   co2_intensity_avg + hydro_dummy + trade,
                 data = pdata, model = "random")

df_pop <- df %>% mutate(log_pop = log(population))
pdata_pop <- pdata.frame(df_pop, index = c("country", "year"))
a_pop <- pggls(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                 co2_intensity_avg + hydro_dummy + log_pop,
               data = pdata_pop, model = "random")

app_row <- function(label, model, var) {
  ct <- summary(model)$CoefTable
  data.frame(
    Variable    = label,
    Coefficient = round(ct[var, "Estimate"], 3),
    `p-value`   = round(ct[var, "Pr(>|z|)"], 3),
    `Post-War`  = get_est(model, "post_war"),
    `R-squared` = round(summary(model)$rsqr, 3),
    check.names = FALSE
  )
}

app_table <- rbind(
  app_row("FDI (% of GDP)", a_fdi, "fdi"),
  app_row("Trade Openness (% of GDP)", a_trade, "trade"),
  app_row("Log Population", a_pop, "log_pop")
)

print(app_table)
# Expected: -0.003 (p 0.937), 0.007 (p 0.569), 0.865 (p 0.101)
# Post-war 2.49, 2.42, 2.37. R-squared 0.829, 0.837, 0.834.

tt(app_table,
   notes = paste("None significant. Post-war effect stable.", sig_note)) |>
  style_tt(j = 2:5, align = "c") |>
  save_tt(opath("appendix_additional_vars.png"), overwrite = TRUE)

# ---- Table A2: variance inflation factors ----------------------------------
# VIF requires an lm object, so the specification is refitted by OLS.

vif_model <- lm(renewable_pct ~ post_war + covid + gdp_pc + gdp_postwar +
                  co2_intensity_avg + hydro_dummy, data = df)
vif_values <- vif(vif_model)
print(round(vif_values, 2))
# Expected: post_war 2.76, covid 1.11, gdp_pc 1.75, gdp_postwar 3.53,
#           co2_intensity_avg 2.13, hydro_dummy 2.08

vif_table <- data.frame(
  Variable = c("Post-War Period", "COVID Period", "GDP per Capita",
               "GDP x Post-War", "Pre-War CO2 Intensity", "Hydro Endowment"),
  VIF = round(as.numeric(vif_values), 2)
)

tt(vif_table,
   notes = "All values below the conventional threshold of 5, indicating no problematic multicollinearity.") |>
  style_tt(j = 2, align = "c") |>
  save_tt(opath("appendix_vif.png"), overwrite = TRUE)

# ---- Table A3: ten-fold cross-validation -----------------------------------
# Uses cv_r2, computed in Section 6 with set.seed(123).

cv_table <- data.frame(
  Fold = c(as.character(1:10), "Average"),
  `R-squared` = sprintf("%.3f", c(cv_r2, mean(cv_r2))),
  check.names = FALSE
)

tt(cv_table,
   notes = paste0("Fold R-squared values range from ",
                  sprintf("%.3f", min(cv_r2)), " to ",
                  sprintf("%.3f", max(cv_r2)), ", averaging ",
                  sprintf("%.3f", mean(cv_r2)), ".")) |>
  style_tt(j = 2, align = "c") |>
  save_tt(opath("appendix_cv.png"), overwrite = TRUE)


# ============================================================================
# END OF SCRIPT
# ============================================================================

cat("\nComplete. All outputs written to:\n", out_dir, "\n")
print(list.files(out_dir))


save.image(file.path(out_dir, "workspace_final_04aug.RData"))
