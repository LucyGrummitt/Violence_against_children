# =============================================================================
# Domestic Violence-related Assault and Sexual Assault
# NSW, children 0–17 years
#
# NOTE ON METHODOLOGICAL BREAK AT 2021:
#   Prior to 2021, multiple victims within a single FDV incident were counted
#   as one event (incident-level). From 2021 onwards, each victim is counted
#   separately (victim-level). This inflates post-2021 counts independently of
#   any true change in victimisation. Accordingly:
#     - Trends are analysed separately before and after 2021.
#     - A formal joinpoint analysis is not conducted (only 7 pre-2021 and
#       4 post-2021 time points are available, insufficient for data-driven
#       breakpoint estimation).
#     - 2021 is instead specified a priori as a meaningful methodological
#       cut-point.
# =============================================================================

library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(purrr)


# =============================================================================
# 0. SHARED SETTINGS AND HELPER FUNCTIONS
# =============================================================================

knot_year <- 2021

# Age-group code → readable label maps (outcome-specific)
age_labels_fdv  <- c(`1` = "0–9", `2` = "10–14", `3` = "10–17")
age_labels_dvsa <- c(`1` = "0–9", `3` = "10–17")

# -----------------------------------------------------------------------------
# mk_piecewise()
#   Adds centred piecewise time variables around the methodological knot.
#   pre  : runs up to (and is 0 at) the knot — captures the pre-2021 slope
#   post : runs from (and is 0 at) the knot — captures the post-2021 slope
# -----------------------------------------------------------------------------
mk_piecewise <- function(df, year_col = "Year", knot = knot_year) {
  df %>%
    mutate(
      Year = as.numeric(.data[[year_col]]),
      pre  = pmin(Year, knot) - knot,   # ≤ 0 before knot, 0 on/after
      post = pmax(Year - knot, 0)       # 0 before knot, ≥ 0 on/after
    )
}

# -----------------------------------------------------------------------------
# apc_from_beta()
#   Converts a log-scale slope (beta) and its standard error to an
#   Annual Percent Change (APC) with 95% CI via normal approximation.
# -----------------------------------------------------------------------------
apc_from_beta <- function(beta, se) {
  tibble(
    APC    = (exp(beta) - 1) * 100,
    APC_lo = (exp(beta - 1.96 * se) - 1) * 100,
    APC_hi = (exp(beta + 1.96 * se) - 1) * 100
  )
}

# -----------------------------------------------------------------------------
# fit_apc_piecewise()
#   Fits a quasi-Poisson piecewise model with offset(log(Population)).
#   Handles three data configurations gracefully:
#     - Data span both sides of the knot → estimates pre AND post slopes
#     - Data only pre-knot              → estimates pre slope only
#     - Data only post-knot             → estimates post slope only
#   Returns a tidy tibble with columns: Period, APC, APC_lo, APC_hi, p
# -----------------------------------------------------------------------------
fit_apc_piecewise <- function(df, knot = knot_year) {
  df  <- mk_piecewise(df, knot = knot)
  has_pre  <- any(df$Year < knot)
  has_post <- any(df$Year >= knot)

  extract_apc <- function(coefs, term, period_label) {
    apc_from_beta(coefs[term, "Estimate"], coefs[term, "Std. Error"]) %>%
      mutate(Period = period_label, p = coefs[term, "Pr(>|t|)"])
  }

  if (has_pre && has_post) {
    m    <- glm(Count ~ pre + post + offset(log(Population)),
                family = quasipoisson, data = df)
    co   <- summary(m)$coefficients
    bind_rows(
      extract_apc(co, "pre",  "Before 2021"),
      extract_apc(co, "post", "2021+")
    )
  } else if (has_pre) {
    m  <- glm(Count ~ pre + offset(log(Population)),
              family = quasipoisson, data = df)
    co <- summary(m)$coefficients
    extract_apc(co, "pre", "Before 2021")
  } else if (has_post) {
    m  <- glm(Count ~ post + offset(log(Population)),
              family = quasipoisson, data = df)
    co <- summary(m)$coefficients
    extract_apc(co, "post", "2021+")
  } else {
    tibble(Period = character(),
           APC = numeric(), APC_lo = numeric(), APC_hi = numeric(),
           p = numeric())
  }
}

# -----------------------------------------------------------------------------
# run_results_loop()
#   Runs fit_apc_piecewise() for every sex × age-group stratum and
#   returns a single formatted results table.
#     data        : full dataset (both sexes)
#     age_groups  : integer vector of AgeGroup codes to include
#     age_labels  : named character vector mapping codes to labels
# -----------------------------------------------------------------------------
run_results_loop <- function(data, age_groups, age_labels) {
  data %>%
    filter(
      AgeGroup %in% age_groups,
      !is.na(Count), !is.na(Population), Population > 0, !is.na(Year)
    ) %>%
    mutate(
      Sex_lbl = factor(Sex, levels = c(0, 1), labels = c("Girls", "Boys"))
    ) %>%
    group_by(Sex_lbl, AgeGroup) %>%
    group_modify(~ fit_apc_piecewise(.x)) %>%
    ungroup() %>%
    mutate(Age = age_labels[as.character(AgeGroup)]) %>%
    dplyr::select(Sex = Sex_lbl, Age, Period, APC, APC_lo, APC_hi, p) %>%
    dplyr::arrange(Sex, Age, Period)
}

# -----------------------------------------------------------------------------
# fmt_results()
#   Rounds APC columns and formats p-values for reporting.
# -----------------------------------------------------------------------------
fmt_results <- function(results) {
  results %>%
    mutate(
      across(c(APC, APC_lo, APC_hi), ~ round(.x, 2)),
      p = signif(p, 3)
    )
}


# =============================================================================
# 1. DV-RELATED ASSAULT (FDV)
# =============================================================================

# --- 1a. Data ---
FDV      <- read_excel("DV_long.xlsx")
FDVgirls <- FDV %>% filter(Sex == 0)
FDVboys  <- FDV %>% filter(Sex == 1)


# --- 1b. Plots ---

# Convenience plotting subset: method-change annotation variables
fdv_plot <- FDV %>%
  filter(AgeGroup %in% c(1, 2, 3)) %>%
  mutate(
    Sex_lbl = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Age_lbl = factor(AgeGroup, levels = c(1, 2, 3),
                     labels = c("0–9", "10–14", "10–17"))
  )

# All age groups, split at 2021 method change
ggplot(
  data = fdv_plot,
  aes(x = Year, y = Rate,
      color = Sex_lbl, linetype = Age_lbl,
      group = interaction(Sex_lbl, Age_lbl))
) +
  geom_line(data = ~ dplyr::filter(.x, Year < 2021), linewidth = 0.9) +
  geom_line(data = ~ dplyr::filter(.x, Year >= 2021), linewidth = 0.9) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 2021, y = Inf, label = "Method change (2021)",
           vjust = -0.3, hjust = 0, size = 3.5, color = "grey40") +
  scale_linetype_manual(
    name   = "Age group",
    values = c("0–9" = "solid", "10–14" = "longdash", "10–17" = "dotdash")
  ) +
  labs(
    title = "Domestic Violence-related Assault Rates by Sex and Age Group",
    x = "Year", y = "Rate per 100,000 children",
    color = "Sex", linetype = "Age group"
  ) +
  theme_minimal()

# Individual age-group plots (for supplementary figures or checking)
for (ag in c(1, 2, 3)) {
  ag_label <- age_labels_fdv[as.character(ag)]
  p <- ggplot(
    data = dplyr::filter(fdv_plot, AgeGroup == ag),
    aes(x = Year, y = Rate, color = Sex_lbl, group = Sex_lbl)
  ) +
    geom_line(data = ~ dplyr::filter(.x, Year < 2021),  linewidth = 0.9) +
    geom_line(data = ~ dplyr::filter(.x, Year >= 2021), linewidth = 0.9) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2020.5, linetype = "dashed", color = "grey50") +
    annotate("text", x = 2021, y = Inf, label = "Method change (2021)",
             vjust = -0.3, hjust = 0, size = 3.5, color = "grey40") +
    labs(
      title = paste0("DV-related Assault, ", ag_label, " year-olds"),
      x = "Year", y = "Rate per 100,000 children", color = "Sex"
    ) +
    theme_minimal()
  print(p)
}


# --- 1c. Trend analysis (quasi-Poisson piecewise, knot = 2021) ---

fdv_results <- run_results_loop(
  data       = FDV,
  age_groups = c(1, 2, 3),
  age_labels = age_labels_fdv
)

print("DV-related Assault — APC by sex, age group, and period:")
print(fmt_results(fdv_results))


# =============================================================================
# 2. DV-RELATED SEXUAL ASSAULT (DVSA)
# =============================================================================

# --- 2a. Data ---
DVSA      <- read_excel("DVSA_long.xlsx")
DVSAgirls <- DVSA %>% filter(Sex == 0)
DVSAboys  <- DVSA %>% filter(Sex == 1)


# --- 2b. Plots ---

# Convenience plotting subset
dvsa_plot <- DVSA %>%
  filter(AgeGroup %in% c(1, 3)) %>%
  mutate(
    Sex_lbl = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Age_lbl = factor(AgeGroup, levels = c(1, 3), labels = c("0–9", "10–17"))
  )

# Both age groups combined, split at 2021 method change
ggplot(
  data = dvsa_plot,
  aes(x = Year, y = Rate,
      color = Sex_lbl, linetype = Age_lbl,
      group = interaction(Sex_lbl, Age_lbl))
) +
  geom_line(data = ~ dplyr::filter(.x, Year < 2021), linewidth = 0.9) +
  geom_line(data = ~ dplyr::filter(.x, Year >= 2021), linewidth = 0.9) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 2021, y = Inf, label = "Method change (2021)",
           vjust = -0.3, hjust = 0, size = 3.5, color = "grey40") +
  scale_linetype_manual(
    name   = "Age group",
    values = c("0–9" = "solid", "10–17" = "longdash")
  ) +
  labs(
    title = "DV-related Sexual Assault Rates by Sex and Age Group",
    x = "Year", y = "Rate per 100,000 children",
    color = "Sex", linetype = "Age group"
  ) +
  theme_minimal()

# Individual age-group plots
for (ag in c(1, 3)) {
  ag_label <- age_labels_dvsa[as.character(ag)]
  p <- ggplot(
    data = dplyr::filter(dvsa_plot, AgeGroup == ag),
    aes(x = Year, y = Rate, color = Sex_lbl, group = Sex_lbl)
  ) +
    geom_line(data = ~ dplyr::filter(.x, Year < 2021),  linewidth = 0.9) +
    geom_line(data = ~ dplyr::filter(.x, Year >= 2021), linewidth = 0.9) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2020.5, linetype = "dashed", color = "grey50") +
    annotate("text", x = 2021, y = Inf, label = "Method change (2021)",
             vjust = -0.3, hjust = 0, size = 3.5, color = "grey40") +
    labs(
      title = paste0("DV-related Sexual Assault, ", ag_label, " year-olds"),
      x = "Year", y = "Rate per 100,000 children", color = "Sex"
    ) +
    theme_minimal()
  print(p)
}


# --- 2c. Trend analysis (quasi-Poisson piecewise, knot = 2021) ---

dvsa_results <- run_results_loop(
  data       = DVSA,
  age_groups = c(1, 3),
  age_labels = age_labels_dvsa
)

print("DV-related Sexual Assault — APC by sex, age group, and period:")
print(fmt_results(dvsa_results))
