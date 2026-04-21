# =============================================================================
# Sexual Assault — trend analysis by sex and age group
# =============================================================================

library(dplyr)
library(ggplot2)
library(broom)
library(segmented)


# =============================================================================
# 0. SHARED HELPERS
# =============================================================================

# -----------------------------------------------------------------------------
# apc_from_model()
#   Extracts the slope and SE for a named coefficient from any fitted model
#   and returns APC with 95% CI via normal approximation on the log scale.
#   Works for both lm() and glm() objects.
# -----------------------------------------------------------------------------
apc_from_model <- function(mod, slope_name = "Year") {
  b  <- coef(mod)[[slope_name]]
  se <- sqrt(vcov(mod)[slope_name, slope_name])
  tibble(
    APC    = (exp(b) - 1) * 100,
    APC_lo = (exp(b - 1.96 * se) - 1) * 100,
    APC_hi = (exp(b + 1.96 * se) - 1) * 100
  )
}

# -----------------------------------------------------------------------------
# delta_aic_1jp()
#   Grid-searches for the best single joinpoint on log(Rate) ~ Year and
#   returns AIC for the linear model, the best 1-joinpoint model, ΔAIC,
#   and the candidate breakpoint year (tau).
#   Margins: at least 3 years required on each side of the knot.
# -----------------------------------------------------------------------------
delta_aic_1jp <- function(df, year_col = "Year", rate_col = "Rate") {
  df <- df %>%
    dplyr::arrange(.data[[year_col]]) %>%
    dplyr::mutate(lr = log(.data[[rate_col]]))

  yrs  <- df[[year_col]]
  cand <- seq(min(yrs) + 3, max(yrs) - 3)

  m0   <- lm(lr ~ Year, data = df)
  aic0 <- AIC(m0)

  best <- list(tau = NA_real_, aic = Inf)
  for (t in cand) {
    h <- pmax(0, df[[year_col]] - t)
    a <- AIC(lm(lr ~ Year + h, data = df))
    if (a < best$aic) best <- list(tau = t, aic = a)
  }

  c(AIC_linear = aic0, AIC_1JP = best$aic,
    deltaAIC   = best$aic - aic0, tau = best$tau)
}

# -----------------------------------------------------------------------------
# seg_apc_table()
#   Extracts pre- and post-breakpoint slopes from a segmented model,
#   converts them to APCs and 95% CIs, and returns a tidy tibble.
# -----------------------------------------------------------------------------
seg_apc_table <- function(seg_mod) {
  sl <- slope(seg_mod)$Year |> as.data.frame()
  tibble(
    Segment = c("Pre-breakpoint", "Post-breakpoint")[seq_len(nrow(sl))],
    APC     = (exp(sl[["Est."]])          - 1) * 100,
    APC_lo  = (exp(sl[["CI(95%).l"]])     - 1) * 100,
    APC_hi  = (exp(sl[["CI(95%).u"]])     - 1) * 100,
    SE_log  = sl[["St.Err."]],
    t_value = sl[["t value"]]
  )
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
# 1. DATA
# =============================================================================

SA_both   <- read_excel("SA_both.xlsx")    # Sex (0=Female, 1=Male), AgeGroup (1=0–9, 3=10–17),
                                            # Year, Rate, Count, Population
SA_females <- SA_both %>% filter(Sex == 0)
SA_males   <- SA_both %>% filter(Sex == 1)

# Convenience subsets
girls_09   <- SA_females %>% filter(AgeGroup == 1)
girls_1017 <- SA_females %>% filter(AgeGroup == 3)
boys_09    <- SA_males   %>% filter(AgeGroup == 1)
boys_1017  <- SA_males   %>% filter(AgeGroup == 3)


# =============================================================================
# 2. PLOTS
# =============================================================================

sa_plot <- SA_both %>%
  filter(AgeGroup %in% c(1, 3)) %>%
  mutate(
    Sex_lbl = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Age_lbl = factor(AgeGroup, levels = c(1, 3), labels = c("0–9", "10–17"))
  )

# Combined plot — colour by sex, linetype by age group
ggplot(
  data = sa_plot,
  aes(x = Year, y = Rate,
      color = Sex_lbl, linetype = Age_lbl,
      group = interaction(Sex_lbl, Age_lbl))
) +
  geom_line() +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("0–9" = "solid", "10–17" = "longdash")) +
  labs(
    title    = "Sexual Assault Rates by Sex and Age Group",
    x        = "Year",
    y        = "Rate per 100,000 children",
    color    = "Sex",
    linetype = "Age group"
  ) +
  theme_minimal()

# Individual age-group plots (faceted by sex)
ggplot(
  data = sa_plot,
  aes(x = Year, y = Rate, color = Sex_lbl, group = Sex_lbl)
) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ Age_lbl, nrow = 1, scales = "free_y") +
  labs(
    title = "Sexual Assault Rates by Age Group and Sex",
    x     = "Year",
    y     = "Rate per 100,000 children",
    color = "Sex"
  ) +
  theme_minimal()


# =============================================================================
# 3. QUASI-POISSON TREND ANALYSIS (primary)
# =============================================================================

# Run quasi-Poisson (Count ~ Year + offset(log(Population))) for every
# sex × age-group stratum and collect results in a single table.

sa_qp_results <- SA_both %>%
  filter(
    AgeGroup %in% c(1, 3),
    !is.na(Count), !is.na(Population), Population > 0
  ) %>%
  mutate(
    Sex_lbl = factor(Sex, levels = c(0, 1), labels = c("Girls", "Boys")),
    Age_lbl = factor(AgeGroup, levels = c(1, 3), labels = c("0–9", "10–17"))
  ) %>%
  group_by(Sex_lbl, Age_lbl) %>%
  group_modify(~ {
    m <- glm(Count ~ Year + offset(log(Population)),
             family = quasipoisson, data = .x)
    apc_from_model(m) %>%
      mutate(p = summary(m)$coefficients["Year", "Pr(>|t|)"])
  }) %>%
  ungroup() %>%
  dplyr::select(Sex = Sex_lbl, Age = Age_lbl, APC, APC_lo, APC_hi, p) %>%
  dplyr::arrange(Sex, Age)

print("Sexual Assault — quasi-Poisson APC by sex and age group:")
print(fmt_results(sa_qp_results))


# =============================================================================
# 4. LOG-LINEAR TREND ANALYSIS (sensitivity / joinpoint input)
# =============================================================================

# Fit lm(log(Rate) ~ Year) for each stratum; used as input to Davies test
# and segmented regression.

lm_girls_09   <- lm(log(Rate) ~ Year, data = girls_09)
lm_girls_1017 <- lm(log(Rate) ~ Year, data = girls_1017)
lm_boys_09    <- lm(log(Rate) ~ Year, data = boys_09)
lm_boys_1017  <- lm(log(Rate) ~ Year, data = boys_1017)

sa_lm_results <- bind_rows(
  apc_from_model(lm_girls_09)   %>% mutate(Sex = "Girls", Age = "0–9",   p = tidy(lm_girls_09)  %>% filter(term == "Year") %>% pull(p.value)),
  apc_from_model(lm_girls_1017) %>% mutate(Sex = "Girls", Age = "10–17", p = tidy(lm_girls_1017) %>% filter(term == "Year") %>% pull(p.value)),
  apc_from_model(lm_boys_09)    %>% mutate(Sex = "Boys",  Age = "0–9",   p = tidy(lm_boys_09)   %>% filter(term == "Year") %>% pull(p.value)),
  apc_from_model(lm_boys_1017)  %>% mutate(Sex = "Boys",  Age = "10–17", p = tidy(lm_boys_1017) %>% filter(term == "Year") %>% pull(p.value))
) %>%
  dplyr::select(Sex, Age, APC, APC_lo, APC_hi, p) %>%
  dplyr::arrange(Sex, Age)

print("Sexual Assault — log-linear APC by sex and age group:")
print(fmt_results(sa_lm_results))


# =============================================================================
# 5. JOINPOINT ANALYSIS
# =============================================================================

# --- 5a. Davies test (is there evidence of any slope change?) ---------------

cat("\n--- Davies test: Girls 0–9 ---\n");   print(davies.test(lm_girls_09,   seg.Z = ~ Year))
cat("\n--- Davies test: Girls 10–17 ---\n"); print(davies.test(lm_girls_1017, seg.Z = ~ Year))
cat("\n--- Davies test: Boys 0–9 ---\n");    print(davies.test(lm_boys_09,    seg.Z = ~ Year))
cat("\n--- Davies test: Boys 10–17 ---\n");  print(davies.test(lm_boys_1017,  seg.Z = ~ Year))

# --- Davies test — collect p-values into a tidy table -------------------

davies_results <- tibble(
  Sex  = c("Girls", "Girls", "Boys",  "Boys"),
  Age  = c("0–9",   "10–17", "0–9",   "10–17"),
  lm   = list(lm_girls_09, lm_girls_1017, lm_boys_09, lm_boys_1017)
) %>%
  mutate(
    davies_p = purrr::map_dbl(lm, ~ davies.test(.x, seg.Z = ~ Year)$p.value)
  ) %>%
  dplyr::select(Sex, Age, davies_p)

print("Davies test p-values (slope change):")
print(davies_results)
# --- 5b. ΔAIC grid search (does a 1-joinpoint model improve fit?) -----------

aic_results <- bind_rows(
  as_tibble_row(delta_aic_1jp(girls_09))   %>% mutate(Sex = "Girls", Age = "0–9"),
  as_tibble_row(delta_aic_1jp(girls_1017)) %>% mutate(Sex = "Girls", Age = "10–17"),
  as_tibble_row(delta_aic_1jp(boys_09))    %>% mutate(Sex = "Boys",  Age = "0–9"),
  as_tibble_row(delta_aic_1jp(boys_1017))  %>% mutate(Sex = "Boys",  Age = "10–17")
) %>%
  dplyr::select(Sex, Age, AIC_linear, AIC_1JP, deltaAIC, tau) %>%
  dplyr::arrange(Sex, Age)

print("ΔAIC — linear vs 1-joinpoint model (negative deltaAIC favours joinpoint):")
print(aic_results)


# --- 5c. Segmented models (fit where joinpoint is supported) ----------------
# Adjust npsi per stratum based on Davies test and ΔAIC results above.

seg_girls_09   <- segmented(lm_girls_09,   seg.Z = ~ Year, npsi = 1)
seg_girls_1017 <- segmented(lm_girls_1017, seg.Z = ~ Year, npsi = 2)
seg_boys_09    <- segmented(lm_boys_09,    seg.Z = ~ Year, npsi = 1)
seg_boys_1017  <- segmented(lm_boys_1017,  seg.Z = ~ Year, npsi = 1)

# Summaries and diagnostic plots
for (obj in list(seg_girls_09, seg_girls_1017, seg_boys_09, seg_boys_1017)) {
  print(summary(obj))
  plot(obj)
}

# --- 5d. Segment-specific APCs from segmented models -----------------------

seg_apc_results <- bind_rows(
  seg_apc_table(seg_girls_09)   %>% mutate(Sex = "Girls", Age = "0–9"),
  seg_apc_table(seg_girls_1017) %>% mutate(Sex = "Girls", Age = "10–17"),
  seg_apc_table(seg_boys_09)    %>% mutate(Sex = "Boys",  Age = "0–9"),
  seg_apc_table(seg_boys_1017)  %>% mutate(Sex = "Boys",  Age = "10–17")
) %>%
  dplyr::select(Sex, Age, Segment, APC, APC_lo, APC_hi) %>%
  dplyr::arrange(Sex, Age, Segment) %>%
  mutate(across(c(APC, APC_lo, APC_hi), ~ round(.x, 2)))

print("Segment-specific APCs from segmented regression:")
print(seg_apc_results)
