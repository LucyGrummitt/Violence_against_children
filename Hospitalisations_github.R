library(dplyr)
library(tidyverse)
library(summarytools)
library(ggplot2)
library(broom)      # For tidy regression outputs
library(readxl)

# ── Helper functions ─────────────────────────────────────────
# APC + 95% CI from a fitted log(Rate) ~ Year model
print_apc <- function(m, label) {
  b  <- coef(m)["Year"]
  se <- sqrt(vcov(m)["Year", "Year"])
  cat(sprintf(
    "%s -- APC: %.2f%% (95%% CI: %.2f%% to %.2f%%)\n",
    label,
    (exp(b)           - 1) * 100,
    (exp(b - 1.96*se) - 1) * 100,
    (exp(b + 1.96*se) - 1) * 100
  ))
}

# APC + 95% CI from a quasi-Poisson count model
qp_apc <- function(data, label) {
  m  <- glm(Count ~ Year + offset(log(Population)),
            family = quasipoisson, data = data)
  b  <- coef(m)["Year"]
  se <- sqrt(vcov(m)["Year", "Year"])
  cat(sprintf(
    "%s -- QP APC: %.2f%% (95%% CI: %.2f%% to %.2f%%)\n",
    label,
    (exp(b)           - 1) * 100,
    (exp(b - 1.96*se) - 1) * 100,
    (exp(b + 1.96*se) - 1) * 100
  ))
}

# Pre/post APC table from a segmented model
seg_apc_table <- function(seg_model, label) {
  sl  <- slope(seg_model)$Year |> as.data.frame()
  apc <- function(b) (exp(b) - 1) * 100
  out <- transform(
    sl,
    Group   = label,
    Segment = c("pre-break", "post-break")[seq_len(nrow(sl))],
    APC     = apc(`Est.`),
    APC_lo  = apc(`CI(95%).l`),
    APC_hi  = apc(`CI(95%).u`)
  )
  out[, c("Group", "Segment", "APC", "APC_lo", "APC_hi", "Est.", "St.Err.", "t value")]
}

# ΔAIC: linear vs. best 1-joinpoint model on log(Rate)
delta_aic <- function(data, label) {
  df   <- data %>% arrange(Year) %>% mutate(lr = log(Rate))
  m0   <- lm(lr ~ Year, data = df)
  aic0 <- AIC(m0)
  
  yrs  <- df$Year
  cand <- seq(min(yrs) + 3, max(yrs) - 3)
  best <- list(tau = NA, aic = Inf)
  
  for (t in cand) {
    h <- pmax(0, df$Year - t)
    m <- lm(lr ~ Year + h, data = df)
    a <- AIC(m)
    if (a < best$aic) best <- list(tau = t, aic = a)
  }
  
  cat(sprintf(
    "%s -- AIC linear: %.2f | AIC 1-JP: %.2f | ΔAIC: %.2f | tau: %d\n",
    label, aic0, best$aic, best$aic - aic0, best$tau
  ))
}

# ── 1. Load and prepare data ────────────────────────────────

hosp <- read_excel("Hospitalisations by sex.xlsx")

# Rename year column for convenience
hosp <- hosp %>% rename(Year = Year_end)

# ── 2. Prepare age-group subsets 0-4, 5-9, 10-14, 15-17──────

hosp_narrow <- hosp %>%
  filter(
    AgeGroup %in% c("0_4", "5_9", "10_14", "15_17"),
    Sex      %in% c("Females", "Males")
  ) %>%
  mutate(
    Age_lbl = factor(AgeGroup,
                     levels = c("0_4", "5_9", "10_14", "15_17"),
                     labels = c("0-4", "5-9", "10-14", "15-17")),
    Sex_lbl = factor(Sex, levels = c("Females", "Males"))
  )

# Convenience subsets
females_0_4   <- hosp_narrow %>% filter(Sex == "Females", AgeGroup == "0_4")
females_5_9   <- hosp_narrow %>% filter(Sex == "Females", AgeGroup == "5_9")
females_10_14 <- hosp_narrow %>% filter(Sex == "Females", AgeGroup == "10_14")
females_15_17 <- hosp_narrow %>% filter(Sex == "Females", AgeGroup == "15_17")

males_0_4   <- hosp_narrow %>% filter(Sex == "Males", AgeGroup == "0_4")
males_5_9   <- hosp_narrow %>% filter(Sex == "Males", AgeGroup == "5_9")
males_10_14 <- hosp_narrow %>% filter(Sex == "Males", AgeGroup == "10_14")
males_15_17 <- hosp_narrow %>% filter(Sex == "Males", AgeGroup == "15_17")

# ── 3. One plot per sex: all four age groups ─────────

ggplot(
  data = hosp_narrow %>% filter(Sex == "Females"),
  aes(x = Year, y = Rate, color = Age_lbl, group = Age_lbl)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Assault Hospitalisation in Females by Age Group",
    x     = "Year",
    y     = "Rate per 100,000 children",
    color = "Age group"
  ) +
  theme_minimal()

ggplot(
  data = hosp_narrow %>% filter(Sex == "Males"),
  aes(x = Year, y = Rate, color = Age_lbl, group = Age_lbl)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Assault Hospitalisation in Males by Age Group",
    x     = "Year",
    y     = "Rate per 100,000 children",
    color = "Age group"
  ) +
  theme_minimal()

# ── 5. Log-rate linear regression APC: narrow groups ───────

lm_females_0_4   <- lm(log(Rate) ~ Year, data = females_0_4)
lm_females_5_9   <- lm(log(Rate) ~ Year, data = females_5_9)
lm_females_10_14 <- lm(log(Rate) ~ Year, data = females_10_14)
lm_females_15_17 <- lm(log(Rate) ~ Year, data = females_15_17)

lm_males_0_4   <- lm(log(Rate) ~ Year, data = males_0_4)
lm_males_5_9   <- lm(log(Rate) ~ Year, data = males_5_9)
lm_males_10_14 <- lm(log(Rate) ~ Year, data = males_10_14)
lm_males_15_17 <- lm(log(Rate) ~ Year, data = males_15_17)

cat("\n--- Log-rate linear regression APC ---\n")
print_apc(lm_females_0_4,   "Females 0-4")
print_apc(lm_females_5_9,   "Females 5-9")
print_apc(lm_females_10_14, "Females 10-14")
print_apc(lm_females_15_17, "Females 15-17")
print_apc(lm_males_0_4,     "Males 0-4")
print_apc(lm_males_5_9,     "Males 5-9")
print_apc(lm_males_10_14,   "Males 10-14")
print_apc(lm_males_15_17,   "Males 15-17")

print(tidy(lm_females_0_4));   print(tidy(lm_females_5_9))
print(tidy(lm_females_10_14)); print(tidy(lm_females_15_17))
print(tidy(lm_males_0_4));     print(tidy(lm_males_5_9))
print(tidy(lm_males_10_14));   print(tidy(lm_males_15_17))

# ── 6. Quasi-Poisson APC ────────────────────

cat("\n--- Quasi-Poisson APC ---\n")
qp_apc(females_0_4,   "Females 0-4")
qp_apc(females_5_9,   "Females 5-9")
qp_apc(females_10_14, "Females 10-14")
qp_apc(females_15_17, "Females 15-17")
qp_apc(males_0_4,     "Males 0-4")
qp_apc(males_5_9,     "Males 5-9")
qp_apc(males_10_14,   "Males 10-14")
qp_apc(males_15_17,   "Males 15-17")

# ── 7. Davies test ──────────────────────────

cat("\n--- Davies tests for slope change ---\n")
davies.test(lm_females_0_4,   seg.Z = ~Year)
davies.test(lm_females_5_9,   seg.Z = ~Year)
davies.test(lm_females_10_14, seg.Z = ~Year)
davies.test(lm_females_15_17, seg.Z = ~Year)
davies.test(lm_males_0_4,     seg.Z = ~Year)
davies.test(lm_males_5_9,     seg.Z = ~Year)
davies.test(lm_males_10_14,   seg.Z = ~Year)
davies.test(lm_males_15_17,   seg.Z = ~Year)

# ── 8. Segmented models ─────────────────────

segmented_females_0_4   <- segmented(lm_females_0_4,   seg.Z = ~Year, npsi = 1)
segmented_females_5_9   <- segmented(lm_females_5_9,   seg.Z = ~Year, npsi = 1)
segmented_females_10_14 <- segmented(lm_females_10_14, seg.Z = ~Year, npsi = 1)
segmented_females_15_17 <- segmented(lm_females_15_17, seg.Z = ~Year, npsi = 1)

segmented_males_0_4   <- segmented(lm_males_0_4,   seg.Z = ~Year, npsi = 1)
segmented_males_5_9   <- segmented(lm_males_5_9,   seg.Z = ~Year, npsi = 1)
segmented_males_10_14 <- segmented(lm_males_10_14, seg.Z = ~Year, npsi = 1)
segmented_males_15_17 <- segmented(lm_males_15_17, seg.Z = ~Year, npsi = 1)

summary(segmented_females_0_4);   summary(segmented_females_5_9)
summary(segmented_females_10_14); summary(segmented_females_15_17)
summary(segmented_males_0_4);     summary(segmented_males_5_9)
summary(segmented_males_10_14);   summary(segmented_males_15_17)

print(seg_apc_table(segmented_females_0_4,   "Females 0-4"))
print(seg_apc_table(segmented_females_5_9,   "Females 5-9"))
print(seg_apc_table(segmented_females_10_14, "Females 10-14"))
print(seg_apc_table(segmented_females_15_17, "Females 15-17"))
print(seg_apc_table(segmented_males_0_4,     "Males 0-4"))
print(seg_apc_table(segmented_males_5_9,     "Males 5-9"))
print(seg_apc_table(segmented_males_10_14,   "Males 10-14"))
print(seg_apc_table(segmented_males_15_17,   "Males 15-17"))

plot(segmented_females_0_4,   main = "Females 0-4")
plot(segmented_females_5_9,   main = "Females 5-9")
plot(segmented_females_10_14, main = "Females 10-14")
plot(segmented_females_15_17, main = "Females 15-17")
plot(segmented_males_0_4,     main = "Males 0-4")
plot(segmented_males_5_9,     main = "Males 5-9")
plot(segmented_males_10_14,   main = "Males 10-14")
plot(segmented_males_15_17,   main = "Males 15-17")

# ── 9. ΔAIC ──────────────────────────────────

cat("\n--- ΔAIC: linear vs. 1-joinpoint model ---\n")
delta_aic(females_0_4,   "Females 0-4")
delta_aic(females_5_9,   "Females 5-9")
delta_aic(females_10_14, "Females 10-14")
delta_aic(females_15_17, "Females 15-17")
delta_aic(males_0_4,     "Males 0-4")
delta_aic(males_5_9,     "Males 5-9")
delta_aic(males_10_14,   "Males 10-14")
delta_aic(males_15_17,   "Males 15-17")



