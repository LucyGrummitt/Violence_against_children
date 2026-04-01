library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(segmented)
library(readxl)

# ── Data ──────────────────────────────────────────────────────────────────────
# Children in substantiations
substantiations <- read_excel("CMrates_long.xlsx")
# Australia-wide
australia <- substantiations %>% filter(State == "9")
after2014 <- australia %>% filter(Financial_year_end >= "2013")
CMgirls <- after2014 %>% filter(Sex == 0)
CMboys <- after2014 %>% filter(Sex == 1)

#CM substantiated cases by type
casetypes <- read_excel("Cases by type.xlsx")

physical  <- casetypes %>% filter(Type == 1)
sexual    <- casetypes %>% filter(Type == 2)
emotional <- casetypes %>% filter(Type == 3)
neglect   <- casetypes %>% filter(Type == 4)

# Plot overall rates per 100,000 children, by sex
ggplot(
  data = dplyr::filter(after2014, Age_group ==5, Type ==5, Sex <=1), 
  aes(x = Financial_year_end, y = Rate_per100000,
      color = factor(Sex, levels = c(0, 1), 
                     labels = 
                       c("Female", "Male")))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(500, 1000)) +
  labs(title = "Substantiated Maltreatment, 0-17 year-olds", x = "Year", 
       y = "Rate per 100,000 children", color = "Sex") +
  theme_minimal()



# ── Fit QP models and extract predicted counts ────────────────────────────────
get_predicted <- function(dat, type_label) {
  m <- glm(Number_substantiated ~ Financial_year_end + offset(log(Population)),
           family = quasipoisson, data = dat)
  dat %>%
    mutate(
      Predicted_count = fitted(m),
      Type            = type_label
    ) %>%
    dplyr::select(Financial_year_end, Type,
                  Observed_count = Number_substantiated, Predicted_count)
}

predicted_all <- bind_rows(
  get_predicted(physical,  "Physical"),
  get_predicted(sexual,    "Sexual"),
  get_predicted(emotional, "Emotional"),
  get_predicted(neglect,   "Neglect")
) %>%
  mutate(Type = factor(Type, levels = c("Physical", "Sexual", "Emotional", "Neglect")))

# ── Shared colour palette ─────────────────────────────────────────────────────
type_colours <- c(
  "Physical"  = "#F8766D",
  "Sexual"    = "#7CAE00",
  "Emotional" = "#00BFC4",
  "Neglect"   = "#C77CFF"
)


# ── Plot all types on one panel ────────────────────────────────────────────
ggplot(predicted_all, aes(x = Financial_year_end, colour = Type)) +
  geom_line(aes(y = Observed_count), linewidth = 0.6) +
  geom_point(aes(y = Observed_count), size = 1.5) +
  geom_line(aes(y = Predicted_count), linewidth = 0.8, linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(predicted_all$Financial_year_end), 
                                  max(predicted_all$Financial_year_end), by = 2)) +
  scale_colour_manual(values = type_colours) +
  labs(
    title    = "Substantiated Child Maltreatment by Type",
    x        = "Year",
    y        = "Number of substantiations",
    colour   = "Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

# Set up subsets and helpers
subsets <- list(
  physical  = physical,
  sexual    = sexual,
  emotional = emotional,
  neglect   = neglect,
  CMgirls = CMgirls,
  CMboys = CMboys
)

# ── Helper: APC from any model ────────────────────────────────────────────────
apc_from_model <- function(mod, slope_name = "Financial_year_end") {
  b  <- coef(mod)[[slope_name]]
  se <- sqrt(vcov(mod)[slope_name, slope_name])
  tibble(
    APC    = (exp(b) - 1) * 100,
    APC_lo = (exp(b - 1.96 * se) - 1) * 100,
    APC_hi = (exp(b + 1.96 * se) - 1) * 100
  )
}

# ── Helper: delta-AIC (linear vs best 1-joinpoint) ───────────────────────────
delta_aic <- function(df) {
  df <- df %>% arrange(Financial_year_end) %>% mutate(lr = log(Rate_per100000))
  m0   <- lm(lr ~ Financial_year_end, data = df)
  aic0 <- AIC(m0)

  yrs  <- df$Financial_year_end
  cand <- seq(min(yrs) + 3, max(yrs) - 3)
  best <- list(tau = NA, aic = Inf)
  for (t in cand) {
    h <- pmax(0, df$Financial_year_end - t)
    m <- lm(lr ~ Financial_year_end + h, data = df)
    a <- AIC(m)
    if (a < best$aic) best <- list(tau = t, aic = a)
  }
  c(AIC_linear = aic0, AIC_1JP = best$aic,
    deltaAIC = best$aic - aic0, tau = best$tau)
}

# ── Main loop ─────────────────────────────────────────────────────────────────
results <- list()

for (nm in names(subsets)) {
  dat <- subsets[[nm]]
  cat("\n\n══════════════════════════════════════════════════════\n")
  cat("  GROUP:", nm, "\n")
  cat("══════════════════════════════════════════════════════\n")

  # 1. Log-linear model (OLS on log-rate)
  lm_fit <- lm(log(Rate_per100000) ~ Financial_year_end, data = dat)
  cat("\n── Log-linear model ──\n")
  print(tidy(lm_fit))
  apc_lm <- apc_from_model(lm_fit) %>% mutate(Group = nm, Model = "LM log-rate")
  cat("APC:", round(apc_lm$APC, 2),
      "95% CI [", round(apc_lm$APC_lo, 2), ",", round(apc_lm$APC_hi, 2), "]\n")

  # 2. Quasi-Poisson on counts with population offset
  qp_fit <- glm(Number_substantiated ~ Financial_year_end + offset(log(Population)),
                family = quasipoisson, data = dat)
  apc_qp <- apc_from_model(qp_fit) %>% mutate(Group = nm, Model = "QP offset")
  cat("\n── Quasi-Poisson model ──\n")
  cat("APC:", round(apc_qp$APC, 2),
      "95% CI [", round(apc_qp$APC_lo, 2), ",", round(apc_qp$APC_hi, 2), "]\n")

  # 3. Davies test for slope change
  cat("\n── Davies test ──\n")
  dav <- davies.test(lm_fit, seg.Z = ~Financial_year_end)
  print(dav)

  # 4. Segmented model (1 joinpoint)
  cat("\n── Segmented model (1 joinpoint) ──\n")
  seg_fit <- tryCatch(
    segmented(lm_fit, seg.Z = ~Financial_year_end, npsi = 1),
    error = function(e) { cat("  Segmented model failed:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(seg_fit)) {
    print(summary(seg_fit))
    sl <- slope(seg_fit)$Financial_year_end %>% as.data.frame()
    seg_apc <- transform(
      sl,
      segment = c("pre", "post")[seq_len(nrow(sl))],
      APC     = (exp(`Est.`) - 1) * 100,
      APC_lo  = (exp(`CI(95%).l`) - 1) * 100,
      APC_hi  = (exp(`CI(95%).u`) - 1) * 100
    )
    cat("\nSegment-wise APC:\n")
    print(seg_apc[, c("segment", "APC", "APC_lo", "APC_hi")])
  }

  # 5. Delta-AIC
  cat("\n── Delta-AIC (linear vs 1-joinpoint) ──\n")
  daic <- delta_aic(dat)
  print(daic)

  # Store tidy APC results for summary table
  results[[nm]] <- bind_rows(apc_lm, apc_qp)
}

# ── Summary table of APCs ─────────────────────────────────────────────────────
cat("\n\n══════════════════════════════════════════════════════\n")
cat("  SUMMARY: APC estimates across all groups and models\n")
cat("══════════════════════════════════════════════════════\n")
summary_tbl <- bind_rows(results) %>%
  dplyr::select(Group, Model, APC, APC_lo, APC_hi) %>%
  mutate(across(c(APC, APC_lo, APC_hi), ~round(., 2)))
print(summary_tbl, n = Inf)
