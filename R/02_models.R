# R/02_models.R
# RQ1: Did the trend in the national dog euthanasia rate change around the
#      FY2013 amendment (Animal Welfare Act, effective 2013-09-01)?
# RQ2: How much do municipalities really differ in dog adoption rate once
#      small-sample noise is accounted for? (beta-binomial, empirical Bayes)
#
# Input : data/processed/national_timeseries.csv, data/processed/municipal_r06.csv
# Output: output/rq1_break_tests.csv, output/rq2_betabinom_estimates.csv,
#         output/rq1_breaks.png, output/rq2_shrinkage.png
# Run   : source("R/02_models.R") from the project root (needs strucchange, VGAM)

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2)
  library(strucchange); library(VGAM); library(here)
})
dir.create(here("output"), showWarnings = FALSE)
set.seed(20261110)

# ---------------------------------------------------------------- RQ1 ----
nat <- read_csv(here("data/processed/national_timeseries.csv"), show_col_types = FALSE) |>
  filter(species == "dog") |>
  arrange(fiscal_year) |>
  mutate(rate = euthanized / intake,
         logit_rate = qlogis(rate),
         t = fiscal_year - 2004)

# (a) Data-driven breakpoints in a linear trend on the logit scale
#     (Bai-Perron; h = 4 years minimum segment because n = 21)
bp <- breakpoints(logit_rate ~ t, data = nat, h = 4)
bp_bic <- summary(bp)$RSS["BIC", ]
n_break <- as.integer(names(which.min(bp_bic)))
break_years <- if (n_break > 0) nat$fiscal_year[breakpoints(bp, breaks = n_break)$breakpoints] else integer(0)

# (b) Chow tests at pre-specified candidate years
#     A break "at year Y" = new intercept & slope from year Y onward.
chow_at <- function(y) {
  pt <- which(nat$fiscal_year == y) - 1          # last obs of first segment
  ct <- sctest(logit_rate ~ t, data = nat, type = "Chow", point = pt)
  tibble(test = "Chow (logit rate, OLS)", break_from = y,
         F = unname(ct$statistic), p_value = ct$p.value)
}
chow <- bind_rows(lapply(c(2013, 2014, 2021), chow_at))

# (c) Same idea with a quasi-binomial GLM (uses the counts, allows overdispersion)
glm_break <- function(y) {
  d <- nat |> mutate(post = as.integer(fiscal_year >= y), t_post = post * (fiscal_year - y))
  m0 <- glm(cbind(euthanized, intake - euthanized) ~ t, family = quasibinomial, data = d)
  m1 <- glm(cbind(euthanized, intake - euthanized) ~ t + post + t_post, family = quasibinomial, data = d)
  a  <- anova(m0, m1, test = "F")
  tibble(test = "quasi-binomial GLM, F", break_from = y,
         F = a$F[2], p_value = a$`Pr(>F)`[2],
         slope_before = coef(m1)["t"], slope_after = coef(m1)["t"] + coef(m1)["t_post"],
         dispersion = summary(m1)$dispersion)
}
glmb <- bind_rows(lapply(c(2013, 2014, 2021), glm_break))

rq1 <- bind_rows(
  chow,
  glmb,
  tibble(test = "Bai-Perron (BIC-selected)", break_from = NA_real_,
         F = NA_real_, p_value = NA_real_,
         note = paste0("breaks=", n_break, if (n_break > 0) paste0(" at FY", paste(break_years + 1, collapse = ", FY")) else ""))
)
write_csv(rq1, here("output/rq1_break_tests.csv"))

fit_bp <- if (n_break > 0) breakpoints(bp, breaks = n_break) else NULL
if (n_break > 0) {
  nat$seg <- breakfactor(fit_bp)
  nat$fitted_bp <- plogis(fitted(lm(logit_rate ~ seg / t, data = nat)))
} else { nat$seg <- factor(1); nat$fitted_bp <- NA }
p1 <- ggplot(nat, aes(fiscal_year, rate)) +
  geom_vline(xintercept = 2013.5, linetype = 2, colour = "grey50") +
  annotate("text", x = 2013.6, y = 0.82, label = "2012 amendment\nin force (Sep 2013)", hjust = 0, size = 3, colour = "grey30") +
  geom_point(size = 2) +
  { if (n_break > 0) geom_line(aes(y = fitted_bp, group = seg), colour = "#1D4F8A", linewidth = 1) } +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.9)) +
  scale_x_continuous(breaks = seq(2004, 2024, 4)) +
  labs(x = "Fiscal year", y = "Dogs euthanized / dogs taken in",
       title = "Dog euthanasia rate with data-driven breakpoints",
       caption = "Source: Ministry of the Environment. Blue: Bai-Perron segmented linear trend on the logit scale") +
  theme_minimal()
ggsave(here("output/rq1_breaks.png"), p1, width = 8, height = 4.5, dpi = 150)

# ---------------------------------------------------------------- RQ2 ----
# NOTE: adoption rate (adopted / intake) is confounded by the return rate:
# a municipality that returns most stray dogs to their owners looks like a
# "low-adoption" place even though that is a good outcome (cor = -0.84 in FY2024).
# So RQ2 models two outcomes that do not penalise returns:
#   euth  : euthanized_total / intake_total             (殺処分率)
#   rehome: adopted / (adopted + euthanized_total)      (返還されなかった犬のうち譲渡に至った割合)
mun <- read_csv(here("data/processed/municipal_r06.csv"), show_col_types = FALSE) |>
  filter(species == "dog", intake_total > 0)
cat(sprintf("cor(return rate, adoption rate) = %.2f\n",
            cor(mun$returned / mun$intake_total, mun$adopted / mun$intake_total)))

fit_bb <- function(d, y, n, label) {
  d <- d |> mutate(.y = {{ y }}, .n = {{ n }}) |> filter(.n > 0)
  m <- suppressWarnings(vglm(cbind(.y, .n - .y) ~ 1, betabinomial, data = d))
  cf <- Coef(m); mu <- unname(cf["mu"]); rho <- unname(cf["rho"])
  ab <- (1 - rho) / rho; a0 <- mu * ab; b0 <- (1 - mu) * ab
  m_type <- suppressWarnings(vglm(cbind(.y, .n - .y) ~ jurisdiction_type, betabinomial, data = d))
  lrt <- suppressWarnings(lrtest_vglm(m_type, m))
  est <- d |>
    transmute(outcome = label, jurisdiction, jurisdiction_type, n = .n, y = .y,
              raw_rate = .y / .n,
              eb_rate = (a0 + .y) / (a0 + b0 + .n),
              lo95 = qbeta(0.025, a0 + .y, b0 + .n - .y),
              hi95 = qbeta(0.975, a0 + .y, b0 + .n - .y),
              flag = case_when(lo95 > mu ~ "Clearly above average",
                               hi95 < mu ~ "Clearly below average",
                               TRUE ~ "Not distinguishable")) |>
    arrange(desc(eb_rate))
  list(est = est, mu = mu, rho = rho, a0 = a0, b0 = b0,
       type_p = lrt@Body[2, "Pr(>Chisq)"], label = label)
}

r_euth   <- fit_bb(mun, euthanized_total, intake_total, "euthanasia_rate")
r_rehome <- fit_bb(mun, adopted, adopted + euthanized_total, "rehoming_share")
est <- bind_rows(r_euth$est, r_rehome$est)
write_csv(est, here("output/rq2_betabinom_estimates.csv"))
rq2_summary <- tibble(
  outcome = c(r_euth$label, r_rehome$label),
  mu = c(r_euth$mu, r_rehome$mu), rho = c(r_euth$rho, r_rehome$rho),
  p_type = c(r_euth$type_p, r_rehome$type_p),
  n_high = c(sum(r_euth$est$flag == "Clearly above average"), sum(r_rehome$est$flag == "Clearly above average")),
  n_low  = c(sum(r_euth$est$flag == "Clearly below average"), sum(r_rehome$est$flag == "Clearly below average")))
write_csv(rq2_summary, here("output/rq2_summary.csv"))

p2 <- ggplot(r_euth$est, aes(raw_rate, eb_rate, size = n, colour = flag)) +
  geom_abline(linetype = 3, colour = "grey60") +
  geom_hline(yintercept = r_euth$mu, linetype = 2, colour = "grey40") +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = c("Clearly above average" = "#B23A3A", "Clearly below average" = "#1D7A46", "Not distinguishable" = "grey60")) +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  labs(x = "Raw euthanasia rate", y = "Shrunken estimate (empirical Bayes)",
       size = "Dogs taken in", colour = NULL,
       title = "Small municipalities are pulled toward the average",
       caption = "Dogs, FY2024. Beta-binomial model. Dashed line: estimated average") +
  theme_minimal()
ggsave(here("output/rq2_shrinkage.png"), p2, width = 8, height = 5.5, dpi = 150)

# ---------------------------------------------------------------- log ----
if (!exists("QUIET")) {
cat("\n== RQ1 ==\n"); print(rq1, width = Inf)
cat("\n== RQ2 ==\n"); print(rq2_summary)
for (r in list(r_euth, r_rehome)) {
  cat("\n--", r$label, "--\n")
  print(r$est |> filter(flag != "Not distinguishable") |> select(jurisdiction, n, raw_rate, eb_rate, lo95, hi95, flag), n = 80)
}
}
