# R/03_rq4.R
# RQ4: Why do dogs get euthanized, and what drives the differences between
#      municipalities? Euthanasia categories (MoE definitions):
#   ① not suitable for adoption (incurable illness, infectious disease, aggression) - vet judgement
#   ② other: no adopter found, capacity limits, cannot manage (e.g. unweaned puppies, unsocialised dogs)
#   ③ died after intake
#
# Input : data/processed/municipal_r06.csv, output/rq2_betabinom_estimates.csv (from R/02_models.R)
# Output: output/rq4_*.csv, output/rq4_categories.png
# Run   : source("R/03_rq4.R") from the project root, after R/02_models.R

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(VGAM); library(here)
})
dir.create(here("output"), showWarnings = FALSE)

flags <- read_csv(here("output/rq2_betabinom_estimates.csv"), show_col_types = FALSE) |>
  filter(outcome == "euthanasia_rate") |>
  select(jurisdiction, flag)

dog <- read_csv(here("data/processed/municipal_r06.csv"), show_col_types = FALSE) |>
  filter(species == "dog", intake_total > 0) |>
  left_join(flags, by = "jurisdiction") |>
  mutate(n = intake_total,
         r1 = euthanized_1 / n, r2 = euthanized_2 / n, r3 = euthanized_3 / n,
         r_total = euthanized_total / n,
         share_young = intake_total_young / n,                   # puppies among intake
         share_stray_adult = intake_unknown_adult / n,           # adult dogs with no known owner
         young_share_of_e1 = ifelse(euthanized_1 > 0, euthanized_1_young / euthanized_1, NA))

# ---- (a) Which category drives the differences? -------------------------
# Var(total) = Cov(r1,total) + Cov(r2,total) + Cov(r3,total), intake-weighted.
wcov <- function(a, b, w) sum(w * (a - weighted.mean(a, w)) * (b - weighted.mean(b, w))) / sum(w)
V <- wcov(dog$r_total, dog$r_total, dog$n)
decomp <- tibble(
  category = c("① not suitable", "② no adopter / capacity", "③ died after intake"),
  share_of_variance = c(wcov(dog$r1, dog$r_total, dog$n),
                        wcov(dog$r2, dog$r_total, dog$n),
                        wcov(dog$r3, dog$r_total, dog$n)) / V)
write_csv(decomp, here("output/rq4_variance_decomposition.csv"))

# ---- (b) Category profile by RQ2 group ----------------------------------
by_group <- dog |>
  group_by(flag) |>
  summarise(municipalities = n(), dogs = sum(n),
            r1 = sum(euthanized_1) / dogs, r2 = sum(euthanized_2) / dogs, r3 = sum(euthanized_3) / dogs,
            young_share_of_e1 = sum(euthanized_1_young, na.rm = TRUE) / sum(euthanized_1),
            .groups = "drop")
write_csv(by_group, here("output/rq4_by_group.csv"))

high <- dog |>
  filter(flag == "Clearly above average") |>
  select(jurisdiction, n, euthanized_1, euthanized_2, euthanized_3, r1, r2, r3, r_total,
         euthanized_1_young, young_share_of_e1, share_young, share_stray_adult) |>
  arrange(desc(r_total))
write_csv(high, here("output/rq4_high_group.csv"))

# ---- (c) Beta-binomial regression for category ① ------------------------
# Is a high ① rate associated with the kind of dogs taken in?
# (ecological association only; ① is a judgement made by each municipality's vets)
dm <- dog |> filter(!is.na(share_young))
m0 <- suppressWarnings(vglm(cbind(euthanized_1, n - euthanized_1) ~ 1, betabinomial, data = dm))
m1 <- suppressWarnings(vglm(cbind(euthanized_1, n - euthanized_1) ~ share_young + share_stray_adult + jurisdiction_type,
                            betabinomial, data = dm))
lrt1 <- suppressWarnings(lrtest_vglm(m1, m0))
cf <- coef(summary(m1))
coefs <- tibble(term = rownames(cf), estimate = cf[, 1], se = cf[, 2], z = cf[, 3], p = cf[, 4]) |>
  filter(!grepl("^\\(Intercept\\):2", term)) |>
  mutate(odds_ratio_per_10pt = ifelse(grepl("share", term), exp(estimate * 0.1), NA))
rho0 <- unname(Coef(m0)["rho"])
rho1 <- unname(plogis(coef(m1)["(Intercept):2"]))   # rho uses a logit link
write_csv(coefs, here("output/rq4_e1_regression.csv"))

# ---- figure: category breakdown for the high group -----------------------
fig <- high |>
  select(jurisdiction, n, r1, r2, r3) |>
  pivot_longer(r1:r3, names_to = "cat", values_to = "rate") |>
  mutate(cat = factor(cat, c("r3", "r2", "r1"),
                      c("③ died after intake", "② no adopter / capacity", "① not suitable")),
         label = paste0(jurisdiction, " (n=", n, ")"),
         label = factor(label, rev(unique(label))))
p <- ggplot(fig, aes(rate, label, fill = cat)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("① not suitable" = "#B23A3A", "② no adopter / capacity" = "#E3A13B",
                               "③ died after intake" = "grey70"),
                    breaks = c("① not suitable", "② no adopter / capacity", "③ died after intake")) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Dogs euthanized / dogs taken in (FY2024)", y = NULL, fill = NULL,
       title = "In high-euthanasia municipalities, most euthanasia is category ①",
       caption = "Municipalities clearly above average in the RQ2 beta-binomial model. Source: Ministry of the Environment") +
  theme_minimal() + theme(legend.position = "top")
ggsave(here("output/rq4_categories.png"), p, width = 8, height = 6, dpi = 150)

if (!exists("QUIET")) {
  cat("\n== variance decomposition ==\n"); print(decomp)
  cat("\n== by group ==\n"); print(by_group, width = Inf)
  cat("\n== high group ==\n"); print(high, width = Inf)
  cat("\n== ① regression (beta-binomial) ==\n"); print(coefs, width = Inf)
  cat(sprintf("rho intercept-only = %.3f, rho with covariates = %.3f\n", rho0, rho1))
  print(lrt1)
}
