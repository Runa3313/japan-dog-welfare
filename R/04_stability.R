# R/04_stability.R
# Are the FY2024 findings (RQ2 and RQ4) stable across FY2019-2024?
#   1. National trend by euthanasia category ①②③
#   2. Each year: beta-binomial empirical Bayes -> which municipalities are clearly above average?
#   3. Rank correlation of euthanasia rates between years
#   4. Mixed model: how much of the variation is a persistent municipality effect vs year-to-year noise?
#   5. Variance decomposition by category, each year (does ① always dominate?)
#
# Input : data/processed/municipal_panel.csv (from R/01_extract_pdf.R)
# Output: output/stab_*.csv, output/stab_*.png
# Run   : source("R/04_stability.R") from the project root (needs VGAM, lme4)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
  library(VGAM); library(lme4); library(here)
})
dir.create(here("output"), showWarnings = FALSE)

panel <- read_csv(here("data/processed/municipal_panel.csv"), show_col_types = FALSE) |>
  filter(species == "dog", intake_total > 0) |>
  mutate(n = intake_total)
years <- sort(unique(panel$fiscal_year))

# Municipalities reported in every year (4 core cities were created during the period:
# 水戸市, 松本市, 一宮市, 吹田市; their prefectures' figures change composition when that happens)
all_years <- panel |> count(jurisdiction) |> filter(n == length(years)) |> pull(jurisdiction)
new_core <- setdiff(unique(panel$jurisdiction), all_years)

# ---- 1. National trend by category ---------------------------------------
nat <- panel |>
  group_by(fiscal_year) |>
  summarise(intake = sum(n), e1 = sum(euthanized_1), e2 = sum(euthanized_2), e3 = sum(euthanized_3),
            e1_young = sum(euthanized_1_young, na.rm = TRUE), .groups = "drop") |>
  mutate(across(e1:e3, \(x) x / intake, .names = "{.col}_rate"))
write_csv(nat, here("output/stab_national_categories.csv"))

# ---- 2. Yearly beta-binomial EB, flag "clearly above average" ------------
eb_year <- function(d) {
  m <- suppressWarnings(vglm(cbind(euthanized_total, n - euthanized_total) ~ 1, betabinomial, data = d))
  cf <- Coef(m); mu <- unname(cf["mu"]); rho <- unname(cf["rho"])
  ab <- (1 - rho) / rho; a0 <- mu * ab; b0 <- (1 - mu) * ab
  d |> transmute(fiscal_year, jurisdiction, jurisdiction_type, n, euthanized_total,
                 raw_rate = euthanized_total / n,
                 eb_rate = (a0 + euthanized_total) / (a0 + b0 + n),
                 lo95 = qbeta(0.025, a0 + euthanized_total, b0 + n - euthanized_total),
                 hi95 = qbeta(0.975, a0 + euthanized_total, b0 + n - euthanized_total),
                 above = lo95 > mu, below = hi95 < mu, mu = mu, rho = rho)
}
eb <- panel |> group_by(fiscal_year) |> group_split() |> lapply(eb_year) |> bind_rows()
write_csv(eb, here("output/stab_eb_by_year.csv"))

yearly_fit <- eb |> distinct(fiscal_year, mu, rho) |>
  left_join(eb |> group_by(fiscal_year) |> summarise(n_above = sum(above), n_below = sum(below)),
            by = "fiscal_year")

persistence <- eb |>
  filter(jurisdiction %in% all_years) |>
  group_by(jurisdiction) |>
  summarise(years_above = sum(above), years_below = sum(below),
            above_2024 = above[fiscal_year == 2024],
            mean_eb = mean(eb_rate), .groups = "drop") |>
  arrange(desc(years_above), desc(mean_eb))
write_csv(persistence, here("output/stab_persistence.csv"))

# Of the municipalities flagged in FY2024, how many years were they flagged?
flag24 <- persistence |> filter(above_2024)
flag_hist <- persistence |> count(years_above, name = "municipalities")

# ---- 3. Rank correlation between years -----------------------------------
wide <- eb |> filter(jurisdiction %in% all_years) |>
  select(jurisdiction, fiscal_year, eb_rate) |>
  pivot_wider(names_from = fiscal_year, values_from = eb_rate)
rank_cor <- cor(wide[, -1], method = "spearman")
adjacent <- tibble(from = years[-length(years)], to = years[-1],
                   spearman = diag(rank_cor[-nrow(rank_cor), -1]))
write_csv(as_tibble(rank_cor, rownames = "year"), here("output/stab_rank_correlation.csv"))

# ---- 4. Mixed model: persistent municipality effect vs year-to-year ------
# logit(p_it) = year_t + u_i + v_it,  u_i ~ N(0, s_u^2) persistent, v_it ~ N(0, s_v^2) yearly
pm <- panel |> filter(jurisdiction %in% all_years) |>
  mutate(year = factor(fiscal_year), jy = interaction(jurisdiction, fiscal_year))
vc <- function(y) {
  f <- as.formula(paste0("cbind(", y, ", n - ", y, ") ~ year + (1 | jurisdiction) + (1 | jy)"))
  m <- glmer(f, family = binomial, data = pm, control = glmerControl(optimizer = "bobyqa"))
  v <- as.data.frame(VarCorr(m))
  s_u <- v$vcov[v$grp == "jurisdiction"]; s_v <- v$vcov[v$grp == "jy"]
  tibble(outcome = y, var_persistent = s_u, var_yearly = s_v,
         share_persistent = s_u / (s_u + s_v))
}
varcomp <- bind_rows(vc("euthanized_total"), vc("euthanized_1"))
write_csv(varcomp, here("output/stab_variance_components.csv"))

# ---- 5. Category decomposition each year ---------------------------------
wcov <- function(a, b, w) sum(w * (a - weighted.mean(a, w)) * (b - weighted.mean(b, w))) / sum(w)
decomp_year <- panel |>
  group_by(fiscal_year) |>
  group_modify(function(d, k) {
    r1 <- d$euthanized_1 / d$n; r2 <- d$euthanized_2 / d$n; r3 <- d$euthanized_3 / d$n
    r <- d$euthanized_total / d$n; V <- wcov(r, r, d$n)
    tibble(cat1 = wcov(r1, r, d$n) / V, cat2 = wcov(r2, r, d$n) / V, cat3 = wcov(r3, r, d$n) / V)
  }) |> ungroup()
write_csv(decomp_year, here("output/stab_decomp_by_year.csv"))

puppies <- panel |>
  filter(coalesce(euthanized_1_young, 0) > 0) |>
  select(fiscal_year, jurisdiction, euthanized_1_young) |>
  arrange(fiscal_year, desc(euthanized_1_young))
write_csv(puppies, here("output/stab_puppy_e1.csv"))

# ---- figures --------------------------------------------------------------
p1 <- nat |>
  select(fiscal_year, `① not suitable` = e1, `② no adopter / capacity` = e2, `③ died after intake` = e3) |>
  pivot_longer(-fiscal_year, names_to = "category", values_to = "dogs") |>
  ggplot(aes(fiscal_year, dogs, colour = category)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = c("① not suitable" = "#B23A3A", "② no adopter / capacity" = "#E3A13B",
                                 "③ died after intake" = "grey55")) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = years) +
  labs(x = "Fiscal year", y = "Dogs euthanized", colour = NULL,
       title = "② fell by 91% since FY2019; ① fell by 59%",
       caption = "Sum of all municipalities. Source: Ministry of the Environment") +
  theme_minimal() + theme(legend.position = "top")
ggsave(here("output/stab_national_categories.png"), p1, width = 8, height = 4.5, dpi = 150)

top <- persistence |> filter(years_above >= 3 | above_2024) |> pull(jurisdiction)
p2 <- eb |> filter(jurisdiction %in% top) |>
  mutate(jurisdiction = factor(jurisdiction, rev(persistence$jurisdiction[persistence$jurisdiction %in% top])),
         status = ifelse(above, "Clearly above average", "Not clearly above")) |>
  ggplot(aes(factor(fiscal_year), jurisdiction, fill = status)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = scales::percent(eb_rate, 1)), size = 2.8) +
  scale_fill_manual(values = c("Clearly above average" = "#E8A3A3", "Not clearly above" = "grey93")) +
  labs(x = "Fiscal year", y = NULL, fill = NULL,
       title = "Which municipalities are clearly above average, year by year",
       caption = "Cell text: empirical-Bayes euthanasia rate. Municipalities flagged in FY2024 or in 3+ of 6 years") +
  theme_minimal() + theme(legend.position = "top", panel.grid = element_blank())
ggsave(here("output/stab_flags_heatmap.png"), p2, width = 8, height = 7, dpi = 150)

if (!exists("QUIET")) {
  cat("\n== national ==\n"); print(nat, width = Inf)
  cat("\n== yearly fit ==\n"); print(yearly_fit)
  cat("\n== years flagged (municipalities in all 6 years) ==\n"); print(flag_hist)
  cat("\n== FY2024 flagged: years above ==\n"); print(flag24 |> select(jurisdiction, years_above, mean_eb), n = 30)
  cat("\n== adjacent-year Spearman ==\n"); print(adjacent)
  cat("\n== variance components ==\n"); print(varcomp)
  cat("\n== category decomposition by year ==\n"); print(decomp_year)
  cat("\n== puppies under ① ==\n"); print(puppies, n = 50)
  cat("\nnew core cities:", new_core, "\n")
}
