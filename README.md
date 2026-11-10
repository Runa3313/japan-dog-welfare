# Japan Dog Welfare Data

Analysis of dog intake, return, adoption, and euthanasia at Japanese public animal shelters, using official statistics from the Ministry of the Environment (環境省).

日本の行政（保健所・動物愛護センター）における犬の引取り・返還・譲渡・殺処分を、環境省の公開統計で分析するプロジェクトです。

## Deliverables (Phase 1)

| # | Deliverable | Status |
|---|---|---|
| 1 | Cleaned, machine-readable CSVs of the official statistics (`data/processed/`) | In progress |
| 2 | Analysis report in R, published with Quarto (`analysis/`) | Not started |
| 3 | Simple dashboard (Shiny, `app/`) — optional | Not started |

## Research questions

1. How have dog euthanasia and adoption rates changed since FY2004, and are there structural breaks?
2. Can differences in adoption rates across municipalities be explained by random variation alone?
3. Is the composition of intake (owner-unknown share, unweaned share) associated with euthanasia rates?
4. How does the breakdown of euthanasia categories (①②③) differ across municipalities?

Note: the data are aggregates by municipality. Conclusions apply to municipalities, not to individual dogs (ecological fallacy).

## Data

Source: 環境省「犬・猫の引取り及び負傷動物等の収容並びに処分の状況」, published as PDF only.
See `data/SOURCES.md` for URLs and download dates, and `DATA_DICTIONARY.md` for definitions.

## Reproduce

```r
# in R, from the project root
install.packages("renv")
renv::restore()          # installs the exact package versions in renv.lock
source("R/01_extract_pdf.R")
source("R/02_clean.R")
quarto::quarto_render("analysis/report.qmd")
```

## Structure

```
data/raw/         original PDFs (never edited)
data/processed/   cleaned CSVs produced by R/ scripts
R/                extraction and cleaning scripts
analysis/         Quarto report
app/              Shiny dashboard
reports/figures/  exported figures
```

## Project docs

PROJECT_PLAN.md · PROGRESS.md · DECISIONS.md · DATA_DICTIONARY.md
