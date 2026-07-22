# FRI Peer Mentor Mindsets Guide

A Quarto book for First-year Research Immersion (FRI) peer mentors at Binghamton University,
supporting FRI Semester 1 (Research Methods: HARP 170). One chapter per week: a mindset, a
self-assessment score against the class distribution, and a journal prompt.

**Live site:** <https://shanemccarty.github.io/FRImindsets/>

> [!IMPORTANT]
> **All data in this draft are simulated** (`data/sim_survey_data.csv`). Every score, table,
> and figure is placeholder content for layout and review. Real results replace them once the
> pre/post Qualtrics survey is collected.

## Requirements

Quarto, plus R with:

```r
install.packages(c("dplyr", "tibble", "purrr", "ggplot2", "kableExtra"))
```

## Render and publish

```bash
quarto render     # builds ./docs
quarto preview    # live preview
```

GitHub Pages serves the `main` branch `/docs` folder, so publishing is just committing the
rendered `docs/` and pushing.

## Structure

| Path | What it is |
|---|---|
| `_quarto.yml` | Book config, chapter order, authors |
| `index.qmd` | Prologue: how to read the guide, curriculum table |
| `wk01…wk15-*.qmd` | One chapter per week |
| `references.qmd` | Measure-by-week table with access status, plus references |
| `_setup.R` | Data load and shared helpers |
| `colors.R` | Construct colour palette (single source of truth) |
| `theme.scss` | Binghamton green theme |
| `data/sim_survey_data.csv` | SIMULATED item-level data |

### Shared helpers (`_setup.R`)

- `score_table()` — password lookup table with percentile and band labels
- `norm_facets()` — class-distribution histograms with Low/Mid/High tertile bands. Always pass
  `scale = c(lo, hi)` so every panel spans the measure's full response range
- `typology_plot()` / `style_type()` — Week 1 Support × Structure quadrants
- `focus_plot()` / `goal_lean()` — Week 2 focus strength × academic/social goal lean

## Swapping in real data

Match the Qualtrics export to the column names in `data/sim_survey_data.csv` (a `PASSWORD`
column plus item columns keyed by week, e.g. `W1_1`…`W1_8`), replace the CSV, and re-render.
Composite scoring and reverse-coding are handled per chapter.

> [!CAUTION]
> This repository is **public**. Each chapter publishes a password-to-score lookup table for
> the whole class. That is harmless with simulated data, but once real responses are loaded,
> anyone who knows or guesses a mentor's password can read their scores, and the full class
> distribution is visible to everyone. Confirm this is acceptable under your IRB or FERPA
> obligations before pushing real data, or move the repository to private and use a different
> distribution route.

## Companion document

`FRI PM Mindsets Strategy` (Word, kept outside this repo) holds Table 1, the concept ×
measure crosswalk, data-collection timing, and the full Qualtrics item bank. **Table 1 is the
source of truth** for week, mindset name, big question, polarity, and instrument; the chapters
follow it.
