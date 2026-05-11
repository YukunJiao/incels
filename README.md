# Replication Materials

This repository contains the replication materials for my master's thesis on symbolic boundaries in r/Incels.

## Files

- `_targets.R`: Main reproducible workflow using the `targets` package.
- `R/`: Helper scripts for text preprocessing, corpus construction, validation, and semantic analysis.
- `comments_raw.rds`: Raw Reddit comments data.
- `submissions_raw.rds`: Raw Reddit submissions data.
- `bot_accounts.csv`: List of bot accounts used during preprocessing.
- `renv.lock`: R package environment lockfile.

## Data

The raw data consist of Reddit comments and submissions. Each file includes metadata such as author, timestamp, score, subreddit information, and text fields.

For ethical issues, human usernames were anonymized, while [deleted] and bot usernames listed in `bot_accounts.csv` were left unchanged.

## Reproduction

Open incels.Rproj in RStudio first. Then restore the project environment by running:

```r
install.packages("remotes")
remotes::install_version("renv", version = "1.1.6")
renv::activate()
renv::restore()
```

Then run the analysis pipeline:

```
targets::tar_make()
```