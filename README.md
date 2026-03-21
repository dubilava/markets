# World Bank Pink Sheet pipeline

This repository contains a production-ready R pipeline that scrapes the World Bank commodity markets page, downloads the latest **Pink Sheet Monthly prices** workbook when a new dated file is needed, and reshapes selected **Market Indices** into analysis-ready CSV outputs.

## What the pipeline does

1. `scripts/fetch_data.R`
   - Scrapes the World Bank commodity markets page with `rvest`.
   - Searches anchor tags for the **Monthly prices** link instead of depending on a brittle table layout.
   - Resolves the Excel workbook URL and downloads it with `httr2`.
   - Saves raw files as `data/raw/CMO-Historical-Data-Monthly_YYYY-MM-DD.xlsx`.
   - Skips the download when today's dated raw file already exists.
   - Writes detailed run logs to `logs/fetch_log.txt`.
   - Includes retry logic and empty-download checks.

2. `scripts/process_data.R`
   - Finds the most recent raw workbook in `data/raw/`.
   - Reads the `Market Indices` worksheet with `readxl`.
   - Detects the header row defensively and extracts only:
     - Energy
     - Cereals
     - Oils and Meals
     - Fertilizers
   - Uses `data.table` to reshape the data into:
     - Long/tidy output: `data/processed/pink_sheet_indices.csv`
     - Wide output: `data/processed/pink_sheet_indices_wide.csv`
   - Validates sheet existence, date parsing, missing series, and empty results.

## Repository structure

```text
.github/workflows/daily.yml
scripts/fetch_data.R
scripts/process_data.R
data/raw/
data/processed/
logs/
renv.lock
```

## Data source

- Source page: <https://www.worldbank.org/en/research/commodity-markets>
- Target asset: **Monthly prices** (`CMO-Historical-Data-Monthly.xlsx`)
- Publisher: World Bank, Commodity Markets / Pink Sheet materials

## Running locally

### 1. Install R dependencies with renv

```r
install.packages("renv")
renv::restore(prompt = FALSE)
```

### 2. Fetch the latest workbook

```bash
Rscript scripts/fetch_data.R
```

### 3. Process the latest workbook

```bash
Rscript scripts/process_data.R
```

## Output files

- Raw workbook snapshots: `data/raw/CMO-Historical-Data-Monthly_YYYY-MM-DD.xlsx`
- Tidy index dataset: `data/processed/pink_sheet_indices.csv`
- Wide index dataset: `data/processed/pink_sheet_indices_wide.csv`
- Fetch log: `logs/fetch_log.txt`

## GitHub Actions automation

The workflow in `.github/workflows/daily.yml`:

- runs every day via cron,
- supports manual execution with `workflow_dispatch`,
- sets up R,
- restores the `renv` environment,
- runs the fetch and process scripts,
- commits and pushes updated raw/processed/log files when new data changes the repository.

Because raw files are date-stamped and the fetch script skips existing daily snapshots, the pipeline is idempotent for repeated runs on the same day.

## Required R packages

This project uses:

- `rvest`
- `httr2`
- `readxl`
- `data.table`
- `stringr`
- `renv`
