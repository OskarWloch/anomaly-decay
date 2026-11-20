# packages, settings and connections

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyfinance)
  library(RSQLite)
  library(dbplyr)
  library(RPostgres)
  library(DBI)
  library(scales)
  library(slider)
  library(data.table)
  library(furrr)
  library(moments)
  library(zoo)
  library(ExpectedReturns)
  library(broom)
  library(fixest)
})

# sample period
start_date <- ymd("1960-01-01")
end_date   <- ymd("2023-12-31")

# settings
# winsorise characteristics at 1%/99% each month (section 3.2)
winsorise_characteristics <- TRUE

# t-stat cut-off for counting an alpha as significant (1.5 used in figure 3)
t_threshold <- 1.5

# output folders
dir.create("data",            showWarnings = FALSE)
dir.create("output/tables",   showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures",  showWarnings = FALSE, recursive = TRUE)

# local sqlite db for intermediate tables
tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

# WRDS - login is read from .Renviron, not stored here
if (Sys.getenv("WRDS_USER") == "" || Sys.getenv("WRDS_PASSWORD") == "") {
  stop("WRDS_USER / WRDS_PASSWORD not set. Add them to ~/.Renviron (see setup.R).")
}

wrds <- dbConnect(
  Postgres(),
  host     = "wrds-pgdata.wharton.upenn.edu",
  dbname   = "wrds",
  port     = 9737,
  sslmode  = "require",
  user     = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)
