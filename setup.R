# run once before run_all.R - installs packages and sets up the WRDS login

cran_packages <- c(
  "tidyverse", "tidyfinance", "RPostgres", "RSQLite", "DBI", "dbplyr",
  "data.table", "slider", "zoo", "furrr", "future", "moments", "broom",
  "fixest", "scales", "remotes", "usethis"
)

missing <- setdiff(cran_packages, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)

# behavioural factors (Daniel, Hirshleifer & Sun)
if (!requireNamespace("ExpectedReturns", quietly = TRUE)) {
  remotes::install_github("JustinMShea/ExpectedReturns")
}

# this opens your .Renviron - add these two lines, save, then restart R:
#
#   WRDS_USER=your_wrds_username
#   WRDS_PASSWORD=your_wrds_password
#
usethis::edit_r_environ(scope = "user")
