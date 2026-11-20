# runs everything in order (scripts share objects so run them in one session)
# slow - mostly the daily CRSP pull and the rolling betas / idiovol / price delay

source("R/00_config.R")
source("R/functions.R")
source("R/characteristic_metadata.R")

source("R/01_compustat.R")           # annual + quarterly accounting variables
source("R/02_crsp.R")                # market-based variables, betas, CCM links
source("R/03_ibes.R")                # analyst variables and earnings surprises
source("R/04_characteristics.R")     # merge into the firm-month panel
source("R/05_portfolio_sorts.R")     # long-short portfolios and factor alphas
source("R/06_publication_decay.R")   # pre/post publication, 2003 break, McLean-Pontiff
source("R/07_figures.R")             # charts saved to output/figures

dbDisconnect(wrds)
dbDisconnect(tidy_finance)
