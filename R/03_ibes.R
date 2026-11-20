# IBES - analyst variables and SUE

# match CRSP to IBES on 6 digit CUSIP
crsp_names <- tbl(wrds, in_schema("crsp", "msenames")) %>%
  select(permno, ncusip, namedt, nameendt) %>%
  collect() %>%
  mutate(
    namedt = coalesce(namedt, ymd("1900-01-01")),
    nameendt = coalesce(nameendt, ymd("2100-01-01"))
  )

crsp_cusip <- crsp_monthly %>%
  left_join(crsp_names, by = "permno") %>%
  filter(date >= namedt & date <= nameendt) %>%
  mutate(cusip6 = substr(ncusip, 1, 6)) %>%
  group_by(permno, date) %>%
  slice(1) %>%  # drop duplicates
  ungroup()

# dispersion, coverage, revisions
ibes_statsum_keys <- crsp_cusip %>%
  select(cusip6, date) %>%
  distinct()

ibes_statsum <- tbl(wrds, in_schema("ibes", "statsum_epsus")) %>%
  filter(statpers >= start_date, statpers <= end_date) %>%
  select(ticker, cusip, statpers, fpedats,fpi, meanest, medest, stdev, numest) %>%
  collect() %>%
  mutate(
    date = floor_date(ymd(statpers), "month"),
    cusip6 = substr(cusip, 1, 6)
  )

ibes_statsum <- ibes_statsum %>%
  semi_join(ibes_statsum_keys, by = c("cusip6", "date"))

ibes_crsp_match <- ibes_statsum %>%
  inner_join(crsp_cusip, by = c("cusip6", "date")) %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(
    disp = stdev / abs(meanest),
    nanalyst = numest,
    chnanalyst = numest - lag(numest),
    chfeps = meanest - lag(meanest),
    sfe = (meanest - medest) / stdev
  ) %>%
  ungroup() %>%
  group_by(permno, date) %>%
  summarise(
    disp = mean(disp, na.rm = TRUE),
    nanalyst = mean(nanalyst, na.rm = TRUE),
    chnanalyst = mean(chnanalyst, na.rm = TRUE),
    chfeps = mean(chfeps, na.rm = TRUE),
    sfe = mean(sfe, na.rm = TRUE),
    .groups = "drop"
  )

# long term growth forecast - latest estimate in t-4 to t-1
ibes_ltg <- tbl(wrds, in_schema("ibes", "statsum_epsus")) %>%
  filter(
    fpi       == "0",                 # LTG
    statpers >= start_date,
    statpers <= end_date,
    !is.na(meanest)
  ) %>%
  select(cusip, statpers, meanest) %>%
  collect() %>%
  transmute(
    cusip6         = str_pad(substr(cusip, 1, 6), 6, "left", "0"),
    statpers_date  = ymd(statpers),
    fgr5yr         = meanest
  ) %>%
  distinct(cusip6, statpers_date, .keep_all = TRUE)

crsp_window <- crsp_cusip %>%
  select(permno, date, cusip6) %>%
  mutate(
    window_start = date %m-% months(4),
    window_end   = date %m-% months(1)
  )

ibes_ltg_matched <- ibes_ltg %>%
  inner_join(crsp_window, by = "cusip6") %>%
  filter(
    statpers_date >= window_start,
    statpers_date <= window_end
  ) %>%
  group_by(permno, date) %>%
  slice_max(order_by = statpers_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(permno, date, fgr5yr)

# SUE - IBES actual minus median forecast
# use compustat seasonal difference if IBES is missing
ibes_forecast <- ibes_statsum %>%
  select(cusip6, date, medest)

ibes_actual <- tbl(wrds, in_schema("ibes", "act_epsus")) %>%
  filter(actdats >= start_date, actdats <= end_date) %>%
  select(ticker, cusip, actdats, value) %>%
  collect() %>%
  mutate(
    date   = floor_date(ymd(actdats), "month"),
    cusip6 = substr(cusip, 1, 6)
  ) %>%
  rename(actual = value) %>%
  select(cusip6, date, actual)

comp_q <- tbl(wrds, in_schema("comp", "fundq")) %>%
  filter(
    datadate >= start_date,
    datadate <= end_date,
    indfmt   == "INDL",
    datafmt  == "STD",
    consol   == "C",
    popsrc   == "D"
  ) %>%
  select(gvkey, cusip, datadate, ibq) %>%
  collect() %>%
  mutate(
    date    = floor_date(ymd(datadate), "month"),
    cusip6  = str_pad(substr(cusip, 1, 6), 6, "left", "0")
  ) %>%
  arrange(cusip6, datadate) %>%
  group_by(cusip6) %>%
  mutate(
    unexp_comp = ibq - lag(ibq, 4)
  ) %>%
  ungroup() %>%
  select(cusip6, date, unexp_comp)

ibes_sue <- ibes_actual %>%
  inner_join(ibes_forecast, by = c("cusip6","date")) %>%
  group_by(cusip6, date) %>%
  summarise(
    med_actual   = median(actual,  na.rm = TRUE),
    med_forecast = median(medest, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(unexp_ibes = med_actual - med_forecast) %>%
  select(cusip6, date, unexp_ibes)

unexp_all <- full_join(ibes_sue, comp_q, by = c("cusip6", "date")) %>%
  mutate(unexp = coalesce(unexp_ibes, unexp_comp))

crsp_monthly_mcap <- crsp_monthly %>%
  mutate(
    date = floor_date(date, "month"),
    mcap = abs(prc * shrout)
  ) %>%
  select(permno, date, mcap, shrout)

sue_quarterly <- unexp_all %>%
  inner_join(crsp_cusip %>% select(permno, cusip6, date),
             by = c("cusip6","date")) %>%
  distinct(permno, date, unexp) %>%
  left_join(crsp_monthly_mcap, by = c("permno","date")) %>%
  mutate(sue = unexp * shrout/ mcap) %>%
  select(permno, date, sue)

dbWriteTable(tidy_finance, "crsp_monthly", value = crsp_monthly, overwrite = TRUE)
