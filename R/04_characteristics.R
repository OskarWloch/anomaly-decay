# build the firm-month panel

crsp_monthly <- crsp_monthly |>
  mutate(date = floor_date(date, unit = "month"))

# rolling joins - each month only uses filings that were already public
compustat_dt <- as.data.table(compustat)
crsp_dt      <- as.data.table(crsp_monthly)[, crsp_date := date]

setkey(compustat_dt, gvkey, fiscal_lag)
setkey(crsp_dt, gvkey, date)

crsp_compustat_dt <- compustat_dt[
  crsp_dt,
  on   = .(gvkey, fiscal_lag <= date),   # filings public by this month
  mult = "last"                          # latest one
][, date := crsp_date]                   # keep CRSP month

compustat_q_dt <- as.data.table(compustat_q)
setkey(compustat_q_dt, gvkey, fiscal_lag)

crsp_compustat_dt <- compustat_q_dt[
  crsp_compustat_dt,
  on   = .(gvkey, fiscal_lag <= date),
  mult = "last"
]

# add market and analyst variables
crsp_compustat <- as_tibble(crsp_compustat_dt) %>%
  left_join(daily_anomalies,          by = c("permno", "date")) %>%
  left_join(momentum_vars,            by = c("permno", "date")) %>%
  left_join(ibes_crsp_match,          by = c("permno", "date")) %>%
  left_join(ibes_ltg_matched,         by = c("permno", "date")) %>%
  left_join(indmom,                   by = c("industry","date")) %>%
  left_join(idiovol,                  by = c("permno","date")) %>%
  left_join(price_delay_monthly,      by = c("permno","date")) %>%
  left_join(sue_quarterly,            by = c("permno", "date"))

crsp_compustat <- crsp_compustat %>%
  group_by(permno) %>%
  arrange(date) %>%
  mutate(
    varroa = zoo::na.locf(varroa, na.rm = FALSE),
    varsgr = zoo::na.locf(varsgr, na.rm = FALSE)
  ) %>%
  ungroup()

# aeavol and ear
# event window = days -1 to +1, baseline volume = days -44 to -15
crsp_daily_dt <- as.data.table(crsp_daily)
setkey(crsp_daily_dt, permno, date)

earnings_dt <- as.data.table(compustat_q)[
  !is.na(rdq), .(gvkey, rdq)
][ccm_linking_table, on = "gvkey", allow.cartesian = TRUE][
  rdq >= linkdt & rdq <= linkenddt, .(permno, rdq)
]

earnings_dt[, `:=`(
  start_evt  = rdq - 1,  end_evt  = rdq + 1,
  start_base = rdq - 44, end_base = rdq - 15
)]

event_stats  <- agg_window(crsp_daily_dt, earnings_dt, "start_evt",  "end_evt")
baseline_vol <- agg_window(crsp_daily_dt, earnings_dt, "start_base", "end_base")[
  , .(permno, rdq, base_vol = vol)
]

aeavol_ear <- merge(event_stats, baseline_vol,
                    by = c("permno", "rdq"), all.x = TRUE)[
  , .(
    permno,
    date   = floor_date(rdq, "month"),
    aeavol = (vol - base_vol) / base_vol,
    ear    = ret
  )
]

# carry forward 2 months
aeavol_ear <- aeavol_ear %>%
  expand_grid(month_offset = 0:2) %>%
  mutate(date = date %m+% months(month_offset)) %>%
  select(-month_offset) %>%
  arrange(permno, date) %>%
  distinct(permno, date, .keep_all = TRUE)

crsp_compustat <- crsp_compustat %>%
  left_join(aeavol_ear, by = c("permno", "date")) %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  fill(sue, .direction = "down") %>%     # carry sue forward
  ungroup()

# remaining characteristics
anomaly_data <- construct_all_anomalies(crsp_compustat)

# ms (Mohanram 2005) - 8 signals vs industry median
anomaly_data <- anomaly_data %>%
  group_by(permno) %>%
  mutate(
    xrd   = coalesce(xrd, 0),
    xad   = coalesce(xad, 0),
    # profitability & accruals
    roa    = ib    / at,
    cfo    = oancf_used / at,
    accr   = (ib - oancf_used) / ((at + lag(at)) / 2),

    # annual intensities
    rdint  = xrd / at,
    capint = capx / at,
    adint  = xad / at
  ) %>%
  ungroup() %>%

  # compare to industry median
  group_by(industry, date) %>%
  mutate(
    sig_roa    = as.integer(roa    > median(roa,    na.rm=TRUE)),
    sig_cfo    = as.integer(cfo    > median(cfo,    na.rm=TRUE)),
    sig_accr   = as.integer(accr   < median(accr,   na.rm=TRUE)),
    sig_varroa = as.integer(varroa < median(varroa, na.rm=TRUE)),
    sig_varsgr = as.integer(varsgr < median(varsgr, na.rm=TRUE)),
    sig_rdint  = as.integer(rdint  > median(rdint,  na.rm=TRUE)),
    sig_capint = as.integer(capint > median(capint, na.rm=TRUE)),
    sig_adint  = as.integer(adint  > median(adint,  na.rm=TRUE)),

    ms = sig_roa + sig_cfo + sig_accr +
      sig_varroa + sig_varsgr +
      sig_rdint + sig_capint + sig_adint
  ) %>%
  ungroup()

anomaly_data <- anomaly_data %>%
  left_join(beta_monthly, by = c("permno", "date")) %>%
  select(any_of(filtered_vars))

# check coverage
missing_vars <- setdiff(filtered_vars, colnames(anomaly_data))
if (length(missing_vars) > 0) {
  warning("Characteristics not constructed: ", paste(missing_vars, collapse = ", "))
}

missing_summary <- anomaly_data %>%
  summarise(across(everything(), ~ mean(is.na(.)), .names = "missing_{.col}")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  arrange(desc(missing_pct)) %>%
  mutate(missing_pct = percent(missing_pct, accuracy = 0.01))

zero_summary <- anomaly_data %>%
  summarise(across(everything(), ~ mean(. == 0, na.rm = TRUE), .names = "zero_{.col}")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "zero_pct") %>%
  mutate(zero_pct = round(zero_pct * 100, 2)) %>%
  arrange(desc(zero_pct))

write_csv(missing_summary, "output/tables/missing_share_by_characteristic.csv")
write_csv(zero_summary,    "output/tables/zero_share_by_characteristic.csv")
