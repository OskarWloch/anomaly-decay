# CRSP - market based characteristics, betas, CCM links

# daily CRSP
crsp_daily <- tbl(wrds, I("crsp.dsf")) %>%
  filter(date >= start_date, date <= end_date) %>%
  select(permno, date, ret, prc, vol, shrout, askhi, bidlo) %>%
  collect() %>%
  mutate(month = floor_date(date, "month"))

# monthly liquidity / volatility measures from daily data
daily_anomalies <- crsp_daily %>%
  mutate(
    dolvol1 = abs(prc * vol),
    daily_turn = vol / (shrout * 1000),
    baspread_daily = (askhi - bidlo) / askhi,
    ill_daily = abs(ret) / (abs(prc) * vol),
    zero_trade_day = if_else(vol == 0 | is.na(vol), 1, 0)
  ) %>%
  group_by(permno, month) %>%
  summarise(
    maxret = max(ret, na.rm = TRUE),
    retvol = sd(ret, na.rm = TRUE),
    turn = mean(daily_turn, na.rm = TRUE),
    std_dolvol = sd(dolvol1, na.rm = TRUE),
    std_turn = sd(daily_turn, na.rm = TRUE),
    baspread = mean(baspread_daily, na.rm = TRUE),
    ill = mean(ill_daily, na.rm = TRUE),
    zerotrade = mean(zero_trade_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = month)

# weekly returns and equal weighted market
crsp_weekly <- crsp_daily %>%
  mutate(week = floor_date(date + days(1), "week", week_start = 1)) %>%
  group_by(permno, week) %>%
  summarise(
    ret_w = prod(1 + ret, na.rm = TRUE) - 1,
    .groups = "drop"
  )

mkt_weekly <- crsp_weekly %>%
  group_by(week) %>%
  summarise(
    mkt_ew = mean(ret_w, na.rm = TRUE),
    .groups = "drop"
  )

crsp_weekly <- crsp_weekly %>%
  left_join(mkt_weekly, by = "week")

# idiosyncratic vol - 156 week rolling market model
idiovol <- crsp_weekly %>%
  arrange(permno, week) %>%
  group_by(permno) %>%
  mutate(
    E_r    = slide_dbl(ret_w,   mean,    .before = 155, .complete = TRUE),
    E_m    = slide_dbl(mkt_ew,  mean,    .before = 155, .complete = TRUE),
    E_rm   = slide2_dbl(ret_w, mkt_ew, ~ mean(.x * .y), .before = 155, .complete = TRUE),
    E_r2   = slide_dbl(ret_w^2, mean,    .before = 155, .complete = TRUE),
    E_m2   = slide_dbl(mkt_ew^2,mean,    .before = 155, .complete = TRUE),
    var_r  = E_r2  - E_r^2,
    var_m  = E_m2  - E_m^2,
    cov_rm = E_rm  - E_r * E_m,
    idiovol = sqrt(var_r - (cov_rm^2 / var_m))
  ) %>%
  ungroup() %>%
  # keep last week of each month
  mutate(month = floor_date(week, "month")) %>%
  group_by(permno, month) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(permno, date = month, idiovol)

# price delay (Hou & Moskowitz 2005)
crsp_m <- crsp_weekly %>%
  mutate(month = floor_date(week, "month")) %>%
  group_by(permno, month) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  rename(
    date  = month,
    ret_m = ret_w,
    mkt_m = mkt_ew
  )

# market lags for price delay
crsp_m <- crsp_m %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(
    mkt_m_lag1 = lag(mkt_m, 1),
    mkt_m_lag2 = lag(mkt_m, 2),
    mkt_m_lag3 = lag(mkt_m, 3),
    mkt_m_lag4 = lag(mkt_m, 4)
  ) %>%
  ungroup()

price_delay_monthly <- crsp_m %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(
    R2_res = rollapplyr(
      data      = cbind(ret_m, mkt_m),
      width     = 36,               # 36 month window
      FUN       = function(z) {
        if (nrow(z) < 24) return(NA_real_)   # need at least 24 obs
        summary(lm(z[,1] ~ z[,2]))$r.squared
      },
      by.column = FALSE,
      fill      = NA
    ),

    R2_unres = rollapplyr(
      data      = cbind(ret_m, mkt_m, lag(mkt_m, 1),
                        lag(mkt_m, 2), lag(mkt_m, 3), lag(mkt_m, 4)),
      width     = 36,
      FUN       = function(z) {
        z <- na.omit(z)
        if (nrow(z) < 24) return(NA_real_)
        y <- z[,1]
        X <- z[,-1, drop=FALSE]
        summary(lm(y ~ X))$r.squared
      },
      by.column = FALSE,
      fill      = NA
    ),

    price_delay = 1 - R2_res / R2_unres
  ) %>%
  ungroup() %>%
  select(permno, date, price_delay)

# monthly CRSP (tidyfinance)
crsp_monthly <- download_data(
  type = "wrds_crsp_monthly",
  start_date = start_date,
  end_date = end_date)

factors_ff3_monthly <- tbl(wrds, I("ff.factors_monthly")) %>%
  filter(date >= start_date, date <= end_date) %>%
  select(date, mktrf) %>%
  collect() %>%
  rename(mkt_excess = mktrf)

dbWriteTable(
  tidy_finance,
  "factors_ff3_monthly",
  value = factors_ff3_monthly,
  overwrite = TRUE)

factors_ff3_monthly <- tbl(tidy_finance, "factors_ff3_monthly") |>
  select(date, mkt_excess) |>
  collect()

crsp_monthly <- crsp_monthly |>
  left_join(factors_ff3_monthly, join_by(date))

# beta - 60 month rolling, min 48 obs
crsp_monthly <- crsp_monthly %>%
  arrange(permno, date)

beta_monthly <- crsp_monthly |>
  estimate_betas(model = "ret_excess ~ mkt_excess", lookback = months(60), min_obs = 48, use_furrr = TRUE)

beta_monthly <- beta_monthly %>%
  mutate(
    beta = beta_mkt_excess,
    betasq = beta^2
  ) %>%
  select(permno, date, beta, betasq)

# dollar volume - tidyfinance monthly doesn't include volume so pull msf
crsp_monthly_vol <- tbl(wrds, I("crsp.msf")) %>%
  filter(date >= start_date, date <= end_date) %>%
  select(permno, date, prc, vol) %>%
  collect() %>%
  mutate(
    date = floor_date(date, unit = "month"),
    dolvol_raw = abs(prc * vol)
  ) %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(dolvol = log(lag(dolvol_raw, 2))) %>%
  ungroup()

crsp_monthly <- crsp_monthly %>%
  left_join(crsp_monthly_vol, by = c("permno", "date"))

# both tables have prc, keep the crsp_monthly one
crsp_monthly <- crsp_monthly %>%
  rename(prc = prc.x)

# industry momentum
industry_ret <- crsp_monthly %>%
  group_by(industry, date) %>%
  summarise(
    ind_ret = weighted.mean(ret, w = mktcap_lag, na.rm=TRUE),
    .groups="drop"
  )

indmom <- industry_ret %>%
  group_by(industry) %>%
  arrange(date) %>%
  mutate(
    indmom = slide_dbl(
      ind_ret,
      ~ prod(1 + .x, na.rm = TRUE) - 1,
      .before    = 12,   # t-12 to t-2
      .after     = -2,
      .complete  = TRUE
    )
  ) %>%
  ungroup() %>%
  select(industry, date, indmom)

# momentum
momentum_vars <- crsp_monthly %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(
    mom1m = lag(ret, 1),
    mom6m = slide_dbl(ret, ~ prod(1 + .x, na.rm = TRUE) - 1, .before = 6, .after = -2, .complete = TRUE),
    mom12m = slide_dbl(ret, ~ prod(1 + .x, na.rm = TRUE) - 1, .before = 12, .after = -2, .complete = TRUE),
    mom36m = slide_dbl(ret, ~ prod(1 + .x, na.rm = TRUE) - 1, .before = 36, .after = -13, .complete = TRUE),
    mom_recent  = slide_dbl(ret, ~ prod(1 + .x, na.rm = TRUE) - 1, .before = 6, .after = -1, .complete = TRUE),
    mom_earlier = slide_dbl(ret, ~ prod(1 + .x, na.rm = TRUE) - 1, .before = 12, .after = -7, .complete = TRUE),
    chmom = mom_recent - mom_earlier
  ) %>%
  ungroup() %>%
  select(permno, date, mom1m, mom6m, mom12m, mom36m, chmom)

crsp_monthly <- crsp_monthly |>
  drop_na(ret_excess, mktcap, mktcap_lag)

# CCM links
ccm_linking_table_db <- tbl(wrds, I("crsp.ccmxpf_lnkhist"))

ccm_linking_table <- ccm_linking_table_db |>
  filter(
    linktype %in% c("LU", "LC") &
      linkprim %in% c("P", "C")
  ) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, today()))

ccm_links <- crsp_monthly |>
  inner_join(ccm_linking_table, 
             join_by(permno), relationship = "many-to-many") |>
  filter(!is.na(gvkey) & 
           (date >= linkdt & date <= linkenddt)) |>
  select(permno, gvkey, date)

crsp_monthly <- crsp_monthly |>
  left_join(ccm_links, join_by(permno, date)) |>
  filter(!is.na(gvkey))
