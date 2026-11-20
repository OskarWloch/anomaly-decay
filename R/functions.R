# helper functions

# characteristics that need the merged CRSP/Compustat panel
construct_all_anomalies <- function(data) {

  data <- data %>%
    arrange(permno, date) %>%
    group_by(permno) %>%
    mutate(
      age        = interval(min(date), date) %/% months(1),
      bm         = ceq / mktcap_lag,
      cash       = che / at,
      cashdebt   = (ib + dp) / ((lt + lag(lt)) / 2),
      cashpr     = (mktcap_lag + dltt - at) / che,
      emp_growth = (emp - lag(emp)) / lag(emp),
      chdrc      = ((rect / sale) * 365 - lag((rect / sale) * 365)) / lag((rect / sale) * 365),
      cf         = oancf / ((at + lag(at)) / 2),
      cfroa      = oancf / ((at + lag(at)) / 2),
      convind    = if_else(!is.na(dc) & dc > 0, 1, 0),
      conv       = dc / dltt,
      ep         = ib / mktcap_lag,
      gma        = (revt - cogs) / lag(at),
      ltnoa      = at - act - lt + lct,
      investa    = (at - lag(at)) / lag(at),
      ipo        = if_else(year(date) == min(year(date), na.rm = TRUE), 1, 0),
      lev        = lt / mktcap_lag,
      mve        = log(mktcap_lag),
      pctacc     = (ib - oancf) / abs(ib),
      gm         = sale - cogs,
      xrd_at     = xrd / at,
      xrd_at_lag = lag(xrd) / lag(at),
      realestate = (fatb + fatl) / ppegt,
      rd_sale    = xrd / sale,
      rdbias     = (xrd / lag(xrd)) - 1 - ib / lag(ceq),
      roaq       = ni / ((at + lag(at)) / 2),
      roeq       = ib / lag(ceq),
      roic       = (ebit - nopi) / (ceq + lt - che),
      secured    = dm / dltt,
      securedind = if_else(!is.na(dm) & dm != 0, 1, 0),
      sin        = if_else((siccd >= 2080 & siccd <= 2086) |
                           (siccd >= 2100 & siccd <= 2199) |
                           (siccd >= 7990 & siccd <= 7999), 1, 0),
      sp         = sale / mktcap_lag,
      spi        = spi / ((at + lag(at)) / 2),
      tang       = (che + 0.715 * rect + 0.547 * invt + 0.535 * ppent) / at,
      xadint     = xad / ((at + lag(at)) / 2),
      xrdint     = xrd / ((at + lag(at)) / 2)
    ) %>%
    ungroup()

  # industry adjusted (vs industry median)
  data <- data %>%
    group_by(industry, date) %>%
    mutate(
      chempia    = hire    - median(hire,    na.rm = TRUE),
      chatoia    = chato   - median(chato,   na.rm = TRUE),
      cfp_ia     = cfp     - median(cfp,     na.rm = TRUE),
      bm_ia      = bm      - median(bm,      na.rm = TRUE),
      chpmia     = chpm    - median(chpm,    na.rm = TRUE),
      grcapx_ia  = grcapx  - median(grcapx,  na.rm = TRUE),
      grltnoa_ia = grltnoa - median(grltnoa, na.rm = TRUE),
      pchcapx_ia = pchcapx - median(pchcapx, na.rm = TRUE),
      mve_ia     = mve     - median(mve,     na.rm = TRUE),
      sale_share = sale / sum(sale, na.rm = TRUE),
      herf       = sum(sale_share^2, na.rm = TRUE)
    ) %>%
    ungroup()

  data
}

# average volume and total return in a window around each announcement
agg_window <- function(crsp_daily_dt, events, start_col, end_col) {

  wins <- events[, .(permno, rdq, start = get(start_col), end = get(end_col))]
  setkey(wins, permno, start, end)

  crsp_int <- copy(crsp_daily_dt)[, date_end := date]
  setkey(crsp_int, permno, date, date_end)

  foverlaps(crsp_int, wins, nomatch = 0L)[
    , .(vol = mean(vol, na.rm = TRUE),
        ret = sum(ret,  na.rm = TRUE)),
    by = .(permno, rdq)
  ]
}

# one long-short sort
run_sort <- function(var, sorting_data, long_high) {

  direction <- if (long_high[[var]]) "top_minus_bottom" else "bottom_minus_top"

  port_rets <- compute_portfolio_returns(
    sorting_data            = sorting_data,
    sorting_variables       = var,
    sorting_method          = "univariate",
    breakpoint_options_main = breakpoint_options(percentiles = c(0.30, 0.70))
  )

  compute_long_short_returns(port_rets, direction = direction) %>%
    mutate(characteristic = var)
}

# alpha for one portfolio on one factor model
run_alpha <- function(port, fset) {
  df   <- inner_join(port, fset, by = "date")
  vars <- c("ret_excess_vw", setdiff(names(fset), "date"))
  df   <- df %>% select(all_of(vars)) %>% drop_na()

  if (nrow(df) < 10) {
    return(tibble(alpha = NA_real_, se = NA_real_, t = NA_real_))
  }

  fit <- lm(ret_excess_vw ~ ., data = df)

  tidy(fit) %>%
    filter(term == "(Intercept)") %>%
    transmute(alpha = estimate, se = std.error, t = statistic)
}

# alphas for every characteristic and model
compute_alphas <- function(long_short, factor_sets) {
  expand_grid(
    characteristic = unique(long_short$characteristic),
    model          = names(factor_sets)
  ) %>%
    mutate(
      fset      = factor_sets[model],
      data      = map(characteristic, ~ long_short %>% filter(characteristic == .x)),
      alpha_res = map2(data, fset, run_alpha)
    ) %>%
    select(characteristic, model, alpha_res) %>%
    unnest(alpha_res)
}

# return stats by period
summarise_by_period <- function(long_short) {
  long_short %>%
    group_by(characteristic, period) %>%
    summarise(
      n      = n(),
      mean   = mean(ret_excess_vw, na.rm = TRUE),
      sd     = sd(ret_excess_vw,   na.rm = TRUE),
      se     = sd / sqrt(n),
      sharpe = mean / sd,
      t_stat = mean / se,
      .groups = "drop"
    )
}
