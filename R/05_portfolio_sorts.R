# portfolio sorts and alphas

# returns
crsp_returns <- tbl(tidy_finance, "crsp_monthly") %>%
  select(permno, date, ret_excess, mktcap_lag) %>%
  collect()

# lag characteristics one month before sorting
sorting_chars <- anomaly_data %>%
  mutate(date = date %m+% months(1))

anoms <- setdiff(names(anomaly_data), c("permno", "gvkey", "date"))

data_for_sorts <- crsp_returns %>%
  inner_join(sorting_chars, by = c("permno", "date"))

if (winsorise_characteristics) {
  data_for_sorts <- data_for_sorts %>%
    group_by(date) %>%
    mutate(across(all_of(anoms), ~ {
      lo <- quantile(.x, 0.01, na.rm = TRUE)
      hi <- quantile(.x, 0.99, na.rm = TRUE)
      pmax(pmin(.x, hi), lo)
    })) %>%
    ungroup()
}

data_for_sorts <- data_for_sorts %>%
  drop_na(ret_excess, mktcap_lag)

# which side to go long
long_high <- set_names(!anoms %in% long_low_characteristics, anoms)

# 30/40/30 value weighted sorts
long_short_all <- map_dfr(
  names(long_high),
  run_sort,
  sorting_data = data_for_sorts,
  long_high    = long_high
)

dbWriteTable(tidy_finance, "long_short_anomalies", long_short_all, overwrite = TRUE)

# factor models
factors_capm <- factors_ff3_monthly %>%
  select(date, mkt = mkt_excess)

factors_ff3 <- download_data(
  type       = "factors_ff_3_monthly",
  start_date = start_date,
  end_date   = end_date
)

factors_ff5 <- download_data_factors_ff(
  type       = "factors_ff_5_2x3_monthly",
  start_date = start_date,
  end_date   = end_date
)

factors_q <- download_data(
  type       = "factors_q5_monthly",
  start_date = start_date,
  end_date   = end_date
)

# behavioural factors (DHS 2020)
data("DHS3.monthly")

factors_bhv <- DHS3.monthly %>%
  as_tibble(rownames = "date") %>%
  mutate(date = floor_date(ymd(date), "month")) %>%
  rename(mkt_excess = MKT.RF)

factor_sets <- list(
  CAPM = factors_capm,
  FF3  = factors_ff3,
  FF5  = factors_ff5,
  Q    = factors_q,
  BHV  = factors_bhv
)

# alphas
alpha_panel <- compute_alphas(long_short_all, factor_sets)

dbWriteTable(tidy_finance, "alpha_estimates", alpha_panel, overwrite = TRUE)

# summary stats
summary_vw <- long_short_all %>%
  rename(ret = ret_excess_vw) %>%
  group_by(characteristic) %>%
  summarise(
    n      = n(),
    mean   = mean(ret, na.rm = TRUE),
    sd     = sd(ret,   na.rm = TRUE),
    se     = sd / sqrt(n),
    t_stat = mean / se,
    sharpe = mean / sd,
    skew   = skewness(ret, na.rm = TRUE),
    kurt   = kurtosis(ret, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean))

# alphas by model, % per month
alpha_by_model <- alpha_panel %>%
  mutate(alpha_pct = 100 * alpha) %>%
  group_by(model) %>%
  summarise(
    mean_alpha          = mean(alpha_pct,   na.rm = TRUE),
    median_alpha        = median(alpha_pct, na.rm = TRUE),
    sd_alpha            = sd(alpha_pct,     na.rm = TRUE),
    pct_significant     = mean(abs(t) > t_threshold,  na.rm = TRUE) * 100,
    n_signif_negative   = sum(alpha < 0 & t < -t_threshold,  na.rm = TRUE),
    pct_signif_negative = mean(alpha < 0 & t < -t_threshold, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# significant alphas
significant_alphas <- alpha_panel %>%
  mutate(alpha_pct = 100 * alpha) %>%
  filter(abs(t) > t_threshold) %>%
  arrange(model, desc(alpha_pct))

# how many alphas are negative
alpha_sign_summary <- alpha_panel %>%
  summarise(
    total_tests         = n(),
    n_negative          = sum(alpha < 0, na.rm = TRUE),
    pct_negative        = mean(alpha < 0, na.rm = TRUE) * 100,
    n_signif_negative   = sum(alpha < 0 & t < -t_threshold, na.rm = TRUE),
    pct_signif_negative = mean(alpha < 0 & t < -t_threshold, na.rm = TRUE) * 100
  )

write_csv(summary_vw,          "output/tables/long_short_summary.csv")
write_csv(alpha_panel,         "output/tables/alphas_full_sample.csv")
write_csv(alpha_by_model,      "output/tables/alphas_by_model.csv")
write_csv(significant_alphas,  "output/tables/significant_alphas.csv")
write_csv(alpha_sign_summary,  "output/tables/alpha_sign_summary.csv")
