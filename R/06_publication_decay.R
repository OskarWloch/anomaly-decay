# publication decay tests

# pre vs post publication
long_short_pub <- long_short_all %>%
  left_join(pub_dates, by = "characteristic") %>%
  mutate(period = if_else(date < pub_date, "pre", "post"))

compare_pub <- long_short_pub %>%
  summarise_by_period() %>%
  pivot_wider(
    id_cols     = characteristic,
    names_from  = period,
    values_from = c(n, mean, sd, se, sharpe, t_stat),
    names_sep   = "_"
  ) %>%
  mutate(
    diff_mean = mean_post - mean_pre,
    pct_drop  = 100 * diff_mean / abs(mean_pre)
  )

# paired t-test
paired_means <- compare_pub %>%
  filter(!is.na(mean_pre) & !is.na(mean_post))

t_test_pub_returns <- t.test(paired_means$mean_pre, paired_means$mean_post, paired = TRUE)

# alphas in each period
alpha_pre  <- long_short_pub %>% filter(period == "pre")  %>% compute_alphas(factor_sets)
alpha_post <- long_short_pub %>% filter(period == "post") %>% compute_alphas(factor_sets)

alpha_both <- bind_rows(
  alpha_pre  %>% mutate(period = "pre"),
  alpha_post %>% mutate(period = "post")
) %>%
  mutate(signif = abs(t) > t_threshold)

alpha_summary_pub <- alpha_both %>%
  group_by(period) %>%
  summarise(
    n          = n(),
    mean_alpha = mean(alpha, na.rm = TRUE),
    sd_alpha   = sd(alpha,   na.rm = TRUE),
    pct_signif = mean(signif, na.rm = TRUE) * 100,
    .groups = "drop"
  )

alpha_wide_pub <- alpha_both %>%
  select(characteristic, model, period, alpha) %>%
  pivot_wider(names_from = period, values_from = alpha)

t_test_pub_alphas <- t.test(alpha_wide_pub$pre, alpha_wide_pub$post, paired = TRUE)

# pre vs post 2003
break_date <- ymd("2003-01-01")

long_short_2003 <- long_short_all %>%
  mutate(period = if_else(date < break_date, "pre2003", "post2003"))

compare_2003 <- long_short_2003 %>%
  summarise_by_period() %>%
  pivot_wider(
    id_cols     = characteristic,
    names_from  = period,
    values_from = c(n, mean, sd, se, sharpe, t_stat),
    names_sep   = "_"
  ) %>%
  mutate(
    diff_mean  = mean_post2003 - mean_pre2003,
    pct_change = 100 * diff_mean / abs(mean_pre2003)
  )

pair_2003 <- compare_2003 %>%
  drop_na(mean_pre2003, mean_post2003)

t_test_2003_returns <- t.test(pair_2003$mean_pre2003, pair_2003$mean_post2003, paired = TRUE)

alpha_2003 <- bind_rows(
  long_short_2003 %>% filter(period == "pre2003")  %>% compute_alphas(factor_sets) %>% mutate(period = "pre2003"),
  long_short_2003 %>% filter(period == "post2003") %>% compute_alphas(factor_sets) %>% mutate(period = "post2003")
)

alpha_compare_2003 <- alpha_2003 %>%
  pivot_wider(
    names_from  = period,
    values_from = c(alpha, se, t),
    names_sep   = "_"
  ) %>%
  mutate(
    delta_alpha = alpha_post2003 - alpha_pre2003,
    pct_change  = 100 * delta_alpha / abs(alpha_pre2003)
  )

t_test_2003_alphas <- t.test(
  alpha_compare_2003$alpha_pre2003,
  alpha_compare_2003$alpha_post2003,
  paired = TRUE
)

# McLean & Pontiff (2016) - characteristic FE, SEs clustered by month
panel_rets <- long_short_all %>%
  left_join(pub_dates,    by = "characteristic") %>%
  left_join(sample_dates, by = "characteristic") %>%
  mutate(
    post_sample = date >  sample_end & date < pub_date,
    post_pub    = date >= pub_date
  )

mp_fit <- feols(
  ret_excess_vw ~ post_sample + post_pub | characteristic,
  data    = panel_rets,
  cluster = ~ date
)

# same again with a 2015 split
split_2015 <- ymd("2015-01-01")

panel_rets <- panel_rets %>%
  mutate(
    post_pub_pre2015  = date >= pub_date & date <  split_2015,
    post_pub_post2015 = date >= pub_date & date >= split_2015
  )

mp_fit_2015 <- feols(
  ret_excess_vw ~ post_sample + post_pub_pre2015 + post_pub_post2015 | characteristic,
  data    = panel_rets,
  cluster = ~ date
)

# save
write_csv(compare_pub,        "output/tables/returns_pre_post_publication.csv")
write_csv(alpha_summary_pub,  "output/tables/alphas_pre_post_publication.csv")
write_csv(compare_2003,       "output/tables/returns_pre_post_2003.csv")
write_csv(alpha_compare_2003, "output/tables/alphas_pre_post_2003.csv")

bind_rows(
  tidy(t_test_pub_returns)  %>% mutate(test = "Returns: pre vs post publication"),
  tidy(t_test_pub_alphas)   %>% mutate(test = "Alphas: pre vs post publication"),
  tidy(t_test_2003_returns) %>% mutate(test = "Returns: pre vs post 2003"),
  tidy(t_test_2003_alphas)  %>% mutate(test = "Alphas: pre vs post 2003")
) %>%
  relocate(test) %>%
  write_csv("output/tables/paired_t_tests.csv")

etable(mp_fit, mp_fit_2015, file = "output/tables/mclean_pontiff_regressions.txt", replace = TRUE)
