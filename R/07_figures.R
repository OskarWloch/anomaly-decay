# figures

theme_set(theme_minimal(base_size = 11))

save_fig <- function(plot, name, width = 7, height = 4.5) {
  ggsave(file.path("output/figures", name), plot, width = width, height = height, dpi = 300)
}

# fig 1 - pre vs post publication
fig1 <- compare_pub %>%
  select(characteristic, mean_pre, mean_post) %>%
  pivot_longer(c(mean_pre, mean_post), names_to = "period", values_to = "mean_ret") %>%
  mutate(period = factor(
    recode(period, mean_pre = "Pre-publication", mean_post = "Post-publication"),
    levels = c("Pre-publication", "Post-publication")
  )) %>%
  ggplot(aes(period, mean_ret * 100)) +
  geom_boxplot(fill = "grey80") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Monthly Long-Short Return: Pre vs Post Publication",
       x = NULL, y = "% per month")

save_fig(fig1, "fig1_pre_post_publication.png")

# fig 2 - pre vs post 2003
fig2 <- compare_2003 %>%
  select(characteristic, mean_pre2003, mean_post2003) %>%
  pivot_longer(c(mean_pre2003, mean_post2003), names_to = "period", values_to = "mean_ret") %>%
  filter(!is.na(mean_ret)) %>%
  mutate(period = factor(
    recode(period, mean_pre2003 = "Pre-2003", mean_post2003 = "Post-2003"),
    levels = c("Pre-2003", "Post-2003")
  )) %>%
  ggplot(aes(period, mean_ret * 100)) +
  geom_boxplot(fill = "grey80") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Monthly Long-Short Return Before vs After 2003",
       x = NULL, y = "% per month")

save_fig(fig2, "fig2_pre_post_2003.png")

# fig 3 - alphas by model
fig3 <- alpha_panel %>%
  mutate(model = factor(model, levels = c("CAPM", "FF3", "FF5", "Q", "BHV"))) %>%
  ggplot(aes(model, alpha * 100)) +
  geom_violin(fill = "grey90", colour = NA, adjust = 1.2) +
  geom_boxplot(width = .15, outlier.shape = NA, fill = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Distribution of Monthly Alphas by Risk Model",
       x = NULL, y = "Alpha (% per month)")

save_fig(fig3, "fig3_alphas_by_model.png")

# fig 4 - pre vs post publication by characteristic
fig4 <- compare_pub %>%
  ggplot(aes(x = mean_pre * 100, y = mean_post * 100)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.70, size = 2) +
  geom_text(aes(label = characteristic), check_overlap = TRUE, size = 3, vjust = 1.2) +
  labs(title    = "Return Decay After Publication",
       subtitle = "Monthly long-short returns by characteristic",
       x = "% per month (pre-publication)",
       y = "% per month (post-publication)")

save_fig(fig4, "fig4_publication_decay_scatter.png", height = 6)

# fig 5 - share of significant alphas
fig5 <- alpha_by_model %>%
  ggplot(aes(reorder(model, -pct_significant), pct_significant)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", pct_significant)), vjust = -0.25, size = 3.5) +
  ylim(0, 100) +
  labs(title = sprintf("Share of Anomalies with |t| > %s (Monthly Alpha)", t_threshold),
       x = NULL, y = "% significant")

save_fig(fig5, "fig5_share_significant_alphas.png")
