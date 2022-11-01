w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

tibble(w, symbols)

portfolio_returns_dplyr_byhand <- asset_returns_long %>% 
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1], 
                             asset == symbols[2] ~ w[2], 
                             asset == symbols[3] ~ w[3], 
                             asset == symbols[4] ~ w[4], 
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>%
  group_by(date) %>% summarise(returns = sum(weighted_returns))

# Tidyquant 
portfolio_returns_tq_rebalanced_monthly <- asset_returns_long %>% 
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")

# Visualizing Portfolio Returns

portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = date, y = returns)) + 
  geom_point(colour = "cornflowerblue")+ 
  xlab("date") + 
  ylab("monthly return") + 
  theme_update(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Portfolio Returns Scatter") + 
  scale_x_date(breaks = scales::breaks_pretty(n=6))


asset_returns_long %>% 
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.15, 
                 # Use alpha to make the fill faded
                 binwidth = .01) +
  geom_histogram(data = portfolio_returns_tq_rebalanced_monthly, 
                 fill = "cornflowerblue", 
                 binwidth = .01) +
  ggtitle("Portfolio and Asset Monthly Returns") + 
  theme_update(plot.title = element_text(hjust = 0.5))


portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) + 
  geom_histogram(binwidth = .01, 
                 colour = "cornflowerblue", 
                 fill = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") + 
  xlab("monthly returns") + 
  ylab("distribution") + 
  theme_update(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Portfolio Histogram and Density")
