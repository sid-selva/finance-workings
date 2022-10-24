# Packages needed ---- 

# install.packages("tidyverse") 
# install.packages("lubridate") 
# install.packages("readxl") 
# install.packages("highcharter") 
# install.packages("tidyquant") 
# install.packages("timetk") 
# install.packages("tibbletime") 
# install.packages("quantmod") 
# install.packages("PerformanceAnalytics") 
# install.packages("scales")

# Load Libraries ---- 
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

prices <- getSymbols(symbols, src = 'yahoo', 
           from = "2016-12-31", 
           to = "2021-12-31", 
           auto.assign = TRUE, 
           warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols)

# Get Returns

prices_monthly <- to.monthly(prices,indexAt = "lastof",OHLC = FALSE) 
# Can also use firstof

prices_monthly %>% head()
 
# OHLC - Open High low close

# Get Returns

asset_returns_xts <- na.omit(Return.calculate(prices_monthly,method = "log"))
?Return.calculate

# Above Function in Performance Analytics , Other method = c("discrete", "log", "difference")

# Returns in Tidyverse 
asset_returns_dplyr <- prices %>% 
  to.monthly(indexAt = "lastof",OHLC = FALSE) %>% 
  data.frame(date = index(.)) %>% 
  # Remove index 
  remove_rownames() %>% 
  pivot_longer(-date, names_to = "asset", values_to = "returns") %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  pivot_wider(names_from = asset, values_from = returns) %>% 
  select(date, symbols) %>% na.omit()

# Returns in Tidyquant 
library(tidyquant)
library(timetk)

asset_returns_tq_builtin <- prices %>% 
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>% 
  pivot_longer(-date, names_to = "asset", values_to = "returns") %>% 
  group_by(asset) %>% 
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "log") %>% 
  pivot_wider(names_from = asset, values_from = monthly.returns) %>% 
  select(date, symbols) %>% 
  na.omit()

asset_returns_long <- 
  asset_returns_dplyr %>% 
  gather(asset, returns, -date)

# Portfolio Weights 
w <- c(0.25, 
       0.25, 
       0.20, 
       0.20, 
       0.10)
