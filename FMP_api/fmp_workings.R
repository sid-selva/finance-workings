# packages <- c("fmpcloudr")

# install.packages(packages)

## Introduction ---- 
library(fmpcloudr)
library(tidyverse)
library(gt)

# Use Your Token ---- 
# fmpc_set_token("token")

symbols = c('AAPL','MSFT','TSLA')

# Balance Sheet
Bal = fmpc_financial_bs_is_cf(symbols,statement = 'balance')
BalG = fmpc_financial_bs_is_cf(symbols,statement = 'balance', growth = TRUE)


aapl_bs <- Bal %>% filter(symbol == "AAPL")

aapl_bs %>% colnames()

aapl_bs %>% glimpse()

aapl_bs %>% mutate(across(c(1,5),lubridate::ymd),
                   acceptedDate = ymd_hms(acceptedDate),
                   across(c(calendarYear, period), as_factor)
                   )

aapl_bs_year <- fmpc_financial_bs_is_cf(symbols = "AAPL",statement = 'balance',quarterly = FALSE,
                                        limit = 5)

aapl_bs_year %>% mutate(across(c(1,5),lubridate::ymd),
                            acceptedDate = ymd_hms(acceptedDate),
                            across(c(calendarYear, period), as_factor)
                        ) %>% select(-link, -finalLink)%>% 
  pivot_longer(cols = 9:52,names_to = "reference",values_to = "value") %>% 
  mutate(reference = as_factor(reference)) %>% 
  pivot_wider(names_from = "date",names_prefix = "FY_",values_from = value)


aapl_bs_year_long <- aapl_bs_year %>% mutate(date = ymd(date)) %>% select(-c(2:8),-link, -finalLink) %>% 
  pivot_longer(cols = 2:45,names_to = "reference",values_to = "value") %>% 
  pivot_wider(names_from = "date",names_prefix = "FY_",values_from = value) 

aapl_bs_year_long 


# Write Clipboard  
aapl_bs_year_long$reference %>% clipr::write_clip() 

# Creating Row Names for Balance Sheet ---- 

extended_ref <- as_tibble(c("Cash And Cash Equivalents",
"Short Term Investments",
"Cash And Short Term Investments",
"Net Receivables",
"Inventory",
"Other Current Assets",
"Total Current Assets",
"Property Plant Equipment Net",
"Goodwill",
"Intangible Assets",
"Goodwill And Intangible Assets",
"LongTerm Investments",
"Tax Assets",
"Other Non-Current Assets",
"Total Non-Current Assets",
"Other Assets",
"Total Assets",
"Account Payables",
"Short Term Debt",
"Tax Payables",
"Deferred Revenue",
"Other Current Liabilities",
"Total Current Liabilities",
"Long Term Debt",
"Deferred Revenue Non-Current",
"Deferred Tax Liabilities Non-Current",
"Other Non-Current Liabilities",
"Total Non-Current Liabilities",
"Other Liabilities",
"Capital Lease Obligations",
"Total Liabilities",
"Preferred Stock",
"Common Stock",
"Retained Earnings",
"Accumulated Other Comprehensive Income (Loss)",
"Other Total Stockholders Equity",
"Total Stockholders Equity",
"Total Equity",
"Total Liabilities And Stockholders Equity",
"Minority Interest",
"Total Liabilities And Total Equity",
"Total Investments",
"Total Debt",
"Net Debt" ) )

colnames(extended_ref) <- "Description"

#### GT Table ---- 

aapl_bs_year_long %>% 
  cbind(extended_ref) %>% 
  select(Description, -reference, contains("FY_"))%>% 
  gt::gt() %>% 
  gt::fmt_currency(columns = c(2:6),accounting = TRUE,
                   suffixing = TRUE,incl_space = TRUE) %>% 
  gt::tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "grey"),
      cell_borders(sides = c("top", "bottom"),
                   color = "black",
                   weight = px(1.5),
                   style = "solid"
                   )
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(17,41)
    )
  ) %>% 
  tab_row_group(label = "Stockholders Equity",rows = 32:41) %>% 
  tab_row_group(label = "Non-Current Liabilities",rows = 24:31) %>% 
  tab_row_group(label = "Current Liabilities",rows = 18:23) %>% 
  tab_row_group(label = "Non-Current Assets", rows = 8:17) %>% 
  tab_row_group(label = "Current Assets", rows = 1:7) %>% 
  tab_style(
    style = list(
      cell_text(style = "oblique",size = "large"),
      cell_fill(color = "gray92")
    ), locations = cells_body(
      columns = everything(),
      rows = c(7,15,23,31,38)
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bolder",color = "white"),
      cell_fill(color = "midnightblue"),
      cell_borders(sides = c("top", "bottom"),
                   color = "black",
                   weight = px(2.5),
                   style = "solid"
                   )
      ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "paleturquoise"),
      cell_borders(sides = c("top", "bottom"),
                   color = "black",
                   weight = px(1.5),
                   style = "solid"
      )
    ),
    locations = cells_row_groups(
      groups = everything()
    )
  )%>% 
  tab_source_note(source_note = str_glue("Data From Finanical Modeling Prep API 
                    https://site.financialmodelingprep.com/developer/docs/"))
  

aapl_bs_assets <- aapl_bs %>% select(1,7:25)

has_rownames(extened_ref)

get_balanceSheet_y(ticker = "AMZN",limit = 5)
