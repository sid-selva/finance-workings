library(fmpcloudr)
library(tidyverse)
library(gt)

get_symbol <- "AAPL"


get_balanceSheet_y <- function(ticker,limit = 5){
  
  data_bs <- fmpc_financial_bs_is_cf(symbols = ticker,
                                     statement = 'balance',
                                     quarterly = FALSE,
                                     limit = limit)
  
  data_bs <- data_bs %>% 
    mutate(date = ymd(date)) %>% 
    select(-c(2:8),-link, -finalLink) %>% 
    pivot_longer(cols = 2:45,names_to = "reference",values_to = "value") %>% 
    pivot_wider(names_from = "date",names_prefix = "FY_",values_from = value) 
  
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
  
  table_bs <- data_bs %>% 
    cbind(extended_ref) %>% 
    select(Description, -reference, contains("FY_"))%>% 
    gt::gt() %>% 
    gt::fmt_currency(columns = c(2:6),
                     accounting = TRUE,
                     suffixing = TRUE,
                     incl_space = TRUE) %>% 
    gt::tab_style(
      style = list(
        cell_text(weight = "bolder"),
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
    )%>% 
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
    ) %>% 
    tab_header(
      title = str_glue("Balance Sheet of {ticker}")
    ) %>% 
    tab_source_note(source_note = str_glue("Data From Finanical Modeling Prep API 
                    https://site.financialmodelingprep.com/developer/docs/"))
  
  return(table_bs)
  
}
