#### Packages

library(tidyquant)
library(tidyverse)
library(ggthemes)
library(fpp3)


#### Download  Data

sc_companies <- c("TSM", "UMC", "INTC", "TXN", "NXP", "NVDA", "AVGO", "QCOM") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2021-06-27")



foundry <- c("TSM", "UMC") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2021-06-27")



idm <- c("INTC", "TXN", "NXP") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2021-06-27")



fabless <- c("NVDA", "AVGO", "QCOM") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2021-06-27")

#### Download  Data





#### Plot Data

ggplot(sc_companies, 
       aes(x = date, y = close, color = symbol)) + 
  geom_line(size=1) +
  labs(title = "Semiconductor Stock Price",
       subtitle = 'A comparation between companies',
       x = 'Date',
       y =  "Close Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") 





ggplot(sc_companies, 
       aes(x = date, y = close, color = symbol)) + 
  geom_line(size=1) +
  labs(title = "Semiconductor Stock Price Growth",
       subtitle = '',
       x = 'Date',
       y =  "Close Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y")



ggplot(foundry, 
       aes(x = date, y = close, color = symbol)) + 
  geom_line(size=1) +
  labs(title = "Semiconductor Stock Price",
       subtitle = 'Foundry business model',
       x = 'Date',
       y =  "Close Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") 





ggplot(fabless, 
       aes(x = date, y = close, color = symbol)) + 
  geom_line(size=1) +
  labs(title = "Semiconductor Stock Price",
       subtitle = 'Fabless business model',
       x = 'Date',
       y =  "Close Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") 


ggplot(idm, 
       aes(x = date, y = close, color = symbol)) + 
  geom_line(size=1) +
  labs(title = "Semiconductor Stock Price",
       subtitle = 'IDM business model',
       x = 'Date',
       y =  "Close Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") 

sc_companies %>% 
  filter(symbol == "QCOM") %>% 
  ggplot(aes(x = date, y = close, color = symbol))  +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  labs(title = "Semiconductor Stock ",
       subtitle = 'Qualcomm Trend',
       x = 'Date',
       y =  "Close Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y")


semicompanies_return_yearly_idm <-  idm %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") 


semicompanies_return_yearly_idm %>%
  ggplot(aes(x = year(date), y = yearly.returns, fill = symbol)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Anual Returns", 
       subtitle = "IDM Business Model",
       y = "Returns", x = "", color = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_tq() +
  scale_fill_tq()



semicompanies_return_yearly_fabless <- fabless %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns")




semicompanies_return_yearly_fabless %>%
  ggplot(aes(x = year(date), y = yearly.returns, fill = symbol)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Anual Returns", 
       subtitle = "Fabless Business Model",
       y = "Returns", x = "", color = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_tq() +
  scale_fill_tq()


semicompanies_return_yearly_foundry <- foundry %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns")



semicompanies_return_yearly_foundry %>%
  ggplot(aes(x = year(date), y = yearly.returns, fill = symbol)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Anual Returns", 
       subtitle = "Foundry Business Model",
       y = "Returns", x = "", color = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_tq() +
  scale_fill_tq()



idm %>% 
  filter(symbol == "NXP") %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "AVGO Candlestick Chart", y = "Closing Price", x = "") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y")


#### Plot Data

