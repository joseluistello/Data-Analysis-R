
library(fpp3)


View(global_economy)


percapita <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)


percapita %>%
  filter(Country == "Mexico",) %>%
  autoplot(GDP_per_capita) +
  labs(y = "Pesos",
       x = "Año",
       title = "Ingresos per capita for Mexico")

  

TSLM(GDP_per_capita ~ trend())

fit <- percapita %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Mexico") %>%
  autoplot(ingresos) +
  labs(y = "$Pesos", title = "Ingresos per capita en México")


