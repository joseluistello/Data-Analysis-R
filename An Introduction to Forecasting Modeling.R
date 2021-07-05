


## Plot Data

percapita <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)



percapita %>% 
  filter(Country == "Mexico") %>% 
  ggplot(aes(x = Year, y = GDP_per_capita))  +
  geom_line(color = "indianred3", 
            size=2 ) +
  geom_smooth() +
  labs(y = "$US",
       x = "Year",
       title = "GPD Per Capita for Mexico") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")


### Model

TSLM(GDP_per_capita ~ trend())

fit <- percapita %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit




fit %>% forecast(h = "3 years")



fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Mexico") %>%
  autoplot(percapita) +
  labs(y = "$USD", title = "GPD Per Capita for Mexico")