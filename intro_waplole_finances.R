# An introduction to the town finances of Walpole, MA (circa summer 2018)

library(tidyverse)
library(scales)
library(tidycensus)


# Analysis of tax rates
# What are the different tax rates
all_tax_rates_by_class <- read_csv("tax_rates_by_class_all.csv")

walpole_rates <- all_tax_rates_by_class %>% filter(Municipality == "Walpole")

walpole_rates %>% 
  select(3:8) %>% 
  gather(property_type, rate, Residential:`Personal Property`) %>% View
  ggplot(aes(x = `Fiscal Year`, y = rate, color = property_type)) +
  geom_line()