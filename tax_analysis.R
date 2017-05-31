# By Andrew Flowers, andrew.w.flowers@gmail.com
# What is the Walpole, MA residential real estate tax burden?
# Source: Source: Massachusetts Division of Local Services
# Link: http://www.mass.gov/dor/local-officials/municipal-databank-and-local-aid-unit/databank-reports-new.html

library(tidyverse)
library(scales)

# Data on total tax levies by class
raw_tax_by_class_to_09 <- read_csv("Tax_LeviesbyClass_2009_present.csv")
raw_tax_by_class_pre_09 <- read_csv("Tax_LeviesbyClass_2003_2008.csv")
raw_tax_by_class <- bind_rows(raw_tax_by_class_to_09, raw_tax_by_class_pre_09)

# Data on total property assessments by class
raw_assessed_by_class <- read_csv("AssessedValuebyClass.csv")
  
# Analysis of tax rates
all_tax_rates_by_class <- read_csv("tax_rates_by_class_all.csv")

walpole_rates <- all_tax_rates_by_class %>% 
  filter(Municipality == "Walpole") %>% 
  group_by(`Fiscal Year`) %>% 
  summarize(walpole_tax = median(Residential)) %>% 
  rename(year = `Fiscal Year`)

state_rates <- all_tax_rates_by_class %>% 
  group_by(`Fiscal Year`) %>% 
  summarize(median_tax = median(Residential)) %>% 
  rename(year = `Fiscal Year`)

state_rates %>% 
  left_join(walpole_rates, by = 'year') %>% 
  rename(`Median for state` = median_tax, Walpole = walpole_tax) %>% 
  gather(`Walpole vs. Massachusetts:`, num, -year) %>% 
  ggplot(aes(x = year, y = num, group = `Walpole vs. Massachusetts:`, color = `Walpole vs. Massachusetts:`)) + 
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  scale_x_continuous(breaks = c(2003:2017))+
  geom_line() +
  xlab("Fiscal Year") + ylab("Property tax rate (per $1,000 in assessed value)") +
  ggtitle("Walpole Property Tax Rates Since 2003 vs. State Median") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = -0.1)) +
  theme(legend.position="bottom") +
  labs(caption = "Source: Massachusetts Division of Local Services")

# Analysis of total tax levies vs. total real estate assesment
raw_assessed_by_class %>% 
  left_join(raw_tax_by_class %>% select(2:4), by = c('NAME', 'Year'= 'YEAR')) %>% 
  rename(res_assessment = Residential, res_tax_levies = RESIDENTIAL) %>% 
  mutate(tax_ratio = res_tax_levies / res_assessment) %>% 
  select(2:4, res_tax_levies, tax_ratio) %>% 
  group_by(Year) %>% 
  summarize(median_ratio = median(tax_ratio)) %>% 
  left_join(raw_assessed_by_class %>% 
              left_join(raw_tax_by_class %>% select(2:4), by = c('NAME', 'Year'= 'YEAR')) %>% 
              rename(res_assessment = Residential, res_tax_levies = RESIDENTIAL) %>% 
              mutate(tax_ratio = res_tax_levies / res_assessment) %>% 
              select(2:4, res_tax_levies, tax_ratio) %>% 
              filter(NAME == "Walpole") %>% 
              group_by(Year) %>% 
              summarize(walpole_ratio = median(tax_ratio)), 
            by = 'Year') %>% 
  filter(!is.na(Year) & Year < 2017) %>% 
  rename(`Median for state` = median_ratio, Walpole = walpole_ratio) %>% 
  gather(`Walpole vs. Massachusetts:`, num, -Year) %>% 
  ggplot(aes(x = Year, y = num, group = `Walpole vs. Massachusetts:`, color = `Walpole vs. Massachusetts:`)) + 
  scale_y_continuous(labels=percent) +
  scale_x_continuous(breaks = c(2003:2016))+
  geom_line() +
  xlab("Fiscal Year") + ylab("Ratio of Property Taxes to Property Assessments") +
  ggtitle("Walpole's Property Taxes Are Pretty Typical") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = -0.1)) +
  theme(legend.position="bottom") +
  labs(caption = "Source: Massachusetts Division of Local Services")

# Analysis of average single family tax bill
tax_bill <- read_csv("avgsinglefamtaxbill.csv")

tax_bill %>% 
  rename(avg_tax_bill = `Single Family Tax Bill*`) %>%
  filter(Municipality == "Walpole") %>% 
  group_by(Year) %>% 
  summarize(walpole_tax = mean(avg_tax_bill))